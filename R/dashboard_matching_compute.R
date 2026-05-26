build_formal_candidate_rows <- function(ym_value) {
  reco_row <- simulation_reco %>%
    filter(ym == ym_value) %>%
    slice(1)

  sim_rows <- simulation_final %>%
    filter(ym == ym_value, simulation_scenario != "no_move") %>%
    left_join(
      distance_detail %>%
        select(source_port, target_port, distance_km, eta_days),
      by = c("source_port", "target_port")
    )

  if (nrow(sim_rows) == 0) {
    return(tibble())
  }

  recommended_share <- if (nrow(reco_row) > 0) reco_row$recommended_move_share[[1]] else NA_real_
  recommended_message <- if (nrow(reco_row) > 0) reco_row$display_message[[1]] else NA_character_

  sim_rows %>%
    mutate(
      candidate_key = paste0(target_port, "::", simulation_scenario),
      simulation_result = case_when(
        pair_success ~ "完全解壓",
        target_stays_safe ~ "可緩解但未解壓",
        TRUE ~ "不建議調度"
      ),
      result_rank = simulation_result_rank(simulation_result),
      pair_tier = case_when(
        !is.na(recommended_share) & abs(move_share - recommended_share) < 1e-8 ~ "正式推薦",
        TRUE ~ "觀察候選"
      ),
      scenario_label = paste0(round(move_share * 100), "%"),
      summary_text = case_when(
        pair_tier == "正式推薦" & !is.na(recommended_message) ~ recommended_message,
        TRUE ~ paste0("模擬移轉 ", round(move_share * 100), "%（", fmt_num(move_amount, 0), " TEU）。")
      ),
      source_status = source_status_before,
      target_status = target_status_before,
      source_pressure = source_pressure_index_before,
      source_pressure_after = source_pressure_index_after,
      source_buffer = source_buffer_index_before,
      source_empty_net = source_empty_net_before,
      target_pressure = target_pressure_index_before,
      target_pressure_after = target_pressure_index_after,
      target_buffer = target_buffer_index_before,
      target_empty_net = target_empty_net_before,
      pressure_before_pct = round(target_pressure_index_before * 100),
      pressure_after_pct = round(target_pressure_index_after * 100),
      pressure_increase_pct = pmax(0, round(target_pressure_index_after * 100) - round(target_pressure_index_before * 100)),
      source_pressure_pct = round(source_pressure_index_before * 100),
      source_pressure_after_pct = round(source_pressure_index_after * 100),
      relief_pct = if_else(
        source_pressure_index_before <= 0,
        0,
        round(pmax(0, (source_pressure_index_before - source_pressure_index_after) / source_pressure_index_before) * 100)
      )
    ) %>%
    transmute(
      ym,
      source_port,
      target_port,
      candidate_key,
      source_status,
      target_status,
      source_pressure,
      source_pressure_after,
      source_buffer,
      source_empty_net,
      target_pressure,
      target_pressure_after,
      target_buffer,
      target_empty_net,
      matching_score,
      move_amount,
      distance_km,
      eta_days,
      simulation_result,
      pair_tier,
      scenario_label,
      summary_text,
      pressure_before_pct,
      pressure_after_pct,
      pressure_increase_pct,
      source_pressure_pct,
      source_pressure_after_pct,
      relief_pct,
      result_rank
    ) %>%
    arrange(result_rank, desc(pair_tier == "正式推薦"), desc(move_amount))
}

default_source_for_month <- function(month_status_tbl) {
  if (nrow(month_status_tbl) == 0) {
    return("高雄港")
  }

  high_row <- month_status_tbl %>%
    filter(status == "高壓力港") %>%
    arrange(desc(pressure_index), desc(empty_net)) %>%
    slice(1)

  if (nrow(high_row) > 0) {
    return(high_row$port[[1]])
  }

  month_status_tbl %>%
    arrange(desc(pressure_index), desc(empty_net)) %>%
    slice(1) %>%
    pull(port)
}

default_move_amount <- function(month_status_tbl, source_port) {
  base_row <- month_status_tbl %>%
    filter(port == !!source_port) %>%
    slice(1)

  if (nrow(base_row) == 0) {
    return(2500)
  }

  move_base <- max(base_row$empty_net[[1]], 0)
  if (is.finite(move_base) && move_base > 0) {
    return(round(move_base * 0.50))
  }

  2500
}

compute_projected_port <- function(port_history, role, move_amount, pressure_weights, buffer_weights) {
  current_row <- slice_tail(port_history, n = 1)

  empty_in_new <- current_row$empty_in
  empty_out_new <- current_row$empty_out
  empty_net_new <- current_row$empty_net
  roll3_empty_net_new <- current_row$roll3_empty_net

  if (role == "source") {
    empty_out_new <- empty_out_new + move_amount
    empty_net_new <- empty_net_new - move_amount
    roll3_empty_net_new <- roll3_empty_net_new - (move_amount / 3)
  } else if (role == "target") {
    empty_in_new <- empty_in_new + move_amount
    empty_net_new <- empty_net_new + move_amount
    roll3_empty_net_new <- roll3_empty_net_new + (move_amount / 3)
  }

  pressure_flow_raw_new <- safe_divide(empty_in_new, current_row$throughput)
  pressure_net_raw_new <- safe_divide(pmax(empty_net_new, 0), current_row$throughput)
  pressure_roll_raw_new <- safe_divide(pmax(roll3_empty_net_new, 0), current_row$throughput)
  buffer_space_raw_new <- pmax(1 - pressure_flow_raw_new, 0)
  buffer_net_raw_new <- safe_divide(pmax(-roll3_empty_net_new, 0), current_row$throughput)
  buffer_export_raw_new <- current_row$export_pull

  pressure_flow_hist <- port_history$pressure_flow_raw
  pressure_net_hist <- port_history$pressure_net_raw
  pressure_roll_hist <- port_history$pressure_roll_raw
  buffer_space_hist <- port_history$buffer_space_raw
  buffer_net_hist <- port_history$buffer_net_raw
  buffer_export_hist <- port_history$buffer_export_raw

  pressure_flow_hist[length(pressure_flow_hist)] <- pressure_flow_raw_new
  pressure_net_hist[length(pressure_net_hist)] <- pressure_net_raw_new
  pressure_roll_hist[length(pressure_roll_hist)] <- pressure_roll_raw_new
  buffer_space_hist[length(buffer_space_hist)] <- buffer_space_raw_new
  buffer_net_hist[length(buffer_net_hist)] <- buffer_net_raw_new
  buffer_export_hist[length(buffer_export_hist)] <- buffer_export_raw_new

  pressure_index_new <-
    pressure_weights[["flow"]] * current_percent_rank(pressure_flow_hist) +
    pressure_weights[["net"]] * current_percent_rank(pressure_net_hist) +
    pressure_weights[["roll"]] * current_percent_rank(pressure_roll_hist)

  buffer_index_new <-
    buffer_weights[["space"]] * current_percent_rank(buffer_space_hist) +
    buffer_weights[["netout"]] * current_percent_rank(buffer_net_hist) +
    buffer_weights[["export"]] * current_percent_rank(buffer_export_hist)

  tibble(
    empty_in_after = empty_in_new,
    empty_out_after = empty_out_new,
    empty_net_after = empty_net_new,
    roll3_empty_net_after = roll3_empty_net_new,
    pressure_index_after = pressure_index_new,
    buffer_index_after = buffer_index_new
  )
}

simulate_pair <- function(
  ym_value,
  source_port,
  target_port,
  move_amount,
  status_table,
  history_table,
  pressure_weights,
  buffer_weights
) {
  month_status <- status_table %>%
    filter(ym == ym_value) %>%
    transmute(
      port,
      pressure_index_before = pressure_index,
      buffer_index_before = buffer_index,
      roll3_empty_net_before = roll3_empty_net,
      status_before = status
    )

  source_row <- month_status %>% filter(port == source_port) %>% slice(1)
  target_row <- month_status %>% filter(port == target_port) %>% slice(1)

  source_history <- history_table %>% filter(port == source_port, ym <= ym_value) %>% arrange(ym)
  target_history <- history_table %>% filter(port == target_port, ym <= ym_value) %>% arrange(ym)

  source_projection <- compute_projected_port(source_history, "source", move_amount, pressure_weights, buffer_weights)
  target_projection <- compute_projected_port(target_history, "target", move_amount, pressure_weights, buffer_weights)

  projected_month <- month_status %>%
    mutate(
      projected_pressure_index = case_when(
        port == source_port ~ source_projection$pressure_index_after[[1]],
        port == target_port ~ target_projection$pressure_index_after[[1]],
        TRUE ~ pressure_index_before
      ),
      projected_buffer_index = case_when(
        port == source_port ~ source_projection$buffer_index_after[[1]],
        port == target_port ~ target_projection$buffer_index_after[[1]],
        TRUE ~ buffer_index_before
      ),
      projected_roll3_empty_net = case_when(
        port == source_port ~ source_projection$roll3_empty_net_after[[1]],
        port == target_port ~ target_projection$roll3_empty_net_after[[1]],
        TRUE ~ roll3_empty_net_before
      )
    )

  high_pressure_cut <- quantile(projected_month$projected_pressure_index, 0.75, na.rm = TRUE, type = 7)
  high_buffer_cut <- quantile(projected_month$projected_buffer_index, 0.75, na.rm = TRUE, type = 7)
  low_pressure_cut <- quantile(projected_month$projected_pressure_index, 0.50, na.rm = TRUE, type = 7)

  projected_month <- projected_month %>%
    mutate(
      projected_status = case_when(
        projected_pressure_index >= high_pressure_cut & projected_roll3_empty_net > 0 ~ "高壓力港",
        projected_buffer_index >= high_buffer_cut & projected_pressure_index <= low_pressure_cut ~ "高緩衝港",
        TRUE ~ "正常港"
      )
    )

  source_status_after <- projected_month %>% filter(port == source_port) %>% pull(projected_status)
  target_status_after <- projected_month %>% filter(port == target_port) %>% pull(projected_status)

  result_class <- simulation_result_class(
    source_status_after = source_status_after,
    target_pressure_after = target_projection$pressure_index_after[[1]],
    high_pressure_cut = high_pressure_cut
  )

  tibble(
    ym = ym_value,
    source_port = source_port,
    target_port = target_port,
    move_amount = move_amount,
    source_status_before = source_row$status_before[[1]],
    target_status_before = target_row$status_before[[1]],
    source_status_after = source_status_after,
    target_status_after = target_status_after,
    source_pressure_before = source_row$pressure_index_before[[1]],
    source_pressure_after = source_projection$pressure_index_after[[1]],
    target_pressure_before = target_row$pressure_index_before[[1]],
    target_pressure_after = target_projection$pressure_index_after[[1]],
    source_buffer_after = source_projection$buffer_index_after[[1]],
    target_buffer_after = target_projection$buffer_index_after[[1]],
    source_empty_net_after = source_projection$empty_net_after[[1]],
    target_empty_net_after = target_projection$empty_net_after[[1]],
    projected_high_pressure_cut = high_pressure_cut,
    projected_high_buffer_cut = high_buffer_cut,
    projected_low_pressure_cut = low_pressure_cut,
    simulation_result = result_class
  )
}

build_candidate_table <- function(
  ym_value,
  source_port,
  move_amount,
  status_table,
  history_table,
  matching_weights,
  pressure_weights,
  buffer_weights
) {
  snapshot <- status_table %>% filter(ym == ym_value)
  source_row <- snapshot %>% filter(port == source_port) %>% slice(1)

  candidates <- snapshot %>%
    filter(port != source_port) %>%
    transmute(
      target_port = port,
      target_status = status,
      target_pressure = pressure_index,
      target_buffer = buffer_index,
      target_empty_net = empty_net
    ) %>%
    left_join(
      distance_class_lookup %>%
        filter(source_port == !!source_port) %>%
        select(target_port, distance_class, distance_factor),
      by = c("target_port")
    ) %>%
    left_join(
      distance_detail %>%
        filter(source_port == !!source_port) %>%
        select(target_port, distance_km, eta_days),
      by = c("target_port")
    ) %>%
    filter(!is.na(distance_factor)) %>%
    mutate(
      ym = ym_value,
      source_port = source_port,
      source_status = source_row$status[[1]],
      source_pressure = source_row$pressure_index[[1]],
      source_buffer = source_row$buffer_index[[1]],
      source_empty_net = source_row$empty_net[[1]],
      matching_score =
        matching_weights[["source"]] * source_pressure +
        matching_weights[["target"]] * target_buffer +
        matching_weights[["distance"]] * distance_factor
    )

  sim_rows <- lapply(seq_len(nrow(candidates)), function(i) {
    row <- candidates[i, ]
    simulate_pair(
      ym_value = ym_value,
      source_port = source_port,
      target_port = row$target_port[[1]],
      move_amount = move_amount,
      status_table = status_table,
      history_table = history_table,
      pressure_weights = pressure_weights,
      buffer_weights = buffer_weights
    )
  }) %>%
    bind_rows()

  candidates %>%
    left_join(sim_rows, by = c("ym" = "ym", "source_port", "target_port")) %>%
    mutate(
      candidate_key = target_port,
      summary_text = case_when(
        simulation_result == "完全解壓" ~ "可讓 source 退出高壓，且 target 保持安全",
        simulation_result == "可緩解但未解壓" ~ "可部分緩解 source 壓力，target 仍可承接",
        TRUE ~ "調度後 target 壓力可能過高，僅供觀察"
      ),
      pressure_before_pct = round(target_pressure * 100),
      pressure_after_pct = round(target_pressure_after * 100),
      pressure_increase_pct = pmax(0, pressure_after_pct - pressure_before_pct),
      source_pressure_pct = round(source_pressure * 100),
      source_pressure_after_pct = round(source_pressure_after * 100),
      relief_pct = ifelse(
        source_pressure <= 0,
        0,
        round(pmax(0, (source_pressure - source_pressure_after) / source_pressure) * 100)
      ),
      result_rank = simulation_result_rank(simulation_result),
      eta_days = eta_days %||% distance_class,
      distance_km = distance_km %||% (distance_class * 200)
    ) %>%
    select(
      ym,
      source_port,
      source_status,
      source_pressure,
      source_buffer,
      source_empty_net,
      target_port,
      candidate_key,
      target_status,
      target_pressure,
      target_buffer,
      target_empty_net,
      distance_class,
      distance_factor,
      distance_km,
      eta_days,
      matching_score,
      move_amount,
      source_status_before,
      target_status_before,
      source_status_after,
      target_status_after,
      source_pressure_before,
      source_pressure_after,
      target_pressure_before,
      target_pressure_after,
      source_buffer_after,
      target_buffer_after,
      source_empty_net_after,
      target_empty_net_after,
      projected_high_pressure_cut,
      projected_high_buffer_cut,
      projected_low_pressure_cut,
      simulation_result,
      summary_text,
      pressure_before_pct,
      pressure_after_pct,
      pressure_increase_pct,
      source_pressure_pct,
      source_pressure_after_pct,
      relief_pct,
      result_rank
    ) %>%
    arrange(result_rank, desc(matching_score), desc(target_buffer), target_port) %>%
    mutate(
      pair_tier = case_when(
        row_number() == 1 & target_status == "高緩衝港" ~ "正式推薦",
        target_status == "高緩衝港" ~ "正式候選",
        TRUE ~ "觀察候選"
      ),
      tier_rank = case_when(
        pair_tier == "正式推薦" ~ 1L,
        pair_tier == "正式候選" ~ 2L,
        TRUE ~ 3L
      )
    ) %>%
    arrange(result_rank, tier_rank, desc(matching_score), target_port)
}
