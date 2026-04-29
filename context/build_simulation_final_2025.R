library(readr)
library(dplyr)
library(purrr)

status_input_path <- "/Users/lee/Documents/BigData/context/status_final_2025.csv"
matching_input_path <- "/Users/lee/Documents/BigData/context/matching_final_2025.csv"
history_input_path <- "/Users/lee/Documents/BigData/context/analysis_table_history_2025.csv"
output_path <- "/Users/lee/Documents/BigData/context/simulation_final_2025.csv"

analysis_universe_label <- "基隆港、臺北港、臺中港、高雄港"

pressure_weights <- c(
  pressure_flow_pct = 0.22,
  pressure_net_pct = 0.60,
  pressure_roll_pct = 0.18
)

buffer_weights <- c(
  buffer_space_pct = 0.45,
  buffer_net_pct = 0.40,
  buffer_export_pct = 0.15
)

move_scenarios <- tibble(
  simulation_scenario = c("no_move", "move_25pct", "move_50pct", "move_75pct"),
  move_share = c(0.00, 0.25, 0.50, 0.75)
)

safe_divide <- function(x, y) {
  ifelse(is.na(y) | y == 0, NA_real_, x / y)
}

current_percent_rank <- function(x) {
  valid <- !is.na(x)
  if (sum(valid) <= 1) {
    return(0)
  }

  ranks <- rank(x[valid], ties.method = "min")
  (ranks[length(ranks)] - 1) / (length(ranks) - 1)
}

base_history <- read_csv(history_input_path, show_col_types = FALSE) %>%
  arrange(port, ym) %>%
  mutate(
    pressure_flow_raw = safe_divide(empty_in, throughput),
    pressure_net_raw = safe_divide(pmax(empty_net, 0), throughput),
    pressure_roll_raw = safe_divide(pmax(roll3_empty_net, 0), throughput),
    buffer_space_raw = pmax(1 - pressure_flow_raw, 0),
    buffer_net_raw = safe_divide(pmax(-roll3_empty_net, 0), throughput),
    buffer_export_raw = export_pull
  )

status_table <- read_csv(status_input_path, show_col_types = FALSE) %>%
  arrange(ym, port)

matching_table <- read_csv(matching_input_path, show_col_types = FALSE) %>%
  arrange(ym, rank_global, source_port, target_port)

compute_projected_port <- function(port_history, role, move_amount) {
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
  } else {
    stop("role must be 'source' or 'target'")
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

  pressure_flow_pct_new <- current_percent_rank(pressure_flow_hist)
  pressure_net_pct_new <- current_percent_rank(pressure_net_hist)
  pressure_roll_pct_new <- current_percent_rank(pressure_roll_hist)
  buffer_space_pct_new <- current_percent_rank(buffer_space_hist)
  buffer_net_pct_new <- current_percent_rank(buffer_net_hist)
  buffer_export_pct_new <- current_percent_rank(buffer_export_hist)

  pressure_index_new <-
    pressure_weights[["pressure_flow_pct"]] * pressure_flow_pct_new +
    pressure_weights[["pressure_net_pct"]] * pressure_net_pct_new +
    pressure_weights[["pressure_roll_pct"]] * pressure_roll_pct_new

  buffer_index_new <-
    buffer_weights[["buffer_space_pct"]] * buffer_space_pct_new +
    buffer_weights[["buffer_net_pct"]] * buffer_net_pct_new +
    buffer_weights[["buffer_export_pct"]] * buffer_export_pct_new

  tibble(
    empty_in_after = empty_in_new,
    empty_out_after = empty_out_new,
    empty_net_after = empty_net_new,
    roll3_empty_net_after = roll3_empty_net_new,
    pressure_flow_raw_after = pressure_flow_raw_new,
    pressure_net_raw_after = pressure_net_raw_new,
    pressure_roll_raw_after = pressure_roll_raw_new,
    buffer_space_raw_after = buffer_space_raw_new,
    buffer_net_raw_after = buffer_net_raw_new,
    buffer_export_raw_after = buffer_export_raw_new,
    pressure_flow_pct_after = pressure_flow_pct_new,
    pressure_net_pct_after = pressure_net_pct_new,
    pressure_roll_pct_after = pressure_roll_pct_new,
    buffer_space_pct_after = buffer_space_pct_new,
    buffer_net_pct_after = buffer_net_pct_new,
    buffer_export_pct_after = buffer_export_pct_new,
    pressure_index_after = pressure_index_new,
    buffer_index_after = buffer_index_new
  )
}

simulate_pair <- function(pair_row) {
  ym_value <- pair_row$ym[[1]]
  source_port_value <- pair_row$source_port[[1]]
  target_port_value <- pair_row$target_port[[1]]

  month_status <- status_table %>%
    filter(ym == ym_value) %>%
    transmute(
      ym,
      port,
      pressure_index_before = pressure_index,
      buffer_index_before = buffer_index,
      roll3_empty_net_before = roll3_empty_net,
      status_before = status
    )

  source_history <- base_history %>%
    filter(port == source_port_value, ym <= ym_value) %>%
    arrange(ym)

  target_history <- base_history %>%
    filter(port == target_port_value, ym <= ym_value) %>%
    arrange(ym)

  source_move_base <- max(pair_row$source_empty_net[[1]], 0)

  map_dfr(seq_len(nrow(move_scenarios)), function(i) {
    scenario_row <- move_scenarios[i, ]
    move_share_value <- scenario_row$move_share[[1]]
    move_amount_value <- source_move_base * move_share_value

    source_projection <- compute_projected_port(
      port_history = source_history,
      role = "source",
      move_amount = move_amount_value
    )

    target_projection <- compute_projected_port(
      port_history = target_history,
      role = "target",
      move_amount = move_amount_value
    )

    projected_month <- month_status %>%
      mutate(
        projected_pressure_index = case_when(
          port == source_port_value ~ source_projection$pressure_index_after[[1]],
          port == target_port_value ~ target_projection$pressure_index_after[[1]],
          TRUE ~ pressure_index_before
        ),
        projected_buffer_index = case_when(
          port == source_port_value ~ source_projection$buffer_index_after[[1]],
          port == target_port_value ~ target_projection$buffer_index_after[[1]],
          TRUE ~ buffer_index_before
        ),
        projected_roll3_empty_net = case_when(
          port == source_port_value ~ source_projection$roll3_empty_net_after[[1]],
          port == target_port_value ~ target_projection$roll3_empty_net_after[[1]],
          TRUE ~ roll3_empty_net_before
        )
      )

    projected_high_pressure_cut <- quantile(projected_month$projected_pressure_index, 0.75, na.rm = TRUE, type = 7)
    projected_high_buffer_cut <- quantile(projected_month$projected_buffer_index, 0.75, na.rm = TRUE, type = 7)
    projected_low_pressure_cut <- quantile(projected_month$projected_pressure_index, 0.50, na.rm = TRUE, type = 7)

    projected_month <- projected_month %>%
      mutate(
        projected_status = case_when(
          projected_pressure_index >= projected_high_pressure_cut & projected_roll3_empty_net > 0 ~ "高壓力港",
          projected_buffer_index >= projected_high_buffer_cut & projected_pressure_index <= projected_low_pressure_cut ~ "高緩衝港",
          TRUE ~ "正常港"
        )
      )

    source_status_after <- projected_month %>%
      filter(port == source_port_value) %>%
      pull(projected_status)

    target_status_after <- projected_month %>%
      filter(port == target_port_value) %>%
      pull(projected_status)

    tibble(
      ym = ym_value,
      analysis_universe = analysis_universe_label,
      matching_scenario = pair_row$matching_scenario[[1]],
      simulation_scenario = scenario_row$simulation_scenario[[1]],
      move_share = move_share_value,
      move_amount = move_amount_value,
      source_port = source_port_value,
      target_port = target_port_value,
      distance_class = pair_row$distance_class[[1]],
      distance_factor = pair_row$distance_factor[[1]],
      matching_score = pair_row$matching_score[[1]],
      rank_global = pair_row$rank_global[[1]],
      rank_within_source = pair_row$rank_within_source[[1]],
      source_status_before = pair_row$source_status[[1]],
      source_pressure_index_before = pair_row$source_pressure_index[[1]],
      source_buffer_index_before = pair_row$source_buffer_index[[1]],
      source_empty_net_before = pair_row$source_empty_net[[1]],
      source_roll3_empty_net_before = pair_row$source_roll3_empty_net[[1]],
      source_pressure_index_after = source_projection$pressure_index_after[[1]],
      source_buffer_index_after = source_projection$buffer_index_after[[1]],
      source_empty_in_after = source_projection$empty_in_after[[1]],
      source_empty_out_after = source_projection$empty_out_after[[1]],
      source_empty_net_after = source_projection$empty_net_after[[1]],
      source_roll3_empty_net_after = source_projection$roll3_empty_net_after[[1]],
      source_status_after = source_status_after,
      target_status_before = pair_row$target_status[[1]],
      target_pressure_index_before = pair_row$target_pressure_index[[1]],
      target_buffer_index_before = pair_row$target_buffer_index[[1]],
      target_empty_net_before = pair_row$target_empty_net[[1]],
      target_roll3_empty_net_before = pair_row$target_roll3_empty_net[[1]],
      target_pressure_index_after = target_projection$pressure_index_after[[1]],
      target_buffer_index_after = target_projection$buffer_index_after[[1]],
      target_empty_in_after = target_projection$empty_in_after[[1]],
      target_empty_out_after = target_projection$empty_out_after[[1]],
      target_empty_net_after = target_projection$empty_net_after[[1]],
      target_roll3_empty_net_after = target_projection$roll3_empty_net_after[[1]],
      target_status_after = target_status_after,
      projected_high_pressure_cut = projected_high_pressure_cut,
      projected_high_buffer_cut = projected_high_buffer_cut,
      projected_low_pressure_cut = projected_low_pressure_cut,
      source_pressure_delta = source_projection$pressure_index_after[[1]] - pair_row$source_pressure_index[[1]],
      target_pressure_delta = target_projection$pressure_index_after[[1]] - pair_row$target_pressure_index[[1]],
      source_relieved = source_status_after != "高壓力港",
      target_stays_safe = target_projection$pressure_index_after[[1]] < projected_high_pressure_cut,
      target_stays_non_high_pressure = target_status_after != "高壓力港",
      pair_success = source_status_after != "高壓力港" &
        target_projection$pressure_index_after[[1]] < projected_high_pressure_cut
    )
  })
}

simulation_table <- map_dfr(seq_len(nrow(matching_table)), function(i) {
  simulate_pair(matching_table[i, ])
}) %>%
  group_by(ym, source_port, target_port) %>%
  mutate(
    max_safe_move_share = if (any(target_stays_safe & move_share > 0)) {
      max(move_share[target_stays_safe & move_share > 0])
    } else {
      0
    },
    max_success_move_share = if (any(pair_success & move_share > 0)) {
      max(move_share[pair_success & move_share > 0])
    } else {
      0
    },
    is_max_safe_share = move_share == max_safe_move_share,
    is_max_success_share = move_share == max_success_move_share
  ) %>%
  ungroup() %>%
  arrange(ym, source_port, target_port, move_share)

write_csv(simulation_table, output_path)

message("Simulation final table written to: ", output_path)
