library(readr)
library(dplyr)

input_path <- "/Users/lee/Documents/BigData/context/simulation_final_2025.csv"
output_path <- "/Users/lee/Documents/BigData/context/simulation_recommendation_final_2025.csv"

pick_recommendation_row <- function(df) {
  max_safe_move_share <- max(df$max_safe_move_share, na.rm = TRUE)
  max_success_move_share <- max(df$max_success_move_share, na.rm = TRUE)

  if (max_success_move_share > 0) {
    chosen <- df %>%
      filter(abs(move_share - max_success_move_share) < 1e-9) %>%
      arrange(desc(move_share), desc(move_amount)) %>%
      slice(1)

    result_class <- "完全解壓"
    reason_code <- "both_ok"
    display_message <- sprintf(
      "建議移轉 %.0f%%（%.2f TEU），可讓 source 退出高壓，且 target 仍維持安全。",
      chosen$move_share[[1]] * 100,
      chosen$move_amount[[1]]
    )
  } else if (max_safe_move_share > 0) {
    chosen <- df %>%
      filter(abs(move_share - max_safe_move_share) < 1e-9) %>%
      arrange(desc(move_share), desc(move_amount)) %>%
      slice(1)

    result_class <- "可緩解但未解壓"
    reason_code <- "source_still_high"
    display_message <- sprintf(
      "建議移轉 %.0f%%（%.2f TEU），target 可安全承接，但 source 仍未退出高壓，屬於先降壓的部分緩解方案。",
      chosen$move_share[[1]] * 100,
      chosen$move_amount[[1]]
    )
  } else {
    chosen <- df %>%
      filter(abs(move_share - 0) < 1e-9) %>%
      slice(1)

    result_class <- "不建議調度"
    reason_code <- "target_not_safe"
    display_message <- "目前正向移轉情境下無安全承接比例，若直接調度，target 可能承壓過高。"
  }

  chosen %>%
    transmute(
      ym,
      analysis_universe,
      matching_scenario,
      source_port,
      target_port,
      distance_class,
      distance_factor,
      matching_score,
      rank_global,
      rank_within_source,
      simulation_result_class = result_class,
      reason_code = reason_code,
      display_message = display_message,
      recommended_simulation_scenario = simulation_scenario,
      recommended_move_share = move_share,
      recommended_move_amount = move_amount,
      source_status_before,
      source_status_after,
      target_status_before,
      target_status_after,
      source_pressure_index_before,
      source_pressure_index_after,
      target_pressure_index_before,
      target_pressure_index_after,
      projected_high_pressure_cut,
      projected_high_buffer_cut,
      projected_low_pressure_cut,
      source_pressure_delta,
      target_pressure_delta,
      source_relieved,
      target_stays_safe,
      target_stays_non_high_pressure,
      pair_success,
      max_safe_move_share,
      max_success_move_share
    )
}

simulation_table <- read_csv(input_path, show_col_types = FALSE) %>%
  arrange(ym, source_port, target_port, move_share)

recommendation_table <- simulation_table %>%
  group_by(ym, analysis_universe, matching_scenario, source_port, target_port) %>%
  group_split() %>%
  lapply(pick_recommendation_row) %>%
  bind_rows() %>%
  ungroup() %>%
  arrange(ym, rank_global, source_port, target_port)

write_csv(recommendation_table, output_path)

message("Simulation recommendation table written to: ", output_path)
