library(readr)
library(dplyr)
library(tidyr)

pair_input_path <- "/Users/lee/Documents/BigData/context/buffer_backtest_port_month.csv"
weight_input_path <- "/Users/lee/Documents/BigData/context/matching_weight_scenarios.csv"
distance_input_path <- "/Users/lee/Downloads/Distance_Matrix - 距離.csv"

candidate_output_path <- "/Users/lee/Documents/BigData/context/matching_candidate_pairs_fixed_2025.csv"
parent_summary_output_path <- "/Users/lee/Documents/BigData/context/matching_parent_backtest_summary.csv"
parent_detail_output_path <- "/Users/lee/Documents/BigData/context/matching_parent_backtest_pair_month.csv"
summary_output_path <- "/Users/lee/Documents/BigData/context/matching_backtest_summary.csv"
detail_output_path <- "/Users/lee/Documents/BigData/context/matching_backtest_pair_month.csv"

fixed_pressure_scenario <- "roc_prev_best"
fixed_buffer_scenario <- "custom_nearby_c"

normalize_port <- function(x) {
  gsub("台", "臺", x)
}

safe_cor <- function(x, y) {
  valid <- !is.na(x) & !is.na(y)
  if (sum(valid) < 2) {
    return(NA_real_)
  }
  suppressWarnings(cor(x[valid], y[valid]))
}

summarise_matching <- function(df) {
  df %>%
    group_by(scenario, family, phase) %>%
    summarise(
      n_candidate_months = n_distinct(ym),
      n_source_months = n_distinct(paste(ym, source_port)),
      n_pairs = n(),
      pooled_outcome_corr = safe_cor(matching_score, outcome_score),
      global_top1_pair_viable_rate = mean(pair_viable_flag[rank_global == 1], na.rm = TRUE),
      global_top1_pair_stable_rate = mean(pair_stable_flag[rank_global == 1], na.rm = TRUE),
      global_top1_source_need_rate = mean(source_next_high_pressure[rank_global == 1], na.rm = TRUE),
      global_top1_target_low_pressure_rate = mean(target_next_low_pressure[rank_global == 1], na.rm = TRUE),
      global_top1_target_nonpositive_roll_rate = mean(target_next_nonpositive_roll3[rank_global == 1], na.rm = TRUE),
      global_top1_mean_outcome_score = mean(outcome_score[rank_global == 1], na.rm = TRUE),
      source_top1_pair_viable_rate = mean(pair_viable_flag[rank_within_source == 1], na.rm = TRUE),
      source_top1_target_low_pressure_rate = mean(target_next_low_pressure[rank_within_source == 1], na.rm = TRUE),
      source_top1_target_nonpositive_roll_rate = mean(target_next_nonpositive_roll3[rank_within_source == 1], na.rm = TRUE),
      source_top1_mean_outcome_score = mean(outcome_score[rank_within_source == 1], na.rm = TRUE),
      mean_source_top1_distance_factor = mean(distance_factor[rank_within_source == 1], na.rm = TRUE),
      .groups = "drop"
    )
}

scenario_table <- read_csv(pair_input_path, show_col_types = FALSE) %>%
  filter(
    pressure_scenario == fixed_pressure_scenario,
    buffer_scenario == fixed_buffer_scenario,
    eligible_eval
  ) %>%
  mutate(port = normalize_port(port))

distance_lookup <- read_csv(distance_input_path, show_col_types = FALSE) %>%
  rename(source_port = 1) %>%
  mutate(source_port = normalize_port(source_port)) %>%
  pivot_longer(
    cols = -source_port,
    names_to = "target_port",
    values_to = "distance_class"
  ) %>%
  mutate(
    target_port = normalize_port(target_port),
    distance_class = as.numeric(distance_class),
    distance_factor = case_when(
      distance_class == 1 ~ 1.00,
      distance_class == 2 ~ 0.85,
      distance_class == 3 ~ 0.70,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(distance_class), distance_class > 0)

sources <- scenario_table %>%
  filter(status == "高壓力港") %>%
  transmute(
    ym,
    source_port = port,
    source_status = status,
    source_pressure_index = pressure_index,
    source_buffer_index = buffer_index,
    source_empty_net = empty_net,
    source_roll3_empty_net = roll3_empty_net,
    source_next_empty_net = next_empty_net,
    source_next_roll3_empty_net = next_roll3_empty_net,
    source_next_pressure_index = next_pressure_index,
    source_next_buffer_index = next_buffer_index,
    source_next_status = next_status
  )

targets <- scenario_table %>%
  filter(status == "高緩衝港") %>%
  transmute(
    ym,
    target_port = port,
    target_status = status,
    target_pressure_index = pressure_index,
    target_buffer_index = buffer_index,
    target_empty_net = empty_net,
    target_roll3_empty_net = roll3_empty_net,
    target_next_empty_net = next_empty_net,
    target_next_roll3_empty_net = next_roll3_empty_net,
    target_next_pressure_index = next_pressure_index,
    target_next_buffer_index = next_buffer_index,
    target_next_status = next_status
  )

candidate_pairs <- inner_join(
  sources,
  targets,
  by = "ym",
  relationship = "many-to-many"
) %>%
  filter(source_port != target_port) %>%
  left_join(distance_lookup, by = c("source_port", "target_port")) %>%
  filter(!is.na(distance_factor)) %>%
  mutate(
    source_pressure_pct = source_pressure_index,
    target_buffer_pct = target_buffer_index,
    distance_pct = distance_factor,
    source_next_positive_empty_net = source_next_empty_net > 0,
    source_next_positive_roll3 = source_next_roll3_empty_net > 0,
    source_next_high_pressure = source_next_status == "高壓力港",
    target_next_low_pressure = target_next_pressure_index < 0.5,
    target_next_not_high_pressure = target_next_status != "高壓力港",
    target_next_nonpositive_roll3 = target_next_roll3_empty_net <= 0,
    pair_viable_flag = source_next_high_pressure & target_next_low_pressure,
    pair_stable_flag = source_next_positive_roll3 & target_next_nonpositive_roll3,
    outcome_score =
      as.integer(source_next_high_pressure) +
      as.integer(source_next_positive_roll3) +
      as.integer(target_next_low_pressure) +
      as.integer(target_next_nonpositive_roll3)
  ) %>%
  arrange(ym, source_port, target_port)

write_csv(candidate_pairs, candidate_output_path)

matching_weights <- read_csv(weight_input_path, show_col_types = FALSE) %>%
  select(scenario, family, phase, component, weight) %>%
  pivot_wider(
    names_from = component,
    values_from = weight,
    names_prefix = "w_"
  )

pair_scores <- tidyr::crossing(matching_weights, candidate_pairs) %>%
  mutate(
    matching_score =
      w_source_pressure_pct * source_pressure_pct +
      w_target_buffer_pct * target_buffer_pct +
      w_distance_pct * distance_pct
  ) %>%
  group_by(scenario, ym) %>%
  arrange(desc(matching_score), desc(distance_factor), desc(target_buffer_pct), target_port, .by_group = TRUE) %>%
  mutate(rank_global = row_number()) %>%
  ungroup() %>%
  group_by(scenario, ym, source_port) %>%
  arrange(desc(matching_score), desc(distance_factor), desc(target_buffer_pct), target_port, .by_group = TRUE) %>%
  mutate(rank_within_source = row_number()) %>%
  ungroup()

parent_summary <- pair_scores %>%
  filter(phase == "parent") %>%
  summarise_matching() %>%
  arrange(
    desc(global_top1_pair_viable_rate),
    desc(source_top1_target_low_pressure_rate),
    desc(global_top1_mean_outcome_score),
    desc(pooled_outcome_corr),
    scenario
  )

winning_parent <- parent_summary %>%
  slice(1)

selected_family <- winning_parent$family[[1]]

family_scores <- pair_scores %>%
  filter(family == selected_family)

family_summary <- family_scores %>%
  summarise_matching() %>%
  arrange(
    desc(global_top1_pair_viable_rate),
    desc(source_top1_target_low_pressure_rate),
    desc(global_top1_mean_outcome_score),
    desc(pooled_outcome_corr),
    scenario
  ) %>%
  mutate(
    selected_family = selected_family,
    selected_parent = winning_parent$scenario[[1]]
  )

write_csv(parent_summary, parent_summary_output_path)
write_csv(filter(pair_scores, phase == "parent"), parent_detail_output_path)
write_csv(family_summary, summary_output_path)
write_csv(family_scores, detail_output_path)

message("Matching candidate pairs written to: ", candidate_output_path)
message("Matching parent summary written to: ", parent_summary_output_path)
message("Matching parent detail written to: ", parent_detail_output_path)
message("Matching family summary written to: ", summary_output_path)
message("Matching family detail written to: ", detail_output_path)
message("Selected family: ", selected_family)
message("Selected parent scenario: ", winning_parent$scenario[[1]])
