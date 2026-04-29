library(readr)
library(dplyr)

scenario_path <- "/Users/lee/Documents/BigData/context/pressure_scenarios_2025.csv"
summary_output_path <- "/Users/lee/Documents/BigData/context/pressure_backtest_summary.csv"
detail_output_path <- "/Users/lee/Documents/BigData/context/pressure_backtest_port_month.csv"

safe_cor <- function(x, y) {
  valid <- !is.na(x) & !is.na(y)
  if (sum(valid) < 2) {
    return(NA_real_)
  }
  suppressWarnings(cor(x[valid], y[valid]))
}

pressure_table <- read_csv(scenario_path, show_col_types = FALSE) %>%
  arrange(scenario, port, ym) %>%
  group_by(scenario, port) %>%
  mutate(
    next_empty_net = lead(empty_net, 1),
    next_roll3_empty_net = lead(roll3_empty_net, 1),
    next_pressure_index = lead(pressure_index, 1)
  ) %>%
  ungroup() %>%
  mutate(
    eligible_eval = enough_history & !is.na(next_empty_net),
    high_pressure_flag = eligible_eval & pressure_status == "高壓力港",
    next_positive_empty_net = next_empty_net > 0,
    next_positive_roll3 = next_roll3_empty_net > 0
  )

summary_table <- pressure_table %>%
  filter(eligible_eval) %>%
  group_by(scenario) %>%
  summarise(
    n_rows = n(),
    n_eval_rows = sum(eligible_eval),
    first_eval_ym = min(ym),
    last_eval_ym = max(ym),
    pressure_lead_corr = safe_cor(pressure_index, next_empty_net),
    pressure_roll_lead_corr = safe_cor(pressure_index, next_roll3_empty_net),
    high_pressure_hit_rate = mean(next_positive_empty_net[high_pressure_flag], na.rm = TRUE),
    high_pressure_roll_hit_rate = mean(next_positive_roll3[high_pressure_flag], na.rm = TRUE),
    mean_pressure_index = mean(pressure_index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pressure_signal_score = pressure_lead_corr + pressure_roll_lead_corr,
    classification_score = high_pressure_hit_rate + high_pressure_roll_hit_rate,
    pressure_overall_score = pressure_signal_score + classification_score
  ) %>%
  arrange(desc(pressure_overall_score))

write_csv(summary_table, summary_output_path)
write_csv(pressure_table, detail_output_path)

message("Pressure-only backtest summary written to: ", summary_output_path)
message("Pressure-only backtest detail written to: ", detail_output_path)

message("")
message("Pressure-only backtest ranking:")
print(summary_table, n = nrow(summary_table))

message("")
message("Top scenario recommendation:")
summary_table %>%
  select(
    scenario,
    pressure_signal_score,
    classification_score,
    pressure_overall_score
  ) %>%
  slice(1) %>%
  print(n = 1)
