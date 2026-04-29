library(readr)
library(dplyr)

input_path <- "/Users/lee/Documents/BigData/context/buffer_fixed_scenarios_2025.csv"
output_path <- "/Users/lee/Documents/BigData/context/status_final_2025.csv"

final_pressure_scenario <- "roc_prev_best"
final_buffer_scenario <- "custom_nearby_c"
excluded_ports <- c("安平港")

status_table <- read_csv(input_path, show_col_types = FALSE) %>%
  filter(
    pressure_scenario == final_pressure_scenario,
    buffer_scenario == final_buffer_scenario,
    !port %in% excluded_ports
  ) %>%
  group_by(ym) %>%
  mutate(
    high_pressure_cut = quantile(pressure_index, 0.75, na.rm = TRUE, type = 7),
    high_buffer_cut = quantile(buffer_index, 0.75, na.rm = TRUE, type = 7),
    low_pressure_cut = quantile(pressure_index, 0.50, na.rm = TRUE, type = 7),
    status = case_when(
      enough_history & pressure_index >= high_pressure_cut & roll3_empty_net > 0 ~ "高壓力港",
      enough_history & buffer_index >= high_buffer_cut & pressure_index <= low_pressure_cut ~ "高緩衝港",
      TRUE ~ "正常港"
    )
  ) %>%
  ungroup() %>%
  transmute(
    ym,
    port,
    history_n,
    pressure_scenario,
    buffer_scenario,
    analysis_universe = "基隆港、臺北港、臺中港、高雄港",
    empty_in,
    empty_out,
    full_in,
    full_out,
    throughput,
    empty_net,
    empty_total_flow,
    full_total_flow,
    export_pull,
    roll3_empty_net,
    cum_empty_net,
    pressure_flow_raw,
    pressure_net_raw,
    pressure_roll_raw,
    buffer_space_raw,
    buffer_net_raw,
    buffer_export_raw,
    pressure_index,
    buffer_index,
    high_pressure_cut,
    high_buffer_cut,
    low_pressure_cut,
    status
  ) %>%
  arrange(ym, port)

write_csv(status_table, output_path)

message("Status final table written to: ", output_path)
