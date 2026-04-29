library(readr)
library(dplyr)
library(tidyr)
library(purrr)

history_path <- "/Users/lee/Documents/BigData/context/analysis_table_history_2025.csv"
weights_path <- "/Users/lee/Documents/BigData/context/weight_scenarios.csv"
output_path <- "/Users/lee/Documents/BigData/context/buffer_fixed_scenarios_2025.csv"

fixed_pressure_scenario <- "roc_prev_best"

history_table <- read_csv(history_path, show_col_types = FALSE) %>%
  arrange(port, ym)

weight_scenarios <- read_csv(weights_path, show_col_types = FALSE)

pressure_weights <- weight_scenarios %>%
  filter(index_type == "pressure", scenario == fixed_pressure_scenario)

buffer_weights <- weight_scenarios %>%
  filter(index_type == "buffer")

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

base_history <- history_table %>%
  mutate(
    pressure_flow_raw = safe_divide(empty_in, throughput),
    pressure_net_raw = safe_divide(pmax(empty_net, 0), throughput),
    pressure_roll_raw = safe_divide(pmax(roll3_empty_net, 0), throughput),
    buffer_space_raw = pmax(1 - pressure_flow_raw, 0),
    buffer_net_raw = safe_divide(pmax(-roll3_empty_net, 0), throughput),
    buffer_export_raw = export_pull
  ) %>%
  arrange(port, ym)

build_current_snapshot <- function(cutoff_ym) {
  train_slice <- base_history %>%
    filter(ym <= cutoff_ym) %>%
    arrange(port, ym)

  train_slice %>%
    group_by(port) %>%
    group_modify(~ {
      port_history <- .x
      current_row <- slice_tail(port_history, n = 1)

      tibble(
        ym = current_row$ym,
        history_n = nrow(port_history),
        empty_in = current_row$empty_in,
        empty_out = current_row$empty_out,
        full_in = current_row$full_in,
        full_out = current_row$full_out,
        throughput = current_row$throughput,
        empty_net = current_row$empty_net,
        empty_total_flow = current_row$empty_total_flow,
        full_total_flow = current_row$full_total_flow,
        empty_share = current_row$empty_share,
        net_pressure_raw = current_row$net_pressure_raw,
        export_pull = current_row$export_pull,
        roll3_empty_net = current_row$roll3_empty_net,
        cum_empty_net = current_row$cum_empty_net,
        roll3_pressure_raw = current_row$roll3_pressure_raw,
        positive_net_streak = current_row$positive_net_streak,
        buffer_raw = current_row$buffer_raw,
        pressure_pct = current_row$pressure_pct,
        buffer_pct = current_row$buffer_pct,
        pressure_z = current_row$pressure_z,
        buffer_z = current_row$buffer_z,
        pressure_flow_raw = current_row$pressure_flow_raw,
        pressure_net_raw = current_row$pressure_net_raw,
        pressure_roll_raw = current_row$pressure_roll_raw,
        buffer_space_raw = current_row$buffer_space_raw,
        buffer_net_raw = current_row$buffer_net_raw,
        buffer_export_raw = current_row$buffer_export_raw,
        pressure_flow_pct = current_percent_rank(port_history$pressure_flow_raw),
        pressure_net_pct = current_percent_rank(port_history$pressure_net_raw),
        pressure_roll_pct = current_percent_rank(port_history$pressure_roll_raw),
        buffer_space_pct = current_percent_rank(port_history$buffer_space_raw),
        buffer_net_pct = current_percent_rank(port_history$buffer_net_raw),
        buffer_export_pct = current_percent_rank(port_history$buffer_export_raw)
      )
    }) %>%
    ungroup()
}

snapshot_table <- map_dfr(sort(unique(base_history$ym)), build_current_snapshot)

pressure_component_long <- snapshot_table %>%
  pivot_longer(
    cols = c(pressure_flow_pct, pressure_net_pct, pressure_roll_pct),
    names_to = "component",
    values_to = "component_value"
  )

buffer_component_long <- snapshot_table %>%
  pivot_longer(
    cols = c(buffer_space_pct, buffer_net_pct, buffer_export_pct),
    names_to = "component",
    values_to = "component_value"
  )

pressure_scores <- pressure_component_long %>%
  inner_join(pressure_weights, by = "component", relationship = "many-to-many") %>%
  mutate(weighted_value = component_value * weight) %>%
  group_by(
    ym, port, history_n,
    empty_in, empty_out, full_in, full_out, throughput,
    empty_net, empty_total_flow, full_total_flow,
    empty_share, net_pressure_raw, export_pull,
    roll3_empty_net, cum_empty_net, roll3_pressure_raw,
    positive_net_streak, buffer_raw,
    pressure_pct, buffer_pct, pressure_z, buffer_z,
    pressure_flow_raw, pressure_net_raw, pressure_roll_raw,
    buffer_space_raw, buffer_net_raw, buffer_export_raw
  ) %>%
  summarise(pressure_index = sum(weighted_value, na.rm = TRUE), .groups = "drop") %>%
  mutate(pressure_scenario = fixed_pressure_scenario)

buffer_scores <- buffer_component_long %>%
  inner_join(buffer_weights, by = "component", relationship = "many-to-many") %>%
  mutate(weighted_value = component_value * weight) %>%
  group_by(
    ym, port, history_n, scenario,
    empty_in, empty_out, full_in, full_out, throughput,
    empty_net, empty_total_flow, full_total_flow,
    empty_share, net_pressure_raw, export_pull,
    roll3_empty_net, cum_empty_net, roll3_pressure_raw,
    positive_net_streak, buffer_raw,
    pressure_pct, buffer_pct, pressure_z, buffer_z,
    pressure_flow_raw, pressure_net_raw, pressure_roll_raw,
    buffer_space_raw, buffer_net_raw, buffer_export_raw
  ) %>%
  summarise(buffer_index = sum(weighted_value, na.rm = TRUE), .groups = "drop") %>%
  rename(buffer_scenario = scenario)

scenario_table <- buffer_scores %>%
  left_join(
    pressure_scores,
    by = c(
      "ym", "port", "history_n",
      "empty_in", "empty_out", "full_in", "full_out", "throughput",
      "empty_net", "empty_total_flow", "full_total_flow",
      "empty_share", "net_pressure_raw", "export_pull",
      "roll3_empty_net", "cum_empty_net", "roll3_pressure_raw",
      "positive_net_streak", "buffer_raw",
      "pressure_pct", "buffer_pct", "pressure_z", "buffer_z",
      "pressure_flow_raw", "pressure_net_raw", "pressure_roll_raw",
      "buffer_space_raw", "buffer_net_raw", "buffer_export_raw"
    )
  ) %>%
  mutate(
    scenario = paste0("pressure_", pressure_scenario, "__buffer_", buffer_scenario)
  ) %>%
  group_by(buffer_scenario, ym) %>%
  mutate(
    enough_history = history_n >= 3,
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
  select(
    scenario, pressure_scenario, buffer_scenario,
    everything()
  ) %>%
  arrange(buffer_scenario, port, ym)

write_csv(scenario_table, output_path)

message("Fixed-pressure buffer scenario table written to: ", output_path)
message("Pressure fixed at: ", fixed_pressure_scenario)
