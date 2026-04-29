library(readr)
library(dplyr)
library(tidyr)
library(purrr)

history_path <- "/Users/lee/Documents/BigData/context/analysis_table_history_2025.csv"
weights_path <- "/Users/lee/Documents/BigData/context/weight_scenarios.csv"
output_path <- "/Users/lee/Documents/BigData/context/pressure_scenarios_2025.csv"

history_table <- read_csv(history_path, show_col_types = FALSE) %>%
  arrange(port, ym)

pressure_weights <- read_csv(weights_path, show_col_types = FALSE) %>%
  filter(index_type == "pressure")

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
    pressure_roll_raw = safe_divide(pmax(roll3_empty_net, 0), throughput)
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
        pressure_flow_raw = current_row$pressure_flow_raw,
        pressure_net_raw = current_row$pressure_net_raw,
        pressure_roll_raw = current_row$pressure_roll_raw,
        pressure_flow_pct = current_percent_rank(port_history$pressure_flow_raw),
        pressure_net_pct = current_percent_rank(port_history$pressure_net_raw),
        pressure_roll_pct = current_percent_rank(port_history$pressure_roll_raw)
      )
    }) %>%
    ungroup()
}

snapshot_table <- map_dfr(sort(unique(base_history$ym)), build_current_snapshot)

component_long <- snapshot_table %>%
  pivot_longer(
    cols = c(pressure_flow_pct, pressure_net_pct, pressure_roll_pct),
    names_to = "component",
    values_to = "component_value"
  )

pressure_scores <- component_long %>%
  inner_join(pressure_weights, by = "component", relationship = "many-to-many") %>%
  mutate(weighted_value = component_value * weight) %>%
  group_by(
    ym, port, history_n, scenario,
    empty_in, empty_out, full_in, full_out, throughput,
    empty_net, empty_total_flow, full_total_flow,
    empty_share, net_pressure_raw, export_pull,
    roll3_empty_net, cum_empty_net, roll3_pressure_raw,
    positive_net_streak,
    pressure_flow_raw, pressure_net_raw, pressure_roll_raw
  ) %>%
  summarise(pressure_index = sum(weighted_value, na.rm = TRUE), .groups = "drop")

pressure_table <- pressure_scores %>%
  group_by(scenario, ym) %>%
  mutate(
    enough_history = history_n >= 3,
    high_pressure_cut = quantile(pressure_index, 0.75, na.rm = TRUE, type = 7),
    pressure_status = case_when(
      enough_history & pressure_index >= high_pressure_cut & roll3_empty_net > 0 ~ "高壓力港",
      TRUE ~ "非高壓力港"
    )
  ) %>%
  ungroup() %>%
  arrange(scenario, port, ym)

write_csv(pressure_table, output_path)

message("Pressure-only scenario table written to: ", output_path)

message("")
message("Pressure scenario snapshot by scenario:")
pressure_table %>%
  group_by(scenario) %>%
  summarise(
    first_ym = min(ym),
    last_ym = max(ym),
    n_rows = n(),
    mean_pressure_index = mean(pressure_index, na.rm = TRUE),
    high_pressure_count = sum(pressure_status == "高壓力港", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(scenario) %>%
  print(n = Inf)
