build_metric_history <- function(history_tbl, ports, pressure_weights, buffer_weights) {
  history_tbl %>%
    filter(port %in% normalize_port(ports)) %>%
    arrange(port, ym) %>%
    group_by(port) %>%
    mutate(
      history_n = row_number(),
      pressure_flow_pct = expanding_percent_rank(pressure_flow_raw),
      pressure_net_pct = expanding_percent_rank(pressure_net_raw),
      pressure_roll_pct = expanding_percent_rank(pressure_roll_raw),
      buffer_space_pct = expanding_percent_rank(buffer_space_raw),
      buffer_net_pct = expanding_percent_rank(buffer_net_raw),
      buffer_export_pct = expanding_percent_rank(buffer_export_raw)
    ) %>%
    ungroup() %>%
    mutate(
      pressure_index =
        pressure_weights[["flow"]] * pressure_flow_pct +
        pressure_weights[["net"]] * pressure_net_pct +
        pressure_weights[["roll"]] * pressure_roll_pct,
      buffer_index =
        buffer_weights[["space"]] * buffer_space_pct +
        buffer_weights[["netout"]] * buffer_net_pct +
        buffer_weights[["export"]] * buffer_export_pct,
      enough_history = history_n >= 3
    )
}

build_status_table <- function(metric_history_tbl) {
  metric_history_tbl %>%
    group_by(ym) %>%
    mutate(
      high_pressure_cut = quantile(pressure_index, 0.75, na.rm = TRUE, type = 7),
      high_buffer_cut = quantile(buffer_index, 0.75, na.rm = TRUE, type = 7),
      low_pressure_cut = quantile(pressure_index, 0.50, na.rm = TRUE, type = 7),
      status = case_when(
        !enough_history ~ "資料暖機中",
        pressure_index >= high_pressure_cut & roll3_empty_net > 0 ~ "高壓力港",
        buffer_index >= high_buffer_cut & pressure_index <= low_pressure_cut ~ "高緩衝港",
        TRUE ~ "正常港"
      )
    ) %>%
    ungroup()
}
