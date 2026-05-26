port_baseline_files <- list(
  base = file.path(context_dir, "port_stat_prophet_base.csv"),
  forecast = file.path(context_dir, "port_stat_prophet_forecast.csv"),
  backtest = file.path(context_dir, "port_stat_prophet_backtest.csv"),
  watchlist = file.path(context_dir, "port_stat_prophet_latest_watchlist.csv")
)

parse_month_date <- function(x) {
  as.Date(substr(as.character(x), 1, 10))
}

read_optional_csv <- function(path) {
  if (!file.exists(path)) {
    return(tibble())
  }

  read_csv(path, show_col_types = FALSE)
}

port_baseline_base <- read_optional_csv(port_baseline_files$base) %>%
  mutate(
    ym = as.integer(ym),
    ds = parse_month_date(ds),
    port = normalize_port(port),
    empty_share_count = safe_divide(empty_container_count, total_container_count),
    full_share_count = safe_divide(full_container_count, total_container_count),
    ym_label = fmt_ym(ym)
  )

port_baseline_forecast <- read_optional_csv(port_baseline_files$forecast) %>%
  mutate(
    ym = as.integer(ym),
    ds = parse_month_date(ds),
    port = normalize_port(port),
    observed_latest_ds = parse_month_date(observed_latest_ds),
    is_forecast = as.logical(is_forecast),
    ym_label = fmt_ym(ym)
  )

port_baseline_backtest <- read_optional_csv(port_baseline_files$backtest) %>%
  mutate(
    ym = as.integer(ym),
    ds = parse_month_date(ds),
    port = normalize_port(port),
    train_end_ds = parse_month_date(train_end_ds),
    ym_label = fmt_ym(ym)
  )

port_baseline_watchlist <- read_optional_csv(port_baseline_files$watchlist) %>%
  mutate(
    ym = as.integer(ym),
    ds = parse_month_date(ds),
    port = normalize_port(port),
    ym_label = fmt_ym(ym),
    proxy_risk_level = factor(proxy_risk_level, levels = c("normal", "watch", "high"))
  ) %>%
  arrange(ym, desc(proxy_risk_score))

port_baseline_available <- nrow(port_baseline_base) > 0 &&
  nrow(port_baseline_forecast) > 0 &&
  nrow(port_baseline_backtest) > 0 &&
  nrow(port_baseline_watchlist) > 0

port_baseline_metric_labels <- c(
  empty_container_count = "空櫃個數",
  full_container_count = "實櫃個數",
  total_container_count = "空實櫃總個數",
  empty_share_count = "空櫃占比"
)

port_baseline_backtest_summary <- port_baseline_backtest %>%
  group_by(metric, metric_label) %>%
  summarise(
    mae = mean(abs_error, na.rm = TRUE),
    mape = mean(ape, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(match(metric, names(port_baseline_metric_labels)))

build_port_baseline_bridge <- function(status_tbl, ym_value) {
  if (!port_baseline_available) {
    return(tibble())
  }

  imarine_rows <- status_tbl %>%
    filter(ym == ym_value, port %in% formal_ports)

  if (nrow(imarine_rows) == 0 || all(imarine_rows$status == "資料暖機中")) {
    return(tibble())
  }

  yhat_rows <- port_baseline_forecast %>%
    filter(
      ym == ym_value,
      !is_forecast,
      port %in% formal_ports,
      metric %in% c("empty_share_count", "total_container_count")
    ) %>%
    select(port, metric, yhat) %>%
    pivot_wider(names_from = metric, values_from = yhat, names_prefix = "yhat_")

  baseline_rows <- port_baseline_base %>%
    filter(ym == ym_value, port %in% formal_ports) %>%
    select(
      ym,
      port,
      empty_container_count,
      total_container_count,
      empty_share_count
    ) %>%
    left_join(yhat_rows, by = "port") %>%
    mutate(
      empty_share_vs_yhat = safe_divide(empty_share_count, yhat_empty_share_count),
      total_vs_yhat = safe_divide(total_container_count, yhat_total_container_count),
      baseline_signal = case_when(
        empty_share_vs_yhat >= 1.10 | total_vs_yhat >= 1.10 ~ "超出季節性預期",
        empty_share_vs_yhat >= 0.95 | total_vs_yhat >= 0.95 ~ "接近季節性上界",
        TRUE ~ "符合季節性範圍"
      )
    )

  status_tbl %>%
    filter(ym == ym_value, port %in% formal_ports) %>%
    select(ym, port, pressure_index, buffer_index, status, empty_net) %>%
    left_join(baseline_rows, by = c("ym", "port")) %>%
    arrange(desc(pressure_index), desc(empty_share_vs_yhat))
}

latest_port_baseline_watchlist <- function(n = 8) {
  port_baseline_watchlist %>%
    slice_head(n = n)
}
