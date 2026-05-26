library(readr)
library(dplyr)
library(tidyr)
library(purrr)

if (!requireNamespace("prophet", quietly = TRUE)) {
  stop(
    paste(
      "R package 'prophet' is required for this script.",
      "Install it in the R environment used by this project before running:",
      "install.packages('prophet')"
    ),
    call. = FALSE
  )
}

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_arg)) {
  normalizePath(sub("^--file=", "", script_arg[[1]]), winslash = "/", mustWork = TRUE)
} else {
  tryCatch(
    normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
    error = function(e) NA_character_
  )
}

context_dir <- if (!is.na(script_path)) {
  dirname(script_path)
} else {
  file.path(getwd(), "context")
}

base_path <- file.path(context_dir, "port_stat_prophet_base.csv")
forecast_output_path <- file.path(context_dir, "port_stat_prophet_forecast.csv")
backtest_output_path <- file.path(context_dir, "port_stat_prophet_backtest.csv")
watchlist_output_path <- file.path(context_dir, "port_stat_prophet_latest_watchlist.csv")

forecast_months <- 8L
backtest_months <- 6L
min_train_rows <- 18L
modeled_ports <- c("基隆港", "臺中港", "高雄港", "臺北港")

metrics <- tibble(
  metric = c(
    "empty_container_count",
    "full_container_count",
    "total_container_count",
    "empty_share_count"
  ),
  metric_label = c(
    "空櫃個數",
    "實櫃個數",
    "空實櫃總個數",
    "空櫃占比"
  )
)

clean_series <- function(df, metric) {
  df %>%
    select(ds, y = all_of(metric)) %>%
    filter(!is.na(y), is.finite(y)) %>%
    arrange(ds)
}

fit_prophet <- function(series) {
  prophet::prophet(
    series,
    yearly.seasonality = TRUE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE,
    seasonality.mode = "additive",
    interval.width = 0.8
  )
}

predict_metric <- function(port_df, port_name, metric, metric_label, periods) {
  series <- clean_series(port_df, metric)
  if (nrow(series) < min_train_rows || n_distinct(series$y) < 2) {
    return(tibble())
  }

  model <- fit_prophet(series)
  future <- prophet::make_future_dataframe(
    model,
    periods = periods,
    freq = "month",
    include_history = TRUE
  )
  latest_observed_ds <- max(series$ds)

  predict(model, future) %>%
    as_tibble() %>%
    select(ds, yhat, yhat_lower, yhat_upper) %>%
    mutate(
      port = port_name,
      metric = metric,
      metric_label = metric_label,
      is_forecast = ds > latest_observed_ds,
      observed_latest_ds = latest_observed_ds
    )
}

score_metric <- function(port_df, port_name, metric, metric_label) {
  series <- clean_series(port_df, metric)
  if (nrow(series) < min_train_rows + backtest_months || n_distinct(series$y) < 2) {
    return(tibble())
  }

  train <- series[seq_len(nrow(series) - backtest_months), , drop = FALSE]
  test <- series[(nrow(series) - backtest_months + 1L):nrow(series), , drop = FALSE]
  if (nrow(train) < min_train_rows) {
    return(tibble())
  }

  model <- fit_prophet(train)
  future <- prophet::make_future_dataframe(
    model,
    periods = backtest_months,
    freq = "month",
    include_history = TRUE
  )

  predict(model, future) %>%
    as_tibble() %>%
    select(ds, yhat, yhat_lower, yhat_upper) %>%
    right_join(test, by = "ds") %>%
    mutate(
      port = port_name,
      metric = metric,
      metric_label = metric_label,
      abs_error = abs(y - yhat),
      ape = if_else(y != 0, abs_error / abs(y), NA_real_),
      train_end_ds = max(train$ds)
    )
}

run_forecast <- function(base) {
  base %>%
    group_by(port) %>%
    group_split() %>%
    map_dfr(function(port_df) {
      port_name <- port_df$port[[1]]
      pmap_dfr(metrics, function(metric, metric_label) {
        predict_metric(port_df, port_name, metric, metric_label, forecast_months)
      })
    }) %>%
    mutate(
      ym = as.integer(format(ds, "%Y%m")),
      yhat = pmax(yhat, 0),
      yhat_lower = pmax(yhat_lower, 0),
      yhat_upper = pmax(yhat_upper, 0)
    ) %>%
    arrange(port, metric, ds)
}

run_backtest <- function(base) {
  base %>%
    group_by(port) %>%
    group_split() %>%
    map_dfr(function(port_df) {
      port_name <- port_df$port[[1]]
      pmap_dfr(metrics, function(metric, metric_label) {
        score_metric(port_df, port_name, metric, metric_label)
      })
    }) %>%
    mutate(ym = as.integer(format(ds, "%Y%m"))) %>%
    arrange(port, metric, ds)
}

compute_risk_cols <- function(df, quantiles) {
  df %>%
    left_join(quantiles, by = "port") %>%
    mutate(
      pred_empty_share = empty_share_count,
      pred_load_ratio_vs_q75 = total_container_count / hist_total_q75,
      pred_empty_share_ratio_vs_q75 = pred_empty_share / hist_empty_share_q75,
      proxy_risk_score = 0.55 * pred_empty_share_ratio_vs_q75 + 0.45 * pred_load_ratio_vs_q75,
      proxy_risk_level = case_when(
        proxy_risk_score < 0.95 ~ "normal",
        proxy_risk_score < 1.10 ~ "watch",
        TRUE ~ "high"
      )
    )
}

add_actual_risk_level <- function(df) {
  df %>% mutate(
    actual_risk_level = case_when(
      is.na(actual_empty_share) ~ NA_character_,
      actual_empty_share / hist_empty_share_q75 >= 1.10 ~ "high",
      actual_empty_share / hist_empty_share_q75 >= 0.95 ~ "watch",
      TRUE ~ "normal"
    )
  )
}

watchlist_cols <- c(
  "ym", "ds", "port",
  "empty_container_count", "total_container_count",
  "pred_empty_share", "pred_load_ratio_vs_q75", "pred_empty_share_ratio_vs_q75",
  "proxy_risk_score", "proxy_risk_level",
  "is_validation", "actual_empty_share", "actual_risk_level"
)

build_watchlist <- function(base, forecast) {
  future <- forecast %>% filter(is_forecast)
  if (nrow(future) == 0) return(tibble())

  quantiles <- base %>%
    group_by(port) %>%
    summarise(
      hist_total_q75 = quantile(total_container_count, 0.75, na.rm = TRUE),
      hist_empty_share_q75 = quantile(empty_share_count, 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  future %>%
    select(port, ds, ym, metric, yhat) %>%
    pivot_wider(names_from = metric, values_from = yhat) %>%
    compute_risk_cols(quantiles) %>%
    mutate(is_validation = FALSE, actual_empty_share = NA_real_) %>%
    add_actual_risk_level() %>%
    select(all_of(watchlist_cols)) %>%
    arrange(ym, desc(proxy_risk_score))
}

build_validation_watchlist <- function(base, cutoff) {
  base_train  <- base %>% filter(ds <= cutoff)
  base_actual <- base %>% filter(ds > cutoff, ds <= as.Date("2025-12-01"))

  if (nrow(base_actual) == 0) return(tibble())

  n_periods <- as.integer(
    (as.numeric(format(as.Date("2025-12-01"), "%Y")) - as.numeric(format(cutoff, "%Y"))) * 12 +
    (as.numeric(format(as.Date("2025-12-01"), "%m")) - as.numeric(format(cutoff, "%m")))
  )
  if (n_periods <= 0) return(tibble())

  preds_long <- base_train %>%
    group_by(port) %>%
    group_split() %>%
    map_dfr(function(port_df) {
      port_name <- port_df$port[[1]]
      pmap_dfr(metrics, function(metric, metric_label) {
        series <- clean_series(port_df, metric)
        if (nrow(series) < min_train_rows || n_distinct(series$y) < 2) return(tibble())

        model <- fit_prophet(series)
        future_df <- prophet::make_future_dataframe(
          model, periods = n_periods, freq = "month", include_history = FALSE
        )
        predict(model, future_df) %>%
          as_tibble() %>%
          select(ds, yhat) %>%
          mutate(
            ds     = as.Date(ds),
            port   = port_name,
            metric = metric,
            yhat   = pmax(yhat, 0)
          ) %>%
          filter(ds >= as.Date("2025-01-01"), ds <= as.Date("2025-12-01"))
      })
    })

  if (nrow(preds_long) == 0) return(tibble())

  pred_wide <- preds_long %>%
    filter(metric %in% c("empty_share_count", "total_container_count", "empty_container_count")) %>%
    select(port, ds, metric, yhat) %>%
    pivot_wider(names_from = metric, values_from = yhat) %>%
    mutate(ym = as.integer(format(ds, "%Y%m")))

  actuals <- base_actual %>%
    select(port, ds, actual_empty_share = empty_share_count)

  quantiles <- base_train %>%
    group_by(port) %>%
    summarise(
      hist_total_q75 = quantile(total_container_count, 0.75, na.rm = TRUE),
      hist_empty_share_q75 = quantile(empty_share_count, 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  pred_wide %>%
    left_join(actuals, by = c("port", "ds")) %>%
    compute_risk_cols(quantiles) %>%
    mutate(is_validation = TRUE) %>%
    add_actual_risk_level() %>%
    select(all_of(watchlist_cols)) %>%
    arrange(ym, desc(proxy_risk_score))
}

build_bridge_watchlist <- function(backtest, base) {
  bridge_ym <- c(202601L, 202602L, 202603L, 202604L)

  pred_wide <- backtest %>%
    filter(ym %in% bridge_ym,
           metric %in% c("empty_share_count", "total_container_count", "empty_container_count")) %>%
    select(port, ds, ym, metric, yhat) %>%
    pivot_wider(names_from = metric, values_from = yhat)

  if (nrow(pred_wide) == 0) return(tibble())

  actual_es <- backtest %>%
    filter(ym %in% bridge_ym, metric == "empty_share_count") %>%
    select(port, ds, actual_empty_share = y)

  quantiles <- base %>%
    filter(ds <= as.Date("2025-10-01")) %>%
    group_by(port) %>%
    summarise(
      hist_total_q75 = quantile(total_container_count, 0.75, na.rm = TRUE),
      hist_empty_share_q75 = quantile(empty_share_count, 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  pred_wide %>%
    left_join(actual_es, by = c("port", "ds")) %>%
    compute_risk_cols(quantiles) %>%
    mutate(is_validation = TRUE) %>%
    add_actual_risk_level() %>%
    select(all_of(watchlist_cols)) %>%
    arrange(ym, desc(proxy_risk_score))
}

validation_cutoff <- as.Date("2024-12-01")

base <- read_csv(base_path, show_col_types = FALSE) %>%
  mutate(ds = as.Date(ds)) %>%
  filter(port %in% modeled_ports)

message("Running full forecast...")
forecast <- run_forecast(base)

message("Running backtest...")
backtest <- run_backtest(base)

message("Building future watchlist (2026-05+)...")
future_watchlist <- build_watchlist(base, forecast)

message("Running 2025 validation (train through ", format(validation_cutoff, "%Y-%m"), ")...")
validation_watchlist <- build_validation_watchlist(base, validation_cutoff)

message("Building 2026-01 to 2026-04 bridge (from backtest)...")
bridge_watchlist <- build_bridge_watchlist(backtest, base)

watchlist <- bind_rows(validation_watchlist, bridge_watchlist, future_watchlist) %>%
  arrange(ym, desc(proxy_risk_score))

write_csv(forecast, forecast_output_path)
write_csv(backtest, backtest_output_path)
write_csv(watchlist, watchlist_output_path)

message("Forecast written to: ", forecast_output_path)
message("Backtest written to: ", backtest_output_path)
message("Watchlist written to: ", watchlist_output_path)
message(sprintf(
  "Watchlist: %d validation rows (2025), %d bridge rows (2026-01~04), %d future rows (2026-05+)",
  nrow(validation_watchlist), nrow(bridge_watchlist), nrow(future_watchlist)
))
