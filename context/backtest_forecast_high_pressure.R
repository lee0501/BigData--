library(readr)
library(dplyr)
library(purrr)

scenario_path <- "/Users/lee/Documents/BigData/context/pressure_scenarios_2025.csv"
summary_output_path <- "/Users/lee/Documents/BigData/context/forecast_high_pressure_backtest_summary.csv"
detail_output_path <- "/Users/lee/Documents/BigData/context/forecast_high_pressure_backtest_detail.csv"
latest_output_path <- "/Users/lee/Documents/BigData/context/forecast_high_pressure_latest_watchlist.csv"

official_scenario <- "roc_prev_best"
formal_ports <- c("基隆港", "臺北港", "臺中港", "高雄港")
streak_step <- 0.05
streak_cap <- 6

add_one_month <- function(ym) {
  year_part <- ym %/% 100
  month_part <- ym %% 100

  ifelse(
    month_part == 12,
    (year_part + 1) * 100 + 1,
    year_part * 100 + month_part + 1
  )
}

safe_auc <- function(score, label) {
  valid <- !is.na(score) & !is.na(label)
  score <- score[valid]
  label <- as.integer(label[valid])

  if (length(score) < 2 || length(unique(label)) < 2) {
    return(NA_real_)
  }

  ranks <- rank(score, ties.method = "average")
  n_pos <- sum(label == 1)
  n_neg <- sum(label == 0)

  if (n_pos == 0 || n_neg == 0) {
    return(NA_real_)
  }

  (sum(ranks[label == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

score_pressure_index <- function(tbl) {
  tbl$pressure_index
}

score_pressure_index_streak_boost <- function(tbl) {
  tbl$pressure_index + streak_step * pmin(tbl$positive_net_streak, streak_cap)
}

score_models <- list(
  list(
    model = "baseline_pressure_index",
    model_note = "直接用當月 pressure_index 當下月風險排序",
    scorer = score_pressure_index
  ),
  list(
    model = "pressure_index_streak_boost",
    model_note = paste0(
      "在 pressure_index 上加入連續淨流入月數加權：pressure_index + ",
      streak_step,
      " * pmin(positive_net_streak, ",
      streak_cap,
      ")"
    ),
    scorer = score_pressure_index_streak_boost
  )
)

window_specs <- tibble(
  window = c("official", "warmup_included"),
  flag_col = c("eligible_official", "eligible_warmup"),
  window_note = c(
    "正式口徑：來源月 history_n >= 3，且可觀察下一月結果。",
    "暖機納入：允許 2025-02 作為來源月，但要求目標月已進入 history_n >= 3。"
  )
)

pressure_table <- read_csv(scenario_path, show_col_types = FALSE) %>%
  filter(
    scenario == official_scenario,
    port %in% formal_ports
  ) %>%
  arrange(port, ym) %>%
  group_by(port) %>%
  mutate(
    next_ym = lead(ym),
    next_history_n = lead(history_n),
    next_pressure_status = lead(pressure_status),
    next_high_pressure_flag = lead(pressure_status == "高壓力港")
  ) %>%
  ungroup() %>%
  transmute(
    origin_ym = ym,
    target_ym = if_else(is.na(next_ym), add_one_month(ym), next_ym),
    port,
    history_n,
    enough_history,
    next_history_n,
    empty_in,
    empty_out,
    throughput,
    empty_net,
    roll3_empty_net,
    positive_net_streak,
    pressure_flow_raw,
    pressure_net_raw,
    pressure_roll_raw,
    pressure_index,
    next_high_pressure_flag,
    next_pressure_status,
    eligible_official = enough_history & !is.na(next_high_pressure_flag),
    eligible_warmup = history_n >= 2 & !is.na(next_high_pressure_flag) & next_history_n >= 3
  )

build_detail_for_window <- function(window_name, flag_col, window_note) {
  window_tbl <- pressure_table %>%
    filter(.data[[flag_col]])

  map_dfr(score_models, function(model_spec) {
    window_tbl %>%
      mutate(
        window = window_name,
        window_note = window_note,
        model = model_spec$model,
        model_note = model_spec$model_note,
        forecast_score = model_spec$scorer(.)
      )
  }) %>%
    group_by(window, model, origin_ym) %>%
    arrange(desc(forecast_score), desc(pressure_index), desc(positive_net_streak), port, .by_group = TRUE) %>%
    mutate(
      rank_within_origin = row_number(),
      top1_flag = rank_within_origin == 1,
      top2_flag = rank_within_origin <= 2
    ) %>%
    ungroup()
}

build_summary_row <- function(tbl) {
  top1_tbl <- tbl %>% filter(top1_flag)
  top2_tbl <- tbl %>% filter(top2_flag)
  top2_month_hit_tbl <- top2_tbl %>%
    group_by(origin_ym) %>%
    summarise(top2_hit = any(next_high_pressure_flag), .groups = "drop")

  tibble(
    n_rows = nrow(tbl),
    n_eval_months = n_distinct(tbl$origin_ym),
    first_origin_ym = min(tbl$origin_ym),
    last_origin_ym = max(tbl$origin_ym),
    positive_cases = sum(tbl$next_high_pressure_flag, na.rm = TRUE),
    pooled_auc = safe_auc(tbl$forecast_score, tbl$next_high_pressure_flag),
    top1_hit_rate = mean(top1_tbl$next_high_pressure_flag, na.rm = TRUE),
    top2_hit_rate = mean(top2_month_hit_tbl$top2_hit, na.rm = TRUE),
    top2_recall = sum(top2_tbl$next_high_pressure_flag, na.rm = TRUE) /
      sum(tbl$next_high_pressure_flag, na.rm = TRUE),
    mean_positive_rank = mean(tbl$rank_within_origin[tbl$next_high_pressure_flag], na.rm = TRUE)
  )
}

detail_table <- pmap_dfr(
  list(window_specs$window, window_specs$flag_col, window_specs$window_note),
  build_detail_for_window
)

summary_table <- detail_table %>%
  group_by(window, window_note, model, model_note) %>%
  group_modify(~ build_summary_row(.x)) %>%
  ungroup() %>%
  arrange(window, desc(top1_hit_rate), desc(pooled_auc), desc(top2_recall), model)

selected_model <- summary_table %>%
  filter(window == "official") %>%
  arrange(desc(top1_hit_rate), desc(pooled_auc), desc(top2_recall), model) %>%
  slice(1) %>%
  pull(model)

latest_origin_ym <- max(pressure_table$origin_ym, na.rm = TRUE)

latest_base <- pressure_table %>%
  filter(origin_ym == latest_origin_ym) %>%
  select(
    origin_ym,
    target_ym,
    port,
    history_n,
    pressure_index,
    positive_net_streak,
    empty_net,
    roll3_empty_net,
    pressure_flow_raw,
    pressure_net_raw,
    pressure_roll_raw
  )

latest_scores_long <- map_dfr(score_models, function(model_spec) {
  latest_base %>%
    mutate(
      model = model_spec$model,
      forecast_score = model_spec$scorer(.)
    ) %>%
    arrange(desc(forecast_score), desc(pressure_index), desc(positive_net_streak), port) %>%
    mutate(rank_within_latest = row_number())
})

latest_scores_wide <- latest_scores_long %>%
  select(port, model, forecast_score, rank_within_latest) %>%
  tidyr::pivot_wider(
    names_from = model,
    values_from = c(forecast_score, rank_within_latest),
    names_glue = "{.value}_{model}"
  )

selected_score_col <- paste0("forecast_score_", selected_model)
selected_rank_col <- paste0("rank_within_latest_", selected_model)

latest_watchlist <- latest_base %>%
  left_join(latest_scores_wide, by = "port") %>%
  mutate(
    selected_model = selected_model,
    selected_score = .data[[selected_score_col]],
    selected_rank = .data[[selected_rank_col]],
    forecast_priority = case_when(
      selected_rank == 1 ~ "高預警",
      selected_rank == 2 ~ "觀察",
      TRUE ~ "低優先"
    ),
    forecast_note = case_when(
      selected_rank == 1 & positive_net_streak >= 3 ~ "目前分數最高，且已有連續堆積訊號。",
      selected_rank == 1 ~ "目前分數最高，建議優先觀察。",
      selected_rank == 2 & positive_net_streak >= 3 ~ "次高分，且連續堆積尚未中斷。",
      selected_rank == 2 ~ "次高分，作為備用觀察港。",
      TRUE ~ "目前排序較後，可維持一般監測。"
    )
  ) %>%
  arrange(selected_rank, desc(selected_score), port)

write_csv(summary_table, summary_output_path)
write_csv(detail_table, detail_output_path)
write_csv(latest_watchlist, latest_output_path)

message("Forecast backtest summary written to: ", summary_output_path)
message("Forecast backtest detail written to: ", detail_output_path)
message("Forecast latest watchlist written to: ", latest_output_path)
message("")
message("Forecast summary:")
print(summary_table, n = nrow(summary_table))
message("")
message("Selected model for latest watchlist: ", selected_model)
