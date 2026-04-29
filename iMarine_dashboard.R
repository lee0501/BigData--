required_packages <- c("shiny", "ggplot2", "dplyr", "readr", "tidyr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    ". Please install them before running the dashboard."
  )
}

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

if (capabilities("cairo")) {
  options(bitmapType = "cairo")
} else if (capabilities("aqua")) {
  options(bitmapType = "quartz")
}

locate_app_dir <- function() {
  candidates <- unique(c(
    getwd(),
    "/Users/lee/Documents/BigData"
  ))

  hits <- candidates[
    vapply(
      candidates,
      function(x) file.exists(file.path(x, "context", "status_final_2025.csv")),
      logical(1)
    )
  ]

  if (!length(hits)) {
    stop("Cannot locate the project directory containing context/status_final_2025.csv")
  }

  normalizePath(hits[[1]], winslash = "/", mustWork = TRUE)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

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

normalize_port <- function(x) {
  gsub("台", "臺", x)
}

display_port <- function(x) {
  gsub("臺", "台", x)
}

fmt_ym <- function(x) {
  sprintf("%04d-%02d", as.integer(x) %/% 100, as.integer(x) %% 100)
}

fmt_num <- function(x, digits = 0) {
  ifelse(
    is.na(x),
    "—",
    format(round(x, digits), big.mark = ",", nsmall = digits, trim = TRUE)
  )
}

fmt_idx <- function(x, digits = 4) {
  ifelse(is.na(x), "—", sprintf(paste0("%.", digits, "f"), x))
}

fmt_pct <- function(x, digits = 0) {
  ifelse(is.na(x), "—", paste0(round(x * 100, digits), "%"))
}

month_to_ym <- function(month_num, year_num = 2025) {
  year_num * 100 + as.integer(month_num)
}

month_to_date <- function(month_num, year_num = 2025) {
  as.Date(sprintf("%04d-%02d-20", year_num, as.integer(month_num)))
}

formal_ports <- c("基隆港", "臺北港", "臺中港", "高雄港")
display_ports <- c("基隆港", "台北港", "台中港", "高雄港")
table_ports <- formal_ports
port_codes <- c(
  "基隆港" = "KEE",
  "臺北港" = "TPE",
  "臺中港" = "TAI",
  "高雄港" = "KAO",
  "花蓮港" = "HUA"
)

status_palette <- c(
  "高壓力港" = "#dc2626",
  "高緩衝港" = "#16a34a",
  "正常港" = "#2563eb",
  "資料暖機中" = "#f59e0b"
)

official_pressure_weights <- c(flow = 0.22, net = 0.60, roll = 0.18)
official_buffer_weights <- c(space = 0.45, netout = 0.40, export = 0.15)
official_matching_weights <- c(source = 0.45, target = 0.35, distance = 0.20)
custom_weight_bounds <- c(min = 0.10, max = 0.60)

strategy_templates <- list(
  flow_focus = list(
    label = "即時壓力型",
    short = "Flow 重",
    description = "提高當月空櫃進港比重，優先辨識短期即時壓力。",
    pressure_weights = c(flow = 0.60, net = 0.25, roll = 0.15)
  ),
  net_focus = list(
    label = "堆積風險型",
    short = "Net 重",
    description = "提高當月淨增加比重，優先辨識已經開始累積的港口。",
    pressure_weights = c(flow = 0.20, net = 0.60, roll = 0.20)
  ),
  trend_focus = list(
    label = "趨勢預警型",
    short = "Trend 重",
    description = "提高近三月趨勢比重，優先辨識持續累積的早期訊號。",
    pressure_weights = c(flow = 0.15, net = 0.25, roll = 0.60)
  )
)

sanitize_weight_vector <- function(values, defaults, min_value = 0.10, max_value = 0.60) {
  vals <- suppressWarnings(as.numeric(values))

  if (length(vals) != length(defaults) || any(is.na(vals))) {
    vals <- as.numeric(defaults)
  }

  vals <- pmin(pmax(vals, min_value), max_value)
  target_sum <- 1

  for (i in seq_len(10)) {
    diff <- target_sum - sum(vals)
    if (abs(diff) < 1e-8) {
      break
    }

    adjustable <- if (diff > 0) which(vals < (max_value - 1e-8)) else which(vals > (min_value + 1e-8))
    if (length(adjustable) == 0) {
      vals <- as.numeric(defaults)
      break
    }

    step <- diff / length(adjustable)
    vals[adjustable] <- vals[adjustable] + step
    vals <- pmin(pmax(vals, min_value), max_value)
  }

  if (abs(sum(vals) - target_sum) > 1e-8) {
    vals <- as.numeric(defaults)
  }

  names(vals) <- names(defaults)
  vals
}

level_class <- function(value) {
  if (is.na(value)) {
    "low"
  } else if (value >= 75) {
    "high"
  } else if (value >= 45) {
    "mid"
  } else {
    "low"
  }
}

level_label <- function(level) {
  if (level == "high") {
    "高壓"
  } else if (level == "mid") {
    "中壓"
  } else {
    "低壓"
  }
}

status_card_class <- function(status_value) {
  if (identical(status_value, "高壓力港")) {
    "high"
  } else if (identical(status_value, "高緩衝港")) {
    "low"
  } else if (identical(status_value, "資料暖機中")) {
    "mid"
  } else {
    "mid"
  }
}

status_badge_label <- function(status_value) {
  if (identical(status_value, "高壓力港")) {
    "正式高壓"
  } else if (identical(status_value, "高緩衝港")) {
    "正式高緩衝"
  } else if (identical(status_value, "資料暖機中")) {
    "資料暖機中"
  } else {
    "正式正常"
  }
}

simulation_result_class <- function(source_status_after, target_pressure_after, high_pressure_cut) {
  source_relieved <- source_status_after != "高壓力港"
  target_safe <- target_pressure_after < high_pressure_cut

  if (source_relieved && target_safe) {
    "完全解壓"
  } else if (target_safe) {
    "可緩解但未解壓"
  } else {
    "不建議調度"
  }
}

simulation_result_rank <- function(x) {
  case_when(
    x == "完全解壓" ~ 1L,
    x == "可緩解但未解壓" ~ 2L,
    TRUE ~ 3L
  )
}

fmt_weight <- function(x) {
  paste0(round(x * 100), "%")
}

save_ggplot_png <- function(plot_obj, width = 960, height = 420, res = 144) {
  file <- tempfile(fileext = ".png")
  device_type <- if (capabilities("cairo")) {
    "cairo"
  } else if (capabilities("aqua")) {
    "quartz"
  } else {
    getOption("bitmapType", "cairo")
  }

  tryCatch({
    grDevices::png(filename = file, width = width, height = height, res = res, type = device_type)
    print(plot_obj)
    grDevices::dev.off()
    file
  }, error = function(e) {
    try(grDevices::dev.off(), silent = TRUE)
    grDevices::png(filename = file, width = width, height = height, res = res)
    graphics::plot.new()
    graphics::text(0.5, 0.5, labels = paste("Plot render failed:", conditionMessage(e)), cex = 1)
    grDevices::dev.off()
    file
  })
}

expanding_percent_rank <- function(x) {
  vapply(
    seq_along(x),
    function(i) current_percent_rank(x[seq_len(i)]),
    numeric(1)
  )
}

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

app_dir <- locate_app_dir()
context_dir <- file.path(app_dir, "context")

status_final <- read_csv(file.path(context_dir, "status_final_2025.csv"), show_col_types = FALSE) %>%
  mutate(
    ym = as.integer(ym),
    port = normalize_port(port),
    ym_label = fmt_ym(ym)
  ) %>%
  arrange(ym, port)

matching_final <- read_csv(file.path(context_dir, "matching_final_2025.csv"), show_col_types = FALSE) %>%
  mutate(
    ym = as.integer(ym),
    source_port = normalize_port(source_port),
    target_port = normalize_port(target_port),
    ym_label = fmt_ym(ym)
  ) %>%
  arrange(ym, rank_global)

status_formal_display <- status_final %>%
  filter(port %in% formal_ports) %>%
  mutate(
    status = if_else(history_n < 3, "資料暖機中", status)
  ) %>%
  arrange(ym, port)

simulation_reco <- read_csv(file.path(context_dir, "simulation_recommendation_final_2025.csv"), show_col_types = FALSE) %>%
  mutate(
    ym = as.integer(ym),
    source_port = normalize_port(source_port),
    target_port = normalize_port(target_port),
    ym_label = fmt_ym(ym)
  ) %>%
  arrange(ym, rank_global)

simulation_final <- read_csv(file.path(context_dir, "simulation_final_2025.csv"), show_col_types = FALSE) %>%
  mutate(
    ym = as.integer(ym),
    source_port = normalize_port(source_port),
    target_port = normalize_port(target_port),
    ym_label = fmt_ym(ym)
  ) %>%
  arrange(ym, rank_global, move_share)

analysis_table <- read_csv(file.path(context_dir, "analysis_table_2025.csv"), show_col_types = FALSE) %>%
  mutate(
    ym = as.integer(ym),
    port = normalize_port(port),
    ym_label = fmt_ym(ym)
  ) %>%
  left_join(
    status_final %>%
      select(ym, port, pressure_index, buffer_index, status),
    by = c("ym", "port")
  ) %>%
  arrange(port, ym)

analysis_history <- read_csv(file.path(context_dir, "analysis_table_history_2025.csv"), show_col_types = FALSE) %>%
  mutate(
    ym = as.integer(ym),
    port = normalize_port(port),
    ym_label = fmt_ym(ym)
  ) %>%
  arrange(port, ym)

base_history <- analysis_history %>%
  mutate(
    pressure_flow_raw = safe_divide(empty_in, throughput),
    pressure_net_raw = safe_divide(pmax(empty_net, 0), throughput),
    pressure_roll_raw = safe_divide(pmax(roll3_empty_net, 0), throughput),
    buffer_space_raw = pmax(1 - pressure_flow_raw, 0),
    buffer_net_raw = safe_divide(pmax(-roll3_empty_net, 0), throughput),
    buffer_export_raw = export_pull
  )

master_table <- read_csv(file.path(context_dir, "master_table_2025.csv"), show_col_types = FALSE) %>%
  mutate(
    ym = as.integer(ym),
    port = normalize_port(port),
    ym_label = fmt_ym(ym),
    pressure = safe_divide(empty_in, throughput),
    buffer = ifelse(is.na(pressure), NA_real_, pmax(1 - pressure, 0))
  ) %>%
  arrange(ym, port)

distance_class_lookup <- read_csv(file.path(context_dir, "port_pair_distance_lookup.csv"), show_col_types = FALSE) %>%
  mutate(
    source_port = normalize_port(source_port),
    target_port = normalize_port(target_port),
    distance_class = as.numeric(distance_class),
    distance_factor = as.numeric(distance_factor)
  )

distance_detail_path <- "/Users/lee/Downloads/Distance_Matrix - 說明.csv"
distance_detail <- if (file.exists(distance_detail_path)) {
  read_csv(distance_detail_path, show_col_types = FALSE, col_names = FALSE) %>%
    slice(4:9) %>%
    transmute(
      source_port = normalize_port(X1),
      target_port = normalize_port(X2),
      distance_km = as.numeric(gsub("[^0-9.]", "", X3)),
      eta_days = as.numeric(gsub("[^0-9.]", "", X4)),
      distance_class = as.numeric(X5)
    ) %>%
    bind_rows(
      transmute(
        .,
        source_port = target_port,
        target_port = source_port,
        distance_km = distance_km,
        eta_days = eta_days,
        distance_class = distance_class
      )
    ) %>%
    distinct(source_port, target_port, .keep_all = TRUE)
} else {
  tibble(
    source_port = character(),
    target_port = character(),
    distance_km = numeric(),
    eta_days = numeric(),
    distance_class = numeric()
  )
}

build_formal_candidate_rows <- function(ym_value) {
  reco_row <- simulation_reco %>%
    filter(ym == ym_value) %>%
    slice(1)

  sim_rows <- simulation_final %>%
    filter(ym == ym_value, simulation_scenario != "no_move") %>%
    left_join(
      distance_detail %>%
        select(source_port, target_port, distance_km, eta_days),
      by = c("source_port", "target_port")
    )

  if (nrow(sim_rows) == 0) {
    return(tibble())
  }

  recommended_share <- if (nrow(reco_row) > 0) reco_row$recommended_move_share[[1]] else NA_real_
  recommended_message <- if (nrow(reco_row) > 0) reco_row$display_message[[1]] else NA_character_

  sim_rows %>%
    mutate(
      candidate_key = paste0(target_port, "::", simulation_scenario),
      simulation_result = case_when(
        pair_success ~ "完全解壓",
        target_stays_safe ~ "可緩解但未解壓",
        TRUE ~ "不建議調度"
      ),
      result_rank = simulation_result_rank(simulation_result),
      pair_tier = case_when(
        !is.na(recommended_share) & abs(move_share - recommended_share) < 1e-8 ~ "正式推薦",
        TRUE ~ "觀察候選"
      ),
      scenario_label = paste0(round(move_share * 100), "%"),
      summary_text = case_when(
        pair_tier == "正式推薦" & !is.na(recommended_message) ~ recommended_message,
        TRUE ~ paste0("模擬移轉 ", round(move_share * 100), "%（", fmt_num(move_amount, 0), " TEU）。")
      ),
      source_status = source_status_before,
      target_status = target_status_before,
      source_pressure = source_pressure_index_before,
      source_pressure_after = source_pressure_index_after,
      source_buffer = source_buffer_index_before,
      source_empty_net = source_empty_net_before,
      target_pressure = target_pressure_index_before,
      target_pressure_after = target_pressure_index_after,
      target_buffer = target_buffer_index_before,
      target_empty_net = target_empty_net_before,
      pressure_before_pct = round(target_pressure_index_before * 100),
      pressure_after_pct = round(target_pressure_index_after * 100),
      pressure_increase_pct = pmax(0, round(target_pressure_index_after * 100) - round(target_pressure_index_before * 100)),
      source_pressure_pct = round(source_pressure_index_before * 100),
      source_pressure_after_pct = round(source_pressure_index_after * 100),
      relief_pct = if_else(
        source_pressure_index_before <= 0,
        0,
        round(pmax(0, (source_pressure_index_before - source_pressure_index_after) / source_pressure_index_before) * 100)
      )
    ) %>%
    transmute(
      ym,
      source_port,
      target_port,
      candidate_key,
      source_status,
      target_status,
      source_pressure,
      source_pressure_after,
      source_buffer,
      source_empty_net,
      target_pressure,
      target_pressure_after,
      target_buffer,
      target_empty_net,
      matching_score,
      move_amount,
      distance_km,
      eta_days,
      simulation_result,
      pair_tier,
      scenario_label,
      summary_text,
      pressure_before_pct,
      pressure_after_pct,
      pressure_increase_pct,
      source_pressure_pct,
      source_pressure_after_pct,
      relief_pct,
      result_rank
    ) %>%
    arrange(result_rank, desc(pair_tier == "正式推薦"), desc(move_amount))
}

default_source_for_month <- function(month_status_tbl) {
  if (nrow(month_status_tbl) == 0) {
    return("高雄港")
  }

  high_row <- month_status_tbl %>%
    filter(status == "高壓力港") %>%
    arrange(desc(pressure_index), desc(empty_net)) %>%
    slice(1)

  if (nrow(high_row) > 0) {
    return(high_row$port[[1]])
  }

  month_status_tbl %>%
    arrange(desc(pressure_index), desc(empty_net)) %>%
    slice(1) %>%
    pull(port)
}

default_move_amount <- function(month_status_tbl, source_port) {
  base_row <- month_status_tbl %>%
    filter(port == !!source_port) %>%
    slice(1)

  if (nrow(base_row) == 0) {
    return(2500)
  }

  move_base <- max(base_row$empty_net[[1]], 0)
  if (is.finite(move_base) && move_base > 0) {
    return(round(move_base * 0.50))
  }

  2500
}

compute_projected_port <- function(port_history, role, move_amount, pressure_weights, buffer_weights) {
  current_row <- slice_tail(port_history, n = 1)

  empty_in_new <- current_row$empty_in
  empty_out_new <- current_row$empty_out
  empty_net_new <- current_row$empty_net
  roll3_empty_net_new <- current_row$roll3_empty_net

  if (role == "source") {
    empty_out_new <- empty_out_new + move_amount
    empty_net_new <- empty_net_new - move_amount
    roll3_empty_net_new <- roll3_empty_net_new - (move_amount / 3)
  } else if (role == "target") {
    empty_in_new <- empty_in_new + move_amount
    empty_net_new <- empty_net_new + move_amount
    roll3_empty_net_new <- roll3_empty_net_new + (move_amount / 3)
  }

  pressure_flow_raw_new <- safe_divide(empty_in_new, current_row$throughput)
  pressure_net_raw_new <- safe_divide(pmax(empty_net_new, 0), current_row$throughput)
  pressure_roll_raw_new <- safe_divide(pmax(roll3_empty_net_new, 0), current_row$throughput)
  buffer_space_raw_new <- pmax(1 - pressure_flow_raw_new, 0)
  buffer_net_raw_new <- safe_divide(pmax(-roll3_empty_net_new, 0), current_row$throughput)
  buffer_export_raw_new <- current_row$export_pull

  pressure_flow_hist <- port_history$pressure_flow_raw
  pressure_net_hist <- port_history$pressure_net_raw
  pressure_roll_hist <- port_history$pressure_roll_raw
  buffer_space_hist <- port_history$buffer_space_raw
  buffer_net_hist <- port_history$buffer_net_raw
  buffer_export_hist <- port_history$buffer_export_raw

  pressure_flow_hist[length(pressure_flow_hist)] <- pressure_flow_raw_new
  pressure_net_hist[length(pressure_net_hist)] <- pressure_net_raw_new
  pressure_roll_hist[length(pressure_roll_hist)] <- pressure_roll_raw_new
  buffer_space_hist[length(buffer_space_hist)] <- buffer_space_raw_new
  buffer_net_hist[length(buffer_net_hist)] <- buffer_net_raw_new
  buffer_export_hist[length(buffer_export_hist)] <- buffer_export_raw_new

  pressure_index_new <-
    pressure_weights[["flow"]] * current_percent_rank(pressure_flow_hist) +
    pressure_weights[["net"]] * current_percent_rank(pressure_net_hist) +
    pressure_weights[["roll"]] * current_percent_rank(pressure_roll_hist)

  buffer_index_new <-
    buffer_weights[["space"]] * current_percent_rank(buffer_space_hist) +
    buffer_weights[["netout"]] * current_percent_rank(buffer_net_hist) +
    buffer_weights[["export"]] * current_percent_rank(buffer_export_hist)

  tibble(
    empty_in_after = empty_in_new,
    empty_out_after = empty_out_new,
    empty_net_after = empty_net_new,
    roll3_empty_net_after = roll3_empty_net_new,
    pressure_index_after = pressure_index_new,
    buffer_index_after = buffer_index_new
  )
}

simulate_pair <- function(
  ym_value,
  source_port,
  target_port,
  move_amount,
  status_table,
  history_table,
  pressure_weights,
  buffer_weights
) {
  month_status <- status_table %>%
    filter(ym == ym_value) %>%
    transmute(
      port,
      pressure_index_before = pressure_index,
      buffer_index_before = buffer_index,
      roll3_empty_net_before = roll3_empty_net,
      status_before = status
    )

  source_row <- month_status %>% filter(port == source_port) %>% slice(1)
  target_row <- month_status %>% filter(port == target_port) %>% slice(1)

  source_history <- history_table %>% filter(port == source_port, ym <= ym_value) %>% arrange(ym)
  target_history <- history_table %>% filter(port == target_port, ym <= ym_value) %>% arrange(ym)

  source_projection <- compute_projected_port(source_history, "source", move_amount, pressure_weights, buffer_weights)
  target_projection <- compute_projected_port(target_history, "target", move_amount, pressure_weights, buffer_weights)

  projected_month <- month_status %>%
    mutate(
      projected_pressure_index = case_when(
        port == source_port ~ source_projection$pressure_index_after[[1]],
        port == target_port ~ target_projection$pressure_index_after[[1]],
        TRUE ~ pressure_index_before
      ),
      projected_buffer_index = case_when(
        port == source_port ~ source_projection$buffer_index_after[[1]],
        port == target_port ~ target_projection$buffer_index_after[[1]],
        TRUE ~ buffer_index_before
      ),
      projected_roll3_empty_net = case_when(
        port == source_port ~ source_projection$roll3_empty_net_after[[1]],
        port == target_port ~ target_projection$roll3_empty_net_after[[1]],
        TRUE ~ roll3_empty_net_before
      )
    )

  high_pressure_cut <- quantile(projected_month$projected_pressure_index, 0.75, na.rm = TRUE, type = 7)
  high_buffer_cut <- quantile(projected_month$projected_buffer_index, 0.75, na.rm = TRUE, type = 7)
  low_pressure_cut <- quantile(projected_month$projected_pressure_index, 0.50, na.rm = TRUE, type = 7)

  projected_month <- projected_month %>%
    mutate(
      projected_status = case_when(
        projected_pressure_index >= high_pressure_cut & projected_roll3_empty_net > 0 ~ "高壓力港",
        projected_buffer_index >= high_buffer_cut & projected_pressure_index <= low_pressure_cut ~ "高緩衝港",
        TRUE ~ "正常港"
      )
    )

  source_status_after <- projected_month %>% filter(port == source_port) %>% pull(projected_status)
  target_status_after <- projected_month %>% filter(port == target_port) %>% pull(projected_status)

  result_class <- simulation_result_class(
    source_status_after = source_status_after,
    target_pressure_after = target_projection$pressure_index_after[[1]],
    high_pressure_cut = high_pressure_cut
  )

  tibble(
    ym = ym_value,
    source_port = source_port,
    target_port = target_port,
    move_amount = move_amount,
    source_status_before = source_row$status_before[[1]],
    target_status_before = target_row$status_before[[1]],
    source_status_after = source_status_after,
    target_status_after = target_status_after,
    source_pressure_before = source_row$pressure_index_before[[1]],
    source_pressure_after = source_projection$pressure_index_after[[1]],
    target_pressure_before = target_row$pressure_index_before[[1]],
    target_pressure_after = target_projection$pressure_index_after[[1]],
    source_buffer_after = source_projection$buffer_index_after[[1]],
    target_buffer_after = target_projection$buffer_index_after[[1]],
    source_empty_net_after = source_projection$empty_net_after[[1]],
    target_empty_net_after = target_projection$empty_net_after[[1]],
    projected_high_pressure_cut = high_pressure_cut,
    projected_high_buffer_cut = high_buffer_cut,
    projected_low_pressure_cut = low_pressure_cut,
    simulation_result = result_class
  )
}

build_candidate_table <- function(
  ym_value,
  source_port,
  move_amount,
  status_table,
  history_table,
  matching_weights,
  pressure_weights,
  buffer_weights
) {
  snapshot <- status_table %>% filter(ym == ym_value)
  source_row <- snapshot %>% filter(port == source_port) %>% slice(1)

  candidates <- snapshot %>%
    filter(port != source_port) %>%
    transmute(
      target_port = port,
      target_status = status,
      target_pressure = pressure_index,
      target_buffer = buffer_index,
      target_empty_net = empty_net
    ) %>%
    left_join(
      distance_class_lookup %>%
        filter(source_port == !!source_port) %>%
        select(target_port, distance_class, distance_factor),
      by = c("target_port")
    ) %>%
    left_join(
      distance_detail %>%
        filter(source_port == !!source_port) %>%
        select(target_port, distance_km, eta_days),
      by = c("target_port")
    ) %>%
    filter(!is.na(distance_factor)) %>%
    mutate(
      ym = ym_value,
      source_port = source_port,
      source_status = source_row$status[[1]],
      source_pressure = source_row$pressure_index[[1]],
      source_buffer = source_row$buffer_index[[1]],
      source_empty_net = source_row$empty_net[[1]],
      matching_score =
        matching_weights[["source"]] * source_pressure +
        matching_weights[["target"]] * target_buffer +
        matching_weights[["distance"]] * distance_factor
    )

  sim_rows <- lapply(seq_len(nrow(candidates)), function(i) {
    row <- candidates[i, ]
    simulate_pair(
      ym_value = ym_value,
      source_port = source_port,
      target_port = row$target_port[[1]],
      move_amount = move_amount,
      status_table = status_table,
      history_table = history_table,
      pressure_weights = pressure_weights,
      buffer_weights = buffer_weights
    )
  }) %>%
    bind_rows()

  candidates %>%
    left_join(sim_rows, by = c("ym" = "ym", "source_port", "target_port")) %>%
    mutate(
      candidate_key = target_port,
      summary_text = case_when(
        simulation_result == "完全解壓" ~ "可讓 source 退出高壓，且 target 保持安全",
        simulation_result == "可緩解但未解壓" ~ "可部分緩解 source 壓力，target 仍可承接",
        TRUE ~ "調度後 target 壓力可能過高，僅供觀察"
      ),
      pressure_before_pct = round(target_pressure * 100),
      pressure_after_pct = round(target_pressure_after * 100),
      pressure_increase_pct = pmax(0, pressure_after_pct - pressure_before_pct),
      source_pressure_pct = round(source_pressure * 100),
      source_pressure_after_pct = round(source_pressure_after * 100),
      relief_pct = ifelse(
        source_pressure <= 0,
        0,
        round(pmax(0, (source_pressure - source_pressure_after) / source_pressure) * 100)
      ),
      result_rank = simulation_result_rank(simulation_result),
      eta_days = eta_days %||% distance_class,
      distance_km = distance_km %||% (distance_class * 200)
    ) %>%
    select(
      ym,
      source_port,
      source_status,
      source_pressure,
      source_buffer,
      source_empty_net,
      target_port,
      candidate_key,
      target_status,
      target_pressure,
      target_buffer,
      target_empty_net,
      distance_class,
      distance_factor,
      distance_km,
      eta_days,
      matching_score,
      move_amount,
      source_status_before,
      target_status_before,
      source_status_after,
      target_status_after,
      source_pressure_before,
      source_pressure_after,
      target_pressure_before,
      target_pressure_after,
      source_buffer_after,
      target_buffer_after,
      source_empty_net_after,
      target_empty_net_after,
      projected_high_pressure_cut,
      projected_high_buffer_cut,
      projected_low_pressure_cut,
      simulation_result,
      summary_text,
      pressure_before_pct,
      pressure_after_pct,
      pressure_increase_pct,
      source_pressure_pct,
      source_pressure_after_pct,
      relief_pct,
      result_rank
    ) %>%
    arrange(result_rank, desc(matching_score), desc(target_buffer), target_port) %>%
    mutate(
      pair_tier = case_when(
        row_number() == 1 & target_status == "高緩衝港" ~ "正式推薦",
        target_status == "高緩衝港" ~ "正式候選",
        TRUE ~ "觀察候選"
      ),
      tier_rank = case_when(
        pair_tier == "正式推薦" ~ 1L,
        pair_tier == "正式候選" ~ 2L,
        TRUE ~ 3L
      )
    ) %>%
    arrange(result_rank, tier_rank, desc(matching_score), target_port)
}

app_css <- r"--(
:root {
    --blue: #2563EB;
    --blue-dark: #1d4ed8;
    --blue-light: #eff6ff;
    --blue-mid: #dbeafe;
    --bg: #eef2f7;
    --white: #ffffff;
    --text: #111827;
    --muted: #6b7280;
    --border: #e5e7eb;
    --row-hover: #f9fafb;
    --danger: #dc2626;
    --danger-bg: #fef2f2;
    --success: #16a34a;
    --success-bg: #f0fdf4;
    --warning: #d97706;
    --warning-bg: #fef3c7;
    --info: #0284c7;
    --info-bg: #e0f2fe;
    --r: 13px;
    --t: all 0.15s ease;
}
*, *::before, *::after {
    margin: 0;
    padding: 0;
    box-sizing: border-box;
}
html, body {
    min-height: 100%;
}
body {
    font-family: 'Inter', 'Noto Sans TC', sans-serif;
    background: var(--bg);
    color: var(--text);
    font-size: 14px;
}
.app {
    display: flex;
    min-height: 100vh;
    align-items: flex-start;
}
.hamburger-btn {
    display: flex;
    flex-direction: column;
    gap: 4px;
    padding: 8px;
    background: none;
    border: 1px solid var(--border);
    border-radius: 8px;
    cursor: pointer;
    flex-shrink: 0;
    transition: var(--t);
}
.hamburger-btn:hover {
    background: var(--blue-light);
    border-color: var(--blue);
}
.hamburger-btn span {
    display: block;
    width: 16px;
    height: 2px;
    background: var(--muted);
    border-radius: 1px;
    transition: var(--t);
}
.hamburger-btn:hover span {
    background: var(--blue);
}
.sidebar-overlay {
    display: none;
    position: fixed;
    inset: 0;
    background: rgba(0, 0, 0, 0.25);
    z-index: 40;
    backdrop-filter: blur(1px);
}
.sidebar {
    position: sticky;
    top: 0;
    align-self: flex-start;
    flex-shrink: 0;
    width: 0;
    min-width: 0;
    height: 100vh;
    overflow-x: hidden;
    overflow-y: auto;
    background: var(--white);
    border-right: 1px solid var(--border);
    display: flex;
    flex-direction: column;
    padding: 0;
    transition: width 0.25s ease, min-width 0.25s ease, box-shadow 0.25s ease;
}
.sidebar.open {
    width: 252px;
    min-width: 252px;
    box-shadow: 4px 0 20px rgba(0, 0, 0, 0.08);
}
.brand {
    display: flex;
    align-items: center;
    gap: 11px;
    padding: 22px 20px 18px;
    border-bottom: 1px solid var(--border);
    flex-shrink: 0;
}
.brand-icon {
    width: 34px;
    height: 34px;
    background: var(--blue);
    border-radius: 8px;
    display: flex;
    align-items: center;
    justify-content: center;
    color: white;
    flex-shrink: 0;
}
.brand-name {
    font-size: 0.95rem;
    font-weight: 700;
    color: var(--text);
    letter-spacing: -0.01em;
}
.nav-section {
    padding: 18px 14px 12px;
}
.nav-section-label {
    font-size: 0.65rem;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.08em;
    color: var(--muted);
    padding: 0 10px 10px;
    opacity: 0.7;
}
.nav-tabs {
    display: flex;
    flex-direction: column;
    gap: 4px;
}
.nav-tab {
    display: flex;
    align-items: center;
    gap: 11px;
    width: 100%;
    padding: 11px 12px;
    border-radius: 10px;
    font-size: 0.86rem;
    font-weight: 500;
    color: var(--muted);
    text-decoration: none;
    cursor: pointer;
    transition: var(--t);
    background: none;
    border: none;
    font-family: inherit;
    text-align: left;
}
.nav-tab:hover {
    background: var(--blue-light);
    color: var(--blue);
}
.nav-tab.active {
    background: var(--blue);
    color: white;
    font-weight: 600;
}
.nav-tab svg {
    flex-shrink: 0;
    opacity: 0.8;
}
.nav-tab.active svg,
.nav-tab:hover svg {
    opacity: 1;
}
.sidebar-filter {
    margin: 0 14px 18px;
    border: 1px solid var(--border);
    border-radius: var(--r);
    background: var(--bg);
    padding: 16px;
    display: flex;
    flex-direction: column;
    gap: 14px;
    flex-shrink: 0;
}
.sf-header {
    display: flex;
    align-items: center;
    gap: 7px;
    font-size: 0.86rem;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.04em;
    color: var(--muted);
}
.sf-header-title {
    display: flex;
    align-items: center;
    gap: 7px;
    flex: 1;
}
.btn-adv {
    display: inline-flex;
    align-items: center;
    gap: 4px;
    padding: 3px 8px;
    border: 1px solid var(--border);
    border-radius: 6px;
    font-size: 0.65rem;
    font-weight: 600;
    color: var(--muted);
    background: var(--white);
    cursor: pointer;
    transition: var(--t);
    font-family: inherit;
    white-space: nowrap;
}
.btn-adv:hover {
    background: var(--blue-light);
    border-color: var(--blue);
    color: var(--blue);
}
.sf-group {
    display: flex;
    flex-direction: column;
    gap: 6px;
}
.sf-label {
    font-size: 0.82rem;
    font-weight: 600;
    color: var(--muted);
}
.sf-select {
    height: 34px;
    padding: 0 10px;
    border: 1px solid var(--border);
    border-radius: 8px;
    font-size: 0.82rem;
    font-family: inherit;
    color: var(--text);
    background: var(--white);
    cursor: pointer;
    width: 100%;
    transition: var(--t);
}
.sf-select:focus {
    outline: none;
    border-color: var(--blue);
    box-shadow: 0 0 0 2px var(--blue-light);
}
.sf-threshold-row {
    display: flex;
    align-items: center;
    justify-content: space-between;
    margin-bottom: 6px;
}
.sf-val {
    font-size: 0.82rem;
    font-weight: 700;
    color: var(--blue);
}
.sf-slider {
    -webkit-appearance: none;
    appearance: none;
    width: 100%;
    height: 4px;
    background: var(--border);
    border-radius: 2px;
    outline: none;
    cursor: pointer;
    margin: 0 0 4px;
}
.sf-slider::-webkit-slider-thumb {
    -webkit-appearance: none;
    width: 16px;
    height: 16px;
    border-radius: 50%;
    background: var(--blue);
    border: 2px solid white;
    box-shadow: 0 0 0 1px var(--blue);
    cursor: pointer;
}
.sf-ticks {
    display: flex;
    justify-content: space-between;
    font-size: 0.65rem;
    color: var(--muted);
    margin-top: 6px;
}
.mode-switch,
.explore-tab-switch {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 8px;
}
.shiny-input-radiogroup {
    margin-bottom: 0;
}
.shiny-input-radiogroup .shiny-options-group {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 8px;
}
.shiny-input-radiogroup .radio-inline {
    margin: 0 !important;
    padding: 8px 10px 8px 30px;
    border: 1px solid var(--border);
    border-radius: 8px;
    background: var(--white);
    color: var(--muted);
    font-size: 0.76rem;
    font-weight: 700;
}
.shiny-input-radiogroup input[type='radio'] {
    margin-top: 2px;
}
.mode-btn,
.explore-tab-btn,
.strategy-btn {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    gap: 6px;
    padding: 8px 10px;
    border: 1px solid var(--border);
    border-radius: 8px;
    background: var(--white);
    color: var(--muted);
    font-size: 0.76rem;
    font-weight: 700;
    cursor: pointer;
    transition: var(--t);
    font-family: inherit;
}
.mode-btn:hover,
.explore-tab-btn:hover,
.strategy-btn:hover {
    border-color: var(--blue-mid);
    color: var(--blue);
    background: var(--blue-light);
}
.mode-btn.active,
.explore-tab-btn.active,
.strategy-btn.active {
    background: var(--blue);
    border-color: var(--blue);
    color: white;
}
.mode-note,
.mode-summary,
.strategy-desc {
    padding: 10px 12px;
    border-radius: 8px;
    border: 1px solid var(--border);
    background: #f9fafb;
    font-size: 0.72rem;
    color: var(--muted);
    line-height: 1.55;
}
.home-block {
    margin-bottom: 16px;
}
.main-area .shiny-input-container {
    width: 100%;
    margin-bottom: 0;
}
.main-area .control-label {
    font-size: 0.74rem;
    font-weight: 700;
    color: var(--muted);
    margin-bottom: 6px;
}
.explore-tab-panel .nav {
    border-bottom: 1px solid var(--border);
    margin-bottom: 10px;
}
.explore-tab-panel .nav-item {
    margin-bottom: -1px;
}
.explore-tab-panel .nav-link {
    border: none;
    border-bottom: 2px solid transparent;
    border-radius: 0;
    background: transparent;
    color: var(--muted);
    font-size: 0.76rem;
    font-weight: 700;
    padding: 8px 12px;
}
.explore-tab-panel .nav-link.active,
.explore-tab-panel .nav-link:hover {
    background: transparent;
    color: var(--blue);
    border-bottom-color: var(--blue);
}
.weight-mini-list {
    display: flex;
    flex-wrap: wrap;
    gap: 6px;
}
.weight-mini-chip {
    display: inline-flex;
    align-items: center;
    gap: 4px;
    padding: 3px 8px;
    border-radius: 999px;
    background: var(--blue-light);
    color: var(--blue);
    font-size: 0.68rem;
    font-weight: 700;
}
.strategy-stack {
    display: flex;
    flex-direction: column;
    gap: 10px;
}
.strategy-card {
    padding: 12px 13px;
    border-radius: 10px;
    border: 1px solid var(--border);
    background: var(--white);
    display: flex;
    flex-direction: column;
    gap: 8px;
}
.strategy-title {
    font-size: 0.82rem;
    font-weight: 700;
    color: var(--text);
}
.strategy-sub {
    font-size: 0.71rem;
    color: var(--muted);
    line-height: 1.5;
}
.adv-help {
    grid-column: 1 / -1;
    padding: 12px 14px;
    border-radius: 10px;
    border: 1px dashed var(--blue-mid);
    background: var(--blue-light);
    color: #334155;
    font-size: 0.73rem;
    line-height: 1.6;
}
.main-area {
    flex: 1;
    min-width: 0;
}
.page {
    padding: 14px 20px 40px;
    display: flex;
    flex-direction: column;
}
.page-head {
    display: flex;
    align-items: center;
    gap: 12px;
    margin-bottom: 12px;
    flex-shrink: 0;
}
.page-title {
    font-size: 1.2rem;
    font-weight: 700;
    color: var(--text);
    letter-spacing: -0.02em;
}
.bento {
    display: grid;
    grid-template-columns: repeat(4, 1fr);
    gap: 12px;
}
.span1 { grid-column: span 1; }
.span2 { grid-column: span 2; }
.span3 { grid-column: span 3; }
.span4 { grid-column: span 4; }
.card {
    background: var(--white);
    border-radius: var(--r);
    border: 1px solid var(--border);
    display: flex;
    flex-direction: column;
}
.c-inner {
    padding: 14px 16px;
    display: flex;
    flex-direction: column;
    flex: 1;
}
.c-head {
    display: flex;
    align-items: center;
    justify-content: space-between;
    margin-bottom: 10px;
    flex-shrink: 0;
}
.c-title {
    font-size: 0.84rem;
    font-weight: 600;
    color: var(--text);
}
.c-subtitle {
    font-size: 0.72rem;
    color: var(--muted);
    margin-top: 2px;
}
.c-chip {
    font-size: 0.68rem;
    font-weight: 600;
    padding: 3px 8px;
    border-radius: 6px;
    background: var(--blue-light);
    color: var(--blue);
    cursor: default;
}
.chart-wrap {
    position: relative;
    height: 220px;
}
.chart-wrap .shiny-plot-output {
    height: 100% !important;
}
.feat-card {
    background: var(--blue);
    border-color: var(--blue);
}
.feat-inner {
    padding: 14px 16px;
    display: flex;
    flex-direction: column;
    gap: 8px;
    flex: 1;
}
.feat-chip {
    display: inline-flex;
    align-items: center;
    gap: 5px;
    background: rgba(255, 255, 255, 0.15);
    color: white;
    padding: 3px 9px;
    border-radius: 6px;
    font-size: 0.68rem;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    width: fit-content;
}
.feat-title {
    font-size: 1rem;
    font-weight: 700;
    color: white;
    line-height: 1.35;
}
.feat-desc {
    font-size: 0.75rem;
    color: rgba(255, 255, 255, 0.78);
    line-height: 1.6;
}
.feat-alert-items {
    display: flex;
    flex-direction: column;
    gap: 9px;
    background: rgba(255, 255, 255, 0.12);
    border-radius: 8px;
    padding: 10px 12px;
}
.feat-alert-item {
    display: flex;
    flex-direction: column;
    gap: 3px;
}
.feat-alert-label {
    font-size: 0.68rem;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.04em;
    color: rgba(255,255,255,0.72);
}
.feat-alert-val {
    font-size: 0.8rem;
    font-weight: 600;
    color: white;
    line-height: 1.45;
}
.feat-kpi-row {
    display: flex;
    align-items: stretch;
    height: 72px;
    background: rgba(255,255,255,0.12);
    border-radius: 8px;
    overflow: hidden;
}
.feat-kpi-item {
    flex: 1;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    gap: 3px;
}
.feat-kpi-sep {
    width: 1px;
    background: rgba(255,255,255,0.22);
    flex-shrink: 0;
}
.feat-kpi-num {
    font-size: 2.4rem;
    font-weight: 700;
    line-height: 1;
}
.feat-kpi-num.danger { color: #ffd5d5; }
.feat-kpi-num.success { color: #d4f9dd; }
.feat-kpi-num.blue { color: #dbeafe; }
.feat-kpi-lbl {
    font-size: 0.72rem;
    color: rgba(255,255,255,0.85);
}
.feat-action {
    display: flex;
    align-items: center;
    gap: 6px;
    margin-top: 0;
    padding: 7px 12px;
    background: rgba(255, 255, 255, 0.15);
    border-radius: 7px;
    color: white;
    font-size: 0.75rem;
    font-weight: 600;
    cursor: pointer;
    transition: var(--t);
    width: fit-content;
    border: none;
    text-decoration: none;
}
.feat-action:hover {
    background: rgba(255, 255, 255, 0.25);
    color: white;
    text-decoration: none;
}
.port-cards {
    display: grid;
    grid-template-columns: repeat(3, 1fr);
    gap: 8px;
}
.port-card {
    border-radius: 10px;
    padding: 11px 13px;
    border: 1px solid transparent;
    display: flex;
    flex-direction: column;
    justify-content: space-between;
}
.port-card.high {
    background: #fef2f2;
    border-color: #fecaca;
}
.port-card.mid {
    background: #fffbeb;
    border-color: #fde68a;
}
.port-card.low {
    background: #f0fdf4;
    border-color: #bbf7d0;
}
.port-card.summary {
    background: var(--blue-light);
    border-color: var(--blue-mid);
}
.port-card.excluded {
    background: #f8fafc;
    border-color: #cbd5e1;
}
.pc-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    margin-bottom: 7px;
}
.pc-name-group {
    display: flex;
    align-items: center;
    gap: 8px;
}
.pc-dot {
    width: 10px;
    height: 10px;
    border-radius: 50%;
    flex-shrink: 0;
}
.pc-dot.high { background: #dc2626; }
.pc-dot.mid { background: #f97316; }
.pc-dot.low { background: #16a34a; }
.pc-dot.excluded { background: #94a3b8; }
.pc-name {
    font-size: 0.88rem;
    font-weight: 700;
    color: var(--text);
}
.pc-code {
    font-size: 0.7rem;
    color: var(--muted);
    margin-top: 1px;
}
.pc-badge {
    font-size: 0.66rem;
    font-weight: 600;
    padding: 2px 8px;
    border-radius: 6px;
    background: rgba(255, 255, 255, 0.75);
    border: 1px solid transparent;
}
.pc-badge.high { color: #dc2626; border-color: #fecaca; }
.pc-badge.mid { color: #f97316; border-color: #fed7aa; }
.pc-badge.low { color: #16a34a; border-color: #bbf7d0; }
.pc-badge.excluded { color: #64748b; border-color: #cbd5e1; }
.pc-stats {
    display: flex;
    justify-content: space-between;
    font-size: 0.72rem;
    color: var(--muted);
    margin-bottom: 5px;
}
.pc-stat-val {
    font-weight: 600;
    color: var(--text);
}
.pc-bar-row {
    display: flex;
    align-items: center;
    gap: 8px;
}
.pc-bar-track {
    flex: 1;
    height: 5px;
    background: rgba(0, 0, 0, 0.09);
    border-radius: 3px;
    overflow: hidden;
}
.pc-bar-fill {
    height: 100%;
    border-radius: 3px;
}
.pc-bar-fill.high { background: #dc2626; }
.pc-bar-fill.mid { background: #f97316; }
.pc-bar-fill.low { background: #16a34a; }
.pc-pct {
    font-size: 0.72rem;
    font-weight: 700;
    min-width: 34px;
    text-align: right;
}
.pc-pct.high { color: #dc2626; }
.pc-pct.mid { color: #f97316; }
.pc-pct.low { color: #16a34a; }
.summary-totals {
    display: flex;
    flex-direction: column;
    gap: 5px;
    flex: 1;
    justify-content: center;
}
.summary-row {
    display: flex;
    justify-content: space-between;
    align-items: baseline;
    font-size: 0.72rem;
}
.summary-row-label {
    color: var(--muted);
}
.summary-row-val {
    font-weight: 700;
    color: var(--blue);
}
.pair-list {
    display: flex;
    flex-direction: column;
    flex: 1;
    justify-content: space-around;
}
.pair-row {
    display: flex;
    align-items: center;
    gap: 6px;
    padding: 10px 2px;
    border-bottom: 1px solid var(--border);
}
.pair-port {
    display: flex;
    flex-direction: column;
    gap: 4px;
    min-width: 60px;
}
.pair-name {
    font-size: 0.88rem;
    font-weight: 600;
    color: var(--text);
}
.pair-tag {
    font-size: 0.68rem;
    font-weight: 600;
}
.pair-tag.danger { color: var(--danger); }
.pair-tag.success { color: var(--success); }
.pair-tag.info { color: var(--blue); }
.pair-arrow {
    flex-shrink: 0;
}
.pair-teu {
    margin-left: auto;
    text-align: right;
    display: flex;
    flex-direction: column;
    align-items: flex-end;
}
.pair-teu-num {
    font-size: 1.1rem;
    font-weight: 700;
    color: var(--blue);
    line-height: 1;
}
.pair-teu-unit {
    font-size: 0.7rem;
    color: var(--muted);
    margin-top: 2px;
}
.ds-section {
    margin-top: 20px;
}
.dt-card {
    background: var(--white);
    border: 1px solid var(--border);
    border-radius: var(--r);
    overflow: hidden;
}
.dt-card-head {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 12px 16px;
    border-bottom: 1px solid var(--border);
}
.dt-card-title {
    font-size: 0.84rem;
    font-weight: 700;
    color: var(--text);
}
.dt-controls {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 10px 16px;
    background: #f9fafb;
    border-bottom: 1px solid var(--border);
    gap: 12px;
    flex-wrap: wrap;
}
.dt-show-wrap, .dt-search-wrap {
    display: flex;
    align-items: center;
    gap: 7px;
    font-size: 0.76rem;
    color: var(--muted);
}
.dt-show-select {
    height: 28px;
    padding: 0 6px;
    border: 1px solid var(--border);
    border-radius: 6px;
    font-size: 0.76rem;
    font-family: inherit;
    color: var(--text);
    background: var(--white);
    cursor: pointer;
}
.dt-search-input {
    height: 28px;
    padding: 0 8px;
    border: 1px solid var(--border);
    border-radius: 6px;
    font-size: 0.76rem;
    font-family: inherit;
    color: var(--text);
    background: var(--white);
    width: 180px;
    outline: none;
}
.dt-search-input:focus {
    border-color: var(--blue);
    box-shadow: 0 0 0 2px var(--blue-light);
}
.dt-table-wrap {
    overflow-x: auto;
}
.dt-table {
    width: 100%;
    border-collapse: collapse;
}
.dt-table th {
    padding: 9px 12px;
    text-align: left;
    font-size: 0.68rem;
    font-weight: 700;
    color: var(--muted);
    background: #f9fafb;
    border-bottom: 1px solid var(--border);
    cursor: pointer;
    white-space: nowrap;
}
.dt-table th:first-child {
    cursor: default;
}
.dt-table th:hover:not(:first-child) {
    background: #f0f4f8;
    color: var(--text);
}
.dt-table td {
    padding: 9px 12px;
    font-size: 0.76rem;
    border-bottom: 1px solid var(--border);
    white-space: nowrap;
    color: var(--text);
}
.dt-row-num {
    color: var(--muted);
    font-size: 0.7rem;
}
.dt-mono {
    font-size: 0.72rem;
    color: var(--muted);
}
.th-inner {
    display: inline-flex;
    align-items: center;
    gap: 5px;
}
.dt-sort-icon {
    display: inline-flex;
    flex-direction: column;
    gap: 2px;
}
.dt-sort-icon .sa,
.dt-sort-icon .sd {
    display: block;
    width: 0;
    height: 0;
    border-left: 4px solid transparent;
    border-right: 4px solid transparent;
    opacity: 0.25;
}
.dt-sort-icon .sa { border-bottom: 5px solid currentColor; }
.dt-sort-icon .sd { border-top: 5px solid currentColor; }
.dt-sort-icon.asc .sa { opacity: 1; }
.dt-sort-icon.desc .sd { opacity: 1; }
.dt-footer {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 10px 16px;
    border-top: 1px solid var(--border);
    background: #f9fafb;
    flex-wrap: wrap;
    gap: 8px;
}
.dt-info {
    font-size: 0.74rem;
    color: var(--muted);
}
.dt-pagination {
    display: flex;
    gap: 4px;
    align-items: center;
}
.dt-page-btn {
    height: 28px;
    padding: 0 12px;
    border: 1px solid var(--border);
    border-radius: 6px;
    font-size: 0.74rem;
    font-family: inherit;
    color: var(--text);
    background: var(--white);
    cursor: pointer;
    transition: background .15s, border-color .15s, color .15s;
}
.dt-page-btn:hover:not(:disabled) {
    background: var(--blue-light);
    border-color: var(--blue);
    color: var(--blue);
}
.dt-page-btn:disabled {
    opacity: 0.38;
    cursor: default;
}
.dt-page-num {
    font-size: 0.74rem;
    color: var(--muted);
    padding: 0 6px;
}
.dt-legend {
    display: flex;
    flex-wrap: wrap;
    gap: 6px 20px;
    padding: 10px 16px;
    border-top: 1px solid var(--border);
    background: #f9fafb;
}
.dt-legend-item {
    font-size: 0.72rem;
    color: var(--muted);
}
.dt-legend-item code {
    font-family: 'SFMono-Regular', Consolas, monospace;
    font-size: 0.7rem;
    background: var(--border);
    color: var(--text);
    padding: 1px 5px;
    border-radius: 4px;
    margin-right: 4px;
}
.port-selectors {
    display: flex;
    gap: 6px;
    flex-wrap: wrap;
}
.port-btn {
    background: var(--white);
    border: 1px solid var(--border);
    color: var(--muted);
    padding: 6px 14px;
    border-radius: 8px;
    font-size: 0.75rem;
    font-weight: 600;
    cursor: pointer;
    transition: var(--t);
    font-family: inherit;
}
.port-btn:hover {
    border-color: var(--blue-mid);
    color: var(--text);
}
.port-btn.active {
    background: var(--blue);
    border-color: var(--blue);
    color: white;
    box-shadow: 0 2px 4px rgba(37, 99, 235, 0.2);
}
.port-btn.disabled {
    background: #f8fafc;
    color: #94a3b8;
    border-color: #e2e8f0;
    cursor: not-allowed;
}
.summary-grid {
    display: grid;
    grid-template-columns: repeat(4, 1fr);
    gap: 16px;
    padding-top: 10px;
    flex: 1;
}
.stat-box {
    background: var(--bg);
    border: 1px solid var(--border);
    border-radius: 10px;
    padding: 16px 14px;
    display: flex;
    flex-direction: column;
    gap: 4px;
    justify-content: center;
}
.stat-label {
    font-size: 0.75rem;
    color: var(--muted);
    font-weight: 600;
}
.stat-value {
    font-size: 1.35rem;
    font-weight: 700;
    color: var(--text);
    line-height: 1;
}
.stat-value.highlight {
    color: var(--blue);
}
.stat-value.danger {
    color: var(--danger);
}
.cross-settings-bar {
    background: var(--white);
    border-radius: var(--r);
    padding: 0 0 16px;
    display: flex;
    align-items: flex-end;
    gap: 12px;
    flex-wrap: wrap;
}
.csb-group {
    display: flex;
    flex-direction: column;
    gap: 5px;
    min-width: 0;
}
.csb-group.csb-narrow {
    flex: 1.2;
    min-width: 150px;
}
.csb-group.csb-md {
    flex: 1;
    min-width: 120px;
}
.csb-group label {
    font-size: 0.72rem;
    font-weight: 700;
    color: var(--muted);
    white-space: nowrap;
}
.csb-group select,
.csb-group input[type='number'],
.csb-group input[type='date'] {
    height: 34px;
    padding: 0 10px;
    border: 1.5px solid var(--border);
    border-radius: 8px;
    font-size: 0.82rem;
    color: var(--text);
    font-family: inherit;
    outline: none;
    background: var(--bg);
    width: 100%;
}
.csb-group select:focus,
.csb-group input[type='number']:focus,
.csb-group input[type='date']:focus {
    border-color: var(--blue);
    background: var(--white);
    box-shadow: 0 0 0 2px var(--blue-light);
}
.csb-select-danger {
    color: var(--danger) !important;
    border-color: #fca5a5 !important;
    background-color: #fff5f5 !important;
}
.csb-actions {
    display: flex;
    flex-direction: row;
    gap: 8px;
    align-items: flex-end;
    flex-shrink: 0;
}
.btn-sim-search {
    height: 34px;
    padding: 0 14px;
    background: var(--blue);
    color: #fff;
    border: none;
    border-radius: 8px;
    font-size: 0.82rem;
    font-weight: 700;
    font-family: inherit;
    cursor: pointer;
    white-space: nowrap;
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 6px;
}
.cross-empty-state {
    background: var(--white);
    border: 1.5px dashed var(--border);
    border-radius: var(--r);
    padding: 40px 24px;
    margin-top: 0;
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 10px;
    text-align: center;
}
.cross-empty-state svg {
    width: 38px;
    height: 38px;
    color: #cbd5e1;
}
.ces-title {
    font-size: 0.88rem;
    font-weight: 700;
    color: var(--muted);
}
.ces-sub {
    font-size: 0.76rem;
    color: var(--muted);
    max-width: 400px;
    line-height: 1.6;
}
.cross-rec-area {
    background: none;
    border: none;
    padding: 0;
    margin-top: 12px;
    display: flex;
    flex-direction: column;
    gap: 14px;
}
.match-right-header {
    display: flex;
    align-items: flex-start;
    justify-content: space-between;
}
.mh-title {
    font-size: 1rem;
    font-weight: 700;
    color: var(--text);
    letter-spacing: -0.01em;
}
.mh-sub {
    font-size: 0.73rem;
    color: var(--muted);
    margin-top: 4px;
}
.sim-kpi-row {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 10px;
}
.sim-kpi-card {
    background: var(--blue-light);
    border: 1px solid var(--blue-mid);
    border-radius: 10px;
    padding: 14px 18px;
    text-align: center;
}
.sk-label {
    font-size: 0.72rem;
    color: var(--muted);
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.04em;
    margin-bottom: 8px;
}
.sk-val {
    font-size: 2.4rem;
    font-weight: 800;
    line-height: 1;
    letter-spacing: -0.02em;
}
.sk-diff {
    font-size: 0.76rem;
    font-weight: 600;
    color: var(--success);
    margin-top: 6px;
}
.match-sugg-grid {
    display: grid;
    grid-template-columns: repeat(3, 1fr);
    gap: 14px;
}
.match-sugg-card {
    background: var(--white);
    border: 1.5px solid var(--border);
    border-radius: var(--r);
    padding: 16px;
    cursor: pointer;
    transition: box-shadow .2s, border-color .2s, transform .2s;
}
.match-sugg-card:hover {
    box-shadow: 0 4px 12px rgba(0, 0, 0, .08);
    transform: translateY(-2px);
}
.match-sugg-card.msc-active {
    border-color: var(--blue);
    border-width: 2px;
    box-shadow: 0 0 0 3px var(--blue-light), 0 4px 16px rgba(37, 99, 235, .1);
}
.msc-header {
    display: flex;
    align-items: center;
    gap: 10px;
    margin-bottom: 10px;
}
.msc-rank {
    width: 28px;
    height: 28px;
    border-radius: 50%;
    background: #f1f5f9;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 0.76rem;
    font-weight: 800;
    color: var(--muted);
    flex-shrink: 0;
}
.match-sugg-card:nth-child(1) .msc-rank { background: #fef3c7; color: #b45309; }
.match-sugg-card:nth-child(2) .msc-rank { background: #f1f5f9; color: #475569; }
.match-sugg-card:nth-child(3) .msc-rank { background: #ede9fe; color: #7c3aed; }
.msc-name {
    font-weight: 800;
    font-size: 1.08rem;
    color: var(--text);
    letter-spacing: -0.01em;
    line-height: 1.2;
}
.msc-code {
    font-size: 0.72rem;
    color: var(--muted);
    margin-top: 2px;
}
.msc-best-badge {
    font-size: 0.62rem;
    font-weight: 700;
    padding: 2px 8px;
    border-radius: 5px;
    background: var(--blue);
    color: white;
    white-space: nowrap;
    margin-left: auto;
}
.msc-summary {
    font-size: 0.71rem;
    color: var(--muted);
    margin: -3px 0 10px;
    line-height: 1.45;
    padding-left: 2px;
}
.msc-tags {
    display: flex;
    gap: 5px;
    flex-wrap: wrap;
    margin-bottom: 12px;
}
.msc-tag {
    font-size: 0.62rem;
    font-weight: 700;
    padding: 2px 8px;
    border-radius: 20px;
}
.msc-tag.green { color: #15803d; background: #dcfce7; }
.msc-tag.blue { color: var(--blue); background: var(--blue-light); }
.msc-tag.amber { color: #b45309; background: #fef3c7; }
.msc-usage-row {
    display: flex;
    justify-content: space-between;
    align-items: baseline;
    font-size: 0.74rem;
    color: var(--muted);
    margin-bottom: 6px;
}
.msc-usage-row span:last-child {
    font-size: 0.96rem;
    font-weight: 800;
}
.rag-dot {
    display: inline-block;
    width: 7px;
    height: 7px;
    border-radius: 50%;
    margin-right: 4px;
    flex-shrink: 0;
    vertical-align: middle;
}
.rag-dot.rag-red { background: #dc2626; }
.rag-dot.rag-amber { background: #d97706; }
.rag-dot.rag-green { background: #16a34a; }
.msc-prog-track {
    height: 6px;
    background: var(--border);
    border-radius: 3px;
    overflow: hidden;
    position: relative;
}
.msc-prog-fill {
    height: 100%;
    border-radius: 3px;
    position: absolute;
    left: 0;
    top: 0;
}
.msc-prog-fill.green { background: var(--success); }
.msc-prog-fill.amber { background: var(--warning); }
.msc-prog-fill.red { background: var(--danger); }
.msc-prog-increase {
    position: absolute;
    top: 0;
    height: 100%;
    border-radius: 0 3px 3px 0;
    background: repeating-linear-gradient(-45deg, rgba(0,0,0,.13) 0px, rgba(0,0,0,.13) 2px, rgba(255,255,255,.25) 2px, rgba(255,255,255,.25) 4px);
}
.msc-stats-row {
    display: flex;
    justify-content: space-between;
    align-items: baseline;
    font-size: 0.74rem;
    color: var(--muted);
    margin-top: 11px;
    padding-top: 10px;
    border-top: 1px solid var(--border);
}
.msc-stat-item {
    display: inline-flex;
    align-items: center;
    gap: 4px;
}
.msc-stat-item strong {
    color: var(--text);
    font-size: 0.92rem;
    font-weight: 800;
}
.cross-confirm-row {
    display: flex;
    justify-content: flex-end;
    gap: 10px;
}
.btn-sim-cancel {
    padding: 7px 14px;
    background: transparent;
    color: var(--muted);
    border: 1.5px solid var(--border);
    border-radius: 8px;
    font-size: 0.8rem;
    font-weight: 700;
    font-family: inherit;
    cursor: pointer;
}
.btn-sim-cancel:hover {
    background: var(--bg);
    border-color: #94a3b8;
    color: var(--text);
}
.adv-backdrop {
    display: none;
    position: fixed;
    inset: 0;
    background: rgba(0, 0, 0, 0.35);
    z-index: 200;
    backdrop-filter: blur(2px);
    align-items: center;
    justify-content: center;
}
.adv-backdrop.open {
    display: flex;
}
.adv-modal {
    background: var(--white);
    border-radius: 16px;
    box-shadow: 0 20px 60px rgba(0, 0, 0, 0.18);
    width: min(860px, 95vw);
    max-height: 90vh;
    overflow-y: auto;
    display: flex;
    flex-direction: column;
}
.adv-modal-head {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 18px 24px 14px;
    border-bottom: 1px solid var(--border);
}
.adv-modal-title {
    font-size: 1rem;
    font-weight: 700;
    color: var(--text);
    letter-spacing: -0.01em;
}
.adv-modal-sub {
    font-size: 0.74rem;
    color: var(--muted);
    margin-top: 2px;
}
.adv-close {
    width: 30px;
    height: 30px;
    border-radius: 8px;
    border: 1px solid var(--border);
    background: none;
    cursor: pointer;
    display: flex;
    align-items: center;
    justify-content: center;
    color: var(--muted);
}
.adv-close:hover {
    background: var(--danger-bg);
    border-color: #fecaca;
    color: var(--danger);
}
.adv-modal-body {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
    gap: 16px;
    padding: 20px 24px;
}
.adv-col {
    background: #f9fafb;
    border: 1px solid var(--border);
    border-radius: 12px;
    padding: 18px;
    display: flex;
    flex-direction: column;
    gap: 12px;
}
.adv-col-head {
    display: flex;
    align-items: center;
    gap: 9px;
}
.adv-col-dot {
    width: 12px;
    height: 12px;
    border-radius: 50%;
}
.adv-col-dot.red { background: #ef4444; }
.adv-col-dot.green { background: #22c55e; }
.adv-col-title {
    font-size: 0.9rem;
    font-weight: 700;
    color: var(--text);
}
.adv-slider-card {
    background: var(--white);
    border: 1px solid var(--border);
    border-radius: 10px;
    padding: 13px 15px;
    display: flex;
    flex-direction: column;
    gap: 8px;
}
.adv-slider-top {
    display: flex;
    align-items: center;
    justify-content: space-between;
}
.adv-slider-name {
    font-size: 0.82rem;
    font-weight: 600;
    color: var(--text);
}
.adv-pct-badge {
    font-size: 0.7rem;
    font-weight: 700;
    padding: 2px 8px;
    border-radius: 5px;
    background: var(--blue-light);
    color: var(--blue);
}
.adv-slider-row {
    display: flex;
    align-items: center;
    gap: 10px;
}
.adv-slider-label {
    font-size: 0.7rem;
    color: var(--muted);
    white-space: nowrap;
    min-width: 38px;
}
.adv-slider {
    -webkit-appearance: none;
    appearance: none;
    flex: 1;
    height: 5px;
    background: var(--blue-mid);
    border-radius: 3px;
    outline: none;
    cursor: pointer;
}
.adv-slider::-webkit-slider-thumb {
    -webkit-appearance: none;
    width: 18px;
    height: 18px;
    border-radius: 50%;
    background: var(--blue);
    border: 2px solid white;
}
.adv-slider-val {
    font-size: 0.82rem;
    font-weight: 700;
    color: var(--text);
    min-width: 24px;
    text-align: right;
}
.adv-modal-foot {
    display: flex;
    align-items: center;
    justify-content: flex-end;
    gap: 8px;
    padding: 14px 24px 18px;
    border-top: 1px solid var(--border);
}
.adv-btn-cancel {
    padding: 7px 16px;
    border: 1.5px solid var(--border);
    border-radius: 8px;
    background: transparent;
    color: var(--muted);
    font-size: 0.8rem;
    font-weight: 600;
    cursor: pointer;
}
.adv-btn-apply {
    padding: 7px 20px;
    border: none;
    border-radius: 8px;
    background: var(--blue);
    color: white;
    font-size: 0.8rem;
    font-weight: 600;
    cursor: pointer;
}
.adv-btn-apply:hover { background: var(--blue-dark); }
.adv-btn-cancel:hover {
    background: var(--bg);
    border-color: #94a3b8;
    color: var(--text);
}
.hidden {
    display: none !important;
}
.shiny-plot-output img {
    width: 100% !important;
    height: 100% !important;
}
@media (max-width: 1150px) {
    .bento { grid-template-columns: repeat(2, 1fr); }
    .span2, .span3, .span4 { grid-column: span 2; }
    .summary-grid, .match-sugg-grid { grid-template-columns: repeat(2, 1fr); }
}
@media (max-width: 768px) {
    .sidebar {
        position: fixed;
        top: 0;
        left: 0;
        height: 100vh;
        z-index: 50;
        align-self: unset;
    }
    .sidebar-overlay.sidebar-visible { display: block; }
    .bento, .summary-grid, .match-sugg-grid, .port-cards, .adv-modal-body {
        grid-template-columns: 1fr;
    }
    .span1, .span2, .span3, .span4 {
        grid-column: span 1;
    }
    .page {
        padding: 12px 16px 32px;
    }
}
)--"

app_js <- r"--(
window.switchPage = function(pageName) {
    if (window.Shiny) {
        Shiny.setInputValue('nav_page_click', pageName + '::' + Date.now(), {priority: 'event'});
    }
};

window.toggleAnalysisPort = function(port) {
    if (window.Shiny) {
        Shiny.setInputValue('toggle_analysis_port', port + '::' + Date.now(), {priority: 'event'});
    }
};

window.selectMatchingTarget = function(port) {
    if (window.Shiny) {
        Shiny.setInputValue('matching_target_click', port + '::' + Date.now(), {priority: 'event'});
    }
};

window.sortRawTable = function(col) {
    if (window.Shiny) {
        Shiny.setInputValue('dt_sort_click', col + '::' + Date.now(), {priority: 'event'});
    }
};

window.rawTableAction = function(actionName) {
    if (window.Shiny) {
        Shiny.setInputValue('dt_action', actionName + '::' + Date.now(), {priority: 'event'});
    }
};

window.runMatchingSimulation = function() {
    if (window.Shiny) {
        Shiny.setInputValue('run_match', Date.now(), {priority: 'event'});
    }
};

window.resetMatchingSimulation = function() {
    if (window.Shiny) {
        Shiny.setInputValue('matching_reset', Date.now(), {priority: 'event'});
    }
};

window.setDashboardMode = function(modeName) {
    if (window.Shiny) {
        Shiny.setInputValue('dashboard_mode_click', modeName + '::' + Date.now(), {priority: 'event'});
    }
};

window.setExploreTab = function(tabName) {
    if (window.Shiny) {
        Shiny.setInputValue('explore_tab_click', tabName + '::' + Date.now(), {priority: 'event'});
    }
};

window.setStrategyTemplate = function(strategyName) {
    if (window.Shiny) {
        Shiny.setInputValue('strategy_template_click', strategyName + '::' + Date.now(), {priority: 'event'});
    }
};

window.weightGroupMap = {
    pressure: ['sl-flow', 'sl-net', 'sl-roll'],
    buffer: ['sl-space', 'sl-netout', 'sl-export'],
    matching: ['sl-source', 'sl-target', 'sl-distance']
};

window.pushWeightInputs = function(ids) {
    if (!window.Shiny) {
        return;
    }
    ids.forEach(id => {
        const el = document.getElementById(id);
        if (el) {
            Shiny.setInputValue(id, parseInt(el.value, 10), {priority: 'event'});
        }
    });
};

window.updateWeightGroupDisplay = function(groupName) {
    const ids = window.weightGroupMap[groupName] || [];
    if (!ids.length || !ids.every(id => document.getElementById(id))) {
        return;
    }
    ids.forEach(id => {
        const val = parseInt(document.getElementById(id).value, 10) || 0;
        const pctEl = document.getElementById(id.replace('sl-', 'pct-'));
        const valEl = document.getElementById(id.replace('sl-', 'val-'));
        if (pctEl) pctEl.textContent = `實質占比: ${val}%`;
        if (valEl) valEl.textContent = val;
    });
};

window.rebalanceTwoWeights = function(firstVal, secondVal, remaining, minVal, maxVal) {
    let a = firstVal;
    let b = secondVal;
    const base = a + b;

    if (base <= 0) {
        a = Math.round(remaining / 2);
        b = remaining - a;
    } else {
        a = Math.round(remaining * a / base);
        b = remaining - a;
    }

    if (a < minVal) {
        a = minVal;
        b = remaining - a;
    }
    if (b < minVal) {
        b = minVal;
        a = remaining - b;
    }
    if (a > maxVal) {
        a = maxVal;
        b = remaining - a;
    }
    if (b > maxVal) {
        b = maxVal;
        a = remaining - b;
    }

    a = Math.max(minVal, Math.min(maxVal, a));
    b = remaining - a;

    if (b < minVal) {
        b = minVal;
        a = remaining - b;
    }
    if (b > maxVal) {
        b = maxVal;
        a = remaining - b;
    }

    return [a, b];
};

window.rebalanceWeightGroup = function(groupName, changedId) {
    const ids = window.weightGroupMap[groupName] || [];
    if (!ids.length || !ids.every(id => document.getElementById(id))) {
        return;
    }

    const minVal = 10;
    const maxVal = 60;
    const total = 100;
    const changedEl = document.getElementById(changedId);
    if (!changedEl) {
        return;
    }

    let changedVal = parseInt(changedEl.value, 10) || minVal;
    changedVal = Math.max(minVal, Math.min(maxVal, changedVal));
    changedEl.value = changedVal;

    const otherIds = ids.filter(id => id !== changedId);
    const firstEl = document.getElementById(otherIds[0]);
    const secondEl = document.getElementById(otherIds[1]);
    if (!firstEl || !secondEl) {
        window.updateWeightGroupDisplay(groupName);
        window.pushWeightInputs(ids);
        return;
    }

    const remaining = total - changedVal;
    const pairVals = window.rebalanceTwoWeights(
        parseInt(firstEl.value, 10) || minVal,
        parseInt(secondEl.value, 10) || minVal,
        remaining,
        minVal,
        maxVal
    );

    firstEl.value = pairVals[0];
    secondEl.value = pairVals[1];

    window.updateWeightGroupDisplay(groupName);
    window.pushWeightInputs(ids);
};

window.refreshAllWeightGroups = function() {
    Object.keys(window.weightGroupMap).forEach(groupName => {
        window.updateWeightGroupDisplay(groupName);
        const ids = window.weightGroupMap[groupName];
        if (ids.every(id => document.getElementById(id))) {
            window.pushWeightInputs(ids);
        }
    });
};

document.addEventListener('DOMContentLoaded', function() {
    const sidebar = document.getElementById('sidebar');
    const overlay = document.getElementById('sidebarOverlay');
    const advBackdrop = document.getElementById('advBackdrop');
    const advBtn = document.getElementById('advBtn');
    const advClose = document.getElementById('advClose');
    const advCancel = document.getElementById('advCancel');
    const advApply = document.getElementById('advApply');

    function openSidebar() {
        sidebar.classList.add('open');
        if (window.innerWidth <= 768) {
            overlay.classList.add('sidebar-visible');
        } else {
            overlay.classList.remove('sidebar-visible');
        }
    }

    function closeSidebar() {
        sidebar.classList.remove('open');
        overlay.classList.remove('sidebar-visible');
    }

    document.querySelectorAll('.js-hamburger').forEach(btn => {
        btn.addEventListener('click', function() {
            if (sidebar.classList.contains('open')) {
                closeSidebar();
            } else {
                openSidebar();
            }
        });
    });

    overlay.addEventListener('click', closeSidebar);

    function openAdv() {
        advBackdrop.classList.add('open');
        document.body.style.overflow = 'hidden';
        setTimeout(function() {
            if (window.refreshAllWeightGroups) {
                window.refreshAllWeightGroups();
            }
        }, 0);
    }

    function closeAdv() {
        advBackdrop.classList.remove('open');
        document.body.style.overflow = '';
    }

    if (advBtn) advBtn.addEventListener('click', openAdv);
    if (advClose) advClose.addEventListener('click', closeAdv);
    if (advCancel) advCancel.addEventListener('click', closeAdv);
    if (advApply) advApply.addEventListener('click', closeAdv);
    if (advBackdrop) {
        advBackdrop.addEventListener('click', function(e) {
            if (e.target === advBackdrop) closeAdv();
        });
    }
    setTimeout(function() {
        if (window.refreshAllWeightGroups) {
            window.refreshAllWeightGroups();
        }
    }, 0);
    if (window.innerWidth > 768) {
        sidebar.classList.add('open');
        overlay.classList.remove('sidebar-visible');
    }
});
)--"

page_head <- function(title_text) {
  div(
    class = "page-head",
    tags$button(
      type = "button",
      class = "hamburger-btn js-hamburger",
      span(),
      span(),
      span()
    ),
    h1(class = "page-title", title_text)
  )
}

nav_tab <- function(page_id, label, active = FALSE, icon_svg) {
  tags$a(
    href = "#",
    class = paste("nav-tab", if (active) "active" else ""),
    `data-page` = page_id,
    onclick = sprintf("switchPage('%s'); return false;", page_id),
    HTML(icon_svg),
    label
  )
}

sidebar_ui <- function(default_month = 3) {
  tags$aside(
    class = "sidebar",
    id = "sidebar",
    div(
      class = "brand",
      div(
        class = "brand-icon",
        HTML("<svg width='16' height='16' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><path d='M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5' /></svg>")
      ),
      span(class = "brand-name", "iMarine")
    ),
    div(
      class = "nav-section",
      div(class = "nav-section-label", "功能導覽"),
      uiOutput("sidebar_nav")
    ),
    div(
      class = "sidebar-filter",
      div(
        class = "sf-header",
        div(
          class = "sf-header-title",
          HTML("<svg width='12' height='12' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><line x1='4' y1='6' x2='20' y2='6' /><line x1='8' y1='12' x2='16' y2='12' /><line x1='11' y1='18' x2='13' y2='18' /></svg>"),
          "篩選條件"
        ),
        tags$button(
          type = "button",
          class = "btn-adv",
          id = "advBtn",
          HTML("<svg width='10' height='10' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2.5'><circle cx='12' cy='12' r='3'/><path d='M19.07 4.93a10 10 0 0 1 0 14.14M4.93 4.93a10 10 0 0 0 0 14.14'/></svg>"),
          "進階設定"
        )
      ),
      div(
        class = "sf-group",
        span(class = "sf-label", "年份"),
        tags$select(
          id = "year-selector",
          class = "sf-select",
          onchange = "Shiny.setInputValue('year_selector', this.value, {priority: 'event'})",
          tags$option(value = "2023", disabled = NA, "2023 年"),
          tags$option(value = "2024", disabled = NA, "2024 年"),
          tags$option(value = "2025", selected = NA, "2025 年")
        )
      ),
      div(
        class = "sf-group",
        span(class = "sf-label", "月份"),
        tags$select(
          id = "month-selector",
          class = "sf-select",
          onchange = "Shiny.setInputValue('month_selector', this.value, {priority: 'event'})",
          lapply(1:12, function(m) {
            tags$option(value = as.character(m), if (m == default_month) selected = NA, paste0(m, " 月"))
          })
        )
      ),
      uiOutput("sidebar_context_panel")
    )
  )
}

advanced_modal_ui <- function() {
  div(
    class = "adv-backdrop",
    id = "advBackdrop",
    div(
      class = "adv-modal",
      div(
        class = "adv-modal-head",
        div(
          div(class = "adv-modal-title", "進階設定"),
          uiOutput("advanced_modal_sub")
        ),
        tags$button(
          type = "button",
          class = "adv-close",
          id = "advClose",
          HTML("<svg width='14' height='14' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2.5'><path d='M18 6 6 18M6 6l12 12' /></svg>")
        )
      ),
      div(class = "adv-modal-body", uiOutput("advanced_modal_body")),
      div(
        class = "adv-modal-foot",
        tags$button(type = "button", class = "adv-btn-cancel", id = "advCancel", "取消"),
        tags$button(type = "button", class = "adv-btn-apply", id = "advApply", "套用")
      )
    )
  )
}

weight_chip_set <- function(named_weights, labels) {
  div(
    class = "weight-mini-list",
    lapply(names(named_weights), function(key) {
      span(
        class = "weight-mini-chip",
        paste0(labels[[key]], " ", fmt_weight(named_weights[[key]]))
      )
    })
  )
}

custom_slider_card <- function(id, title, sub, value, group_name) {
  div(
    class = "adv-slider-card",
    div(
      class = "adv-slider-top",
      span(class = "adv-slider-name", HTML(sprintf("%s <span style='color:var(--muted);font-weight:500;'>(%s)</span>", title, sub))),
      span(class = "adv-pct-badge", id = paste0("pct-", id), paste0("實質占比: ", value, "%"))
    ),
    div(
      class = "adv-slider-row",
      span(class = "adv-slider-label", "權重"),
      tags$input(
        id = paste0("sl-", id),
        class = "adv-slider",
        type = "range",
        min = 10,
        max = 60,
        step = 1,
        value = value,
        oninput = sprintf("rebalanceWeightGroup('%s', 'sl-%s')", group_name, id)
      ),
      span(class = "adv-slider-val", id = paste0("val-", id), value)
    )
  )
}

overview_page_ui <- function() {
  div(
    class = "page",
    page_head("空櫃總覽"),
    fluidRow(
      column(
        width = 6,
        div(class = "card feat-card home-block", div(class = "feat-inner", uiOutput("overview_featured")))
      ),
      column(
        width = 6,
        div(
          class = "card home-block",
          div(
            class = "c-inner",
            div(class = "c-head", span(class = "c-title", "各港口壓力指標"), uiOutput("overview_month_chip")),
            div(class = "chart-wrap", style = "height:280px;", imageOutput("overview_pressure_chart", height = "280px"))
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 8,
        div(
          class = "card home-block",
          div(
            class = "c-inner",
            div(
              class = "c-head",
              span(class = "c-title", "各港口狀態"),
              tags$a(href = "#", class = "feat-action", onclick = "switchPage('analysis'); return false;", "前往港口分析")
            ),
            uiOutput("overview_port_cards")
          )
        )
      ),
      column(
        width = 4,
        div(
          class = "card home-block",
          div(
            class = "c-inner",
            div(
              class = "c-head",
              span(class = "c-title", "調度推薦"),
              tags$a(href = "#", class = "feat-action", onclick = "switchPage('matching'); return false;", "前往跨港媒合")
            ),
            div(class = "pair-list", uiOutput("overview_pair_rows"))
          )
        )
      )
    ),
    div(
      class = "ds-section",
      div(
        class = "dt-card",
        div(
          class = "dt-card-head",
          span(class = "dt-card-title", "原始數據"),
          uiOutput("overview_table_meta")
        ),
        div(
          class = "dt-controls",
          div(
            class = "dt-show-wrap",
            "Show",
            tags$select(
              id = "dt-show",
              class = "dt-show-select",
              onchange = "Shiny.setInputValue('dt_show', this.value, {priority:'event'})",
              tags$option(value = "5", "5"),
              tags$option(value = "10", selected = NA, "10"),
              tags$option(value = "20", "20")
            ),
            "entries"
          ),
          div(
            class = "dt-search-wrap",
            "Search:",
            tags$input(
              id = "dt-search",
              class = "dt-search-input",
              type = "text",
              oninput = "Shiny.setInputValue('dt_search', this.value, {priority:'event'})"
            )
          )
        ),
        div(
          class = "dt-table-wrap",
          tags$table(
            class = "dt-table",
            id = "dt-main",
            tags$thead(
              tags$tr(
                tags$th(div(class = "th-inner", HTML("&nbsp;"))),
                lapply(
                  list(
                    list(col = "ym", label = "ym"),
                    list(col = "port", label = "port"),
                    list(col = "empty_in", label = "empty_in"),
                    list(col = "empty_out", label = "empty_out"),
                    list(col = "empty_net", label = "empty_net"),
                    list(col = "throughput", label = "throughput"),
                    list(col = "pressure", label = "pressure"),
                    list(col = "buffer", label = "buffer")
                  ),
                  function(x) {
                    tags$th(
                      onclick = sprintf("sortRawTable('%s')", x$col),
                      div(
                        class = "th-inner",
                        x$label,
                        uiOutput(paste0("sort_icon_", x$col))
                      )
                    )
                  }
                )
              )
            ),
            tags$tbody(uiOutput("overview_table_rows"))
          )
        ),
        div(
          class = "dt-footer",
          span(class = "dt-info", uiOutput("overview_table_info")),
          div(class = "dt-pagination", uiOutput("overview_table_pager"))
        ),
        div(
          class = "dt-legend",
          span(class = "dt-legend-item", HTML("<code>empty_in</code>臺灣進口貨空櫃")),
          span(class = "dt-legend-item", HTML("<code>empty_out</code>臺灣出口貨實櫃")),
          span(class = "dt-legend-item", HTML("<code>empty_net</code>臺灣進口貨空櫃 − 臺灣出口貨實櫃")),
          span(class = "dt-legend-item", HTML("<code>throughput</code>國際商港貨櫃裝卸量"))
        )
      )
    )
  )
}

analysis_page_ui <- function() {
  div(
    class = "page",
    page_head("港口分析"),
    fluidRow(
      column(
        width = 6,
        div(
          class = "card home-block",
          div(
            class = "c-inner",
            div(class = "c-head", span(class = "c-title", "空櫃進出趨勢"), uiOutput("analysis_chip1")),
            div(class = "chart-wrap", style = "height:320px;", imageOutput("analysis_chart1", height = "320px"))
          )
        )
      ),
      column(
        width = 6,
        div(
          class = "card home-block",
          div(
            class = "c-inner",
            div(class = "c-head", span(class = "c-title", "淨流量與壓力趨勢"), uiOutput("analysis_chip2")),
            div(class = "chart-wrap", style = "height:320px;", imageOutput("analysis_chart2", height = "320px"))
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(
          class = "card home-block",
          div(
            class = "c-inner",
            div(
              class = "c-head",
              style = "align-items:flex-start;",
              div(
                style = "display:flex; flex-direction:column; gap:4px;",
                span(class = "c-title", "港口分析摘要"),
                span(style = "font-size:0.7rem; color:var(--muted);", "各港口動態數據檢視")
              ),
              div(class = "port-selectors", uiOutput("analysis_port_buttons"))
            ),
            uiOutput("analysis_summary_boxes")
          )
        )
      )
    )
  )
}

matching_page_ui <- function() {
  div(
    class = "page",
    page_head("跨港媒合"),
    fluidRow(
      column(
        width = 12,
        div(
          class = "card home-block",
          div(
            class = "c-inner",
            div(
              class = "c-head",
              div(
                div(class = "c-title", "調度配對設定"),
                div(class = "c-subtitle", "設定來源港口與調度參數，選擇查詢方案或執行預測推演")
              ),
              div(
                style = "display:flex;align-items:center;gap:12px;",
                span(style = "display:inline-flex;align-items:center;gap:4px;font-size:0.67rem;color:var(--muted);white-space:nowrap;opacity:0.85;", uiOutput("matching_update_stamp")),
                span(class = "c-chip", "模擬配置")
              )
            ),
            uiOutput("matching_settings_bar"),
            uiOutput("matching_logic_note"),
            uiOutput("matching_results_state")
          )
        )
      )
    )
  )
}

explore_page_ui <- function() {
  div(
    class = "page",
    page_head("探索模式"),
    fluidRow(
      column(
        width = 4,
        div(
          class = "card home-block",
          div(
            class = "c-inner",
            div(class = "c-head", span(class = "c-title", "探索設定")),
            div(
              class = "sf-group",
              span(class = "sf-label", "探索類型"),
              selectInput(
                inputId = "explore_mode_select",
                label = NULL,
                choices = c("策略模板" = "strategy", "使用者自訂" = "custom"),
                selected = "strategy",
                width = "100%"
              )
            ),
            uiOutput("explore_control_panel")
          )
        )
      ),
      column(
        width = 8,
        div(
          class = "card home-block",
          div(
            class = "c-inner",
            div(class = "c-head", span(class = "c-title", "探索模式說明"), span(class = "c-chip", "Sensitivity Analysis")),
            uiOutput("explore_summary_panel")
          )
        ),
        div(
          class = "card home-block",
          div(
            class = "c-inner",
            div(class = "c-head", span(class = "c-title", "探索結果預覽"), uiOutput("overview_month_chip")),
            uiOutput("explore_port_cards")
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(
          class = "card home-block",
          div(
            class = "c-inner",
            div(class = "c-head", span(class = "c-title", "探索模式候選配對"), span(class = "c-chip", "即時重算")),
            uiOutput("explore_candidates_table")
          )
        )
      )
    )
  )
}

ui <- fluidPage(
  tags$head(
    tags$title("iMarine 空櫃失衡決策平台"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Noto+Sans+TC:wght@400;500;700&display=swap"
    ),
    tags$style(HTML(app_css)),
    tags$script(HTML(app_js))
  ),
  div(
    class = "app",
    div(class = "sidebar-overlay", id = "sidebarOverlay"),
    sidebar_ui(default_month = 3),
    div(
      class = "main-area",
      tabsetPanel(
        id = "main_nav",
        type = "hidden",
        selected = "overview",
        tabPanel(title = NULL, value = "overview", overview_page_ui()),
        tabPanel(title = NULL, value = "analysis", analysis_page_ui()),
        tabPanel(title = NULL, value = "matching", matching_page_ui()),
        tabPanel(title = NULL, value = "explore", explore_page_ui())
      )
    )
  ),
  advanced_modal_ui()
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    main_nav = "overview",
    analysis_ports = c("高雄港"),
    dt_page = 1L,
    dt_sort_col = NULL,
    dt_sort_dir = "asc",
    match_has_run = FALSE,
    match_selected_target = NULL
  )

  observeEvent(input$nav_page_click, {
    rv$main_nav <- sub("::.*$", "", input$nav_page_click)
    updateTabsetPanel(session, "main_nav", selected = rv$main_nav)
  })

  observeEvent(input$main_nav, {
    if (!is.null(input$main_nav) && nzchar(input$main_nav)) {
      rv$main_nav <- input$main_nav
    }
  }, ignoreInit = TRUE)

  selected_year <- reactive(as.integer(input$year_selector %||% 2025))
  selected_month <- reactive(as.integer(input$month_selector %||% 3))
  selected_ym <- reactive(month_to_ym(selected_month(), selected_year()))

  active_mode <- reactive(if ((input$main_nav %||% rv$main_nav %||% "overview") == "explore") "explore" else "formal")
  active_explore_tab <- reactive(input$explore_mode_select %||% "strategy")
  active_strategy_key <- reactive(input$strategy_template_select %||% "flow_focus")
  active_strategy <- reactive(strategy_templates[[active_strategy_key()]])

  read_weight_pct <- function(input_id, default_pct) {
    val <- suppressWarnings(as.numeric(input[[input_id]]))
    if (is.null(val) || length(val) == 0 || all(is.na(val))) {
      default_pct / 100
    } else {
      val[[1]] / 100
    }
  }

  custom_pressure_weights <- reactive({
    sanitize_weight_vector(c(
      flow = read_weight_pct("sl-flow", 22),
      net = read_weight_pct("sl-net", 60),
      roll = read_weight_pct("sl-roll", 18)
    ), official_pressure_weights)
  })

  custom_buffer_weights <- reactive({
    sanitize_weight_vector(c(
      space = read_weight_pct("sl-space", 45),
      netout = read_weight_pct("sl-netout", 40),
      export = read_weight_pct("sl-export", 15)
    ), official_buffer_weights)
  })

  custom_matching_weights <- reactive({
    sanitize_weight_vector(c(
      source = read_weight_pct("sl-source", 45),
      target = read_weight_pct("sl-target", 35),
      distance = read_weight_pct("sl-distance", 20)
    ), official_matching_weights)
  })

  active_pressure_weights <- reactive({
    if (active_mode() == "formal") {
      official_pressure_weights
    } else if (active_explore_tab() == "strategy") {
      active_strategy()$pressure_weights
    } else {
      custom_pressure_weights()
    }
  })

  active_buffer_weights <- reactive({
    if (active_mode() == "formal") {
      official_buffer_weights
    } else if (active_explore_tab() == "strategy") {
      official_buffer_weights
    } else {
      custom_buffer_weights()
    }
  })

  active_matching_weights <- reactive({
    if (active_mode() == "formal") {
      official_matching_weights
    } else if (active_explore_tab() == "strategy") {
      official_matching_weights
    } else {
      custom_matching_weights()
    }
  })

  computed_history_formal <- reactive({
    build_metric_history(
      history_tbl = base_history,
      ports = formal_ports,
      pressure_weights = active_pressure_weights(),
      buffer_weights = active_buffer_weights()
    )
  })

  computed_status_formal <- reactive({
    build_status_table(computed_history_formal())
  })

  computed_history_display <- reactive({
    build_metric_history(
      history_tbl = base_history,
      ports = table_ports,
      pressure_weights = active_pressure_weights(),
      buffer_weights = active_buffer_weights()
    )
  })

  overview_month_status <- reactive({
    if (active_mode() == "formal") {
      status_formal_display %>%
        filter(ym == selected_ym())
    } else {
      computed_status_formal() %>%
        filter(ym == selected_ym())
    }
  })

  overview_default_source <- reactive({
    if (active_mode() == "formal") {
      reco_row <- simulation_reco %>%
        filter(ym == selected_ym()) %>%
        slice(1)

      if (nrow(reco_row) > 0) {
        reco_row$source_port[[1]]
      } else {
        default_source_for_month(overview_month_status())
      }
    } else {
      default_source_for_month(overview_month_status())
    }
  })

  overview_default_move_amount <- reactive({
    if (active_mode() == "formal") {
      reco_row <- simulation_reco %>%
        filter(ym == selected_ym()) %>%
        slice(1)

      if (nrow(reco_row) > 0) {
        reco_row$recommended_move_amount[[1]]
      } else {
        default_move_amount(overview_month_status(), overview_default_source())
      }
    } else {
      default_move_amount(overview_month_status(), overview_default_source())
    }
  })

  overview_candidates <- reactive({
    if (nrow(overview_month_status()) == 0) {
      return(tibble())
    }

    if (active_mode() == "formal") {
      build_formal_candidate_rows(selected_ym())
    } else {
      build_candidate_table(
        ym_value = selected_ym(),
        source_port = overview_default_source(),
        move_amount = overview_default_move_amount(),
        status_table = computed_status_formal(),
        history_table = computed_history_formal(),
        matching_weights = active_matching_weights(),
        pressure_weights = active_pressure_weights(),
        buffer_weights = active_buffer_weights()
      ) %>%
        slice_head(n = 3)
    }
  })

  explore_month_status <- reactive({
    computed_status_formal() %>%
      filter(ym == selected_ym())
  })

  explore_candidates <- reactive({
    snapshot <- explore_month_status()

    if (nrow(snapshot) == 0) {
      return(tibble())
    }

    build_candidate_table(
      ym_value = selected_ym(),
      source_port = default_source_for_month(snapshot),
      move_amount = default_move_amount(snapshot, default_source_for_month(snapshot)),
      status_table = computed_status_formal(),
      history_table = computed_history_formal(),
      matching_weights = active_matching_weights(),
      pressure_weights = active_pressure_weights(),
      buffer_weights = active_buffer_weights()
    ) %>%
      slice_head(n = 5)
  })

  observeEvent(input$toggle_analysis_port, {
    port_clicked <- normalize_port(sub("::.*$", "", input$toggle_analysis_port))
    current <- rv$analysis_ports

    if (port_clicked %in% current) {
      if (length(current) > 1) {
        rv$analysis_ports <- setdiff(current, port_clicked)
      }
    } else {
      rv$analysis_ports <- c(current, port_clicked)
    }
  })

  analysis_selected_ports <- reactive({
    rv$analysis_ports %||% c("高雄港")
  })

  analysis_data_selected <- reactive({
    if (active_mode() == "formal") {
      status_formal_display %>%
        filter(port %in% analysis_selected_ports())
    } else {
      computed_status_formal() %>%
        filter(port %in% analysis_selected_ports())
    }
  })

  observeEvent(input$dt_show, {
    rv$dt_page <- 1L
  })

  observeEvent(input$dt_search, {
    rv$dt_page <- 1L
  })

  observeEvent(input$dt_sort_click, {
    col <- sub("::.*$", "", input$dt_sort_click)
    if (identical(rv$dt_sort_col, col)) {
      rv$dt_sort_dir <- if (identical(rv$dt_sort_dir, "asc")) "desc" else "asc"
    } else {
      rv$dt_sort_col <- col
      rv$dt_sort_dir <- "asc"
    }
    rv$dt_page <- 1L
  })

  observeEvent(input$dt_action, {
    action <- sub("::.*$", "", input$dt_action)
    total_rows <- nrow(filtered_raw_table())
    page_size <- as.integer(input$dt_show %||% 10)
    total_pages <- max(1L, ceiling(total_rows / page_size))

    if (action == "prev") {
      rv$dt_page <- max(1L, rv$dt_page - 1L)
    } else if (action == "next") {
      rv$dt_page <- min(total_pages, rv$dt_page + 1L)
    }
  })

  raw_table_data <- reactive({
    if (active_mode() == "formal") {
      status_formal_display %>%
        transmute(
          ym = ym_label,
          port = display_port(port),
          empty_in,
          empty_out,
          empty_net,
          throughput,
          pressure = pressure_index,
          buffer = buffer_index
        )
    } else {
      computed_history_display() %>%
        filter(port %in% normalize_port(table_ports)) %>%
        transmute(
          ym = ym_label,
          port = display_port(port),
          empty_in,
          empty_out,
          empty_net,
          throughput,
          pressure = pressure_index,
          buffer = buffer_index
        )
    }
  })

  output$sidebar_nav <- renderUI({
    current_page <- input$main_nav %||% rv$main_nav %||% "overview"

    div(
      class = "nav-tabs",
      nav_tab(
        "overview",
        "空櫃總覽",
        identical(current_page, "overview"),
        "<svg width='14' height='14' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><rect x='3' y='3' width='7' height='7' /><rect x='14' y='3' width='7' height='7' /><rect x='14' y='14' width='7' height='7' /><rect x='3' y='14' width='7' height='7' /></svg>"
      ),
      nav_tab(
        "analysis",
        "港口分析",
        identical(current_page, "analysis"),
        "<svg width='14' height='14' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><circle cx='12' cy='12' r='10' /><line x1='2' y1='12' x2='22' y2='12' /><path d='M12 2a15.3 15.3 0 0 1 4 10 15.3 15.3 0 0 1-4 10 15.3 15.3 0 0 1-4-10 15.3 15.3 0 0 1 4-10z' /></svg>"
      ),
      nav_tab(
        "matching",
        "跨港媒合",
        identical(current_page, "matching"),
        "<svg width='14' height='14' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><path d='M8 3v3a2 2 0 0 1-2 2H3m18 0h-3a2 2 0 0 1-2-2V3m0 18v-3a2 2 0 0 1 2-2h3M3 16h3a2 2 0 0 1 2 2v3' /></svg>"
      ),
      nav_tab(
        "explore",
        "探索模式",
        identical(current_page, "explore"),
        "<svg width='14' height='14' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><path d='M12 3v18M3 12h18'/></svg>"
      )
    )
  })

  output$sidebar_context_panel <- renderUI({
    if (active_mode() == "formal") {
      div(
        class = "mode-note",
        tags$strong("正式模式"),
        tags$br(),
        "高壓 / 高緩衝採月內相對門檻（Q75 / Q50）分類，再搭配近三月淨增加條件；因此 80% 或 100% 只是相對指標值，不保證一定是正式高壓。"
      )
    } else {
      div(
        class = "mode-note",
        tags$strong("探索模式"),
        tags$br(),
        "探索模式只用於敏感度分析。策略模板改變 Pressure 權重重心；使用者自訂則允許在受限範圍內重配權重，不覆蓋正式研究結論。"
      )
    }
  })

  output$advanced_modal_sub <- renderUI({
    if (active_mode() == "formal") {
      div(class = "adv-modal-sub", "正式模式鎖定研究正式權重，這裡只提供公式與參數說明。")
    } else if (active_explore_tab() == "strategy") {
      div(class = "adv-modal-sub", "探索模式中的策略模板會改變壓力指標的重點，方便非技術使用者比較結果。")
    } else {
      div(class = "adv-modal-sub", "探索模式中的使用者自訂會即時重算 Pressure / Buffer / Matching，僅供敏感度分析。")
    }
  })

  output$advanced_modal_body <- renderUI({
    pressure_labels <- c(flow = "Flow", net = "Net", roll = "Trend")
    buffer_labels <- c(space = "Space", netout = "NetOut", export = "Export")
    matching_labels <- c(source = "Source", target = "Target", distance = "Distance")

    if (active_mode() == "formal") {
      return(tagList(
        div(
          class = "adv-help",
          "正式模式不允許直接覆寫研究正式權重。若要比較不同決策偏好，請切換到探索模式；探索結果只作敏感度分析，不覆蓋正式結論。"
        ),
        div(
          class = "adv-col",
          div(class = "adv-col-head", span(class = "adv-col-dot red"), span(class = "adv-col-title", "Pressure 正式權重")),
          div(class = "strategy-sub", "壓力指標以當月淨增加為主，流入占比與近三月趨勢為輔。"),
          weight_chip_set(official_pressure_weights, pressure_labels)
        ),
        div(
          class = "adv-col",
          div(class = "adv-col-head", span(class = "adv-col-dot green"), span(class = "adv-col-title", "Buffer 正式權重")),
          div(class = "strategy-sub", "緩衝能力以空間餘裕與歷史淨流出為主，再參考出口拉力。"),
          weight_chip_set(official_buffer_weights, buffer_labels)
        ),
        div(
          class = "adv-col",
          div(class = "adv-col-head", span(class = "adv-col-dot green"), span(class = "adv-col-title", "Matching 正式權重")),
          div(class = "strategy-sub", "媒合排序先看來源港壓力，再看目標港緩衝與距離阻力。"),
          weight_chip_set(official_matching_weights, matching_labels)
        )
      ))
    }

    if (active_explore_tab() == "strategy") {
      return(tagList(
        div(
          class = "adv-help",
          "策略模板不讓使用者直接拉數字，而是以研究可解釋的三種偏好預設跑結果，幫助不熟公式的人理解不同決策立場會如何改變推薦。"
        ),
        div(
          class = "adv-col",
          div(class = "adv-col-head", span(class = "adv-col-dot red"), span(class = "adv-col-title", "Strategy Mode")),
          div(
            class = "strategy-stack",
            lapply(names(strategy_templates), function(key) {
              strategy <- strategy_templates[[key]]
              div(
                class = "strategy-card",
                div(class = paste("strategy-btn", if (active_strategy_key() == key) "active" else ""), strategy$label),
                div(class = "strategy-sub", strategy$description),
                weight_chip_set(strategy$pressure_weights, pressure_labels)
              )
            })
          )
        ),
        div(
          class = "adv-col",
          div(class = "adv-col-head", span(class = "adv-col-dot green"), span(class = "adv-col-title", "固定參數")),
          div(class = "strategy-sub", "策略模板僅調整 Pressure 三構面；Buffer 與 Matching 維持正式權重，避免一次同時改動過多構面而難以解釋。"),
          weight_chip_set(official_buffer_weights, buffer_labels),
          weight_chip_set(official_matching_weights, matching_labels)
        ),
        tags$script(HTML("window.refreshAllWeightGroups && window.refreshAllWeightGroups();"))
      ))
    }

    tagList(
      div(
        class = "adv-help",
        "使用者自訂模式的滑桿已移到「探索模式」功能頁，避免在 sidebar / modal 內重複出現。這裡只保留規則說明：每組權重總和固定 100%，單一權重限制在 10% 到 60%。"
      ),
      div(
        class = "adv-col",
        div(class = "adv-col-head", span(class = "adv-col-dot red"), span(class = "adv-col-title", "Pressure 自訂規則")),
        weight_chip_set(custom_pressure_weights(), pressure_labels)
      ),
      div(
        class = "adv-col",
        div(class = "adv-col-head", span(class = "adv-col-dot green"), span(class = "adv-col-title", "Buffer 自訂規則")),
        weight_chip_set(custom_buffer_weights(), buffer_labels)
      ),
      div(
        class = "adv-col",
        div(class = "adv-col-head", span(class = "adv-col-dot green"), span(class = "adv-col-title", "Matching 自訂規則")),
        weight_chip_set(custom_matching_weights(), matching_labels)
      )
    )
  })

  filtered_raw_table <- reactive({
    data <- raw_table_data()
    query <- tolower(input$dt_search %||% "")

    if (nzchar(query)) {
      data <- data %>%
        filter(
          apply(., 1, function(row) any(grepl(query, tolower(as.character(row)), fixed = TRUE)))
        )
    }

    if (!is.null(rv$dt_sort_col)) {
      col_name <- rv$dt_sort_col
      desc_dir <- identical(rv$dt_sort_dir, "desc")

      if (col_name %in% names(data)) {
        if (is.numeric(data[[col_name]])) {
          data <- data %>% arrange(if (desc_dir) desc(.data[[col_name]]) else .data[[col_name]])
        } else {
          data <- data %>% arrange(if (desc_dir) desc(.data[[col_name]]) else .data[[col_name]])
        }
      }
    }

    data
  })

  paged_raw_table <- reactive({
    data <- filtered_raw_table()
    page_size <- as.integer(input$dt_show %||% 10)
    total_pages <- max(1L, ceiling(nrow(data) / page_size))
    page_now <- min(rv$dt_page, total_pages)
    rv$dt_page <- page_now
    start_idx <- (page_now - 1L) * page_size + 1L
    end_idx <- min(nrow(data), page_now * page_size)

    list(
      data = if (nrow(data) == 0) data[0, ] else data[start_idx:end_idx, , drop = FALSE],
      total = nrow(data),
      page = page_now,
      page_size = page_size,
      pages = total_pages,
      start = if (nrow(data) == 0) 0L else start_idx,
      end = if (nrow(data) == 0) 0L else end_idx
    )
  })

  match_source_choices <- reactive({
    if (active_mode() == "formal") {
      reco_row <- simulation_reco %>%
        filter(ym == selected_ym()) %>%
        slice(1)

      if (nrow(reco_row) > 0) {
        source_port <- reco_row$source_port[[1]]
        source_row <- overview_month_status() %>% filter(port == source_port) %>% slice(1)
        pressure_lbl <- if (nrow(source_row) > 0) round(source_row$pressure_index[[1]] * 100) else 0
        return(tibble(
          value = source_port,
          label = paste0(display_port(source_port), " (", port_codes[source_port], ") | 壓力 ", pressure_lbl)
        ))
      }
    }

    snapshot <- overview_month_status() %>%
      arrange(desc(pressure_index), desc(empty_net))

    if (nrow(snapshot) == 0) {
      return(tibble(value = character(), label = character()))
    }

    choices <- snapshot %>%
      transmute(
        value = port,
        label = paste0(
          display_port(port),
          " (",
          port_codes[port],
          ") | 壓力 ",
          round(pressure_index * 100)
        )
      )

    choices
  })

  match_current_source <- reactive({
    src_value <- input$sim_src %||% display_port(overview_default_source())
    if (!nzchar(src_value)) {
      overview_default_source()
    } else {
      normalize_port(src_value)
    }
  })

  match_current_volume <- reactive({
    as.numeric(input$sim_volume %||% round(overview_default_move_amount(), 0))
  })

  match_current_date <- reactive({
    input$sim_date %||% as.character(month_to_date(selected_month(), selected_year()))
  })

  observeEvent(selected_ym(), {
    rv$match_has_run <- FALSE
    rv$match_selected_target <- NULL
  })

  observeEvent(input$run_match, {
    rv$match_has_run <- TRUE
    current_candidates <- match_candidates()
    if (nrow(current_candidates) > 0) {
      rv$match_selected_target <- current_candidates$candidate_key[[1]]
    }
  })

  observeEvent(input$matching_reset, {
    rv$match_has_run <- FALSE
    rv$match_selected_target <- NULL
  })

  observeEvent(input$matching_target_click, {
    rv$match_selected_target <- sub("::.*$", "", input$matching_target_click)
  })

  match_candidates <- reactive({
    if (nrow(overview_month_status()) == 0) {
      return(tibble())
    }

    if (active_mode() == "formal") {
      build_formal_candidate_rows(selected_ym())
    } else {
      build_candidate_table(
        ym_value = selected_ym(),
        source_port = match_current_source(),
        move_amount = match_current_volume(),
        status_table = computed_status_formal(),
        history_table = computed_history_formal(),
        matching_weights = active_matching_weights(),
        pressure_weights = active_pressure_weights(),
        buffer_weights = active_buffer_weights()
      ) %>%
        slice_head(n = 3)
    }
  })

  match_selected_card <- reactive({
    cards <- match_candidates()
    if (nrow(cards) == 0) {
      return(tibble())
    }

    chosen <- cards %>%
      filter(candidate_key == rv$match_selected_target) %>%
      slice(1)

    if (nrow(chosen) == 0) cards %>% slice(1) else chosen
  })

  output$overview_month_chip <- renderUI({
    span(class = "c-chip", fmt_ym(selected_ym()))
  })

  output$overview_featured <- renderUI({
    snapshot <- overview_month_status()
    candidates <- overview_candidates()

    if (nrow(snapshot) == 0) {
      return(div(class = "feat-desc", "本月份目前沒有可供顯示的正式分析資料。"))
    }

    urgent <- snapshot %>%
      arrange(desc(pressure_index), desc(empty_net)) %>%
      slice(1)

    high_count <- sum(snapshot$status == "高壓力港")
    buffer_count <- sum(snapshot$status == "高緩衝港")
    pair_count <- nrow(candidates)
    prev_snapshot <- if (active_mode() == "formal") {
      status_formal_display %>% filter(ym == selected_ym() - 1)
    } else {
      computed_status_formal() %>% filter(ym == selected_ym() - 1)
    }
    pressure_delta <- mean(snapshot$pressure_index, na.rm = TRUE) -
      mean(prev_snapshot$pressure_index, na.rm = TRUE)
    pressure_delta_pct <- ifelse(is.finite(pressure_delta), round(pressure_delta * 100), 0)

    urgent_text <- paste0(
      display_port(urgent$port[[1]]),
      " 壓力指標達 ",
      fmt_idx(urgent$pressure_index[[1]], 4),
      if (urgent$status[[1]] == "高壓力港") "，已進入正式高壓區。" else "，為本月相對壓力最高港。"
    )

    action_text <- if (nrow(candidates) > 0) {
      top_row <- candidates %>% slice(1)
      paste0(
        "建議優先檢查「",
        display_port(top_row$source_port[[1]]),
        " → ",
        display_port(top_row$target_port[[1]]),
        "」方案，預設調度量 ",
        fmt_num(if ("move_amount" %in% names(top_row)) top_row$move_amount[[1]] else overview_default_move_amount(), 0),
        " TEU。"
      )
    } else {
      "本月未形成有效候選配對，暫以觀察各港壓力變化為主。"
    }

    tagList(
      div(
        style = "flex-shrink:0;",
        div(
          style = "display:flex;align-items:center;gap:8px;margin-bottom:4px;",
          div(class = "feat-title", "現況總結"),
          span(
            class = "feat-chip",
            HTML("<svg width='10' height='10' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2.5'><path d='M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z' /><line x1='12' y1='9' x2='12' y2='13' /><line x1='12' y1='17' x2='12.01' y2='17' /></svg>"),
            if (high_count > 0) "嚴重預警" else "平穩觀察"
          )
        ),
        div(
          class = "feat-desc",
          paste0(
            "目前有 ",
            high_count,
            " 個港口處於高壓狀態，平均壓力指數較上月",
            ifelse(pressure_delta_pct >= 0, "上升 ", "下降 "),
            abs(pressure_delta_pct),
            "%。"
          )
        )
      ),
      div(
        class = "feat-alert-items",
        div(
          class = "feat-alert-item",
          span(class = "feat-alert-label", "最緊急事項"),
          span(class = "feat-alert-val", urgent_text)
        ),
        div(
          class = "feat-alert-item",
          span(class = "feat-alert-label", "建議行動"),
          span(class = "feat-alert-val", action_text)
        )
      ),
      div(
        class = "feat-kpi-row",
        div(class = "feat-kpi-item", span(class = "feat-kpi-num danger", high_count), span(class = "feat-kpi-lbl", "高壓力港口")),
        div(class = "feat-kpi-sep"),
        div(class = "feat-kpi-item", span(class = "feat-kpi-num success", buffer_count), span(class = "feat-kpi-lbl", "可承接港口")),
        div(class = "feat-kpi-sep"),
        div(class = "feat-kpi-item", span(class = "feat-kpi-num blue", pair_count), span(class = "feat-kpi-lbl", "候選配對"))
      )
    )
  })

  output$explore_control_panel <- renderUI({
    slider_value <- function(id, fallback) {
      isolate({
        val <- suppressWarnings(as.numeric(input[[id]]))
        if (is.null(val) || length(val) == 0 || all(is.na(val))) fallback else val[[1]]
      })
    }

    if (active_explore_tab() == "strategy") {
      div(
        class = "sf-group",
        span(class = "sf-label", "策略模板"),
        selectInput(
          inputId = "strategy_template_select",
          label = NULL,
          choices = setNames(names(strategy_templates), vapply(strategy_templates, `[[`, character(1), "label")),
          selected = active_strategy_key(),
          width = "100%"
        ),
        div(class = "mode-note", active_strategy()$description)
      )
    } else {
      tagList(
        div(class = "mode-note", "以下 3 組權重都會自動維持總和 100%，且每一項限制在 10% 到 60%。"),
        div(class = "adv-col", div(class = "adv-col-head", span(class = "adv-col-dot red"), span(class = "adv-col-title", "Pressure 權重")), custom_slider_card("flow", "空櫃進港比重", "Flow", slider_value("sl-flow", 22), "pressure"), custom_slider_card("net", "當月淨增加", "Net", slider_value("sl-net", 60), "pressure"), custom_slider_card("roll", "近三月淨增加", "Trend", slider_value("sl-roll", 18), "pressure")),
        div(class = "adv-col", div(class = "adv-col-head", span(class = "adv-col-dot green"), span(class = "adv-col-title", "Buffer 權重")), custom_slider_card("space", "空間餘裕", "Space", slider_value("sl-space", 45), "buffer"), custom_slider_card("netout", "歷史淨流出", "Net Out", slider_value("sl-netout", 40), "buffer"), custom_slider_card("export", "出口拉力", "Export", slider_value("sl-export", 15), "buffer")),
        div(class = "adv-col", div(class = "adv-col-head", span(class = "adv-col-dot green"), span(class = "adv-col-title", "Matching 權重")), custom_slider_card("source", "來源港壓力", "Source", slider_value("sl-source", 45), "matching"), custom_slider_card("target", "目標港緩衝", "Target", slider_value("sl-target", 35), "matching"), custom_slider_card("distance", "距離阻力", "Distance", slider_value("sl-distance", 20), "matching")),
        tags$script(HTML("window.refreshAllWeightGroups && window.refreshAllWeightGroups();"))
      )
    }
  })

  output$explore_summary_panel <- renderUI({
    pressure_labels <- c(flow = "Flow", net = "Net", roll = "Trend")
    buffer_labels <- c(space = "Space", netout = "NetOut", export = "Export")
    matching_labels <- c(source = "Source", target = "Target", distance = "Distance")

    if (active_explore_tab() == "strategy") {
      tagList(
        div(class = "feat-desc", "策略模板會固定 Buffer 與 Matching 權重，只調整 Pressure 三構面，方便比較不同決策立場對高壓識別與推薦結果的影響。"),
        weight_chip_set(active_pressure_weights(), pressure_labels),
        weight_chip_set(active_buffer_weights(), buffer_labels),
        weight_chip_set(active_matching_weights(), matching_labels)
      )
    } else {
      tagList(
        div(class = "feat-desc", "使用者自訂模式允許在可解釋的範圍內重配權重。結果只作敏感度分析，不覆蓋正式模式結論。"),
        weight_chip_set(active_pressure_weights(), pressure_labels),
        weight_chip_set(active_buffer_weights(), buffer_labels),
        weight_chip_set(active_matching_weights(), matching_labels)
      )
    }
  })

  output$explore_port_cards <- renderUI({
    snapshot <- explore_month_status()

    if (nrow(snapshot) == 0) {
      return(div(class = "feat-desc", "本月份沒有可供探索模式顯示的資料。"))
    }

    ordered_ports <- snapshot %>%
      arrange(desc(pressure_index), desc(empty_net)) %>%
      pull(port)

    div(class = "port-cards", lapply(ordered_ports, function(port_name) {
      row <- snapshot %>% filter(port == port_name) %>% slice(1)
      pct_val <- round(row$pressure_index[[1]] * 100)
      lvl <- status_card_class(row$status[[1]])

      div(
        class = paste("port-card", lvl),
        div(
          class = "pc-header",
          div(
            class = "pc-name-group",
            div(class = paste("pc-dot", lvl)),
            div(
              div(class = "pc-name", display_port(port_name)),
              div(class = "pc-code", port_codes[[port_name]])
            )
          ),
          span(class = paste("pc-badge", lvl), status_badge_label(row$status[[1]]))
        ),
        div(class = "pc-stats", span("探索壓力 / 緩衝"), span(class = "pc-stat-val", paste0(fmt_idx(row$pressure_index[[1]], 3), " / ", fmt_idx(row$buffer_index[[1]], 3)))),
        div(class = "pc-bar-row", span(style = "font-size:0.72rem;color:var(--muted);white-space:nowrap;", "壓力指標"), div(class = "pc-bar-track", div(class = paste("pc-bar-fill", lvl), style = paste0("width:", pct_val, "%"))), span(class = paste("pc-pct", lvl), paste0(pct_val, "%")))
      )
    }))
  })

  output$explore_candidates_table <- renderUI({
    rows <- explore_candidates()

    if (nrow(rows) == 0) {
      return(div(class = "feat-desc", "目前探索模式下沒有形成有效候選配對。"))
    }

    tags$table(
      class = "dt-table",
      tags$thead(
        tags$tr(
          tags$th("來源港"),
          tags$th("建議承接港"),
          tags$th("建議調度量"),
          tags$th("模擬結果"),
          tags$th("說明")
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(rows)), function(i) {
          row <- rows[i, ]
          tags$tr(
            tags$td(display_port(row$source_port[[1]])),
            tags$td(display_port(row$target_port[[1]])),
            tags$td(paste0(fmt_num(row$move_amount[[1]], 0), " TEU")),
            tags$td(row$simulation_result[[1]]),
            tags$td(row$summary_text[[1]])
          )
        })
      )
    )
  })

  output$overview_pressure_chart <- renderImage({
    chart_df <- overview_month_status() %>%
      mutate(
        port_display = display_port(port),
        high_flag = pressure_index >= high_pressure_cut
      )

    validate(need(nrow(chart_df) > 0, "本月份沒有可顯示的壓力資料。"))

    plot_obj <- ggplot(chart_df, aes(x = reorder(port_display, pressure_index), y = pressure_index, fill = high_flag)) +
      geom_col(width = 0.58, show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = c(`TRUE` = "#dc2626", `FALSE` = "#2563EB")) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 11, base_family = "") +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#f3f4f6"),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(color = "#111827"),
        axis.text.x = element_text(color = "#6b7280"),
        plot.margin = margin(0, 8, 0, 0)
      )
    file <- save_ggplot_png(plot_obj, width = 960, height = 420, res = 144)
    list(src = file, contentType = "image/png", width = "100%", height = "280px")
  }, deleteFile = TRUE)

  output$overview_port_cards <- renderUI({
    snapshot <- overview_month_status()
    card_order <- c("基隆港", "臺北港", "高雄港", "臺中港")

    cards <- lapply(card_order, function(port_name) {
      row <- snapshot %>% filter(port == port_name) %>% slice(1)
      pct_val <- round(row$pressure_index[[1]] * 100)
      lvl <- status_card_class(row$status[[1]])
      lbl <- status_badge_label(row$status[[1]])

      div(
        class = paste("port-card", lvl),
        div(
          class = "pc-header",
          div(
            class = "pc-name-group",
            div(class = paste("pc-dot", lvl)),
            div(
              div(class = "pc-name", display_port(port_name)),
              div(class = "pc-code", port_codes[[port_name]])
            )
          ),
          span(class = paste("pc-badge", lvl), lbl)
        ),
        div(
          class = "pc-stats",
          span("正式狀態 / 壓力"),
          span(class = "pc-stat-val", paste0(display_port(row$status[[1]]), " / ", fmt_idx(row$pressure_index[[1]], 3)))
        ),
        div(
          class = "pc-bar-row",
          span(style = "font-size:0.72rem;color:var(--muted);white-space:nowrap;", "壓力指標"),
          div(class = "pc-bar-track", div(class = paste("pc-bar-fill", lvl), style = paste0("width:", pct_val, "%"))),
          span(class = paste("pc-pct", lvl), paste0(pct_val, "%"))
        ),
        div(
          style = "font-size:0.68rem;color:#64748b;line-height:1.4;margin-top:6px;",
          if (identical(row$status[[1]], "資料暖機中")) {
            "目前歷史樣本未滿 3 個月，正式 status 暫不判高壓 / 高緩衝。"
          } else {
            "正式分類採月內相對門檻（Q75 / Q50）與近三月淨增加條件，不是單看是否超過 75%。"
          }
        )
      )
    })

    summary_card <- {
      sum_pressure <- round(mean(snapshot$pressure_index, na.rm = TRUE) * 100)
      div(
        class = "port-card summary",
        div(
          class = "pc-header",
          div(
            class = "pc-name-group",
            div(class = "pc-dot", style = "background:var(--blue);"),
            div(
              div(class = "pc-name", "全港總覽"),
              div(class = "pc-code", "ALL · 4 PORTS")
            )
          ),
          span(class = "pc-badge", style = "color:var(--blue);border-color:var(--blue-mid);", "總計")
        ),
        div(
          class = "summary-totals",
          div(class = "pc-stats", span("高壓 / 緩衝"), span(class = "pc-stat-val", paste0(sum(snapshot$status == '高壓力港'), " / ", sum(snapshot$status == '高緩衝港')))),
          div(
            class = "summary-row",
            span(class = "summary-row-label", "正式分析宇宙"),
            span(class = "summary-row-val", "4 港")
          )
        ),
        div(
          class = "pc-bar-row",
          span(style = "font-size:0.72rem;color:var(--blue);white-space:nowrap;font-weight:600;", "平均壓力"),
          div(class = "pc-bar-track", style = "background:var(--blue-mid);", div(class = "pc-bar-fill", style = paste0("width:", sum_pressure, "%;background:var(--blue);"))),
          span(class = "pc-pct", style = "color:var(--blue);", paste0(sum_pressure, "%"))
        )
      )
    }

    div(class = "port-cards", tagList(cards, summary_card))
  })

  output$overview_pair_rows <- renderUI({
    rows <- overview_candidates()

    if (nrow(rows) == 0) {
      return(
        div(class = "pair-row", "本月沒有可顯示的候選配對。")
      )
    }

    lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, ]
      tag_class <- if (row$simulation_result[[1]] == "不建議調度") "danger" else "success"
      move_amount_val <- if ("move_amount" %in% names(row)) row$move_amount[[1]] else overview_default_move_amount()
      target_tag <- if (row$pair_tier[[1]] == "觀察候選") {
        if (!is.null(row$scenario_label[[1]]) && nzchar(row$scenario_label[[1]])) paste0("觀察方案 ", row$scenario_label[[1]]) else "觀察候選"
      } else {
        "可承接"
      }
      div(
        class = "pair-row",
        div(
          class = "pair-port",
          span(class = "pair-name", display_port(row$source_port[[1]])),
          span(class = "pair-tag danger", row$source_status[[1]])
        ),
        HTML("<svg class='pair-arrow' width='16' height='16' viewBox='0 0 24 24' fill='none' stroke='#9ca3af' stroke-width='2'><path d='M5 12h14M13 6l6 6-6 6' /></svg>"),
        div(
          class = "pair-port",
          span(class = "pair-name", display_port(row$target_port[[1]])),
          span(class = paste("pair-tag", tag_class), target_tag)
        ),
        div(
          class = "pair-teu",
          span(class = "pair-teu-num", fmt_num(move_amount_val, 0)),
          span(class = "pair-teu-unit", "TEU")
        )
      )
    })
  })

  output$overview_table_meta <- renderUI({
    tbl <- raw_table_data()
    span(
      class = "c-chip",
      paste0(nrow(tbl), " 筆資料・", length(unique(tbl$port)), " 港口・", length(unique(tbl$ym)), " 個月份")
    )
  })

  render_sort_icon <- function(col_name) {
    renderUI({
      cls <- "dt-sort-icon"
      if (identical(rv$dt_sort_col, col_name)) {
        cls <- paste(cls, rv$dt_sort_dir)
      }
      div(class = cls, span(class = "sa"), span(class = "sd"))
    })
  }

  output$sort_icon_ym <- render_sort_icon("ym")
  output$sort_icon_port <- render_sort_icon("port")
  output$sort_icon_empty_in <- render_sort_icon("empty_in")
  output$sort_icon_empty_out <- render_sort_icon("empty_out")
  output$sort_icon_empty_net <- render_sort_icon("empty_net")
  output$sort_icon_throughput <- render_sort_icon("throughput")
  output$sort_icon_pressure <- render_sort_icon("pressure")
  output$sort_icon_buffer <- render_sort_icon("buffer")

  output$overview_table_rows <- renderUI({
    page_info <- paged_raw_table()
    rows <- page_info$data

    if (nrow(rows) == 0) {
      return(tags$tr(tags$td(colspan = 9, "No entries found")))
    }

    tagList(
      lapply(seq_len(nrow(rows)), function(i) {
        row <- rows[i, ]
        tags$tr(
          tags$td(class = "dt-row-num", page_info$start + i - 1),
          tags$td(row$ym),
          tags$td(row$port),
          tags$td(fmt_num(row$empty_in, 0)),
          tags$td(fmt_num(row$empty_out, 0)),
          tags$td(fmt_num(row$empty_net, 0)),
          tags$td(fmt_num(row$throughput, 0)),
          tags$td(class = "dt-mono", fmt_idx(row$pressure, 6)),
          tags$td(class = "dt-mono", fmt_idx(row$buffer, 6))
        )
      })
    )
  })

  output$overview_table_info <- renderUI({
    page_info <- paged_raw_table()

    if (page_info$total == 0) {
      HTML("No entries found")
    } else {
      HTML(sprintf("Showing %d to %d of %d entries", page_info$start, page_info$end, page_info$total))
    }
  })

  output$overview_table_pager <- renderUI({
    page_info <- paged_raw_table()

    tagList(
      tags$button(
        type = "button",
        class = "dt-page-btn",
        disabled = if (page_info$page <= 1) NA else NULL,
        onclick = "rawTableAction('prev')",
        "Previous"
      ),
      span(class = "dt-page-num", paste0("Page ", page_info$page, " / ", page_info$pages)),
      tags$button(
        type = "button",
        class = "dt-page-btn",
        disabled = if (page_info$page >= page_info$pages) NA else NULL,
        onclick = "rawTableAction('next')",
        "Next"
      )
    )
  })

  output$analysis_chip1 <- renderUI({
    ports <- analysis_selected_ports()
    label <- if (length(ports) > 1) paste0("綜合比較 (", length(ports), " 港)") else display_port(ports[[1]])
    span(class = "c-chip", label)
  })

  output$analysis_chip2 <- renderUI({
    ports <- analysis_selected_ports()
    label <- if (length(ports) > 1) paste0("綜合比較 (", length(ports), " 港)") else display_port(ports[[1]])
    span(class = "c-chip", label)
  })

  output$analysis_port_buttons <- renderUI({
    buttons <- lapply(formal_ports, function(port_name) {
      tags$button(
        type = "button",
        class = paste("port-btn", if (port_name %in% analysis_selected_ports()) "active" else ""),
        onclick = sprintf("toggleAnalysisPort('%s')", display_port(port_name)),
        display_port(port_name)
      )
    })

    tagList(buttons)
  })

  output$analysis_chart1 <- renderImage({
    plot_df <- analysis_data_selected() %>%
      mutate(port_display = display_port(port)) %>%
      pivot_longer(
        cols = c(empty_in, empty_out),
        names_to = "metric",
        values_to = "value"
      ) %>%
      mutate(
        metric_label = ifelse(metric == "empty_in", "進港", "出港"),
        group_label = paste0(port_display, " (", metric_label, ")")
      )

    validate(need(nrow(plot_df) > 0, "沒有可顯示的港口分析資料。"))

    plot_obj <- ggplot(plot_df, aes(x = ym_label, y = value, group = group_label, color = port_display, linetype = metric_label)) +
      geom_line(linewidth = 1.0) +
      geom_point(size = 2.0) +
      scale_linetype_manual(values = c("進港" = "solid", "出港" = "dashed")) +
      labs(x = NULL, y = "TEU", color = NULL, linetype = NULL) +
      theme_minimal(base_size = 11, base_family = "") +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(color = "#6b7280"),
        axis.text.y = element_text(color = "#6b7280")
      )
    file <- save_ggplot_png(plot_obj, width = 1100, height = 480, res = 144)
    list(src = file, contentType = "image/png", width = "100%", height = "320px")
  }, deleteFile = TRUE)

  output$analysis_chart2 <- renderImage({
    plot_df <- analysis_data_selected() %>%
      mutate(
        port_display = display_port(port),
        pressure_scaled = pressure_index * 100
      ) %>%
      pivot_longer(
        cols = c(empty_net, pressure_scaled),
        names_to = "metric",
        values_to = "value"
      ) %>%
      mutate(
        metric_label = ifelse(metric == "empty_net", "淨流量", "壓力×100"),
        group_label = paste0(port_display, " (", metric_label, ")")
      )

    validate(need(nrow(plot_df) > 0, "沒有可顯示的港口分析資料。"))

    plot_obj <- ggplot(plot_df, aes(x = ym_label, y = value, group = group_label, color = port_display, linetype = metric_label)) +
      geom_line(linewidth = 1.0) +
      geom_point(size = 2.0) +
      scale_linetype_manual(values = c("淨流量" = "solid", "壓力×100" = "dashed")) +
      labs(x = NULL, y = "值", color = NULL, linetype = NULL) +
      theme_minimal(base_size = 11, base_family = "") +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(color = "#6b7280"),
        axis.text.y = element_text(color = "#6b7280")
      )
    file <- save_ggplot_png(plot_obj, width = 1100, height = 480, res = 144)
    list(src = file, contentType = "image/png", width = "100%", height = "320px")
  }, deleteFile = TRUE)

  output$analysis_summary_boxes <- renderUI({
    ports <- analysis_selected_ports()
    focus_rows <- analysis_data_selected() %>%
      filter(ym == selected_ym())

    sum_throughput <- sum(focus_rows$throughput, na.rm = TRUE)
    sum_in <- sum(focus_rows$empty_in, na.rm = TRUE)
    sum_out <- sum(focus_rows$empty_out, na.rm = TRUE)
    sum_net <- sum(focus_rows$empty_net, na.rm = TRUE)
    max_pressure <- max(focus_rows$pressure_index, na.rm = TRUE)
    avg_buffer <- mean(focus_rows$buffer_index, na.rm = TRUE)

    div(
      class = "summary-grid",
      div(class = "stat-box", div(class = "stat-label", "分析港口"), div(class = "stat-value", if (length(ports) > 1) paste0("綜合比較 (", length(ports), " 港)") else display_port(ports[[1]]))),
      div(class = "stat-box", div(class = "stat-label", "最新月份"), div(class = "stat-value", fmt_ym(selected_ym()))),
      div(class = "stat-box", div(class = "stat-label", "當月吞吐量"), div(class = "stat-value highlight", HTML(paste0(fmt_num(sum_throughput, 0), " <span style='font-size:0.7rem;color:var(--muted)'>TEU</span>")))),
      div(class = "stat-box", div(class = "stat-label", "當月淨流量"), div(class = "stat-value highlight", HTML(paste0(fmt_num(sum_net, 0), " <span style='font-size:0.7rem;color:var(--muted)'>TEU</span>")))),
      div(class = "stat-box", div(class = "stat-label", "空櫃進港"), div(class = "stat-value", HTML(paste0(fmt_num(sum_in, 0), " <span style='font-size:0.7rem;color:var(--muted)'>TEU</span>")))),
      div(class = "stat-box", div(class = "stat-label", "空櫃出港"), div(class = "stat-value", HTML(paste0(fmt_num(sum_out, 0), " <span style='font-size:0.7rem;color:var(--muted)'>TEU</span>")))),
      div(class = "stat-box", div(class = "stat-label", "壓力指標"), div(class = "stat-value danger", fmt_idx(max_pressure, 4))),
      div(class = "stat-box", div(class = "stat-label", "承接緩衝"), div(class = "stat-value", fmt_idx(avg_buffer, 4)))
    )
  })

  output$matching_update_stamp <- renderUI({
    HTML(
      if (active_mode() == "formal") {
        "正式模式・定稿資料"
      } else {
        paste0("探索模式・", if (active_explore_tab() == "strategy") "策略模板" else "使用者自訂", "・即時重算")
      }
    )
  })

  output$matching_logic_note <- renderUI({
    div(
      class = "mode-note",
      if (active_mode() == "formal") {
        "正式模式的推演結果不是現場即時計算，而是直接讀取 context/simulation_final_2025.csv 與 context/simulation_recommendation_final_2025.csv 的定稿結果。"
      } else {
        "探索模式的推演結果會依目前權重即時重算，核心邏輯在 build_candidate_table()，並以重算後的 status / history 作為來源。"
      }
    )
  })

  output$matching_settings_bar <- renderUI({
    choices <- match_source_choices()
    default_source <- display_port(overview_default_source())
    default_volume <- round(overview_default_move_amount(), 0)
    current_source <- display_port(match_current_source())
    current_volume <- round(match_current_volume(), 0)
    current_date <- match_current_date()

    div(
      class = "cross-settings-bar",
      div(
        class = "csb-group csb-narrow",
        tags$label(`for` = "sim-src", "來源港口 — 高壓力"),
        tags$select(
          id = "sim-src",
          class = "csb-select-danger",
          onchange = "Shiny.setInputValue('sim_src', this.value, {priority:'event'})",
          if (nrow(choices) == 0) {
            tags$option(value = "", "本月無可用來源港")
          } else {
            lapply(seq_len(nrow(choices)), function(i) {
              choice <- choices[i, ]
              tags$option(
                value = display_port(choice$value[[1]]),
                if (display_port(choice$value[[1]]) == current_source) selected = NA,
                choice$label[[1]]
              )
            })
          }
        )
      ),
      div(
        class = "csb-group csb-md",
        tags$label(`for` = "sim-volume", "調度數量 (TEU)"),
        tags$input(
          id = "sim-volume",
          type = "number",
          min = "1",
          value = current_volume,
          oninput = "Shiny.setInputValue('sim_volume', this.value, {priority:'event'})"
        )
      ),
      div(
        class = "csb-group csb-md",
        tags$label(`for` = "sim-date", "預計執行日"),
        tags$input(
          id = "sim-date",
          type = "date",
          value = current_date,
          onchange = "Shiny.setInputValue('sim_date', this.value, {priority:'event'})"
        )
      ),
      div(
        class = "csb-actions",
        tags$button(
          type = "button",
          class = "btn-sim-search",
          onclick = "runMatchingSimulation()",
          HTML("<svg width='12' height='12' viewBox='0 0 24 24' fill='currentColor' stroke='none'><polygon points='5 3 19 12 5 21 5 3' /></svg>"),
          "執行預測推演"
        )
      )
    )
  })

  output$matching_results_state <- renderUI({
    if (!rv$match_has_run) {
      return(
        div(
          id = "cross-empty-state",
          class = "cross-empty-state",
          HTML("<svg xmlns='http://www.w3.org/2000/svg' fill='none' viewBox='0 0 24 24' stroke-width='1.5' stroke='currentColor'><path stroke-linecap='round' stroke-linejoin='round' d='M9.75 3.104v5.714a2.25 2.25 0 01-.659 1.591L5 14.5M9.75 3.104c-.251.023-.501.05-.75.082m.75-.082a24.301 24.301 0 014.5 0m0 0v5.714a2.25 2.25 0 001.591 2.25l3.75 1.5M14.25 3.104c.251.023.501.05.75.082M19.5 14.25v-2.625a.75.75 0 01.75-.75h2.25' /></svg>"),
          div(class = "ces-title", "尚未執行查詢"),
          div(class = "ces-sub", "設定來源港口、調度數量與執行日期後，點擊「執行預測推演」")
        )
      )
    }

    cards <- match_candidates()
    selected_card <- match_selected_card()

    if (nrow(cards) == 0 || nrow(selected_card) == 0) {
      return(
        div(
          class = "cross-empty-state",
          div(class = "ces-title", "目前沒有可用候選"),
          div(class = "ces-sub", "該月份沒有可組成的 target 候選。")
        )
      )
    }

    source_drop <- selected_card$source_pressure[[1]] - selected_card$source_pressure_after[[1]]
    source_relief_pct <- ifelse(selected_card$source_pressure[[1]] <= 0, 0, round(source_drop / selected_card$source_pressure[[1]] * 100))
    selected_class <- selected_card$simulation_result[[1]]

    card_ui <- lapply(seq_len(nrow(cards)), function(i) {
      row <- cards[i, ]
      active_cls <- if (row$candidate_key[[1]] == selected_card$candidate_key[[1]]) "msc-active" else ""
      bar_cls <- if (row$pressure_after_pct[[1]] >= 75) "red" else if (row$pressure_after_pct[[1]] >= 45) "amber" else "green"
      rag_cls <- if (row$pressure_before_pct[[1]] >= 75) "rag-red" else if (row$pressure_before_pct[[1]] >= 45) "rag-amber" else "rag-green"
      code_suffix <- if ("scenario_label" %in% names(row) && !is.null(row$scenario_label[[1]]) && nzchar(row$scenario_label[[1]])) {
        paste0("・", row$scenario_label[[1]])
      } else if (i == 1) {
        "・最優先"
      } else if (i == 2) {
        "・次選"
      } else {
        "・備選"
      }

      tags$div(
        class = paste("match-sugg-card", active_cls),
        onclick = sprintf("selectMatchingTarget('%s')", row$candidate_key[[1]]),
        div(
          class = "msc-header",
          div(class = "msc-rank", HTML(c("①", "②", "③")[i])),
          div(
            style = "flex:1;min-width:0;",
            div(class = "msc-name", display_port(row$target_port[[1]])),
            div(class = "msc-code", paste0(port_codes[[row$target_port[[1]]]], code_suffix))
          ),
          if (i == 1) span(class = "msc-best-badge", "系統首選")
        ),
        div(class = "msc-summary", row$summary_text[[1]]),
        div(
          class = "msc-tags",
          if (row$pair_tier[[1]] == "正式推薦") span(class = "msc-tag blue", "正式推薦"),
          if (row$target_status[[1]] == "高緩衝港") span(class = "msc-tag green", "可承接"),
          if (row$simulation_result[[1]] == "可緩解但未解壓") span(class = "msc-tag amber", "部分緩解"),
          if (row$simulation_result[[1]] == "不建議調度") span(class = "msc-tag amber", "需保守")
        ),
        div(
          class = "msc-usage-row",
          span(class = "msc-util-label", HTML(sprintf("<span class='rag-dot %s'></span>壓力", rag_cls))),
          span(
            style = paste0("color:", if (bar_cls == "red") "var(--danger)" else if (bar_cls == "amber") "var(--warning)" else "var(--success)", ";font-weight:700;"),
            paste0(row$pressure_before_pct[[1]], "%")
          )
        ),
        div(
          class = "msc-prog-track",
          div(class = paste("msc-prog-fill", bar_cls), style = paste0("width:", row$pressure_before_pct[[1]], "%")),
          div(
            class = "msc-prog-increase",
            style = paste0("left:", row$pressure_before_pct[[1]], "%;width:", row$pressure_increase_pct[[1]], "%;background-color:",
                           if (bar_cls == "red") "rgba(220,38,38,.35)" else if (bar_cls == "amber") "rgba(217,119,6,.35)" else "rgba(22,163,74,.35)", ";")
          )
        ),
        div(
          class = "msc-stats-row",
          span(
            class = "msc-stat-item",
            HTML("<svg width='11' height='11' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><circle cx='12' cy='12' r='10' /><polyline points='12 6 12 12 16 14' /></svg>"),
            HTML(paste0("<strong>", fmt_num(row$eta_days[[1]], 1), "</strong> 天"))
          ),
          span(
            class = "msc-stat-item",
            HTML("<svg width='11' height='11' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><path d='M21 10c0 7-9 13-9 13s-9-6-9-13a9 9 0 0 1 18 0z' /><circle cx='12' cy='10' r='3' /></svg>"),
            paste0(fmt_num(row$distance_km[[1]], 1), " km")
          )
        )
      )
    })

    div(
      id = "cross-results",
      div(
        class = "cross-rec-area",
        div(
          class = "match-right-header",
          div(
            div(class = "mh-title", "可承接港口建議"),
            div(class = "mh-sub", "依綜合評分排序（容量・距離・成本）")
          ),
          span(class = "c-chip", "3 個方案")
        ),
        div(
          id = "sim-result-content",
          class = "sim-kpi-row",
          div(
            class = "sim-kpi-card",
            div(class = "sk-label", paste0(display_port(selected_card$source_port[[1]]), " 壓力指數")),
            div(class = "sk-val", style = "color:var(--success);", round(selected_card$source_pressure_after[[1]] * 100)),
            div(class = "sk-diff", HTML(paste0("↓ ", round(source_drop * 100), " <span style='font-size:0.68rem'>(原 ", round(selected_card$source_pressure[[1]] * 100), ")</span>")))
          ),
          div(
            class = "sim-kpi-card",
            div(class = "sk-label", "整體疏解率"),
            div(class = "sk-val", style = "color:var(--blue);", paste0(source_relief_pct, "%")),
            div(class = "sk-diff", paste0("調移 ", fmt_num(match_current_volume(), 0), " TEU"))
          )
        ),
        div(class = "match-sugg-grid", card_ui),
        div(
          class = "cross-confirm-row",
          tags$button(
            type = "button",
            class = "btn-sim-cancel",
            onclick = "resetMatchingSimulation()",
            "取消 / 重設"
          )
        )
      )
    )
  })
}

app <- shinyApp(ui, server)

if (sys.nframe() == 0) {
  shiny::runApp(app, launch.browser = interactive())
} else {
  app
}
