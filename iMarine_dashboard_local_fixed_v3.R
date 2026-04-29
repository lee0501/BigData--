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
display_ports <- c("基隆港", "台北港", "台中港", "高雄港", "花蓮港")
table_ports <- c("基隆港", "臺北港", "臺中港", "高雄港", "花蓮港")
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
  "正常港" = "#2563eb"
)

pressure_weights <- c(flow = 0.22, net = 0.60, roll = 0.18)
buffer_weights <- c(space = 0.45, netout = 0.40, export = 0.15)
matching_weights <- c(source = 0.45, target = 0.35, distance = 0.20)

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

simulation_reco <- read_csv(file.path(context_dir, "simulation_recommendation_final_2025.csv"), show_col_types = FALSE) %>%
  mutate(
    ym = as.integer(ym),
    source_port = normalize_port(source_port),
    target_port = normalize_port(target_port),
    ym_label = fmt_ym(ym)
  ) %>%
  arrange(ym, rank_global)

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
    slice(3:7) %>%
    transmute(
      source_port = normalize_port(X1),
      target_port = normalize_port(X2),
      distance_km = as.numeric(gsub("[^0-9.]", "", X3)),
      eta_days = as.numeric(X4),
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

default_source_for_month <- function(ym_value) {
  formal_row <- matching_final %>%
    filter(ym == ym_value) %>%
    arrange(rank_global) %>%
    slice(1)

  if (nrow(formal_row) > 0) {
    return(formal_row$source_port[[1]])
  }

  status_final %>%
    filter(ym == ym_value) %>%
    arrange(desc(pressure_index), desc(empty_net)) %>%
    slice(1) %>%
    pull(port)
}

default_move_amount <- function(ym_value, source_port) {
  reco_row <- simulation_reco %>%
    filter(ym == ym_value, source_port == !!source_port) %>%
    slice(1)

  if (nrow(reco_row) > 0) {
    return(reco_row$recommended_move_amount[[1]])
  }

  base_row <- status_final %>%
    filter(ym == ym_value, port == !!source_port) %>%
    slice(1)

  move_base <- max(base_row$empty_net[[1]], 0)
  if (move_base > 0) {
    return(move_base * 0.50)
  }

  2500
}

compute_projected_port <- function(port_history, role, move_amount) {
  current_row <- tail(port_history, 1)

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

simulate_pair <- function(ym_value, source_port, target_port, move_amount) {
  month_status <- status_final %>%
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

  source_history <- base_history %>% filter(port == source_port, ym <= ym_value) %>% arrange(ym)
  target_history <- base_history %>% filter(port == target_port, ym <= ym_value) %>% arrange(ym)

  source_projection <- compute_projected_port(source_history, "source", move_amount)
  target_projection <- compute_projected_port(target_history, "target", move_amount)

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

build_candidate_table <- function(ym_value, source_port, move_amount) {
  snapshot <- status_final %>% filter(ym == ym_value)
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
        filter(source_port == !!source_port),
      by = c("target_port")
    ) %>%
    left_join(
      distance_detail %>%
        filter(source_port == !!source_port),
      by = c("target_port")
    ) %>%
    filter(!is.na(distance_factor)) %>%
    mutate(
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
    simulate_pair(ym_value, source_port, row$target_port[[1]], move_amount)
  }) %>%
    bind_rows()

  formal_month <- matching_final %>%
    filter(ym == ym_value, source_port == !!source_port) %>%
    slice(1)

  candidates %>%
    left_join(sim_rows, by = c("ym" = "ym", "source_port", "target_port")) %>%
    mutate(
      pair_tier = case_when(
        nrow(formal_month) > 0 & target_port == formal_month$target_port[[1]] ~ "正式推薦",
        target_status == "高緩衝港" ~ "正式候選",
        TRUE ~ "觀察候選"
      ),
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
      tier_rank = case_when(
        pair_tier == "正式推薦" ~ 1L,
        pair_tier == "正式候選" ~ 2L,
        TRUE ~ 3L
      ),
      eta_days = eta_days %||% distance_class,
      distance_km = distance_km %||% (distance_class * 200)
    ) %>%
    arrange(result_rank, tier_rank, desc(matching_score), target_port)
}
safe_build_candidate_table <- function(ym_value, source_port, move_amount) {
  out <- tryCatch(
    build_candidate_table(ym_value, source_port, move_amount),
    error = function(e) {
      snapshot <- status_final %>% filter(ym == ym_value)
      if (nrow(snapshot) == 0) {
        return(tibble())
      }
      source_row <- snapshot %>%
        arrange(desc(pressure_index), desc(empty_net)) %>%
        slice(1)
      source_port2 <- source_port
      if (is.na(source_port2) || !(source_port2 %in% snapshot$port)) {
        source_port2 <- source_row$port[[1]]
      }
      snapshot %>%
        filter(port != source_port2) %>%
        arrange(desc(buffer_index), pressure_index) %>%
        head(3) %>%
        transmute(
          ym = ym_value,
          source_port = source_port2,
          target_port = port,
          target_status = status,
          target_pressure = pressure_index,
          target_buffer = buffer_index,
          target_empty_net = empty_net,
          source_status = source_row$status[[1]],
          source_pressure = source_row$pressure_index[[1]],
          source_buffer = source_row$buffer_index[[1]],
          source_empty_net = source_row$empty_net[[1]],
          matching_score = buffer_index - pressure_index,
          source_status_before = source_row$status[[1]],
          target_status_before = status,
          source_status_after = source_row$status[[1]],
          target_status_after = status,
          source_pressure_before = source_row$pressure_index[[1]],
          source_pressure_after = source_row$pressure_index[[1]],
          target_pressure_before = pressure_index,
          target_pressure_after = pressure_index,
          source_buffer_after = source_row$buffer_index[[1]],
          target_buffer_after = buffer_index,
          source_empty_net_after = source_row$empty_net[[1]],
          target_empty_net_after = empty_net,
          projected_high_pressure_cut = NA_real_,
          projected_high_buffer_cut = NA_real_,
          projected_low_pressure_cut = NA_real_,
          simulation_result = "可觀察候選",
          pair_tier = ifelse(status == "高緩衝港", "正式候選", "觀察候選"),
          summary_text = "相容模式下依緩衝指標排序之候選",
          pressure_before_pct = round(pressure_index * 100),
          pressure_after_pct = round(pressure_index * 100),
          pressure_increase_pct = 0,
          source_pressure_pct = round(source_row$pressure_index[[1]] * 100),
          source_pressure_after_pct = round(source_row$pressure_index[[1]] * 100),
          relief_pct = 0,
          result_rank = 2L,
          tier_rank = ifelse(status == "高緩衝港", 2L, 3L),
          eta_days = NA_real_,
          distance_km = NA_real_
        )
    }
  )
  out
}


app_css <- paste(c(
  ":root {",
  "    --blue: #2563EB;",
  "    --blue-dark: #1d4ed8;",
  "    --blue-light: #eff6ff;",
  "    --blue-mid: #dbeafe;",
  "    --bg: #eef2f7;",
  "    --white: #ffffff;",
  "    --text: #111827;",
  "    --muted: #6b7280;",
  "    --border: #e5e7eb;",
  "    --row-hover: #f9fafb;",
  "    --danger: #dc2626;",
  "    --danger-bg: #fef2f2;",
  "    --success: #16a34a;",
  "    --success-bg: #f0fdf4;",
  "    --warning: #d97706;",
  "    --warning-bg: #fef3c7;",
  "    --info: #0284c7;",
  "    --info-bg: #e0f2fe;",
  "    --r: 13px;",
  "    --t: all 0.15s ease;",
  "}",
  "*, *::before, *::after {",
  "    margin: 0;",
  "    padding: 0;",
  "    box-sizing: border-box;",
  "}",
  "html, body {",
  "    min-height: 100%;",
  "}",
  "body {",
  "    font-family: 'Inter', 'Noto Sans TC', sans-serif;",
  "    background: var(--bg);",
  "    color: var(--text);",
  "    font-size: 14px;",
  "}",
  ".app {",
  "    display: flex;",
  "    min-height: 100vh;",
  "    align-items: flex-start;",
  "}",
  ".hamburger-btn {",
  "    display: flex;",
  "    flex-direction: column;",
  "    gap: 4px;",
  "    padding: 8px;",
  "    background: none;",
  "    border: 1px solid var(--border);",
  "    border-radius: 8px;",
  "    cursor: pointer;",
  "    flex-shrink: 0;",
  "    transition: var(--t);",
  "}",
  ".hamburger-btn:hover {",
  "    background: var(--blue-light);",
  "    border-color: var(--blue);",
  "}",
  ".hamburger-btn span {",
  "    display: block;",
  "    width: 16px;",
  "    height: 2px;",
  "    background: var(--muted);",
  "    border-radius: 1px;",
  "    transition: var(--t);",
  "}",
  ".hamburger-btn:hover span {",
  "    background: var(--blue);",
  "}",
  ".sidebar-overlay {",
  "    display: none;",
  "    position: fixed;",
  "    inset: 0;",
  "    background: rgba(0, 0, 0, 0.25);",
  "    z-index: 40;",
  "    backdrop-filter: blur(1px);",
  "}",
  ".sidebar {",
  "    position: sticky;",
  "    top: 0;",
  "    align-self: flex-start;",
  "    flex-shrink: 0;",
  "    width: 0;",
  "    min-width: 0;",
  "    height: 100vh;",
  "    overflow-x: hidden;",
  "    overflow-y: auto;",
  "    background: var(--white);",
  "    border-right: 1px solid var(--border);",
  "    display: flex;",
  "    flex-direction: column;",
  "    padding: 0;",
  "    transition: width 0.25s ease, min-width 0.25s ease, box-shadow 0.25s ease;",
  "}",
  ".sidebar.open {",
  "    width: 252px;",
  "    min-width: 252px;",
  "    box-shadow: 4px 0 20px rgba(0, 0, 0, 0.08);",
  "}",
  ".brand {",
  "    display: flex;",
  "    align-items: center;",
  "    gap: 11px;",
  "    padding: 22px 20px 18px;",
  "    border-bottom: 1px solid var(--border);",
  "    flex-shrink: 0;",
  "}",
  ".brand-icon {",
  "    width: 34px;",
  "    height: 34px;",
  "    background: var(--blue);",
  "    border-radius: 8px;",
  "    display: flex;",
  "    align-items: center;",
  "    justify-content: center;",
  "    color: white;",
  "    flex-shrink: 0;",
  "}",
  ".brand-name {",
  "    font-size: 0.95rem;",
  "    font-weight: 700;",
  "    color: var(--text);",
  "    letter-spacing: -0.01em;",
  "}",
  ".nav-section {",
  "    padding: 18px 14px 12px;",
  "}",
  ".nav-section-label {",
  "    font-size: 0.65rem;",
  "    font-weight: 700;",
  "    text-transform: uppercase;",
  "    letter-spacing: 0.08em;",
  "    color: var(--muted);",
  "    padding: 0 10px 10px;",
  "    opacity: 0.7;",
  "}",
  ".nav-tabs {",
  "    display: flex;",
  "    flex-direction: column;",
  "    gap: 4px;",
  "}",
  ".nav-tab {",
  "    display: flex;",
  "    align-items: center;",
  "    gap: 11px;",
  "    width: 100%;",
  "    padding: 11px 12px;",
  "    border-radius: 10px;",
  "    font-size: 0.86rem;",
  "    font-weight: 500;",
  "    color: var(--muted);",
  "    text-decoration: none;",
  "    cursor: pointer;",
  "    transition: var(--t);",
  "    background: none;",
  "    border: none;",
  "    font-family: inherit;",
  "    text-align: left;",
  "}",
  ".nav-tab:hover {",
  "    background: var(--blue-light);",
  "    color: var(--blue);",
  "}",
  ".nav-tab.active {",
  "    background: var(--blue);",
  "    color: white;",
  "    font-weight: 600;",
  "}",
  ".nav-tab svg {",
  "    flex-shrink: 0;",
  "    opacity: 0.8;",
  "}",
  ".nav-tab.active svg,",
  ".nav-tab:hover svg {",
  "    opacity: 1;",
  "}",
  ".sidebar-filter {",
  "    margin: 0 14px 18px;",
  "    border: 1px solid var(--border);",
  "    border-radius: var(--r);",
  "    background: var(--bg);",
  "    padding: 16px;",
  "    display: flex;",
  "    flex-direction: column;",
  "    gap: 14px;",
  "    flex-shrink: 0;",
  "}",
  ".sf-header {",
  "    display: flex;",
  "    align-items: center;",
  "    gap: 7px;",
  "    font-size: 0.86rem;",
  "    font-weight: 700;",
  "    text-transform: uppercase;",
  "    letter-spacing: 0.04em;",
  "    color: var(--muted);",
  "}",
  ".sf-header-title {",
  "    display: flex;",
  "    align-items: center;",
  "    gap: 7px;",
  "    flex: 1;",
  "}",
  ".btn-adv {",
  "    display: inline-flex;",
  "    align-items: center;",
  "    gap: 4px;",
  "    padding: 3px 8px;",
  "    border: 1px solid var(--border);",
  "    border-radius: 6px;",
  "    font-size: 0.65rem;",
  "    font-weight: 600;",
  "    color: var(--muted);",
  "    background: var(--white);",
  "    cursor: pointer;",
  "    transition: var(--t);",
  "    font-family: inherit;",
  "    white-space: nowrap;",
  "}",
  ".btn-adv:hover {",
  "    background: var(--blue-light);",
  "    border-color: var(--blue);",
  "    color: var(--blue);",
  "}",
  ".sf-group {",
  "    display: flex;",
  "    flex-direction: column;",
  "    gap: 6px;",
  "}",
  ".sf-label {",
  "    font-size: 0.82rem;",
  "    font-weight: 600;",
  "    color: var(--muted);",
  "}",
  ".sf-select {",
  "    height: 34px;",
  "    padding: 0 10px;",
  "    border: 1px solid var(--border);",
  "    border-radius: 8px;",
  "    font-size: 0.82rem;",
  "    font-family: inherit;",
  "    color: var(--text);",
  "    background: var(--white);",
  "    cursor: pointer;",
  "    width: 100%;",
  "    transition: var(--t);",
  "}",
  ".sf-select:focus {",
  "    outline: none;",
  "    border-color: var(--blue);",
  "    box-shadow: 0 0 0 2px var(--blue-light);",
  "}",
  ".sf-threshold-row {",
  "    display: flex;",
  "    align-items: center;",
  "    justify-content: space-between;",
  "    margin-bottom: 6px;",
  "}",
  ".sf-val {",
  "    font-size: 0.82rem;",
  "    font-weight: 700;",
  "    color: var(--blue);",
  "}",
  ".sf-slider {",
  "    -webkit-appearance: none;",
  "    appearance: none;",
  "    width: 100%;",
  "    height: 4px;",
  "    background: var(--border);",
  "    border-radius: 2px;",
  "    outline: none;",
  "    cursor: pointer;",
  "    margin: 0 0 4px;",
  "}",
  ".sf-slider::-webkit-slider-thumb {",
  "    -webkit-appearance: none;",
  "    width: 16px;",
  "    height: 16px;",
  "    border-radius: 50%;",
  "    background: var(--blue);",
  "    border: 2px solid white;",
  "    box-shadow: 0 0 0 1px var(--blue);",
  "    cursor: pointer;",
  "}",
  ".sf-ticks {",
  "    display: flex;",
  "    justify-content: space-between;",
  "    font-size: 0.65rem;",
  "    color: var(--muted);",
  "    margin-top: 6px;",
  "}",
  ".main-area {",
  "    flex: 1;",
  "    min-width: 0;",
  "}",
  ".page {",
  "    padding: 14px 20px 40px;",
  "    display: flex;",
  "    flex-direction: column;",
  "}",
  ".page-head {",
  "    display: flex;",
  "    align-items: center;",
  "    gap: 12px;",
  "    margin-bottom: 12px;",
  "    flex-shrink: 0;",
  "}",
  ".page-title {",
  "    font-size: 1.2rem;",
  "    font-weight: 700;",
  "    color: var(--text);",
  "    letter-spacing: -0.02em;",
  "}",
  ".bento {",
  "    display: grid;",
  "    grid-template-columns: repeat(4, 1fr);",
  "    gap: 12px;",
  "}",
  ".span1 { grid-column: span 1; }",
  ".span2 { grid-column: span 2; }",
  ".span3 { grid-column: span 3; }",
  ".span4 { grid-column: span 4; }",
  ".card {",
  "    background: var(--white);",
  "    border-radius: var(--r);",
  "    border: 1px solid var(--border);",
  "    display: flex;",
  "    flex-direction: column;",
  "}",
  ".c-inner {",
  "    padding: 14px 16px;",
  "    display: flex;",
  "    flex-direction: column;",
  "    flex: 1;",
  "}",
  ".c-head {",
  "    display: flex;",
  "    align-items: center;",
  "    justify-content: space-between;",
  "    margin-bottom: 10px;",
  "    flex-shrink: 0;",
  "}",
  ".c-title {",
  "    font-size: 0.84rem;",
  "    font-weight: 600;",
  "    color: var(--text);",
  "}",
  ".c-subtitle {",
  "    font-size: 0.72rem;",
  "    color: var(--muted);",
  "    margin-top: 2px;",
  "}",
  ".c-chip {",
  "    font-size: 0.68rem;",
  "    font-weight: 600;",
  "    padding: 3px 8px;",
  "    border-radius: 6px;",
  "    background: var(--blue-light);",
  "    color: var(--blue);",
  "    cursor: default;",
  "}",
  ".chart-wrap {",
  "    position: relative;",
  "    height: 220px;",
  "}",
  ".chart-wrap .shiny-plot-output {",
  "    height: 100% !important;",
  "}",
  ".feat-card {",
  "    background: var(--blue);",
  "    border-color: var(--blue);",
  "}",
  ".feat-inner {",
  "    padding: 14px 16px;",
  "    display: flex;",
  "    flex-direction: column;",
  "    gap: 8px;",
  "    flex: 1;",
  "}",
  ".feat-chip {",
  "    display: inline-flex;",
  "    align-items: center;",
  "    gap: 5px;",
  "    background: rgba(255, 255, 255, 0.15);",
  "    color: white;",
  "    padding: 3px 9px;",
  "    border-radius: 6px;",
  "    font-size: 0.68rem;",
  "    font-weight: 700;",
  "    text-transform: uppercase;",
  "    letter-spacing: 0.05em;",
  "    width: fit-content;",
  "}",
  ".feat-title {",
  "    font-size: 1rem;",
  "    font-weight: 700;",
  "    color: white;",
  "    line-height: 1.35;",
  "}",
  ".feat-desc {",
  "    font-size: 0.75rem;",
  "    color: rgba(255, 255, 255, 0.78);",
  "    line-height: 1.6;",
  "}",
  ".feat-alert-items {",
  "    display: flex;",
  "    flex-direction: column;",
  "    gap: 9px;",
  "    background: rgba(255, 255, 255, 0.12);",
  "    border-radius: 8px;",
  "    padding: 10px 12px;",
  "}",
  ".feat-alert-item {",
  "    display: flex;",
  "    flex-direction: column;",
  "    gap: 3px;",
  "}",
  ".feat-alert-label {",
  "    font-size: 0.68rem;",
  "    font-weight: 700;",
  "    text-transform: uppercase;",
  "    letter-spacing: 0.04em;",
  "    color: rgba(255,255,255,0.72);",
  "}",
  ".feat-alert-val {",
  "    font-size: 0.8rem;",
  "    font-weight: 600;",
  "    color: white;",
  "    line-height: 1.45;",
  "}",
  ".feat-kpi-row {",
  "    display: flex;",
  "    align-items: stretch;",
  "    height: 72px;",
  "    background: rgba(255,255,255,0.12);",
  "    border-radius: 8px;",
  "    overflow: hidden;",
  "}",
  ".feat-kpi-item {",
  "    flex: 1;",
  "    display: flex;",
  "    flex-direction: column;",
  "    align-items: center;",
  "    justify-content: center;",
  "    gap: 3px;",
  "}",
  ".feat-kpi-sep {",
  "    width: 1px;",
  "    background: rgba(255,255,255,0.22);",
  "    flex-shrink: 0;",
  "}",
  ".feat-kpi-num {",
  "    font-size: 2.4rem;",
  "    font-weight: 700;",
  "    line-height: 1;",
  "}",
  ".feat-kpi-num.danger { color: #ffd5d5; }",
  ".feat-kpi-num.success { color: #d4f9dd; }",
  ".feat-kpi-num.blue { color: #dbeafe; }",
  ".feat-kpi-lbl {",
  "    font-size: 0.72rem;",
  "    color: rgba(255,255,255,0.85);",
  "}",
  ".feat-action {",
  "    display: flex;",
  "    align-items: center;",
  "    gap: 6px;",
  "    margin-top: 0;",
  "    padding: 7px 12px;",
  "    background: rgba(255, 255, 255, 0.15);",
  "    border-radius: 7px;",
  "    color: white;",
  "    font-size: 0.75rem;",
  "    font-weight: 600;",
  "    cursor: pointer;",
  "    transition: var(--t);",
  "    width: fit-content;",
  "    border: none;",
  "    text-decoration: none;",
  "}",
  ".feat-action:hover {",
  "    background: rgba(255, 255, 255, 0.25);",
  "    color: white;",
  "    text-decoration: none;",
  "}",
  ".port-cards {",
  "    display: grid;",
  "    grid-template-columns: repeat(3, 1fr);",
  "    gap: 8px;",
  "}",
  ".port-card {",
  "    border-radius: 10px;",
  "    padding: 11px 13px;",
  "    border: 1px solid transparent;",
  "    display: flex;",
  "    flex-direction: column;",
  "    justify-content: space-between;",
  "}",
  ".port-card.high {",
  "    background: #fef2f2;",
  "    border-color: #fecaca;",
  "}",
  ".port-card.mid {",
  "    background: #fffbeb;",
  "    border-color: #fde68a;",
  "}",
  ".port-card.low {",
  "    background: #f0fdf4;",
  "    border-color: #bbf7d0;",
  "}",
  ".port-card.summary {",
  "    background: var(--blue-light);",
  "    border-color: var(--blue-mid);",
  "}",
  ".port-card.excluded {",
  "    background: #f8fafc;",
  "    border-color: #cbd5e1;",
  "}",
  ".pc-header {",
  "    display: flex;",
  "    align-items: center;",
  "    justify-content: space-between;",
  "    margin-bottom: 7px;",
  "}",
  ".pc-name-group {",
  "    display: flex;",
  "    align-items: center;",
  "    gap: 8px;",
  "}",
  ".pc-dot {",
  "    width: 10px;",
  "    height: 10px;",
  "    border-radius: 50%;",
  "    flex-shrink: 0;",
  "}",
  ".pc-dot.high { background: #dc2626; }",
  ".pc-dot.mid { background: #f97316; }",
  ".pc-dot.low { background: #16a34a; }",
  ".pc-dot.excluded { background: #94a3b8; }",
  ".pc-name {",
  "    font-size: 0.88rem;",
  "    font-weight: 700;",
  "    color: var(--text);",
  "}",
  ".pc-code {",
  "    font-size: 0.7rem;",
  "    color: var(--muted);",
  "    margin-top: 1px;",
  "}",
  ".pc-badge {",
  "    font-size: 0.66rem;",
  "    font-weight: 600;",
  "    padding: 2px 8px;",
  "    border-radius: 6px;",
  "    background: rgba(255, 255, 255, 0.75);",
  "    border: 1px solid transparent;",
  "}",
  ".pc-badge.high { color: #dc2626; border-color: #fecaca; }",
  ".pc-badge.mid { color: #f97316; border-color: #fed7aa; }",
  ".pc-badge.low { color: #16a34a; border-color: #bbf7d0; }",
  ".pc-badge.excluded { color: #64748b; border-color: #cbd5e1; }",
  ".pc-stats {",
  "    display: flex;",
  "    justify-content: space-between;",
  "    font-size: 0.72rem;",
  "    color: var(--muted);",
  "    margin-bottom: 5px;",
  "}",
  ".pc-stat-val {",
  "    font-weight: 600;",
  "    color: var(--text);",
  "}",
  ".pc-bar-row {",
  "    display: flex;",
  "    align-items: center;",
  "    gap: 8px;",
  "}",
  ".pc-bar-track {",
  "    flex: 1;",
  "    height: 5px;",
  "    background: rgba(0, 0, 0, 0.09);",
  "    border-radius: 3px;",
  "    overflow: hidden;",
  "}",
  ".pc-bar-fill {",
  "    height: 100%;",
  "    border-radius: 3px;",
  "}",
  ".pc-bar-fill.high { background: #dc2626; }",
  ".pc-bar-fill.mid { background: #f97316; }",
  ".pc-bar-fill.low { background: #16a34a; }",
  ".pc-pct {",
  "    font-size: 0.72rem;",
  "    font-weight: 700;",
  "    min-width: 34px;",
  "    text-align: right;",
  "}",
  ".pc-pct.high { color: #dc2626; }",
  ".pc-pct.mid { color: #f97316; }",
  ".pc-pct.low { color: #16a34a; }",
  ".summary-totals {",
  "    display: flex;",
  "    flex-direction: column;",
  "    gap: 5px;",
  "    flex: 1;",
  "    justify-content: center;",
  "}",
  ".summary-row {",
  "    display: flex;",
  "    justify-content: space-between;",
  "    align-items: baseline;",
  "    font-size: 0.72rem;",
  "}",
  ".summary-row-label {",
  "    color: var(--muted);",
  "}",
  ".summary-row-val {",
  "    font-weight: 700;",
  "    color: var(--blue);",
  "}",
  ".pair-list {",
  "    display: flex;",
  "    flex-direction: column;",
  "    flex: 1;",
  "    justify-content: space-around;",
  "}",
  ".pair-row {",
  "    display: flex;",
  "    align-items: center;",
  "    gap: 6px;",
  "    padding: 10px 2px;",
  "    border-bottom: 1px solid var(--border);",
  "}",
  ".pair-port {",
  "    display: flex;",
  "    flex-direction: column;",
  "    gap: 4px;",
  "    min-width: 60px;",
  "}",
  ".pair-name {",
  "    font-size: 0.88rem;",
  "    font-weight: 600;",
  "    color: var(--text);",
  "}",
  ".pair-tag {",
  "    font-size: 0.68rem;",
  "    font-weight: 600;",
  "}",
  ".pair-tag.danger { color: var(--danger); }",
  ".pair-tag.success { color: var(--success); }",
  ".pair-tag.info { color: var(--blue); }",
  ".pair-arrow {",
  "    flex-shrink: 0;",
  "}",
  ".pair-teu {",
  "    margin-left: auto;",
  "    text-align: right;",
  "    display: flex;",
  "    flex-direction: column;",
  "    align-items: flex-end;",
  "}",
  ".pair-teu-num {",
  "    font-size: 1.1rem;",
  "    font-weight: 700;",
  "    color: var(--blue);",
  "    line-height: 1;",
  "}",
  ".pair-teu-unit {",
  "    font-size: 0.7rem;",
  "    color: var(--muted);",
  "    margin-top: 2px;",
  "}",
  ".ds-section {",
  "    margin-top: 20px;",
  "}",
  ".dt-card {",
  "    background: var(--white);",
  "    border: 1px solid var(--border);",
  "    border-radius: var(--r);",
  "    overflow: hidden;",
  "}",
  ".dt-card-head {",
  "    display: flex;",
  "    align-items: center;",
  "    justify-content: space-between;",
  "    padding: 12px 16px;",
  "    border-bottom: 1px solid var(--border);",
  "}",
  ".dt-card-title {",
  "    font-size: 0.84rem;",
  "    font-weight: 700;",
  "    color: var(--text);",
  "}",
  ".dt-controls {",
  "    display: flex;",
  "    align-items: center;",
  "    justify-content: space-between;",
  "    padding: 10px 16px;",
  "    background: #f9fafb;",
  "    border-bottom: 1px solid var(--border);",
  "    gap: 12px;",
  "    flex-wrap: wrap;",
  "}",
  ".dt-show-wrap, .dt-search-wrap {",
  "    display: flex;",
  "    align-items: center;",
  "    gap: 7px;",
  "    font-size: 0.76rem;",
  "    color: var(--muted);",
  "}",
  ".dt-show-select {",
  "    height: 28px;",
  "    padding: 0 6px;",
  "    border: 1px solid var(--border);",
  "    border-radius: 6px;",
  "    font-size: 0.76rem;",
  "    font-family: inherit;",
  "    color: var(--text);",
  "    background: var(--white);",
  "    cursor: pointer;",
  "}",
  ".dt-search-input {",
  "    height: 28px;",
  "    padding: 0 8px;",
  "    border: 1px solid var(--border);",
  "    border-radius: 6px;",
  "    font-size: 0.76rem;",
  "    font-family: inherit;",
  "    color: var(--text);",
  "    background: var(--white);",
  "    width: 180px;",
  "    outline: none;",
  "}",
  ".dt-search-input:focus {",
  "    border-color: var(--blue);",
  "    box-shadow: 0 0 0 2px var(--blue-light);",
  "}",
  ".dt-table-wrap {",
  "    overflow-x: auto;",
  "}",
  ".dt-table {",
  "    width: 100%;",
  "    border-collapse: collapse;",
  "}",
  ".dt-table th {",
  "    padding: 9px 12px;",
  "    text-align: left;",
  "    font-size: 0.68rem;",
  "    font-weight: 700;",
  "    color: var(--muted);",
  "    background: #f9fafb;",
  "    border-bottom: 1px solid var(--border);",
  "    cursor: pointer;",
  "    white-space: nowrap;",
  "}",
  ".dt-table th:first-child {",
  "    cursor: default;",
  "}",
  ".dt-table th:hover:not(:first-child) {",
  "    background: #f0f4f8;",
  "    color: var(--text);",
  "}",
  ".dt-table td {",
  "    padding: 9px 12px;",
  "    font-size: 0.76rem;",
  "    border-bottom: 1px solid var(--border);",
  "    white-space: nowrap;",
  "    color: var(--text);",
  "}",
  ".dt-row-num {",
  "    color: var(--muted);",
  "    font-size: 0.7rem;",
  "}",
  ".dt-mono {",
  "    font-size: 0.72rem;",
  "    color: var(--muted);",
  "}",
  ".th-inner {",
  "    display: inline-flex;",
  "    align-items: center;",
  "    gap: 5px;",
  "}",
  ".dt-sort-icon {",
  "    display: inline-flex;",
  "    flex-direction: column;",
  "    gap: 2px;",
  "}",
  ".dt-sort-icon .sa,",
  ".dt-sort-icon .sd {",
  "    display: block;",
  "    width: 0;",
  "    height: 0;",
  "    border-left: 4px solid transparent;",
  "    border-right: 4px solid transparent;",
  "    opacity: 0.25;",
  "}",
  ".dt-sort-icon .sa { border-bottom: 5px solid currentColor; }",
  ".dt-sort-icon .sd { border-top: 5px solid currentColor; }",
  ".dt-sort-icon.asc .sa { opacity: 1; }",
  ".dt-sort-icon.desc .sd { opacity: 1; }",
  ".dt-footer {",
  "    display: flex;",
  "    align-items: center;",
  "    justify-content: space-between;",
  "    padding: 10px 16px;",
  "    border-top: 1px solid var(--border);",
  "    background: #f9fafb;",
  "    flex-wrap: wrap;",
  "    gap: 8px;",
  "}",
  ".dt-info {",
  "    font-size: 0.74rem;",
  "    color: var(--muted);",
  "}",
  ".dt-pagination {",
  "    display: flex;",
  "    gap: 4px;",
  "    align-items: center;",
  "}",
  ".dt-page-btn {",
  "    height: 28px;",
  "    padding: 0 12px;",
  "    border: 1px solid var(--border);",
  "    border-radius: 6px;",
  "    font-size: 0.74rem;",
  "    font-family: inherit;",
  "    color: var(--text);",
  "    background: var(--white);",
  "    cursor: pointer;",
  "    transition: background .15s, border-color .15s, color .15s;",
  "}",
  ".dt-page-btn:hover:not(:disabled) {",
  "    background: var(--blue-light);",
  "    border-color: var(--blue);",
  "    color: var(--blue);",
  "}",
  ".dt-page-btn:disabled {",
  "    opacity: 0.38;",
  "    cursor: default;",
  "}",
  ".dt-page-num {",
  "    font-size: 0.74rem;",
  "    color: var(--muted);",
  "    padding: 0 6px;",
  "}",
  ".dt-legend {",
  "    display: flex;",
  "    flex-wrap: wrap;",
  "    gap: 6px 20px;",
  "    padding: 10px 16px;",
  "    border-top: 1px solid var(--border);",
  "    background: #f9fafb;",
  "}",
  ".dt-legend-item {",
  "    font-size: 0.72rem;",
  "    color: var(--muted);",
  "}",
  ".dt-legend-item code {",
  "    font-family: 'SFMono-Regular', Consolas, monospace;",
  "    font-size: 0.7rem;",
  "    background: var(--border);",
  "    color: var(--text);",
  "    padding: 1px 5px;",
  "    border-radius: 4px;",
  "    margin-right: 4px;",
  "}",
  ".port-selectors {",
  "    display: flex;",
  "    gap: 6px;",
  "    flex-wrap: wrap;",
  "}",
  ".port-btn {",
  "    background: var(--white);",
  "    border: 1px solid var(--border);",
  "    color: var(--muted);",
  "    padding: 6px 14px;",
  "    border-radius: 8px;",
  "    font-size: 0.75rem;",
  "    font-weight: 600;",
  "    cursor: pointer;",
  "    transition: var(--t);",
  "    font-family: inherit;",
  "}",
  ".port-btn:hover {",
  "    border-color: var(--blue-mid);",
  "    color: var(--text);",
  "}",
  ".port-btn.active {",
  "    background: var(--blue);",
  "    border-color: var(--blue);",
  "    color: white;",
  "    box-shadow: 0 2px 4px rgba(37, 99, 235, 0.2);",
  "}",
  ".port-btn.disabled {",
  "    background: #f8fafc;",
  "    color: #94a3b8;",
  "    border-color: #e2e8f0;",
  "    cursor: not-allowed;",
  "}",
  ".summary-grid {",
  "    display: grid;",
  "    grid-template-columns: repeat(4, 1fr);",
  "    gap: 16px;",
  "    padding-top: 10px;",
  "    flex: 1;",
  "}",
  ".stat-box {",
  "    background: var(--bg);",
  "    border: 1px solid var(--border);",
  "    border-radius: 10px;",
  "    padding: 16px 14px;",
  "    display: flex;",
  "    flex-direction: column;",
  "    gap: 4px;",
  "    justify-content: center;",
  "}",
  ".stat-label {",
  "    font-size: 0.75rem;",
  "    color: var(--muted);",
  "    font-weight: 600;",
  "}",
  ".stat-value {",
  "    font-size: 1.35rem;",
  "    font-weight: 700;",
  "    color: var(--text);",
  "    line-height: 1;",
  "}",
  ".stat-value.highlight {",
  "    color: var(--blue);",
  "}",
  ".stat-value.danger {",
  "    color: var(--danger);",
  "}",
  ".cross-settings-bar {",
  "    background: var(--white);",
  "    border-radius: var(--r);",
  "    padding: 0 0 16px;",
  "    display: flex;",
  "    align-items: flex-end;",
  "    gap: 12px;",
  "    flex-wrap: wrap;",
  "}",
  ".csb-group {",
  "    display: flex;",
  "    flex-direction: column;",
  "    gap: 5px;",
  "    min-width: 0;",
  "}",
  ".csb-group.csb-narrow {",
  "    flex: 1.2;",
  "    min-width: 150px;",
  "}",
  ".csb-group.csb-md {",
  "    flex: 1;",
  "    min-width: 120px;",
  "}",
  ".csb-group label {",
  "    font-size: 0.72rem;",
  "    font-weight: 700;",
  "    color: var(--muted);",
  "    white-space: nowrap;",
  "}",
  ".csb-group select,",
  ".csb-group input[type='number'],",
  ".csb-group input[type='date'] {",
  "    height: 34px;",
  "    padding: 0 10px;",
  "    border: 1.5px solid var(--border);",
  "    border-radius: 8px;",
  "    font-size: 0.82rem;",
  "    color: var(--text);",
  "    font-family: inherit;",
  "    outline: none;",
  "    background: var(--bg);",
  "    width: 100%;",
  "}",
  ".csb-group select:focus,",
  ".csb-group input[type='number']:focus,",
  ".csb-group input[type='date']:focus {",
  "    border-color: var(--blue);",
  "    background: var(--white);",
  "    box-shadow: 0 0 0 2px var(--blue-light);",
  "}",
  ".csb-select-danger {",
  "    color: var(--danger) !important;",
  "    border-color: #fca5a5 !important;",
  "    background-color: #fff5f5 !important;",
  "}",
  ".csb-actions {",
  "    display: flex;",
  "    flex-direction: row;",
  "    gap: 8px;",
  "    align-items: flex-end;",
  "    flex-shrink: 0;",
  "}",
  ".btn-sim-search {",
  "    height: 34px;",
  "    padding: 0 14px;",
  "    background: var(--blue);",
  "    color: #fff;",
  "    border: none;",
  "    border-radius: 8px;",
  "    font-size: 0.82rem;",
  "    font-weight: 700;",
  "    font-family: inherit;",
  "    cursor: pointer;",
  "    white-space: nowrap;",
  "    display: flex;",
  "    align-items: center;",
  "    justify-content: center;",
  "    gap: 6px;",
  "}",
  ".cross-empty-state {",
  "    background: var(--white);",
  "    border: 1.5px dashed var(--border);",
  "    border-radius: var(--r);",
  "    padding: 40px 24px;",
  "    margin-top: 0;",
  "    display: flex;",
  "    flex-direction: column;",
  "    align-items: center;",
  "    gap: 10px;",
  "    text-align: center;",
  "}",
  ".cross-empty-state svg {",
  "    width: 38px;",
  "    height: 38px;",
  "    color: #cbd5e1;",
  "}",
  ".ces-title {",
  "    font-size: 0.88rem;",
  "    font-weight: 700;",
  "    color: var(--muted);",
  "}",
  ".ces-sub {",
  "    font-size: 0.76rem;",
  "    color: var(--muted);",
  "    max-width: 400px;",
  "    line-height: 1.6;",
  "}",
  ".cross-rec-area {",
  "    background: none;",
  "    border: none;",
  "    padding: 0;",
  "    margin-top: 12px;",
  "    display: flex;",
  "    flex-direction: column;",
  "    gap: 14px;",
  "}",
  ".match-right-header {",
  "    display: flex;",
  "    align-items: flex-start;",
  "    justify-content: space-between;",
  "}",
  ".mh-title {",
  "    font-size: 1rem;",
  "    font-weight: 700;",
  "    color: var(--text);",
  "    letter-spacing: -0.01em;",
  "}",
  ".mh-sub {",
  "    font-size: 0.73rem;",
  "    color: var(--muted);",
  "    margin-top: 4px;",
  "}",
  ".sim-kpi-row {",
  "    display: grid;",
  "    grid-template-columns: 1fr 1fr;",
  "    gap: 10px;",
  "}",
  ".sim-kpi-card {",
  "    background: var(--blue-light);",
  "    border: 1px solid var(--blue-mid);",
  "    border-radius: 10px;",
  "    padding: 14px 18px;",
  "    text-align: center;",
  "}",
  ".sk-label {",
  "    font-size: 0.72rem;",
  "    color: var(--muted);",
  "    font-weight: 700;",
  "    text-transform: uppercase;",
  "    letter-spacing: 0.04em;",
  "    margin-bottom: 8px;",
  "}",
  ".sk-val {",
  "    font-size: 2.4rem;",
  "    font-weight: 800;",
  "    line-height: 1;",
  "    letter-spacing: -0.02em;",
  "}",
  ".sk-diff {",
  "    font-size: 0.76rem;",
  "    font-weight: 600;",
  "    color: var(--success);",
  "    margin-top: 6px;",
  "}",
  ".match-sugg-grid {",
  "    display: grid;",
  "    grid-template-columns: repeat(3, 1fr);",
  "    gap: 14px;",
  "}",
  ".match-sugg-card {",
  "    background: var(--white);",
  "    border: 1.5px solid var(--border);",
  "    border-radius: var(--r);",
  "    padding: 16px;",
  "    cursor: pointer;",
  "    transition: box-shadow .2s, border-color .2s, transform .2s;",
  "}",
  ".match-sugg-card:hover {",
  "    box-shadow: 0 4px 12px rgba(0, 0, 0, .08);",
  "    transform: translateY(-2px);",
  "}",
  ".match-sugg-card.msc-active {",
  "    border-color: var(--blue);",
  "    border-width: 2px;",
  "    box-shadow: 0 0 0 3px var(--blue-light), 0 4px 16px rgba(37, 99, 235, .1);",
  "}",
  ".msc-header {",
  "    display: flex;",
  "    align-items: center;",
  "    gap: 10px;",
  "    margin-bottom: 10px;",
  "}",
  ".msc-rank {",
  "    width: 28px;",
  "    height: 28px;",
  "    border-radius: 50%;",
  "    background: #f1f5f9;",
  "    display: flex;",
  "    align-items: center;",
  "    justify-content: center;",
  "    font-size: 0.76rem;",
  "    font-weight: 800;",
  "    color: var(--muted);",
  "    flex-shrink: 0;",
  "}",
  ".match-sugg-card:nth-child(1) .msc-rank { background: #fef3c7; color: #b45309; }",
  ".match-sugg-card:nth-child(2) .msc-rank { background: #f1f5f9; color: #475569; }",
  ".match-sugg-card:nth-child(3) .msc-rank { background: #ede9fe; color: #7c3aed; }",
  ".msc-name {",
  "    font-weight: 800;",
  "    font-size: 1.08rem;",
  "    color: var(--text);",
  "    letter-spacing: -0.01em;",
  "    line-height: 1.2;",
  "}",
  ".msc-code {",
  "    font-size: 0.72rem;",
  "    color: var(--muted);",
  "    margin-top: 2px;",
  "}",
  ".msc-best-badge {",
  "    font-size: 0.62rem;",
  "    font-weight: 700;",
  "    padding: 2px 8px;",
  "    border-radius: 5px;",
  "    background: var(--blue);",
  "    color: white;",
  "    white-space: nowrap;",
  "    margin-left: auto;",
  "}",
  ".msc-summary {",
  "    font-size: 0.71rem;",
  "    color: var(--muted);",
  "    margin: -3px 0 10px;",
  "    line-height: 1.45;",
  "    padding-left: 2px;",
  "}",
  ".msc-tags {",
  "    display: flex;",
  "    gap: 5px;",
  "    flex-wrap: wrap;",
  "    margin-bottom: 12px;",
  "}",
  ".msc-tag {",
  "    font-size: 0.62rem;",
  "    font-weight: 700;",
  "    padding: 2px 8px;",
  "    border-radius: 20px;",
  "}",
  ".msc-tag.green { color: #15803d; background: #dcfce7; }",
  ".msc-tag.blue { color: var(--blue); background: var(--blue-light); }",
  ".msc-tag.amber { color: #b45309; background: #fef3c7; }",
  ".msc-usage-row {",
  "    display: flex;",
  "    justify-content: space-between;",
  "    align-items: baseline;",
  "    font-size: 0.74rem;",
  "    color: var(--muted);",
  "    margin-bottom: 6px;",
  "}",
  ".msc-usage-row span:last-child {",
  "    font-size: 0.96rem;",
  "    font-weight: 800;",
  "}",
  ".rag-dot {",
  "    display: inline-block;",
  "    width: 7px;",
  "    height: 7px;",
  "    border-radius: 50%;",
  "    margin-right: 4px;",
  "    flex-shrink: 0;",
  "    vertical-align: middle;",
  "}",
  ".rag-dot.rag-red { background: #dc2626; }",
  ".rag-dot.rag-amber { background: #d97706; }",
  ".rag-dot.rag-green { background: #16a34a; }",
  ".msc-prog-track {",
  "    height: 6px;",
  "    background: var(--border);",
  "    border-radius: 3px;",
  "    overflow: hidden;",
  "    position: relative;",
  "}",
  ".msc-prog-fill {",
  "    height: 100%;",
  "    border-radius: 3px;",
  "    position: absolute;",
  "    left: 0;",
  "    top: 0;",
  "}",
  ".msc-prog-fill.green { background: var(--success); }",
  ".msc-prog-fill.amber { background: var(--warning); }",
  ".msc-prog-fill.red { background: var(--danger); }",
  ".msc-prog-increase {",
  "    position: absolute;",
  "    top: 0;",
  "    height: 100%;",
  "    border-radius: 0 3px 3px 0;",
  "    background: repeating-linear-gradient(-45deg, rgba(0,0,0,.13) 0px, rgba(0,0,0,.13) 2px, rgba(255,255,255,.25) 2px, rgba(255,255,255,.25) 4px);",
  "}",
  ".msc-stats-row {",
  "    display: flex;",
  "    justify-content: space-between;",
  "    align-items: baseline;",
  "    font-size: 0.74rem;",
  "    color: var(--muted);",
  "    margin-top: 11px;",
  "    padding-top: 10px;",
  "    border-top: 1px solid var(--border);",
  "}",
  ".msc-stat-item {",
  "    display: inline-flex;",
  "    align-items: center;",
  "    gap: 4px;",
  "}",
  ".msc-stat-item strong {",
  "    color: var(--text);",
  "    font-size: 0.92rem;",
  "    font-weight: 800;",
  "}",
  ".cross-confirm-row {",
  "    display: flex;",
  "    justify-content: flex-end;",
  "    gap: 10px;",
  "}",
  ".btn-sim-cancel {",
  "    padding: 7px 14px;",
  "    background: transparent;",
  "    color: var(--muted);",
  "    border: 1.5px solid var(--border);",
  "    border-radius: 8px;",
  "    font-size: 0.8rem;",
  "    font-weight: 700;",
  "    font-family: inherit;",
  "    cursor: pointer;",
  "}",
  ".btn-sim-cancel:hover {",
  "    background: var(--bg);",
  "    border-color: #94a3b8;",
  "    color: var(--text);",
  "}",
  ".adv-backdrop {",
  "    display: none;",
  "    position: fixed;",
  "    inset: 0;",
  "    background: rgba(0, 0, 0, 0.35);",
  "    z-index: 200;",
  "    backdrop-filter: blur(2px);",
  "    align-items: center;",
  "    justify-content: center;",
  "}",
  ".adv-backdrop.open {",
  "    display: flex;",
  "}",
  ".adv-modal {",
  "    background: var(--white);",
  "    border-radius: 16px;",
  "    box-shadow: 0 20px 60px rgba(0, 0, 0, 0.18);",
  "    width: min(860px, 95vw);",
  "    max-height: 90vh;",
  "    overflow-y: auto;",
  "    display: flex;",
  "    flex-direction: column;",
  "}",
  ".adv-modal-head {",
  "    display: flex;",
  "    align-items: center;",
  "    justify-content: space-between;",
  "    padding: 18px 24px 14px;",
  "    border-bottom: 1px solid var(--border);",
  "}",
  ".adv-modal-title {",
  "    font-size: 1rem;",
  "    font-weight: 700;",
  "    color: var(--text);",
  "    letter-spacing: -0.01em;",
  "}",
  ".adv-modal-sub {",
  "    font-size: 0.74rem;",
  "    color: var(--muted);",
  "    margin-top: 2px;",
  "}",
  ".adv-close {",
  "    width: 30px;",
  "    height: 30px;",
  "    border-radius: 8px;",
  "    border: 1px solid var(--border);",
  "    background: none;",
  "    cursor: pointer;",
  "    display: flex;",
  "    align-items: center;",
  "    justify-content: center;",
  "    color: var(--muted);",
  "}",
  ".adv-close:hover {",
  "    background: var(--danger-bg);",
  "    border-color: #fecaca;",
  "    color: var(--danger);",
  "}",
  ".adv-modal-body {",
  "    display: grid;",
  "    grid-template-columns: 1fr 1fr;",
  "    gap: 16px;",
  "    padding: 20px 24px;",
  "}",
  ".adv-col {",
  "    background: #f9fafb;",
  "    border: 1px solid var(--border);",
  "    border-radius: 12px;",
  "    padding: 18px;",
  "    display: flex;",
  "    flex-direction: column;",
  "    gap: 12px;",
  "}",
  ".adv-col-head {",
  "    display: flex;",
  "    align-items: center;",
  "    gap: 9px;",
  "}",
  ".adv-col-dot {",
  "    width: 12px;",
  "    height: 12px;",
  "    border-radius: 50%;",
  "}",
  ".adv-col-dot.red { background: #ef4444; }",
  ".adv-col-dot.green { background: #22c55e; }",
  ".adv-col-title {",
  "    font-size: 0.9rem;",
  "    font-weight: 700;",
  "    color: var(--text);",
  "}",
  ".adv-slider-card {",
  "    background: var(--white);",
  "    border: 1px solid var(--border);",
  "    border-radius: 10px;",
  "    padding: 13px 15px;",
  "    display: flex;",
  "    flex-direction: column;",
  "    gap: 8px;",
  "}",
  ".adv-slider-top {",
  "    display: flex;",
  "    align-items: center;",
  "    justify-content: space-between;",
  "}",
  ".adv-slider-name {",
  "    font-size: 0.82rem;",
  "    font-weight: 600;",
  "    color: var(--text);",
  "}",
  ".adv-pct-badge {",
  "    font-size: 0.7rem;",
  "    font-weight: 700;",
  "    padding: 2px 8px;",
  "    border-radius: 5px;",
  "    background: var(--blue-light);",
  "    color: var(--blue);",
  "}",
  ".adv-slider-row {",
  "    display: flex;",
  "    align-items: center;",
  "    gap: 10px;",
  "}",
  ".adv-slider-label {",
  "    font-size: 0.7rem;",
  "    color: var(--muted);",
  "    white-space: nowrap;",
  "    min-width: 38px;",
  "}",
  ".adv-slider {",
  "    -webkit-appearance: none;",
  "    appearance: none;",
  "    flex: 1;",
  "    height: 5px;",
  "    background: var(--blue-mid);",
  "    border-radius: 3px;",
  "    outline: none;",
  "    cursor: pointer;",
  "}",
  ".adv-slider::-webkit-slider-thumb {",
  "    -webkit-appearance: none;",
  "    width: 18px;",
  "    height: 18px;",
  "    border-radius: 50%;",
  "    background: var(--blue);",
  "    border: 2px solid white;",
  "}",
  ".adv-slider-val {",
  "    font-size: 0.82rem;",
  "    font-weight: 700;",
  "    color: var(--text);",
  "    min-width: 24px;",
  "    text-align: right;",
  "}",
  ".adv-modal-foot {",
  "    display: flex;",
  "    align-items: center;",
  "    justify-content: flex-end;",
  "    gap: 8px;",
  "    padding: 14px 24px 18px;",
  "    border-top: 1px solid var(--border);",
  "}",
  ".adv-btn-cancel {",
  "    padding: 7px 16px;",
  "    border: 1.5px solid var(--border);",
  "    border-radius: 8px;",
  "    background: transparent;",
  "    color: var(--muted);",
  "    font-size: 0.8rem;",
  "    font-weight: 600;",
  "    cursor: pointer;",
  "}",
  ".adv-btn-apply {",
  "    padding: 7px 20px;",
  "    border: none;",
  "    border-radius: 8px;",
  "    background: var(--blue);",
  "    color: white;",
  "    font-size: 0.8rem;",
  "    font-weight: 600;",
  "    cursor: pointer;",
  "}",
  ".adv-btn-apply:hover { background: var(--blue-dark); }",
  ".adv-btn-cancel:hover {",
  "    background: var(--bg);",
  "    border-color: #94a3b8;",
  "    color: var(--text);",
  "}",
  ".hidden {",
  "    display: none !important;",
  "}",
  ".shiny-plot-output img {",
  "    width: 100% !important;",
  "    height: 100% !important;",
  "}",
  "@media (max-width: 1150px) {",
  "    .bento { grid-template-columns: repeat(2, 1fr); }",
  "    .span2, .span3, .span4 { grid-column: span 2; }",
  "    .summary-grid, .match-sugg-grid { grid-template-columns: repeat(2, 1fr); }",
  "}",
  "@media (max-width: 768px) {",
  "    .sidebar {",
  "        position: fixed;",
  "        top: 0;",
  "        left: 0;",
  "        height: 100vh;",
  "        z-index: 50;",
  "        align-self: unset;",
  "    }",
  "    .sidebar-overlay.show { display: block; }",
  "    .bento, .summary-grid, .match-sugg-grid, .port-cards, .adv-modal-body {",
  "        grid-template-columns: 1fr;",
  "    }",
  "    .span1, .span2, .span3, .span4 {",
  "        grid-column: span 1;",
  "    }",
  "    .page {",
  "        padding: 12px 16px 32px;",
  "    }",
  "}"
), collapse = "\n")

# Compatibility layout patch for older RStudio Viewer / browsers
app_css <- paste(app_css, paste(c(
  ".main-area { width: 100%; overflow-x: hidden; }",
  ".page-view { min-width: 0; width: 100%; }",
  ".card { min-width: 0; overflow: hidden; }",
  ".chart-wrap { min-height: 220px; }",
  ".dt-table-wrap { width: 100%; overflow-x: auto; }",
  ".dt-table { table-layout: auto; min-width: 920px; }",
  ".dt-table td, .dt-table th { vertical-align: middle; }",
  ".dt-table .shiny-html-output, .dt-table .shiny-bound-output { display: contents; }",
  ".sidebar-overlay { pointer-events: none; opacity: 0; }",
  ".sidebar-overlay.show { pointer-events: auto; opacity: 1; }",
  ".adv-backdrop { pointer-events: none; }",
  ".adv-backdrop.open { pointer-events: auto; }",
  "@media (min-width: 769px) { .sidebar-overlay.show { display: block; } }"
), collapse = "\n"), sep = "\n")


app_js <- paste(c(
  "function safeGet(id) { return document.getElementById(id); }",
  "window.switchPage = function(pageName) {",
  "  var views = document.querySelectorAll('.page-view');",
  "  for (var i = 0; i < views.length; i++) { views[i].style.display = 'none'; }",
  "  var page = document.getElementById('page-' + pageName);",
  "  if (page) { page.style.display = 'flex'; }",
  "  var tabs = document.querySelectorAll('.nav-tab');",
  "  for (var j = 0; j < tabs.length; j++) {",
  "    tabs[j].classList.remove('active');",
  "    if (tabs[j].getAttribute('data-page') === pageName) { tabs[j].classList.add('active'); }",
  "  }",
  "  if (window.Shiny) { Shiny.setInputValue('nav_page', pageName, {priority: 'event'}); }",
  "  return false;",
  "};",
  "window.toggleAnalysisPort = function(port) { if (window.Shiny) { Shiny.setInputValue('toggle_analysis_port', port + '::' + (new Date()).getTime(), {priority: 'event'}); } return false; };",
  "window.selectMatchingTarget = function(port) { if (window.Shiny) { Shiny.setInputValue('matching_target_click', port + '::' + (new Date()).getTime(), {priority: 'event'}); } return false; };",
  "window.sortRawTable = function(col) { if (window.Shiny) { Shiny.setInputValue('dt_sort_click', col + '::' + (new Date()).getTime(), {priority: 'event'}); } return false; };",
  "window.rawTableAction = function(actionName) { if (window.Shiny) { Shiny.setInputValue('dt_action', actionName + '::' + (new Date()).getTime(), {priority: 'event'}); } return false; };",
  "window.runMatchingSimulation = function() { if (window.Shiny) { Shiny.setInputValue('run_match', (new Date()).getTime(), {priority: 'event'}); } return false; };",
  "window.resetMatchingSimulation = function() { if (window.Shiny) { Shiny.setInputValue('matching_reset', (new Date()).getTime(), {priority: 'event'}); } return false; };",
  "window.updateAdvPcts = function(ids, pctIds) {",
  "  var vals = []; var total = 0;",
  "  for (var i = 0; i < ids.length; i++) { var el = safeGet(ids[i]); var v = el ? (parseInt(el.value, 10) || 0) : 0; vals.push(v); total += v; }",
  "  for (var j = 0; j < ids.length; j++) {",
  "    var pctEl = safeGet(pctIds[j]); var valEl = safeGet(ids[j].replace('sl-', 'val-'));",
  "    var pct = total > 0 ? (vals[j] / total * 100).toFixed(1) : '0.0';",
  "    if (pctEl) { pctEl.textContent = '實質占比: ' + pct + '%'; }",
  "    if (valEl) { valEl.textContent = vals[j]; }",
  "  }",
  "};",
  "window.bindDashboardEvents = function() {",
  "  var sidebar = safeGet('sidebar'); var overlay = safeGet('sidebarOverlay');",
  "  var pressureSlider = safeGet('pressure-slider'); var pressureVal = safeGet('pressure-val');",
  "  var advBackdrop = safeGet('advBackdrop'); var advBtn = safeGet('advBtn'); var advClose = safeGet('advClose'); var advCancel = safeGet('advCancel'); var advApply = safeGet('advApply');",
  "  function openSidebar() { if (sidebar) { sidebar.classList.add('open'); } if (overlay) { overlay.classList.add('show'); } }",
  "  function closeSidebar() { if (sidebar) { sidebar.classList.remove('open'); } if (overlay) { overlay.classList.remove('show'); } }",
  "  var hams = document.querySelectorAll('.js-hamburger');",
  "  for (var i = 0; i < hams.length; i++) { hams[i].onclick = function(e) { if (e) { e.preventDefault(); } if (sidebar && sidebar.classList.contains('open')) { closeSidebar(); } else { openSidebar(); } return false; }; }",
  "  if (overlay) { overlay.onclick = function() { closeSidebar(); return false; }; }",
  "  if (pressureSlider && pressureVal) { pressureSlider.oninput = function() { pressureVal.textContent = this.value + '%'; if (window.Shiny) { Shiny.setInputValue('pressure_slider', parseInt(this.value, 10), {priority:'event'}); } }; }",
  "  function openAdv() { if (advBackdrop) { advBackdrop.classList.add('open'); } document.body.style.overflow = 'hidden'; }",
  "  function closeAdv() { if (advBackdrop) { advBackdrop.classList.remove('open'); } document.body.style.overflow = ''; }",
  "  if (advBtn) { advBtn.onclick = function(e) { if (e) { e.preventDefault(); } openAdv(); return false; }; }",
  "  if (advClose) { advClose.onclick = closeAdv; } if (advCancel) { advCancel.onclick = closeAdv; } if (advApply) { advApply.onclick = closeAdv; }",
  "  if (advBackdrop) { advBackdrop.onclick = function(e) { if (e && e.target === advBackdrop) { closeAdv(); } }; }",
  "  var advPressureIds = ['sl-flow', 'sl-net', 'sl-roll']; var advPressurePct = ['pct-flow', 'pct-net', 'pct-roll'];",
  "  var advBufferIds = ['sl-space', 'sl-netout', 'sl-export']; var advBufferPct = ['pct-space', 'pct-netout', 'pct-export'];",
  "  var all = advPressureIds.concat(advBufferIds);",
  "  for (var k = 0; k < all.length; k++) { var el = safeGet(all[k]); if (el) { el.oninput = function() { window.updateAdvPcts(advPressureIds, advPressurePct); window.updateAdvPcts(advBufferIds, advBufferPct); }; } }",
  "  window.updateAdvPcts(advPressureIds, advPressurePct); window.updateAdvPcts(advBufferIds, advBufferPct);",
  "  closeSidebar(); switchPage('overview');",
  "};",
  "if (document.readyState === 'loading') { document.addEventListener('DOMContentLoaded', window.bindDashboardEvents); } else { window.bindDashboardEvents(); }"
), collapse = "\n")

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
      div(
        class = "nav-tabs",
        nav_tab(
          "overview",
          "空櫃總覽",
          TRUE,
          "<svg width='14' height='14' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><rect x='3' y='3' width='7' height='7' /><rect x='14' y='3' width='7' height='7' /><rect x='14' y='14' width='7' height='7' /><rect x='3' y='14' width='7' height='7' /></svg>"
        ),
        nav_tab(
          "analysis",
          "港口分析",
          FALSE,
          "<svg width='14' height='14' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><circle cx='12' cy='12' r='10' /><line x1='2' y1='12' x2='22' y2='12' /><path d='M12 2a15.3 15.3 0 0 1 4 10 15.3 15.3 0 0 1-4 10 15.3 15.3 0 0 1-4-10 15.3 15.3 0 0 1 4-10z' /></svg>"
        ),
        nav_tab(
          "matching",
          "跨港媒合",
          FALSE,
          "<svg width='14' height='14' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><path d='M8 3v3a2 2 0 0 1-2 2H3m18 0h-3a2 2 0 0 1-2-2V3m0 18v-3a2 2 0 0 1 2-2h3M3 16h3a2 2 0 0 1 2 2v3' /></svg>"
        )
      )
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
      div(
        class = "sf-group",
        div(
          class = "sf-threshold-row",
          span(class = "sf-label", "壓力門檻"),
          span(class = "sf-val", id = "pressure-val", "50%")
        ),
        tags$input(
          id = "pressure-slider",
          class = "sf-slider",
          type = "range",
          min = 0,
          max = 100,
          step = 5,
          value = 50
        ),
        div(
          class = "sf-ticks",
          span("0%"),
          span("50%"),
          span("100%")
        )
      )
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
          div(class = "adv-modal-sub", "調整壓力指數與緩衝能力的計算權重")
        ),
        tags$button(
          type = "button",
          class = "adv-close",
          id = "advClose",
          HTML("<svg width='14' height='14' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2.5'><path d='M18 6 6 18M6 6l12 12' /></svg>")
        )
      ),
      div(
        class = "adv-modal-body",
        div(
          class = "adv-col",
          div(
            class = "adv-col-head",
            span(class = "adv-col-dot red"),
            span(class = "adv-col-title", HTML("壓力指數設定 <span style='color:var(--muted);font-weight:500;'>(Pressure)</span>"))
          ),
          lapply(
            list(
              list(id = "flow", title = "空櫃進港比重", sub = "Flow", val = 22),
              list(id = "net", title = "當月淨增加", sub = "Net", val = 60),
              list(id = "roll", title = "近三月淨增加", sub = "Roll", val = 18)
            ),
            function(x) {
              div(
                class = "adv-slider-card",
                div(
                  class = "adv-slider-top",
                  span(class = "adv-slider-name", HTML(sprintf("%s <span style='color:var(--muted);font-weight:500;'>(%s)</span>", x$title, x$sub))),
                  span(class = "adv-pct-badge", id = paste0("pct-", x$id), "實質占比: —")
                ),
                div(
                  class = "adv-slider-row",
                  span(class = "adv-slider-label", "調整權重"),
                  tags$input(id = paste0("sl-", x$id), class = "adv-slider", type = "range", min = 0, max = 100, value = x$val),
                  span(class = "adv-slider-val", id = paste0("val-", x$id), x$val)
                )
              )
            }
          )
        ),
        div(
          class = "adv-col",
          div(
            class = "adv-col-head",
            span(class = "adv-col-dot green"),
            span(class = "adv-col-title", HTML("緩衝能力設定 <span style='color:var(--muted);font-weight:500;'>(Buffer)</span>"))
          ),
          lapply(
            list(
              list(id = "space", title = "空間餘裕", sub = "Space", val = 45),
              list(id = "netout", title = "歷史淨流出", sub = "Net Out", val = 40),
              list(id = "export", title = "出口拉力", sub = "Export", val = 15)
            ),
            function(x) {
              div(
                class = "adv-slider-card",
                div(
                  class = "adv-slider-top",
                  span(class = "adv-slider-name", HTML(sprintf("%s <span style='color:var(--muted);font-weight:500;'>(%s)</span>", x$title, x$sub))),
                  span(class = "adv-pct-badge", id = paste0("pct-", x$id), "實質占比: —")
                ),
                div(
                  class = "adv-slider-row",
                  span(class = "adv-slider-label", "調整權重"),
                  tags$input(id = paste0("sl-", x$id), class = "adv-slider", type = "range", min = 0, max = 100, value = x$val),
                  span(class = "adv-slider-val", id = paste0("val-", x$id), x$val)
                )
              )
            }
          )
        )
      ),
      div(
        class = "adv-modal-foot",
        tags$button(type = "button", class = "adv-btn-cancel", id = "advCancel", "取消"),
        tags$button(type = "button", class = "adv-btn-apply", id = "advApply", "套用")
      )
    )
  )
}

overview_page_ui <- function() {
  div(
    id = "page-overview",
    class = "page page-view",
    page_head("空櫃總覽"),
    div(
      class = "bento",
      div(
        class = "card feat-card span2",
        div(class = "feat-inner", uiOutput("overview_featured"))
      ),
      div(
        class = "card span2",
        div(
          class = "c-inner",
          div(
            class = "c-head",
            span(class = "c-title", "各港口壓力指標"),
            uiOutput("overview_month_chip")
          ),
          div(class = "chart-wrap", plotOutput("overview_pressure_chart", height = "100%"))
        )
      ),
      div(
        class = "card span3",
        div(
          class = "c-inner",
          div(
            class = "c-head",
            span(class = "c-title", "各港口狀態"),
            tags$a(
              href = "#",
              class = "feat-action",
              onclick = "switchPage('analysis'); return false;",
              HTML("<svg width='12' height='12' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2.5'><circle cx='12' cy='12' r='10' /><line x1='2' y1='12' x2='22' y2='12' /><path d='M12 2a15.3 15.3 0 0 1 4 10 15.3 15.3 0 0 1-4 10 15.3 15.3 0 0 1-4-10 15.3 15.3 0 0 1 4-10z' /></svg>"),
              "前往港口分析"
            )
          ),
          div(class = "port-cards", uiOutput("overview_port_cards"))
        )
      ),
      div(
        class = "card span1",
        div(
          class = "c-inner",
          div(
            class = "c-head",
            span(class = "c-title", "調度推薦"),
            tags$a(
              href = "#",
              class = "feat-action",
              onclick = "switchPage('matching'); return false;",
              HTML("<svg width='12' height='12' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2.5'><path d='M8 3v3a2 2 0 0 1-2 2H3m18 0h-3a2 2 0 0 1-2-2V3m0 18v-3a2 2 0 0 1 2-2h3M3 16h3a2 2 0 0 1 2 2v3' /></svg>"),
              "前往跨港媒合"
            )
          ),
          div(class = "pair-list", uiOutput("overview_pair_rows"))
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
            uiOutput("overview_table_rows", container = tags$tbody)
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
    id = "page-analysis",
    class = "page page-view",
    style = "display:none;",
    page_head("港口分析"),
    div(
      class = "bento",
      div(
        class = "card span2",
        div(
          class = "c-inner",
          div(
            class = "c-head",
            span(class = "c-title", "空櫃進出趨勢"),
            uiOutput("analysis_chip1")
          ),
          div(class = "chart-wrap", plotOutput("analysis_chart1", height = "100%"))
        )
      ),
      div(
        class = "card span2",
        div(
          class = "c-inner",
          div(
            class = "c-head",
            span(class = "c-title", "淨流量與壓力趨勢"),
            uiOutput("analysis_chip2")
          ),
          div(class = "chart-wrap", plotOutput("analysis_chart2", height = "100%"))
        )
      ),
      div(
        class = "card span4",
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
          div(class = "summary-grid", uiOutput("analysis_summary_boxes"))
        )
      )
    )
  )
}

matching_page_ui <- function() {
  div(
    id = "page-matching",
    class = "page page-view",
    style = "display:none;",
    page_head("跨港媒合"),
    div(
      class = "bento",
      div(
        class = "card span4",
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
              span(
                style = "display:inline-flex;align-items:center;gap:4px;font-size:0.67rem;color:var(--muted);white-space:nowrap;opacity:0.85;",
                HTML("<svg width='11' height='11' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><circle cx='12' cy='12' r='10' /><polyline points='12 6 12 12 16 14' /></svg>"),
                uiOutput("matching_update_stamp")
              ),
              span(class = "c-chip", "模擬配置")
            )
          ),
          uiOutput("matching_settings_bar"),
          uiOutput("matching_results_state")
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
      overview_page_ui(),
      analysis_page_ui(),
      matching_page_ui()
    )
  ),
  advanced_modal_ui()
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    analysis_ports = c("高雄港"),
    dt_page = 1L,
    dt_sort_col = NULL,
    dt_sort_dir = "asc",
    match_has_run = FALSE,
    match_selected_target = NULL
  )

  selected_year <- reactive(as.integer(input$year_selector %||% 2025))
  selected_month <- reactive(as.integer(input$month_selector %||% 3))
  selected_ym <- reactive(month_to_ym(selected_month(), selected_year()))

  overview_month_status <- reactive({
    status_final %>%
      filter(ym == selected_ym())
  })

  overview_default_source <- reactive({
    default_source_for_month(selected_ym())
  })

  overview_default_move_amount <- reactive({
    default_move_amount(selected_ym(), overview_default_source())
  })

  overview_candidates <- reactive({
    safe_build_candidate_table(
      ym_value = selected_ym(),
      source_port = overview_default_source(),
      move_amount = overview_default_move_amount()
    ) %>%
      head(3)
  })

  overview_formal_match <- reactive({
    matching_final %>%
      filter(ym == selected_ym()) %>%
      arrange(rank_global)
  })

  overview_sim_reco <- reactive({
    simulation_reco %>%
      filter(ym == selected_ym()) %>%
      arrange(rank_global)
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
    analysis_table %>%
      filter(port %in% analysis_selected_ports())
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
    master_table %>%
      filter(port %in% normalize_port(table_ports)) %>%
      transmute(
        ym = ym_label,
        port = display_port(port),
        empty_in,
        empty_out,
        empty_net,
        throughput,
        pressure = pressure,
        buffer = buffer
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
        ord <- order(data[[col_name]], na.last = TRUE)
        if (desc_dir) {
          ord <- rev(ord)
        }
        data <- data[ord, , drop = FALSE]
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
    snapshot <- overview_month_status() %>%
      arrange(desc(pressure_index), desc(empty_net))

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
    normalize_port(input$sim_src %||% overview_default_source())
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
      rv$match_selected_target <- current_candidates$target_port[[1]]
    }
  })

  observeEvent(input$matching_reset, {
    rv$match_has_run <- FALSE
    rv$match_selected_target <- NULL
  })

  observeEvent(input$matching_target_click, {
    rv$match_selected_target <- normalize_port(sub("::.*$", "", input$matching_target_click))
  })

  match_candidates <- reactive({
    safe_build_candidate_table(
      ym_value = selected_ym(),
      source_port = match_current_source(),
      move_amount = match_current_volume()
    ) %>%
      head(3)
  })

  match_selected_card <- reactive({
    cards <- match_candidates()
    if (nrow(cards) == 0) {
      return(tibble())
    }

    chosen <- cards %>%
      filter(target_port == rv$match_selected_target) %>%
      slice(1)

    if (nrow(chosen) == 0) cards %>% slice(1) else chosen
  })

  output$overview_month_chip <- renderUI({
    span(class = "c-chip", fmt_ym(selected_ym()))
  })

  output$overview_featured <- renderUI({
    snapshot <- overview_month_status()
    candidates <- overview_candidates()
    urgent <- snapshot %>%
      arrange(desc(pressure_index), desc(empty_net)) %>%
      slice(1)

    high_count <- sum(snapshot$status == "高壓力港")
    buffer_count <- sum(snapshot$status == "高緩衝港")
    pair_count <- nrow(candidates)
    pressure_delta <- mean(snapshot$pressure_index, na.rm = TRUE) -
      mean(status_final %>% filter(ym == selected_ym() - 1) %>% pull(pressure_index), na.rm = TRUE)
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
        fmt_num(overview_default_move_amount(), 0),
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

  output$overview_pressure_chart <- renderPlot({
    chart_df <- overview_month_status() %>% mutate(port_display = display_port(port))
    vals <- chart_df$pressure_index
    names(vals) <- chart_df$port_display
    if (length(vals) == 0 || all(is.na(vals))) {
      plot.new(); text(0.5, 0.5, "No data")
    } else {
      ord <- order(vals, na.last = TRUE)
      par(mar = c(3, 7, 1, 1))
      barplot(vals[ord], horiz = TRUE, las = 1, xlab = "", col = "grey70", border = NA)
      grid(nx = NA, ny = NULL, col = "grey90")
    }
  }, res = 96)


  output$overview_port_cards <- renderUI({
    snapshot <- overview_month_status()
    flower_row <- master_table %>%
      filter(ym == selected_ym(), port == "花蓮港") %>%
      slice(1)

    card_order <- c("基隆港", "臺北港", "高雄港", "臺中港")

    cards <- lapply(card_order, function(port_name) {
      row <- snapshot %>% filter(port == port_name) %>% slice(1)
      pct_val <- round(row$pressure_index[[1]] * 100)
      lvl <- level_class(pct_val)
      lbl <- level_label(lvl)

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
          span("壓力 / 緩衝"),
          span(class = "pc-stat-val", paste0(fmt_idx(row$pressure_index[[1]], 3), " / ", fmt_idx(row$buffer_index[[1]], 3)))
        ),
        div(
          class = "pc-bar-row",
          span(style = "font-size:0.72rem;color:var(--muted);white-space:nowrap;", "壓力"),
          div(class = "pc-bar-track", div(class = paste("pc-bar-fill", lvl), style = paste0("width:", pct_val, "%"))),
          span(class = paste("pc-pct", lvl), paste0(pct_val, "%"))
        )
      )
    })

    excluded_card <- if (nrow(flower_row) > 0) {
      div(
        class = "port-card excluded",
        div(
          class = "pc-header",
          div(
            class = "pc-name-group",
            div(class = "pc-dot excluded"),
            div(
              div(class = "pc-name", "花蓮港"),
              div(class = "pc-code", port_codes[["花蓮港"]])
            )
          ),
          span(class = "pc-badge excluded", "未納分析")
        ),
        div(
          class = "pc-stats",
          span("空櫃 / 吞吐量"),
          span(class = "pc-stat-val", paste0(fmt_num(flower_row$empty_in[[1]], 0), " / ", fmt_num(flower_row$throughput[[1]], 0)))
        ),
        div(
          class = "pc-bar-row",
          span(style = "font-size:0.72rem;color:var(--muted);white-space:nowrap;", "說明"),
          div(style = "font-size:0.72rem;color:#64748b;line-height:1.4;", "2025 正式 status / matching 未納入")
        )
      )
    } else {
      NULL
    }

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

    tagList(cards, excluded_card, summary_card)
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
          span(class = paste("pair-tag", tag_class), if (row$pair_tier[[1]] == "觀察候選") "觀察候選" else "可承接")
        ),
        div(
          class = "pair-teu",
          span(class = "pair-teu-num", fmt_num(overview_default_move_amount(), 0)),
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
    buttons <- c(
      lapply(formal_ports, function(port_name) {
        tags$button(
          type = "button",
          class = paste("port-btn", if (port_name %in% analysis_selected_ports()) "active" else ""),
          onclick = sprintf("toggleAnalysisPort('%s')", display_port(port_name)),
          display_port(port_name)
        )
      }),
      list(
        tags$button(
          type = "button",
          class = "port-btn disabled",
          title = "花蓮港未納入 2025 正式分析宇宙",
          "花蓮"
        )
      )
    )

    tagList(buttons)
  })

  output$analysis_chart1 <- renderPlot({
    plot_df <- analysis_data_selected() %>% arrange(port, ym)
    if (nrow(plot_df) == 0) { plot.new(); text(0.5, 0.5, "No data"); return(invisible(NULL)) }
    par(mar = c(4, 5, 2, 1))
    ylim <- range(c(plot_df$empty_in, plot_df$empty_out), na.rm = TRUE)
    xlabels <- unique(plot_df$ym_label)
    xvals <- seq_len(length(xlabels))
    plot(NA, xlim = range(xvals), ylim = ylim, xaxt = "n", xlab = "", ylab = "TEU")
    axis(1, at = xvals, labels = xlabels, las = 2, cex.axis = 0.75)
    grid(col = "grey90")
    ports <- unique(plot_df$port)
    for (i in seq_along(ports)) {
      d <- plot_df[plot_df$port == ports[i], ]
      xx <- match(d$ym_label, xlabels)
      lines(xx, d$empty_in, lty = 1, lwd = 2)
      points(xx, d$empty_in, pch = 16)
      lines(xx, d$empty_out, lty = 2, lwd = 2)
      points(xx, d$empty_out, pch = 1)
    }
    legend("topright", legend = c("進港", "出港"), lty = c(1,2), bty = "n", cex = 0.8)
  }, res = 96)


  output$analysis_chart2 <- renderPlot({
    plot_df <- analysis_data_selected() %>% arrange(port, ym)
    if (nrow(plot_df) == 0) { plot.new(); text(0.5, 0.5, "No data"); return(invisible(NULL)) }
    par(mar = c(4, 5, 2, 1))
    yvals <- c(plot_df$empty_net, plot_df$pressure_index * 100)
    ylim <- range(yvals, na.rm = TRUE)
    xlabels <- unique(plot_df$ym_label)
    xvals <- seq_len(length(xlabels))
    plot(NA, xlim = range(xvals), ylim = ylim, xaxt = "n", xlab = "", ylab = "值")
    axis(1, at = xvals, labels = xlabels, las = 2, cex.axis = 0.75)
    grid(col = "grey90")
    ports <- unique(plot_df$port)
    for (i in seq_along(ports)) {
      d <- plot_df[plot_df$port == ports[i], ]
      xx <- match(d$ym_label, xlabels)
      lines(xx, d$empty_net, lty = 1, lwd = 2)
      points(xx, d$empty_net, pch = 16)
      lines(xx, d$pressure_index * 100, lty = 2, lwd = 2)
      points(xx, d$pressure_index * 100, pch = 1)
    }
    legend("topright", legend = c("淨流量", "壓力×100"), lty = c(1,2), bty = "n", cex = 0.8)
  }, res = 96)


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

    tagList(
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
    HTML("最後更新：2026/04/28 20:00")
  })

  output$matching_settings_bar <- renderUI({
    choices <- match_source_choices()
    default_source <- display_port(overview_default_source())
    default_volume <- round(default_move_amount(selected_ym(), overview_default_source()), 0)
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
          lapply(seq_len(nrow(choices)), function(i) {
            choice <- choices[i, ]
            tags$option(
              value = display_port(choice$value[[1]]),
              if (display_port(choice$value[[1]]) == current_source) selected = NA,
              choice$label[[1]]
            )
          })
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
      active_cls <- if (row$target_port[[1]] == selected_card$target_port[[1]]) "msc-active" else ""
      bar_cls <- if (row$pressure_after_pct[[1]] >= 75) "red" else if (row$pressure_after_pct[[1]] >= 45) "amber" else "green"
      rag_cls <- if (row$pressure_before_pct[[1]] >= 75) "rag-red" else if (row$pressure_before_pct[[1]] >= 45) "rag-amber" else "rag-green"

      tags$div(
        class = paste("match-sugg-card", active_cls),
        onclick = sprintf("selectMatchingTarget('%s')", display_port(row$target_port[[1]])),
        div(
          class = "msc-header",
          div(class = "msc-rank", HTML(c("①", "②", "③")[i])),
          div(
            style = "flex:1;min-width:0;",
            div(class = "msc-name", display_port(row$target_port[[1]])),
            div(class = "msc-code", paste0(port_codes[[row$target_port[[1]]]], if (i == 1) "・最優先" else if (i == 2) "・次選" else "・備選"))
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
