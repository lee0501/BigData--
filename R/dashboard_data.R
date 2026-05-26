app_dir <- locate_app_dir()
context_dir <- file.path(app_dir, "context")

dashboard_context_files <- c(
  "status_final_2025.csv",
  "matching_final_2025.csv",
  "simulation_recommendation_final_2025.csv",
  "simulation_final_2025.csv",
  "analysis_table_2025.csv",
  "analysis_table_history_2025.csv",
  "master_table_2025.csv",
  "port_pair_distance_lookup.csv"
)

build_data_origin_report <- function() {
  available_ym <- if (exists("status_final", inherits = TRUE)) {
    sort(unique(get("status_final", inherits = TRUE)$ym))
  } else {
    integer()
  }

  file_lines <- vapply(
    dashboard_context_files,
    function(file_name) {
      file_path <- file.path(context_dir, file_name)
      sprintf("%-45s %s", file_name, ifelse(file.exists(file_path), "OK", "MISSING"))
    },
    character(1)
  )

  c(
    "Project directory:",
    app_dir,
    "",
    "Context directory:",
    context_dir,
    "",
    "資料檔案狀態:",
    file_lines,
    "",
    "Available ym values:",
    paste(fmt_ym(available_ym), collapse = ", ")
  )
}

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

distance_detail_path <- file.path(context_dir, "input", "Distance_Matrix - define.csv")
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

next_month_watchlist <- {
  p <- file.path(context_dir, "forecast_high_pressure_latest_watchlist.csv")
  if (file.exists(p)) {
    read_csv(p, show_col_types = FALSE) %>%
      mutate(port = normalize_port(port)) %>%
      arrange(selected_rank)
  } else {
    tibble()
  }
}

next_month_backtest_top1 <- {
  p <- file.path(context_dir, "forecast_high_pressure_backtest_summary.csv")
  if (file.exists(p)) {
    read_csv(p, show_col_types = FALSE) %>%
      filter(window == "official", model == "pressure_index_streak_boost") %>%
      slice(1)
  } else {
    tibble()
  }
}

next_month_available <- nrow(next_month_watchlist) > 0
