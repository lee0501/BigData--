library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)

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
input_dir <- file.path(context_dir, "input")

count_input_path <- file.path(input_dir, "port_container_count_by_empty_full.csv")
direction_input_path <- file.path(input_dir, "port_container_teu_by_direction.csv")

empty_full_output_path <- file.path(context_dir, "port_stat_empty_full_monthly.csv")
direction_output_path <- file.path(context_dir, "port_stat_direction_monthly.csv")
base_output_path <- file.path(context_dir, "port_stat_prophet_base.csv")

fill_right <- function(x) {
  current <- ""
  out <- character(length(x))
  for (i in seq_along(x)) {
    value <- ifelse(is.na(x[[i]]), "", as.character(x[[i]]))
    if (nzchar(value)) {
      current <- value
    }
    out[[i]] <- current
  }
  out
}

to_number <- function(x) {
  cleaned <- gsub(",", "", as.character(x), fixed = TRUE)
  cleaned <- ifelse(cleaned %in% c("", "-", "NA"), "0", cleaned)
  suppressWarnings(as.numeric(cleaned))
}

parse_roc_month <- function(x) {
  label <- as.character(x)
  is_month <- grepl("^[0-9]{3}年[0-9]{2}月$", label)
  year <- rep(NA_integer_, length(label))
  month <- rep(NA_integer_, length(label))
  year[is_month] <- as.integer(sub("^([0-9]{3})年[0-9]{2}月$", "\\1", label[is_month])) + 1911L
  month[is_month] <- as.integer(sub("^[0-9]{3}年([0-9]{2})月$", "\\1", label[is_month]))
  ym <- year * 100L + month
  ds <- rep(as.Date(NA), length(label))
  ds[is_month] <- as.Date(sprintf("%04d-%02d-01", year[is_month], month[is_month]))
  tibble(is_month = is_month, ym = ym, ds = ds)
}

normalize_port <- function(x) {
  gsub("台", "臺", x)
}

read_stat_csv <- function(path) {
  raw <- read.csv(
    path,
    header = FALSE,
    check.names = FALSE,
    na.strings = "",
    fileEncoding = "UTF-8-BOM"
  )
  raw[is.na(raw)] <- ""
  raw
}

build_empty_full_monthly <- function(path) {
  raw <- read_stat_csv(path)

  h1 <- fill_right(as.character(raw[1, ]))
  h2 <- fill_right(as.character(raw[2, ]))
  h3 <- as.character(raw[3, ])

  map <- tibble(
    col = seq_along(h1),
    empty_full_type = h1,
    port = h2,
    container_size = h3
  ) %>%
    filter(
      empty_full_type %in% c("總計", "實櫃", "空櫃"),
      port != "港口別",
      container_size %in% c("貨櫃類型別總計", "10呎", "20呎", "40呎", "45呎")
    )

  data_rows <- raw[-c(1:4), , drop = FALSE]
  date_info <- parse_roc_month(data_rows[[1]])
  data_rows <- data_rows[date_info$is_month, , drop = FALSE]
  date_info <- date_info[date_info$is_month, ]

  row_index <- seq_len(nrow(data_rows))
  long <- bind_rows(lapply(row_index, function(i) {
    tibble(
      ym = date_info$ym[[i]],
      ds = date_info$ds[[i]],
      empty_full_type = map$empty_full_type,
      port = normalize_port(map$port),
      container_size = map$container_size,
      value = to_number(unlist(data_rows[i, map$col], use.names = FALSE))
    )
  }))

  count_monthly <- long %>%
    filter(container_size == "貨櫃類型別總計") %>%
    transmute(
      ym,
      ds,
      port,
      empty_full_type,
      container_count = value
    )

  teu_monthly <- long %>%
    filter(container_size %in% c("10呎", "20呎", "40呎", "45呎")) %>%
    mutate(
      teu_factor = case_when(
        container_size == "10呎" ~ 0.5,
        container_size == "20呎" ~ 1.0,
        container_size == "40呎" ~ 2.0,
        container_size == "45呎" ~ 2.25,
        TRUE ~ NA_real_
      ),
      teu_est = value * teu_factor
    ) %>%
    group_by(ym, ds, port, empty_full_type) %>%
    summarise(container_teu_est = sum(teu_est, na.rm = TRUE), .groups = "drop")

  count_monthly %>%
    left_join(teu_monthly, by = c("ym", "ds", "port", "empty_full_type")) %>%
    arrange(ym, port, empty_full_type)
}

build_direction_monthly <- function(path) {
  raw <- read_stat_csv(path)

  h1 <- fill_right(as.character(raw[1, ]))
  h2 <- fill_right(as.character(raw[2, ]))
  h3 <- as.character(raw[3, ])

  map <- tibble(
    col = seq_along(h1),
    direction_type = h1,
    port = h2,
    empty_full_type = h3
  ) %>%
    filter(
      direction_type %in% c("總計", "進港", "出港"),
      port != "港口別",
      empty_full_type == "空實櫃別總計"
    )

  data_rows <- raw[-c(1:4), , drop = FALSE]
  date_info <- parse_roc_month(data_rows[[1]])
  data_rows <- data_rows[date_info$is_month, , drop = FALSE]
  date_info <- date_info[date_info$is_month, ]

  row_index <- seq_len(nrow(data_rows))
  bind_rows(lapply(row_index, function(i) {
    tibble(
      ym = date_info$ym[[i]],
      ds = date_info$ds[[i]],
      direction_type = map$direction_type,
      port = normalize_port(map$port),
      teu = to_number(unlist(data_rows[i, map$col], use.names = FALSE))
    )
  })) %>%
    arrange(ym, port, direction_type)
}

empty_full_monthly <- build_empty_full_monthly(count_input_path)
direction_monthly <- build_direction_monthly(direction_input_path)

empty_full_wide <- empty_full_monthly %>%
  mutate(
    type_key = case_when(
      empty_full_type == "總計" ~ "total",
      empty_full_type == "實櫃" ~ "full",
      empty_full_type == "空櫃" ~ "empty",
      TRUE ~ empty_full_type
    )
  ) %>%
  select(ym, ds, port, type_key, container_count, container_teu_est) %>%
  pivot_wider(
    names_from = type_key,
    values_from = c(container_count, container_teu_est),
    names_glue = "{type_key}_{.value}"
  )

direction_wide <- direction_monthly %>%
  mutate(
    direction_key = case_when(
      direction_type == "總計" ~ "total",
      direction_type == "進港" ~ "in",
      direction_type == "出港" ~ "out",
      TRUE ~ direction_type
    )
  ) %>%
  select(ym, ds, port, direction_key, teu) %>%
  pivot_wider(
    names_from = direction_key,
    values_from = teu,
    names_glue = "{direction_key}_teu"
  )

prophet_base <- empty_full_wide %>%
  full_join(direction_wide, by = c("ym", "ds", "port")) %>%
  mutate(
    empty_share_count = empty_container_count / total_container_count,
    full_share_count = full_container_count / total_container_count,
    empty_share_teu_est = empty_container_teu_est / total_container_teu_est,
    full_share_teu_est = full_container_teu_est / total_container_teu_est,
    direction_balance_teu = in_teu - out_teu,
    direction_balance_rate = direction_balance_teu / total_teu
  ) %>%
  arrange(port, ym)

write_csv(empty_full_monthly, empty_full_output_path)
write_csv(direction_monthly, direction_output_path)
write_csv(prophet_base, base_output_path)

message("Empty/full monthly table written to: ", empty_full_output_path)
message("Direction monthly table written to: ", direction_output_path)
message("Prophet base table written to: ", base_output_path)
