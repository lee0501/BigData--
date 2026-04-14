library(readr)
library(dplyr)
library(tidyr)

master_path <- "/Users/lee/Documents/BigData/context/master_table_2025.csv"
check_summary_path <- "/Users/lee/Documents/BigData/context/check_summary.csv"
zero_throughput_path <- "/Users/lee/Documents/BigData/context/check_zero_throughput.csv"
port_summary_path <- "/Users/lee/Documents/BigData/context/check_port_summary.csv"

master <- read_csv(master_path, show_col_types = FALSE)

message("Checking file: ", master_path)
message("Rows: ", nrow(master))
message("Columns: ", ncol(master))

required_cols <- c(
  "ym", "port", "empty_in", "empty_out", "full_in", "full_out",
  "throughput", "empty_net", "empty_total_flow", "full_total_flow",
  "empty_share", "net_pressure_raw", "export_pull",
  "roll3_empty_net", "cum_empty_net"
)

missing_cols <- setdiff(required_cols, names(master))

if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
} else {
  message("Column check: OK")
}

duplicate_keys <- master %>%
  count(ym, port, name = "n") %>%
  filter(n > 1)

if (nrow(duplicate_keys) > 0) {
  message("Duplicate ym-port keys found:")
  print(duplicate_keys, n = Inf)
} else {
  message("Duplicate key check: OK")
}

na_summary <- master %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "na_count") %>%
  filter(na_count > 0)

if (nrow(na_summary) > 0) {
  message("Columns with NA values:")
  print(na_summary, n = Inf)
} else {
  message("NA check: OK")
}

zero_throughput <- master %>%
  filter(is.na(throughput) | throughput == 0) %>%
  select(ym, port, throughput)

if (nrow(zero_throughput) > 0) {
  message("Rows with missing or zero throughput:")
  print(zero_throughput, n = Inf)
} else {
  message("Throughput zero/NA check: OK")
}

write_csv(zero_throughput, zero_throughput_path)

negative_values <- master %>%
  filter(
    empty_in < 0 | empty_out < 0 | full_in < 0 | full_out < 0 | throughput < 0
  ) %>%
  select(ym, port, empty_in, empty_out, full_in, full_out, throughput)

if (nrow(negative_values) > 0) {
  message("Rows with unexpected negative values:")
  print(negative_values, n = Inf)
} else {
  message("Negative value check: OK")
}

port_month_count <- master %>%
  count(port, name = "month_count") %>%
  arrange(desc(month_count), port)

message("Month count by port:")
print(port_month_count, n = Inf)

port_summary <- master %>%
  group_by(port) %>%
  summarise(
    months = n(),
    total_empty_in = sum(empty_in, na.rm = TRUE),
    total_empty_out = sum(empty_out, na.rm = TRUE),
    total_full_in = sum(full_in, na.rm = TRUE),
    total_full_out = sum(full_out, na.rm = TRUE),
    total_throughput = sum(throughput, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_throughput))

message("Port summary:")
print(port_summary, n = Inf)

write_csv(port_summary, port_summary_path)

check_summary <- tibble::tibble(
  metric = c(
    "row_count",
    "column_count",
    "missing_required_columns",
    "duplicate_key_rows",
    "columns_with_na",
    "rows_with_zero_or_missing_throughput",
    "rows_with_negative_values"
  ),
  value = c(
    nrow(master),
    ncol(master),
    length(missing_cols),
    nrow(duplicate_keys),
    nrow(na_summary),
    nrow(zero_throughput),
    nrow(negative_values)
  )
)

write_csv(check_summary, check_summary_path)

message("Check complete.")
