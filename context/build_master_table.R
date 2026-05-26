library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(slider)
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

# Input files
enter_empty_path <- file.path(input_dir, "enter-empty.csv")
enter_full_path <- file.path(input_dir, "enter-full.csv")
exit_empty_path <- file.path(input_dir, "exit-empty.csv")
exit_full_path <- file.path(input_dir, "exit-full.csv")
throughput_path <- file.path(input_dir, "throughput.csv")

# Output file
output_path <- file.path(context_dir, "master_table_2025.csv")

# Initial port mapping based on currently confirmed major ports.
port_map <- tribble(
  ~taiwan_port_code, ~port,
  "TWKHH", "高雄港",
  "TWTXG", "臺中港",
  "TWTPE", "臺北港",
  "TWKEL", "基隆港"
)

clean_flow <- function(path, value_name) {
  read_csv(path, show_col_types = FALSE) %>%
    transmute(
      ym = str_replace(`年月`, "/", ""),
      taiwan_port_code = `臺灣港`,
      value = `20呎標準貨櫃`
    ) %>%
    inner_join(port_map, by = "taiwan_port_code") %>%
    group_by(ym, port) %>%
    summarise(!!value_name := sum(value, na.rm = TRUE), .groups = "drop")
}

empty_in <- clean_flow(enter_empty_path, "empty_in")
full_in <- clean_flow(enter_full_path, "full_in")
empty_out <- clean_flow(exit_empty_path, "empty_out")
full_out <- clean_flow(exit_full_path, "full_out")

throughput <- read_csv(throughput_path, show_col_types = FALSE) %>%
  transmute(
    ym = as.character(`年月`),
    port = `港口別`,
    throughput = `總計`
  ) %>%
  filter(
    str_starts(ym, "2025"),
    port %in% c("基隆港", "臺中港", "高雄港", "臺北港", "安平港", "花蓮港", "蘇澳港")
  )

master <- empty_in %>%
  full_join(empty_out, by = c("ym", "port")) %>%
  full_join(full_in, by = c("ym", "port")) %>%
  full_join(full_out, by = c("ym", "port")) %>%
  full_join(throughput, by = c("ym", "port")) %>%
  mutate(
    across(c(empty_in, empty_out, full_in, full_out), ~ replace_na(., 0)),
    empty_net = empty_in - empty_out,
    empty_total_flow = empty_in + empty_out,
    full_total_flow = full_in + full_out,
    empty_share = empty_total_flow / throughput,
    net_pressure_raw = empty_net / throughput,
    export_pull = full_out / throughput
  ) %>%
  arrange(port, ym) %>%
  group_by(port) %>%
  mutate(
    roll3_empty_net = slide_dbl(
      empty_net,
      mean,
      .before = 2,
      .complete = FALSE,
      na.rm = TRUE
    ),
    cum_empty_net = cumsum(empty_net)
  ) %>%
  ungroup()

write_csv(master, output_path)

message("Master table written to: ", output_path)
