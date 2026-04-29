library(readr)
library(dplyr)
library(tidyr)

status_input_path <- "/Users/lee/Documents/BigData/context/status_final_2025.csv"
distance_input_path <- "/Users/lee/Downloads/Distance_Matrix - 距離.csv"
distance_lookup_output_path <- "/Users/lee/Documents/BigData/context/port_pair_distance_lookup.csv"
output_path <- "/Users/lee/Documents/BigData/context/matching_final_2025.csv"

matching_source_weight <- 0.45
matching_target_weight <- 0.35
matching_distance_weight <- 0.20

normalize_port <- function(x) {
  gsub("台", "臺", x)
}

status_table <- read_csv(status_input_path, show_col_types = FALSE) %>%
  mutate(port = normalize_port(port))

distance_lookup <- read_csv(distance_input_path, show_col_types = FALSE) %>%
  rename(source_port = 1) %>%
  mutate(source_port = normalize_port(source_port)) %>%
  pivot_longer(
    cols = -source_port,
    names_to = "target_port",
    values_to = "distance_class"
  ) %>%
  mutate(
    target_port = normalize_port(target_port),
    distance_class = as.numeric(distance_class),
    distance_factor = case_when(
      distance_class == 1 ~ 1.00,
      distance_class == 2 ~ 0.85,
      distance_class == 3 ~ 0.70,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(distance_class), distance_class > 0) %>%
  arrange(source_port, target_port)

write_csv(distance_lookup, distance_lookup_output_path)

sources <- status_table %>%
  filter(status == "高壓力港") %>%
  transmute(
    ym,
    source_port = port,
    source_status = status,
    source_pressure_index = pressure_index,
    source_buffer_index = buffer_index,
    source_empty_net = empty_net,
    source_roll3_empty_net = roll3_empty_net
  )

targets <- status_table %>%
  filter(status == "高緩衝港") %>%
  transmute(
    ym,
    target_port = port,
    target_status = status,
    target_pressure_index = pressure_index,
    target_buffer_index = buffer_index,
    target_empty_net = empty_net,
    target_roll3_empty_net = roll3_empty_net
  )

matching_table <- inner_join(
  sources,
  targets,
  by = "ym",
  relationship = "many-to-many"
) %>%
  filter(source_port != target_port) %>%
  left_join(distance_lookup, by = c("source_port", "target_port")) %>%
  filter(!is.na(distance_factor)) %>%
  mutate(
    matching_scenario = "custom",
    analysis_universe = "基隆港、臺北港、臺中港、高雄港",
    matching_score =
      matching_source_weight * source_pressure_index +
      matching_target_weight * target_buffer_index +
      matching_distance_weight * distance_factor
  ) %>%
  group_by(ym) %>%
  arrange(desc(matching_score), desc(distance_factor), desc(target_buffer_index), target_port, .by_group = TRUE) %>%
  mutate(rank_global = row_number()) %>%
  ungroup() %>%
  group_by(ym, source_port) %>%
  arrange(desc(matching_score), desc(distance_factor), desc(target_buffer_index), target_port, .by_group = TRUE) %>%
  mutate(rank_within_source = row_number()) %>%
  ungroup() %>%
  transmute(
    ym,
    analysis_universe,
    matching_scenario,
    source_port,
    source_status,
    source_pressure_index,
    source_buffer_index,
    source_empty_net,
    source_roll3_empty_net,
    target_port,
    target_status,
    target_pressure_index,
    target_buffer_index,
    target_empty_net,
    target_roll3_empty_net,
    distance_class,
    distance_factor,
    matching_score,
    rank_global,
    rank_within_source
  ) %>%
  arrange(ym, rank_global, source_port, target_port)

write_csv(matching_table, output_path)

message("Distance lookup written to: ", distance_lookup_output_path)
message("Matching final table written to: ", output_path)

