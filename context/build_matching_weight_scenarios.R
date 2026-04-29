library(readr)
library(dplyr)
library(tibble)

output_path <- "/Users/lee/Documents/BigData/context/matching_weight_scenarios.csv"

# Matching score design logic:
# 1. First compare the two parent schemes: roc vs custom.
# 2. The matching score uses three components:
#    - source_pressure_pct: urgency of the source port
#    - target_buffer_pct: receiving stability of the target port
#    - distance_pct: dispatch friction proxy
# 3. If one parent scheme wins, only extend nearby scenarios around that family.

matching_weight_scenarios <- tribble(
  ~scenario, ~family, ~phase, ~component, ~weight, ~rank, ~source_note,
  "roc", "roc", "parent", "source_pressure_pct", 0.611, 1L, "ROC baseline: source urgency first",
  "roc", "roc", "parent", "target_buffer_pct", 0.278, 2L, "ROC baseline: target buffer second",
  "roc", "roc", "parent", "distance_pct", 0.111, 3L, "ROC baseline: distance friction third",
  "custom", "custom", "parent", "source_pressure_pct", 0.45, 1L, "Custom default from app_formula_design.md",
  "custom", "custom", "parent", "target_buffer_pct", 0.35, 2L, "Custom default from app_formula_design.md",
  "custom", "custom", "parent", "distance_pct", 0.20, 3L, "Custom default from app_formula_design.md",
  "roc_nearby_a", "roc", "nearby", "source_pressure_pct", 0.58, 1L, "Nearby ROC: slightly less source, more target",
  "roc_nearby_a", "roc", "nearby", "target_buffer_pct", 0.30, 2L, "Nearby ROC: slightly less source, more target",
  "roc_nearby_a", "roc", "nearby", "distance_pct", 0.12, 3L, "Nearby ROC: slightly less source, more target",
  "roc_nearby_b", "roc", "nearby", "source_pressure_pct", 0.55, 1L, "Nearby ROC: less source, more distance",
  "roc_nearby_b", "roc", "nearby", "target_buffer_pct", 0.30, 2L, "Nearby ROC: less source, more distance",
  "roc_nearby_b", "roc", "nearby", "distance_pct", 0.15, 3L, "Nearby ROC: less source, more distance",
  "roc_nearby_c", "roc", "nearby", "source_pressure_pct", 0.64, 1L, "Nearby ROC: more source, less target",
  "roc_nearby_c", "roc", "nearby", "target_buffer_pct", 0.25, 2L, "Nearby ROC: more source, less target",
  "roc_nearby_c", "roc", "nearby", "distance_pct", 0.11, 3L, "Nearby ROC: more source, less target",
  "roc_nearby_d", "roc", "nearby", "source_pressure_pct", 0.61, 1L, "Nearby ROC: keep source, increase distance",
  "roc_nearby_d", "roc", "nearby", "target_buffer_pct", 0.24, 2L, "Nearby ROC: keep source, increase distance",
  "roc_nearby_d", "roc", "nearby", "distance_pct", 0.15, 3L, "Nearby ROC: keep source, increase distance",
  "roc_nearby_e", "roc", "nearby", "source_pressure_pct", 0.58, 1L, "Nearby ROC: reduce source, raise distance and target balance",
  "roc_nearby_e", "roc", "nearby", "target_buffer_pct", 0.25, 2L, "Nearby ROC: reduce source, raise distance and target balance",
  "roc_nearby_e", "roc", "nearby", "distance_pct", 0.17, 3L, "Nearby ROC: reduce source, raise distance and target balance",
  "custom_nearby_a", "custom", "nearby", "source_pressure_pct", 0.50, 1L, "Nearby custom: more source urgency",
  "custom_nearby_a", "custom", "nearby", "target_buffer_pct", 0.30, 2L, "Nearby custom: more source urgency",
  "custom_nearby_a", "custom", "nearby", "distance_pct", 0.20, 3L, "Nearby custom: more source urgency",
  "custom_nearby_b", "custom", "nearby", "source_pressure_pct", 0.40, 1L, "Nearby custom: more target buffer",
  "custom_nearby_b", "custom", "nearby", "target_buffer_pct", 0.40, 2L, "Nearby custom: more target buffer",
  "custom_nearby_b", "custom", "nearby", "distance_pct", 0.20, 3L, "Nearby custom: more target buffer",
  "custom_nearby_c", "custom", "nearby", "source_pressure_pct", 0.45, 1L, "Nearby custom: shift distance to target buffer",
  "custom_nearby_c", "custom", "nearby", "target_buffer_pct", 0.40, 2L, "Nearby custom: shift distance to target buffer",
  "custom_nearby_c", "custom", "nearby", "distance_pct", 0.15, 3L, "Nearby custom: shift distance to target buffer",
  "custom_nearby_d", "custom", "nearby", "source_pressure_pct", 0.45, 1L, "Nearby custom: more distance, less target",
  "custom_nearby_d", "custom", "nearby", "target_buffer_pct", 0.30, 2L, "Nearby custom: more distance, less target",
  "custom_nearby_d", "custom", "nearby", "distance_pct", 0.25, 3L, "Nearby custom: more distance, less target",
  "custom_nearby_e", "custom", "nearby", "source_pressure_pct", 0.50, 1L, "Nearby custom: more source and distance",
  "custom_nearby_e", "custom", "nearby", "target_buffer_pct", 0.25, 2L, "Nearby custom: more source and distance",
  "custom_nearby_e", "custom", "nearby", "distance_pct", 0.25, 3L, "Nearby custom: more source and distance"
) %>%
  arrange(family, phase, scenario, rank)

write_csv(matching_weight_scenarios, output_path)

message("Matching weight scenarios written to: ", output_path)
