library(readr)
library(dplyr)
library(tibble)

output_path <- "/Users/lee/Documents/BigData/context/weight_scenarios.csv"

# Weight design logic:
# 1. Pressure 3.2 should first compare the two parent schemes: roc vs custom.
# 2. Since the current pressure-only backtest shows roc performs better than custom,
#    the formal main line should continue with roc as the baseline.
# 3. Therefore roc_down_* and roc_up_* are the key sensitivity tests for the formal version.
# 4. custom_nearby_* can still be kept as exploratory appendix scenarios, but they are
#    no longer the main decision path once roc has already beaten custom.
#
# ROC baseline for 3 components:
#   pressure_net_pct  = 0.611
#   pressure_flow_pct = 0.278
#   pressure_roll_pct = 0.111
#
# ROC sensitivity design used in the current test:
# - roc_prev_best retains the previous best candidate from the earlier nearby test.
# - roc_down_1 / 2 / 3 test lower pressure_net weights than ROC.
# - roc_up_1 / 2 / 3 test higher pressure_net weights than ROC.
# - roc_bridge_1 / 2 / 3 test the direct path between roc_prev_best and roc_up_3.
# - All variants keep pressure_net as the largest component and keep weights summing to 1.
#
# Previous best candidate:
#   roc_prev_best: net 0.600, flow 0.220, roll 0.180
#
# Downward net-weight tests:
#   roc_down_1: net 0.580, flow 0.270, roll 0.150
#   roc_down_2: net 0.560, flow 0.280, roll 0.160
#   roc_down_3: net 0.540, flow 0.290, roll 0.170
#
# Upward net-weight tests:
#   roc_up_1: net 0.640, flow 0.250, roll 0.110
#   roc_up_2: net 0.670, flow 0.230, roll 0.100
#   roc_up_3: net 0.700, flow 0.210, roll 0.090
#
# Bridge tests between roc_prev_best and roc_up_3:
#   roc_bridge_1: net 0.625, flow 0.218, roll 0.157
#   roc_bridge_2: net 0.650, flow 0.215, roll 0.135
#   roc_bridge_3: net 0.675, flow 0.213, roll 0.112
#
# Custom baseline and custom_nearby_* are retained below only for exploratory comparison.

weight_scenarios <- tribble(
  ~scenario, ~index_type, ~component, ~weight, ~rank, ~source_note,
  "roc", "pressure", "pressure_net_pct", 0.611, 1L, "ROC baseline: prioritize actual accumulation first",
  "roc", "pressure", "pressure_flow_pct", 0.278, 2L, "ROC baseline: current inflow share second",
  "roc", "pressure", "pressure_roll_pct", 0.111, 3L, "ROC baseline: recent trend third",
  "roc_prev_best", "pressure", "pressure_net_pct", 0.60, 1L, "Previous best ROC nearby candidate: net 0.600, flow 0.220, roll 0.180",
  "roc_prev_best", "pressure", "pressure_flow_pct", 0.22, 2L, "Previous best ROC nearby candidate: net 0.600, flow 0.220, roll 0.180",
  "roc_prev_best", "pressure", "pressure_roll_pct", 0.18, 3L, "Previous best ROC nearby candidate: net 0.600, flow 0.220, roll 0.180",
  "roc_down_1", "pressure", "pressure_net_pct", 0.58, 1L, "ROC down 1: net 0.611 -> 0.580, flow 0.278 -> 0.270, roll 0.111 -> 0.150",
  "roc_down_1", "pressure", "pressure_flow_pct", 0.27, 2L, "ROC down 1: net 0.611 -> 0.580, flow 0.278 -> 0.270, roll 0.111 -> 0.150",
  "roc_down_1", "pressure", "pressure_roll_pct", 0.15, 3L, "ROC down 1: net 0.611 -> 0.580, flow 0.278 -> 0.270, roll 0.111 -> 0.150",
  "roc_down_2", "pressure", "pressure_net_pct", 0.56, 1L, "ROC down 2: net 0.611 -> 0.560, flow 0.278 -> 0.280, roll 0.111 -> 0.160",
  "roc_down_2", "pressure", "pressure_flow_pct", 0.28, 2L, "ROC down 2: net 0.611 -> 0.560, flow 0.278 -> 0.280, roll 0.111 -> 0.160",
  "roc_down_2", "pressure", "pressure_roll_pct", 0.16, 3L, "ROC down 2: net 0.611 -> 0.560, flow 0.278 -> 0.280, roll 0.111 -> 0.160",
  "roc_down_3", "pressure", "pressure_net_pct", 0.54, 1L, "ROC down 3: net 0.611 -> 0.540, flow 0.278 -> 0.290, roll 0.111 -> 0.170",
  "roc_down_3", "pressure", "pressure_flow_pct", 0.29, 2L, "ROC down 3: net 0.611 -> 0.540, flow 0.278 -> 0.290, roll 0.111 -> 0.170",
  "roc_down_3", "pressure", "pressure_roll_pct", 0.17, 3L, "ROC down 3: net 0.611 -> 0.540, flow 0.278 -> 0.290, roll 0.111 -> 0.170",
  "roc_bridge_1", "pressure", "pressure_net_pct", 0.625, 1L, "ROC bridge 1: between prev_best and up_3; net 0.625, flow 0.218, roll 0.157",
  "roc_bridge_1", "pressure", "pressure_flow_pct", 0.218, 2L, "ROC bridge 1: between prev_best and up_3; net 0.625, flow 0.218, roll 0.157",
  "roc_bridge_1", "pressure", "pressure_roll_pct", 0.157, 3L, "ROC bridge 1: between prev_best and up_3; net 0.625, flow 0.218, roll 0.157",
  "roc_bridge_2", "pressure", "pressure_net_pct", 0.650, 1L, "ROC bridge 2: between prev_best and up_3; net 0.650, flow 0.215, roll 0.135",
  "roc_bridge_2", "pressure", "pressure_flow_pct", 0.215, 2L, "ROC bridge 2: between prev_best and up_3; net 0.650, flow 0.215, roll 0.135",
  "roc_bridge_2", "pressure", "pressure_roll_pct", 0.135, 3L, "ROC bridge 2: between prev_best and up_3; net 0.650, flow 0.215, roll 0.135",
  "roc_bridge_3", "pressure", "pressure_net_pct", 0.675, 1L, "ROC bridge 3: between prev_best and up_3; net 0.675, flow 0.213, roll 0.112",
  "roc_bridge_3", "pressure", "pressure_flow_pct", 0.213, 2L, "ROC bridge 3: between prev_best and up_3; net 0.675, flow 0.213, roll 0.112",
  "roc_bridge_3", "pressure", "pressure_roll_pct", 0.112, 3L, "ROC bridge 3: between prev_best and up_3; net 0.675, flow 0.213, roll 0.112",
  "roc_up_1", "pressure", "pressure_net_pct", 0.64, 1L, "ROC up 1: net 0.611 -> 0.640, flow 0.278 -> 0.250, roll 0.111 -> 0.110",
  "roc_up_1", "pressure", "pressure_flow_pct", 0.25, 2L, "ROC up 1: net 0.611 -> 0.640, flow 0.278 -> 0.250, roll 0.111 -> 0.110",
  "roc_up_1", "pressure", "pressure_roll_pct", 0.11, 3L, "ROC up 1: net 0.611 -> 0.640, flow 0.278 -> 0.250, roll 0.111 -> 0.110",
  "roc_up_2", "pressure", "pressure_net_pct", 0.67, 1L, "ROC up 2: net 0.611 -> 0.670, flow 0.278 -> 0.230, roll 0.111 -> 0.100",
  "roc_up_2", "pressure", "pressure_flow_pct", 0.23, 2L, "ROC up 2: net 0.611 -> 0.670, flow 0.278 -> 0.230, roll 0.111 -> 0.100",
  "roc_up_2", "pressure", "pressure_roll_pct", 0.10, 3L, "ROC up 2: net 0.611 -> 0.670, flow 0.278 -> 0.230, roll 0.111 -> 0.100",
  "roc_up_3", "pressure", "pressure_net_pct", 0.70, 1L, "ROC up 3: net 0.611 -> 0.700, flow 0.278 -> 0.210, roll 0.111 -> 0.090",
  "roc_up_3", "pressure", "pressure_flow_pct", 0.21, 2L, "ROC up 3: net 0.611 -> 0.700, flow 0.278 -> 0.210, roll 0.111 -> 0.090",
  "roc_up_3", "pressure", "pressure_roll_pct", 0.09, 3L, "ROC up 3: net 0.611 -> 0.700, flow 0.278 -> 0.210, roll 0.111 -> 0.090",
  "custom", "pressure", "pressure_flow_pct", 0.35, 2L, "Custom rule from app_formula_design.md",
  "custom", "pressure", "pressure_net_pct", 0.40, 1L, "Custom rule from app_formula_design.md",
  "custom", "pressure", "pressure_roll_pct", 0.25, 3L, "Custom rule from app_formula_design.md",
  "custom_nearby_a", "pressure", "pressure_flow_pct", 0.30, 2L, "Sensitivity test around custom weights",
  "custom_nearby_a", "pressure", "pressure_net_pct", 0.45, 1L, "Sensitivity test around custom weights",
  "custom_nearby_a", "pressure", "pressure_roll_pct", 0.25, 3L, "Sensitivity test around custom weights",
  "custom_nearby_b", "pressure", "pressure_flow_pct", 0.40, 2L, "Sensitivity test around custom weights",
  "custom_nearby_b", "pressure", "pressure_net_pct", 0.35, 1L, "Sensitivity test around custom weights",
  "custom_nearby_b", "pressure", "pressure_roll_pct", 0.25, 3L, "Sensitivity test around custom weights",
  "custom_nearby_c", "pressure", "pressure_flow_pct", 0.35, 2L, "Sensitivity test around custom weights",
  "custom_nearby_c", "pressure", "pressure_net_pct", 0.45, 1L, "Sensitivity test around custom weights",
  "custom_nearby_c", "pressure", "pressure_roll_pct", 0.20, 3L, "Sensitivity test around custom weights",
  "custom_nearby_d", "pressure", "pressure_flow_pct", 0.40, 2L, "Sensitivity test around custom weights",
  "custom_nearby_d", "pressure", "pressure_net_pct", 0.40, 1L, "Sensitivity test around custom weights",
  "custom_nearby_d", "pressure", "pressure_roll_pct", 0.20, 3L, "Sensitivity test around custom weights",
  "custom_nearby_e", "pressure", "pressure_flow_pct", 0.30, 2L, "Sensitivity test around custom weights",
  "custom_nearby_e", "pressure", "pressure_net_pct", 0.40, 1L, "Sensitivity test around custom weights",
  "custom_nearby_e", "pressure", "pressure_roll_pct", 0.30, 3L, "Sensitivity test around custom weights",
  "roc", "buffer", "buffer_space_pct", 0.611, 1L, "ROC baseline from 權重資料.md",
  "roc", "buffer", "buffer_net_pct", 0.278, 2L, "ROC baseline from 權重資料.md",
  "roc", "buffer", "buffer_export_pct", 0.111, 3L, "ROC baseline from 權重資料.md",
  "custom", "buffer", "buffer_space_pct", 0.45, 1L, "Custom rule from app_formula_design.md",
  "custom", "buffer", "buffer_net_pct", 0.35, 2L, "Custom rule from app_formula_design.md",
  "custom", "buffer", "buffer_export_pct", 0.20, 3L, "Custom rule from app_formula_design.md",
  "custom_nearby_a", "buffer", "buffer_space_pct", 0.50, 1L, "Sensitivity test around custom weights",
  "custom_nearby_a", "buffer", "buffer_net_pct", 0.30, 2L, "Sensitivity test around custom weights",
  "custom_nearby_a", "buffer", "buffer_export_pct", 0.20, 3L, "Sensitivity test around custom weights",
  "custom_nearby_b", "buffer", "buffer_space_pct", 0.40, 1L, "Sensitivity test around custom weights",
  "custom_nearby_b", "buffer", "buffer_net_pct", 0.40, 2L, "Sensitivity test around custom weights",
  "custom_nearby_b", "buffer", "buffer_export_pct", 0.20, 3L, "Sensitivity test around custom weights",
  "custom_nearby_c", "buffer", "buffer_space_pct", 0.45, 1L, "Sensitivity test around custom weights",
  "custom_nearby_c", "buffer", "buffer_net_pct", 0.40, 2L, "Sensitivity test around custom weights",
  "custom_nearby_c", "buffer", "buffer_export_pct", 0.15, 3L, "Sensitivity test around custom weights",
  "custom_nearby_d", "buffer", "buffer_space_pct", 0.50, 1L, "Sensitivity test around custom weights",
  "custom_nearby_d", "buffer", "buffer_net_pct", 0.35, 2L, "Sensitivity test around custom weights",
  "custom_nearby_d", "buffer", "buffer_export_pct", 0.15, 3L, "Sensitivity test around custom weights",
  "custom_nearby_e", "buffer", "buffer_space_pct", 0.40, 1L, "Sensitivity test around custom weights",
  "custom_nearby_e", "buffer", "buffer_net_pct", 0.35, 2L, "Sensitivity test around custom weights",
  "custom_nearby_e", "buffer", "buffer_export_pct", 0.25, 3L, "Sensitivity test around custom weights"
) %>%
  arrange(index_type, scenario, rank)

write_csv(weight_scenarios, output_path)

message("Weight scenarios written to: ", output_path)

pressure_summary <- weight_scenarios %>%
  filter(index_type == "pressure") %>%
  select(scenario, component, weight) %>%
  tidyr::pivot_wider(names_from = component, values_from = weight) %>%
  arrange(
    factor(
      scenario,
      levels = c(
        "roc", "custom",
        "roc_prev_best",
        "roc_down_1", "roc_down_2", "roc_down_3",
        "roc_bridge_1", "roc_bridge_2", "roc_bridge_3",
        "roc_up_1", "roc_up_2", "roc_up_3",
        "custom_nearby_a", "custom_nearby_b", "custom_nearby_c",
        "custom_nearby_d", "custom_nearby_e"
      )
    )
  )

message("")
message("Pressure weight scenarios:")
print(pressure_summary, n = nrow(pressure_summary))
