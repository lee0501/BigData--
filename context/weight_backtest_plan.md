# 權重情境回測：設計、結果與限制

本文件記錄 Pressure / Buffer / Matching 三層權重的回測設計原則、採用結果與使用限制。詳細執行過程與每輪情境比較的推導過程已刪減；若需查閱，請從 git 歷史記錄中找回原始版本。

---

## 1. 回測設計原則

**為什麼用 T+1 回測**：本研究無法取得真實調度執行結果，因此不能做傳統監督式預測評估。採用的是 **policy backtest**：在時間點 `t` 用當期資料做分類決策，再看 `t+1` 的實際狀況是否符合判斷方向。

**Expanding window**：每一個評估時點 `t`，只能使用 `1 ~ t` 月可得的資料，不得使用未來資訊。

**分層定案**：先固定 Pressure，再固定 Pressure + Buffer，最後才定 Matching。避免三層同時變動難以解釋。

---

## 2. Pressure 定案（3.2）

**回測方式**：Pressure-only，固定 `Buffer = roc` 作為控制組，比較各 Pressure 情境在 T+1 高壓港辨識的前瞻能力。

**情境範圍**：ROC 基準（`roc`）、ROC 鄰近家族（`roc_prev_best`、`roc_down_*`、`roc_bridge_*`、`roc_up_*`）、自治權重（`custom`）及其鄰近家族（`custom_nearby_*`）。

**結果**：

| 情境 | 結果 |
|------|------|
| `roc_prev_best` | **第 1，正式採用** |
| `roc_down_1` | 第 2 |
| `custom_nearby_*` | 鄰近 custom 家族整體劣於 roc 家族 |

**正式採用**：

```text
pressure_index =
0.22 * pct(pressure_flow_raw) +
0.60 * pct(pressure_net_raw) +
0.18 * pct(pressure_roll_raw)
```

**解讀**：`pressure_net_raw`（當月是否真的淨增加）的辨識力最強，`pressure_flow_raw` 與 `pressure_roll_raw` 作為輔助。

---

## 3. Buffer 定案（3.3）

**回測方式**：固定 `Pressure = roc_prev_best`，只比較 Buffer 情境的高緩衝港穩定性。

**結果**：

| 情境 | overall_score | 說明 |
|------|--------------|------|
| `custom_nearby_c` | 1.3148 | **第 1，暫定採用** |
| `custom_nearby_b` | 1.3112 | 第 2 |
| `custom` | 1.2526 | 第 3 |
| `roc` | 1.0976 | 最後，過度偏重 buffer_space 不足以支撐辨識需求 |

**正式採用**（暫定，Buffer 仍為公開資料 proxy）：

```text
buffer_index =
0.45 * pct(buffer_space_raw) +
0.35 * pct(buffer_net_raw) +
0.20 * pct(buffer_export_raw)
```

> 注意：`custom_nearby_c` 與 `custom` 差距有限，且 Buffer 缺乏真實容量驗證，因此不宜把此結論宣稱為 Buffer 最終定案。

---

## 4. Matching 定案（3.5）

**回測方式**：固定 `Pressure = roc_prev_best`、`Buffer = custom_nearby_c`，比較 Matching 父方案（`roc vs custom`）再沿優勝家族做鄰近實驗。

**第一輪：父方案比較**

| 情境 | source_need_rate | outcome_score | 結果 |
|------|-----------------|---------------|------|
| `custom` | 1.0000 | 3.3333 | **勝出** |
| `roc` | 0.8889 | 3.2222 | 較差 |

`custom` 較能把「下期仍維持高壓力」的來源港排到前面。

**第二輪：custom 家族鄰近比較**

| 情境 | pooled_outcome_corr |
|------|---------------------|
| `custom_nearby_b` | 0.2060 |
| `custom_nearby_d` | 0.1896 |
| `custom` | 0.1873 |

**正式採用**：

```text
matching_score =
0.45 * source_pressure_pct +
0.35 * target_buffer_pct +
0.20 * distance_factor
```

> `custom_nearby_b`（0.40 / 0.40 / 0.20）在 top-1 決策指標上與 `custom` 幾乎一致，差異主要在 `pooled_outcome_corr`。保守起見，正式版採 `custom`，`custom_nearby_b` 列為敏感度分析下的暫定優先候選。

---

## 5. 使用限制

1. **樣本數少**：目前資料只有 2025 年（9–10 個可用月份），T+1 回測期間短，結論需保守解讀。
2. **Buffer 為 proxy**：缺乏真實堆場空間驗證，不代表精準承接上限。
3. **無真實調度結果**：Matching 回測評估的是「來源港是否仍有壓力」，不是「調度後真的改善了」。
4. **官方統計基準獨立**：Prophet 那條線（官方基準頁）不屬於本文件的 Pressure / Buffer 權重回測範圍，用途與口徑不同。

---

## 6. 重跑指令（執行順序）

```r
# 主線：Pressure 定案
source("context/build_master_table.R")
source("context/check_master_table.R")
source("context/build_analysis_table.R")
source("context/build_history_features.R")
source("context/build_pressure_scenarios.R")
source("context/backtest_pressure_only.R")

# 主線：Buffer 定案
source("context/build_buffer_scenarios_fixed_pressure.R")
source("context/backtest_buffer_fixed_pressure.R")

# 主線：正式輸出
source("context/build_status_final_2025.R")
source("context/build_matching_final_2025.R")
source("context/build_simulation_final_2025.R")
source("context/build_simulation_recommendation_final_2025.R")

# 探索支線（非主線）
source("context/build_weight_scenarios.R")
source("context/build_index_scenarios.R")
source("context/backtest_weight_scenarios.R")
source("context/build_matching_weight_scenarios.R")
source("context/backtest_matching_fixed_indices.R")

# 官方統計基準（獨立支線）
source("context/build_port_stat_history.R")
source("context/build_port_stat_prophet_forecast.R")
```
