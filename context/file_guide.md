# File Guide

## 1. 本文件的角色

這份文件現在是本專案最主要的「檔案地圖」與「資料流向說明」。

它的目的有四個：

1. 告訴你每一支 `R` 腳本會讀什麼、產出什麼。
2. 告訴你每一個 `csv` 是拿來解釋什麼內容。
3. 告訴你正式主線、探索支線與儀表板輸出之間的關係。
4. 告訴你若要重跑流程，腳本應該依什麼順序執行。

如果你只想先理解整個專案在做什麼，請先讀根目錄的 [`README.md`](../README.md)。
如果你想知道研究邏輯與公式，請看 [`bigdata.md`](bigdata.md) 與 [`app_formula_design.md`](app_formula_design.md)。
如果你想知道「哪支腳本產哪張表、下一步接去哪裡」，就直接看這份文件。

## 2. 原始資料命名對照

| 原始資料名稱 | 專案內使用名稱 | 主要用途 |
|---|---|---|
| iMarine 航港發展資料庫－臺灣進口貨空櫃 | `enter-empty.csv` | 建立 `empty_in` |
| iMarine 航港發展資料庫－臺灣出口貨空櫃 | `exit-empty.csv` | 建立 `empty_out` |
| iMarine 航港發展資料庫－臺灣進口貨實櫃 | `enter-full.csv` | 建立 `full_in` |
| iMarine 航港發展資料庫－臺灣出口貨實櫃 | `exit-full.csv` | 建立 `full_out` |
| 臺灣地區國際商港貨櫃裝卸量 | `throughput.csv` | 建立 `throughput` |
| 港口距離矩陣 | `Distance_Matrix - 距離.csv` | 建立 `distance_class` 與 `distance_factor` |
| 國際商港貨櫃裝卸量(個數) | `port_container_count_by_empty_full.csv` | 建立官方多年空櫃、實櫃、總櫃個數與空櫃占比基準 |
| 國際商港貨櫃裝卸量(TEU)─按進出港分 | `port_container_teu_by_direction.csv` | 建立官方多年進港、出港、總 TEU 背景欄位 |

## 3. 先看懂整體資料流

```text
原始流量資料 + throughput
-> master_table_2025.csv
-> analysis_table_2025.csv
-> analysis_table_history_2025.csv

-> weight_scenarios.csv
-> pressure_scenarios_2025.csv
-> pressure_backtest_summary.csv / pressure_backtest_port_month.csv

-> buffer_fixed_scenarios_2025.csv
-> buffer_backtest_summary.csv / buffer_backtest_port_month.csv
-> status_final_2025.csv

-> matching_final_2025.csv
-> simulation_final_2025.csv
-> simulation_recommendation_final_2025.csv

-> dashboard scripts
```

另外新增一條「Forecast 預警實驗支線」：

```text
pressure_scenarios_2025.csv
-> backtest_forecast_high_pressure.R
-> forecast_high_pressure_backtest_summary.csv
-> forecast_high_pressure_backtest_detail.csv
-> forecast_high_pressure_latest_watchlist.csv
```

目前正式 dashboard 使用的是另一條「官方多年統計基準線」：

```text
context/input/port_container_count_by_empty_full.csv
context/input/port_container_teu_by_direction.csv
-> build_port_stat_history.R
-> port_stat_empty_full_monthly.csv
-> port_stat_direction_monthly.csv
-> port_stat_prophet_base.csv
-> build_port_stat_prophet_forecast.R
-> port_stat_prophet_forecast.csv
-> port_stat_prophet_backtest.csv
-> port_stat_prophet_latest_watchlist.csv
-> R/dashboard_port_baseline.R
-> 官方基準 dashboard page
```

這條線的定位是「多年官方基準與季節性合理性驗證」，不是 iMarine 的正式媒合模型。分兩層輸出：Bridge 對照以 Prophet `yhat` 判斷 iMarine 高壓港是否高於往年同期預期；優先檢查清單以同港多年歷史 Q75 為門檻供提前關注。兩者口徑不同，不互相取代。

另外有一條「探索性整體情境線」：

```text
analysis_table_history_2025.csv
-> weight_scenarios.csv
-> analysis_table_scenarios_2025.csv
-> weight_backtest_summary.csv / weight_backtest_port_month.csv
```

以及一條「Matching 回測支線」：

```text
buffer_backtest_port_month.csv
-> matching_weight_scenarios.csv
-> matching_candidate_pairs_fixed_2025.csv
-> matching_parent_backtest_summary.csv / matching_parent_backtest_pair_month.csv
-> matching_backtest_summary.csv / matching_backtest_pair_month.csv
```

## 4. 正式主線與探索支線怎麼分

### 正式主線

正式主線是目前最適合拿來報告與對外說明的流程：

1. 建立主表與分析樣本。
2. 建立歷史特徵。
3. 用 `Pressure-only` 回測定案 `Pressure`。
4. 固定 `Pressure` 後回測 `Buffer`。
5. 產出正式 `status`。
6. 產出正式 `matching`。
7. 做調度比例模擬與建議輸出。

### 探索支線

探索支線主要用來補充說明，不是目前最優先的正式敘事：

- `build_index_scenarios.R` / `backtest_weight_scenarios.R`
  - 同時把 `Pressure` 與 `Buffer` 放在整體 scenario 裡比較。
- `build_matching_weight_scenarios.R` / `backtest_matching_fixed_indices.R`
  - 在固定 `Pressure` / `Buffer` 條件下，回測 `Matching` 權重家族。

### Forecast 預警實驗支線

這條支線的角色是：

- 補一個「提早一個月」的港口風險 watchlist
- 不覆蓋正式 `Pressure / Buffer / Matching / Simulation` 主線
- 正式口徑與暖機口徑分開評估，不把暖機樣本混成主結論

目前 forecast 實驗固定：

- 使用正式 `Pressure` 情境 `roc_prev_best`
- 只看正式四港 `基隆港、臺北港、臺中港、高雄港`
- 預測目標為「下一月是否成為高壓力港」

## 5. 各腳本與輸出對照

### 5.1 原始資料整併層

#### `build_master_table.R`

- 讀取：
  - `enter-empty.csv`
  - `enter-full.csv`
  - `exit-empty.csv`
  - `exit-full.csv`
  - `throughput.csv`
- 產出：
  - `master_table_2025.csv`
- 作用：
  - 把原始公開資料整合成 `ym × port` 主表。
  - 先算出 `empty_in`、`empty_out`、`full_in`、`full_out`、`throughput`。
  - 再補上 `empty_net`、`empty_total_flow`、`full_total_flow`、`empty_share`、`net_pressure_raw`、`export_pull`、`roll3_empty_net`、`cum_empty_net`。
- 這張表在說明什麼：
  - 它是整個專案的資料基底，後面所有分析都從這裡往下長。
- 下游關係：
  - `check_master_table.R`
  - `build_analysis_table.R`

#### `check_master_table.R`

- 讀取：
  - `master_table_2025.csv`
- 產出：
  - `check_summary.csv`
  - `check_zero_throughput.csv`
  - `check_port_summary.csv`
- 作用：
  - 驗證主表欄位是否完整。
  - 檢查 `ym + port` 是否唯一。
  - 找出 `throughput` 為 0 或缺值的列。
  - 做各港口月份與全年量級摘要。
- 這張表在說明什麼：
  - 它不是分析結果，而是資料品質檢查結果。
- 下游關係：
  - 幫助決定哪些港口適合進入正式分析宇宙。

#### `build_analysis_table.R`

- 讀取：
  - `master_table_2025.csv`
- 產出：
  - `analysis_table_2025.csv`
- 作用：
  - 從主表中篩出目前要納入分析的樣本。
  - 目前保留 `基隆港、臺中港、高雄港、臺北港、安平港`，且要求 `throughput > 0`。
- 這張表在說明什麼：
  - 它是正式分析的入口表，不再是「有資料就全收」。
- 下游關係：
  - `build_history_features.R`
  - 儀表板原始展示資料之一

### 5.2 歷史特徵層

#### `build_history_features.R`

- 讀取：
  - `analysis_table_2025.csv`
- 產出：
  - `analysis_table_history_2025.csv`
- 作用：
  - 補齊歷史序列欄位，例如：
    - `roll3_empty_net`
    - `roll3_pressure_raw`
    - `positive_net_streak`
    - `buffer_raw`
    - `pressure_pct`
    - `buffer_pct`
    - `pressure_z`
    - `buffer_z`
- 這張表在說明什麼：
  - 它是所有回測流程的共同基底表，也是正式主線最重要的中介資料。
- 下游關係：
  - `build_weight_scenarios.R`
  - `build_index_scenarios.R`
  - `build_pressure_scenarios.R`
  - `build_buffer_scenarios_fixed_pressure.R`
  - `build_simulation_final_2025.R`

### 5.3 權重情境層

#### `build_weight_scenarios.R`

- 讀取：
  - 無前置輸入表，直接在腳本內定義情境。
- 產出：
  - `weight_scenarios.csv`
- 作用：
  - 建立 `Pressure` 與 `Buffer` 的權重情境表。
  - 內容包含：
    - `Pressure`：`roc`、`custom`、`roc_prev_best`、`roc_down_*`、`roc_bridge_*`、`roc_up_*`、`custom_nearby_*`
    - `Buffer`：`roc`、`custom`、`custom_nearby_*`
- 這張表在說明什麼：
  - 它回答「我們到底測了哪些權重組合」。
- 下游關係：
  - `build_index_scenarios.R`
  - `build_pressure_scenarios.R`
  - `build_buffer_scenarios_fixed_pressure.R`

### 5.4 探索性整體 scenario 線

#### `build_index_scenarios.R`

- 讀取：
  - `analysis_table_history_2025.csv`
  - `weight_scenarios.csv`
- 產出：
  - `analysis_table_scenarios_2025.csv`
- 作用：
  - 同時把 `Pressure` 與 `Buffer` 權重套到歷史資料，對每個 `scenario` 產生：
    - `pressure_index`
    - `buffer_index`
    - `status`
- 這張表在說明什麼：
  - 它是一張整體情境總表，適合做探索性比較。
- 下游關係：
  - `backtest_weight_scenarios.R`

#### `backtest_weight_scenarios.R`

- 讀取：
  - `analysis_table_scenarios_2025.csv`
- 產出：
  - `weight_backtest_summary.csv`
  - `weight_backtest_port_month.csv`
- 作用：
  - 用整體 scenario 做 `T+1` 回測，綜合比較：
    - `Pressure` 前瞻能力
    - `Buffer` 穩定性
    - 分類合理性
- 這張表在說明什麼：
  - 它適合當整體參考，但不建議直接拿來作為 3.2 `Pressure` 定案依據。

### 5.5 正式 `Pressure` 定案線

#### `build_pressure_scenarios.R`

- 讀取：
  - `analysis_table_history_2025.csv`
  - `weight_scenarios.csv`
- 產出：
  - `pressure_scenarios_2025.csv`
- 作用：
  - 只處理 `Pressure` 權重，計算每個港口、每個月份、每個情境的：
    - `pressure_flow_pct`
    - `pressure_net_pct`
    - `pressure_roll_pct`
    - `pressure_index`
    - `pressure_status`
- 這張表在說明什麼：
  - 它是 `Pressure-only` 的中間結果表。
- 下游關係：
  - `backtest_pressure_only.R`

#### `backtest_pressure_only.R`

- 讀取：
  - `pressure_scenarios_2025.csv`
- 產出：
  - `pressure_backtest_summary.csv`
  - `pressure_backtest_port_month.csv`
- 作用：
  - 以 `T+1` 比較不同 `Pressure` 權重對下期 `empty_net` 與 `roll3_empty_net` 的辨識力。
- 這張表在說明什麼：
  - `pressure_backtest_summary.csv`：哪個 `Pressure` 方案比較好。
  - `pressure_backtest_port_month.csv`：各港口、各月份的細節驗證。
- 目前結論：
  - `roc > custom`
  - ROC 鄰近家族中 `roc_prev_best` 排名第 1。

#### `backtest_forecast_high_pressure.R`

- 讀取：
  - `pressure_scenarios_2025.csv`
- 產出：
  - `forecast_high_pressure_backtest_summary.csv`
  - `forecast_high_pressure_backtest_detail.csv`
  - `forecast_high_pressure_latest_watchlist.csv`
- 作用：
  - 固定正式 `Pressure` 情境 `roc_prev_best` 與正式四港。
  - 把 forecast 實驗拆成兩個窗口：
    - `official`：來源月 `history_n >= 3`
    - `warmup_included`：允許 `2025-02 -> 2025-03` 納入，但不把它混進正式主結論
  - 比較兩種下月高壓力預警分數：
    - `baseline_pressure_index`
    - `pressure_index_streak_boost`
- 這張表在說明什麼：
  - `forecast_high_pressure_backtest_summary.csv`：兩個窗口下，哪個預警分數較適合拿來做下月 watchlist。
  - `forecast_high_pressure_backtest_detail.csv`：逐月逐港的回測明細與月內排序。
  - `forecast_high_pressure_latest_watchlist.csv`：最新月份往下一月的實務觀察清單。
- 使用定位：
  - 它是預警支線，不覆蓋正式 `Pressure / Buffer / Matching / Simulation` 主線。

### 5.6 固定 `Pressure` 後的 `Buffer` 定案線

#### `build_buffer_scenarios_fixed_pressure.R`

- 讀取：
  - `analysis_table_history_2025.csv`
  - `weight_scenarios.csv`
- 產出：
  - `buffer_fixed_scenarios_2025.csv`
- 作用：
  - 固定 `pressure_scenario = roc_prev_best`。
  - 只比較不同 `Buffer` 權重對：
    - `buffer_index`
    - `status`
    - 高緩衝港分類
    的影響。
- 這張表在說明什麼：
  - 它是 `Pressure` 已定案後，用來比較 `Buffer` 的正式中間表。
- 下游關係：
  - `backtest_buffer_fixed_pressure.R`
  - `build_status_final_2025.R`

#### `backtest_buffer_fixed_pressure.R`

- 讀取：
  - `buffer_fixed_scenarios_2025.csv`
- 產出：
  - `buffer_backtest_summary.csv`
  - `buffer_backtest_port_month.csv`
- 作用：
  - 比較哪組 `Buffer` 權重更能讓高緩衝港在下一期維持安全。
- 這張表在說明什麼：
  - `buffer_backtest_summary.csv`：哪個 `Buffer` 方案較佳。
  - `buffer_backtest_port_month.csv`：各港口、各月份的細節驗證。
- 目前結論：
  - `custom_nearby_c` 排名第 1，`custom_nearby_b` 次之。

#### `build_status_final_2025.R`

- 讀取：
  - `buffer_fixed_scenarios_2025.csv`
- 產出：
  - `status_final_2025.csv`
- 作用：
  - 固定目前正式輸出層設定：
    - `pressure_scenario = roc_prev_best`
    - `buffer_scenario = custom_nearby_c`
  - 並排除 `安平港`，形成正式 `status` 輸出表。
- 這張表在說明什麼：
  - 這是最終的港口狀態表，回答每月哪些港口是：
    - `高壓力港`
    - `高緩衝港`
    - `正常港`
- 下游關係：
  - `build_matching_final_2025.R`
  - `build_simulation_final_2025.R`
  - 所有儀表板腳本

### 5.7 `Matching` 回測支線

#### `build_matching_weight_scenarios.R`

- 讀取：
  - 無前置輸入表，直接在腳本內定義情境。
- 產出：
  - `matching_weight_scenarios.csv`
- 作用：
  - 建立 `Matching` 權重家族：
    - `roc`
    - `custom`
    - `roc_nearby_*`
    - `custom_nearby_*`
- 這張表在說明什麼：
  - 它回答 `Matching` 排序到底測了哪些權重。
- 下游關係：
  - `backtest_matching_fixed_indices.R`

#### `backtest_matching_fixed_indices.R`

- 讀取：
  - `buffer_backtest_port_month.csv`
  - `matching_weight_scenarios.csv`
  - `Distance_Matrix - 距離.csv`
- 產出：
  - `matching_candidate_pairs_fixed_2025.csv`
  - `matching_parent_backtest_summary.csv`
  - `matching_parent_backtest_pair_month.csv`
  - `matching_backtest_summary.csv`
  - `matching_backtest_pair_month.csv`
- 作用：
  - 固定：
    - `pressure_scenario = roc_prev_best`
    - `buffer_scenario = custom_nearby_c`
  - 先比較 `roc vs custom` 父方案，再沿勝出家族做鄰近微調。
- 這張表在說明什麼：
  - `matching_candidate_pairs_fixed_2025.csv`：候選港對基底表。
  - `matching_parent_backtest_summary.csv`：`roc vs custom` 父方案比較。
  - `matching_backtest_summary.csv`：勝出家族內的鄰近情境比較。
- 目前結論：
  - 父方案由 `custom` 勝出。
  - 鄰近家族內 `custom_nearby_b` 的 `pooled_outcome_corr` 略高，但與 `custom` 的 top-1 決策表現差距不大。

### 5.8 正式 `Matching` 輸出線

#### `build_matching_final_2025.R`

- 讀取：
  - `status_final_2025.csv`
  - `Distance_Matrix - 距離.csv`
- 產出：
  - `port_pair_distance_lookup.csv`
  - `matching_final_2025.csv`
- 作用：
  - 以正式 `status` 為基礎建立來源港與承接港候選配對。
  - 目前正式輸出採：
    - `source_weight = 0.45`
    - `target_weight = 0.35`
    - `distance_weight = 0.20`
  - 對每個月份產出：
    - `matching_score`
    - `rank_global`
    - `rank_within_source`
- 這張表在說明什麼：
  - `matching_final_2025.csv` 是給儀表板與正式簡報使用的跨港媒合結果表。
- 下游關係：
  - `build_simulation_final_2025.R`
  - 所有儀表板腳本

### 5.9 模擬與建議輸出線

#### `build_simulation_final_2025.R`

- 讀取：
  - `status_final_2025.csv`
  - `matching_final_2025.csv`
  - `analysis_table_history_2025.csv`
- 產出：
  - `simulation_final_2025.csv`
- 作用：
  - 對每一組候選港對模擬：
    - `no_move`
    - `move_25pct`
    - `move_50pct`
    - `move_75pct`
  - 並重算來源港與目標港移轉後的：
    - `pressure_index_after`
    - `buffer_index_after`
    - `status_after`
    - `pair_success`
- 這張表在說明什麼：
  - 它回答「如果真的移一部分，來源港是否解壓、目標港是否仍安全」。
- 下游關係：
  - `build_simulation_recommendation_final_2025.R`
  - 儀表板

#### `build_simulation_recommendation_final_2025.R`

- 讀取：
  - `simulation_final_2025.csv`
- 產出：
  - `simulation_recommendation_final_2025.csv`
- 作用：
  - 從每組港對的多個模擬情境中，挑出最適合展示與溝通的建議版本。
  - 目前輸出三種結果類型：
    - `完全解壓`
    - `可緩解但未解壓`
    - `不建議調度`
- 這張表在說明什麼：
  - 它是最接近「管理建議句子」的結果表，可以直接拿去做儀表板與簡報文案。

### 5.10 儀表板層

#### `iMarine_dashboard_fixed_R3_compatible.R`
#### `iMarine_dashboard_local_fixed_v3.R`
#### `iMarine_dashboard_stable_local.R`

- 讀取：
  - `status_final_2025.csv`
  - `matching_final_2025.csv`
  - `simulation_recommendation_final_2025.csv`
  - `analysis_table_2025.csv`
  - `analysis_table_history_2025.csv`
  - `master_table_2025.csv`
  - `port_pair_distance_lookup.csv`
- 產出：
  - 不另寫 `csv`，直接提供本地儀表板介面。
- 作用：
  - 把前述正式輸出表轉成互動式決策展示工具。

## 6. 各 `csv` 最簡單的一句話理解

- `master_table_2025.csv`：原始公開資料整合後的港口月別主表。
- `check_summary.csv`：主表資料品質檢查摘要。
- `check_zero_throughput.csv`：哪些港口月份的 `throughput` 為 0 或缺值。
- `check_port_summary.csv`：各港全年總量與月份摘要。
- `analysis_table_2025.csv`：正式分析樣本表。
- `analysis_table_history_2025.csv`：回測與指標建構的歷史特徵基底表。
- `weight_scenarios.csv`：`Pressure` / `Buffer` 權重情境表。
- `analysis_table_scenarios_2025.csv`：探索性整體 scenario 總表。
- `weight_backtest_summary.csv`：探索性整體 scenario 的總結排名。
- `weight_backtest_port_month.csv`：探索性整體 scenario 的港口月份細節。
- `pressure_scenarios_2025.csv`：`Pressure-only` 中間表。
- `pressure_backtest_summary.csv`：正式 `Pressure` 回測排名總表。
- `pressure_backtest_port_month.csv`：正式 `Pressure` 回測細節表。
- `forecast_high_pressure_backtest_summary.csv`：Forecast 預警回測總表。
- `forecast_high_pressure_backtest_detail.csv`：Forecast 預警逐月逐港明細。
- `forecast_high_pressure_latest_watchlist.csv`：最新月份的下月風險 watchlist。
- `buffer_fixed_scenarios_2025.csv`：固定 `Pressure` 後的 `Buffer` 中間表。
- `buffer_backtest_summary.csv`：`Buffer` 小實驗排名總表。
- `buffer_backtest_port_month.csv`：`Buffer` 小實驗細節表。
- `status_final_2025.csv`：正式狀態表。
- `matching_weight_scenarios.csv`：`Matching` 權重情境表。
- `matching_candidate_pairs_fixed_2025.csv`：候選港對基底表。
- `matching_parent_backtest_summary.csv`：`Matching` 父方案比較總表。
- `matching_parent_backtest_pair_month.csv`：`Matching` 父方案細節表。
- `matching_backtest_summary.csv`：`Matching` 勝出家族的鄰近情境比較總表。
- `matching_backtest_pair_month.csv`：`Matching` 勝出家族細節表。
- `port_pair_distance_lookup.csv`：港對距離分類與距離因子對照表。
- `matching_final_2025.csv`：正式跨港媒合排序表。
- `simulation_final_2025.csv`：正式模擬情境表。
- `simulation_recommendation_final_2025.csv`：最終建議表。

## 7. 若要重跑完整正式主線，順序如下

### 7.1 從原始資料開始重跑

```r
source("/Users/lee/Documents/BigData/context/build_master_table.R")
source("/Users/lee/Documents/BigData/context/check_master_table.R")
source("/Users/lee/Documents/BigData/context/build_analysis_table.R")
source("/Users/lee/Documents/BigData/context/build_history_features.R")
source("/Users/lee/Documents/BigData/context/build_weight_scenarios.R")
source("/Users/lee/Documents/BigData/context/build_pressure_scenarios.R")
source("/Users/lee/Documents/BigData/context/backtest_pressure_only.R")
source("/Users/lee/Documents/BigData/context/build_buffer_scenarios_fixed_pressure.R")
source("/Users/lee/Documents/BigData/context/backtest_buffer_fixed_pressure.R")
source("/Users/lee/Documents/BigData/context/build_status_final_2025.R")
source("/Users/lee/Documents/BigData/context/build_matching_final_2025.R")
source("/Users/lee/Documents/BigData/context/build_simulation_final_2025.R")
source("/Users/lee/Documents/BigData/context/build_simulation_recommendation_final_2025.R")
```

### 7.2 只想重跑正式回測與輸出

前提是：

- `analysis_table_history_2025.csv` 已存在
- `weight_scenarios.csv` 已存在

則可從這裡開始：

```r
source("/Users/lee/Documents/BigData/context/build_pressure_scenarios.R")
source("/Users/lee/Documents/BigData/context/backtest_pressure_only.R")
source("/Users/lee/Documents/BigData/context/build_buffer_scenarios_fixed_pressure.R")
source("/Users/lee/Documents/BigData/context/backtest_buffer_fixed_pressure.R")
source("/Users/lee/Documents/BigData/context/build_status_final_2025.R")
source("/Users/lee/Documents/BigData/context/build_matching_final_2025.R")
source("/Users/lee/Documents/BigData/context/build_simulation_final_2025.R")
source("/Users/lee/Documents/BigData/context/build_simulation_recommendation_final_2025.R")
```

### 7.3 只想重跑探索性比較

```r
source("/Users/lee/Documents/BigData/context/build_weight_scenarios.R")
source("/Users/lee/Documents/BigData/context/build_index_scenarios.R")
source("/Users/lee/Documents/BigData/context/backtest_weight_scenarios.R")
source("/Users/lee/Documents/BigData/context/build_matching_weight_scenarios.R")
source("/Users/lee/Documents/BigData/context/backtest_matching_fixed_indices.R")
```

### 7.4 只想重跑 Forecast 預警實驗

前提是：

- `pressure_scenarios_2025.csv` 已存在

則可直接跑：

```r
source("/Users/lee/Documents/BigData/context/backtest_forecast_high_pressure.R")
```

## 8. 目前最重要的結論怎麼讀

### `Pressure`

- 先比較 `roc` 與 `custom`
- 結果 `roc` 較佳
- 再對 ROC 鄰近方案做敏感度分析
- 目前 `roc_prev_best` 排名第 1

### `Buffer`

- 固定 `Pressure = roc_prev_best`
- 比較不同 `Buffer` 權重
- 目前 `custom_nearby_c` 排名第 1，`custom_nearby_b` 次之

### `Matching`

- 固定 `Pressure = roc_prev_best`
- 固定 `Buffer = custom_nearby_c`
- 父方案比較由 `custom` 勝出
- 鄰近家族內 `custom_nearby_b` 的相關性略高，但正式輸出目前仍採較保守的 `custom`

## 9. 操作注意事項

- 目前多支腳本使用硬編碼絕對路徑，特別是原始 `csv` 與距離矩陣來源位於 `/Users/lee/Downloads`。
- 若原始資料檔案位置改變，需先調整腳本中的 `input_path`。
- 儀表板腳本依賴 `context/` 目錄下的最終輸出表，因此若最終 `csv` 未更新，儀表板看到的也會是舊結果。

## 10. 建議閱讀順序

1. [`../README.md`](../README.md)
2. [`bigdata.md`](bigdata.md)
3. [`app_formula_design.md`](app_formula_design.md)
4. [`file_guide.md`](file_guide.md)
5. [`weight_backtest_plan.md`](weight_backtest_plan.md)
6. [`權重資料.md`](權重資料.md)
7. [`develop log.md`](develop%20log.md)
