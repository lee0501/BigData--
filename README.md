# iMarine空櫃失衡決策平台

本專案為「交通部航港局第 6 屆航港大數據創意應用競賽」所提出的跨港空櫃決策支援平台。平台以 iMarine 航港發展資料庫與公開航港統計為基礎，建立一套公開資料驅動的跨港空櫃調度決策支援模式，協助辨識「哪些港口空櫃壓力偏高」、「哪些港口仍具承接緩衝」，並產出具優先順序的跨港媒合與情境模擬結果。

本平台的定位不是取代航商或港區的實際調度系統，而是補足調度決策前的資訊透明缺口：在業者決定是否合作調度之前，先提供共同的港口壓力狀態、承接穩定性與跨港優先順序視角。長期而言，平台可作為藍色公路與綠色港口政策的量化決策輔助工具，協助降低陸運壓力、協作阻礙與潛在碳排放。

## 專案定位與問題意識

台灣港口的空櫃問題不只是「總量夠不夠」，更常見的是「不同港口之間分布不均」。某些港口可能因空櫃持續流入而形成堆場與作業壓力，另一些港口則仍具有承接緩衝。若缺乏共同的資料判讀基準，跨港調度容易回到經驗判斷，也不容易形成跨業者或跨港口的協作共識。

因此，本平台的核心不是建立單純的視覺化看板，而是把空櫃流動、港口壓力、承接緩衝與跨港距離轉換成一套可解釋、可回測、可持續更新的決策支援流程。它的角色是協助使用者先釐清「哪裡需要釋放」、「哪裡可以承接」、「哪一組港口值得優先討論」，再透過情境模擬檢查調度後是否可能產生新的壓力。

專案目前聚焦於四個主要國際商港：

- 基隆港
- 臺北港
- 臺中港
- 高雄港

## 專案目標

本專案回答三個核心問題：

1. 哪些港口已出現空櫃累積壓力，應優先釋放空櫃。
2. 哪些港口目前仍具承接能力，可作為候選承接端。
3. 在公開資料與距離規則下，哪些跨港組合最值得優先列入調度討論。

整體方法採用 push-based 調度視角：先判斷「哪裡已累積到需要釋放」，再尋找「哪裡具備承接緩衝」，最後透過媒合分數與移轉比例模擬，將港口狀態轉換為可討論的調度方案。

## 功能與成果

本專案目前已完成從資料整併、指標建構、回測校準、狀態分類、跨港媒合、情境模擬到 dashboard 展示的主要流程。成果可分為四類：

| 類別 | 成果 |
|---|---|
| 資料成果 | 建立 `港口 × 年月` 分析主表，並產出狀態、媒合、模擬與預警相關 CSV。 |
| 模型成果 | 建立 `Pressure Index`、`Buffer Index`、`Matching Score` 與調度比例模擬流程。 |
| 驗證成果 | 以 ROC 權重作為起點，透過 T+1 expanding window 回測比較權重情境，降低主觀設定與未來資料洩漏風險。 |
| 產品成果 | 建立 Shiny dashboard，整合港口壓力狀態、跨港媒合排序、模擬建議、Forecast 預警與 Prophet 官方基準對照。 |

目前已完成的功能包含：

- 整合公開資料為 `港口 × 年月` 分析主表。
- 建立 `Pressure Index` 辨識空櫃壓力較高的港口。
- 建立 `Buffer Index` 辨識仍具承接緩衝的港口。
- 依照月內相對門檻將港口分為高壓力港、高緩衝港與正常港。
- 建立高壓力港與高緩衝港之間的跨港候選組合。
- 透過 `Matching Score` 產生跨港媒合優先順序。
- 以不同移轉比例模擬來源港是否解壓、目標港是否仍能維持安全。
- 以官方多年統計與時間序列基準補充港口季節性判讀。
- 提供 Shiny dashboard 供互動式檢視，整合壓力狀態、媒合排序、情境模擬與基準對照。

主要輸出檔案：

| 檔案 | 說明 |
|---|---|
| `context/status_final_2025.csv` | 每月各港口的壓力、緩衝與狀態分類結果 |
| `context/matching_final_2025.csv` | 高壓力港與高緩衝港的跨港候選組合與排序 |
| `context/simulation_final_2025.csv` | 不同移轉比例下的模擬結果 |
| `context/simulation_recommendation_final_2025.csv` | 可供 dashboard 與決策討論使用的模擬建議 |
| `context/forecast_high_pressure_latest_watchlist.csv` | 下一期高壓力港預警實驗清單 |
| `context/port_stat_prophet_latest_watchlist.csv` | 官方多年統計基準下的港口觀察清單 |

## 方法概要

本平台採用「規則式指標模型 + 回測校準」的分析模式。設計重點不是建立黑盒預測模型，而是讓公開資料可以被轉換為可解釋、可重現、可回測的決策流程。

### 1. 資料整合

專案先將公開資料整合為 `ym × port` 結構，主要欄位包含：

- `empty_in`：空櫃進港量
- `empty_out`：空櫃出港量
- `full_in`：實櫃進口量
- `full_out`：實櫃出口量
- `throughput`：港口總貨櫃裝卸量

並進一步衍生：

- `empty_net = empty_in - empty_out`
- `empty_total_flow = empty_in + empty_out`
- `roll3_empty_net`：近三月平均空櫃淨流量
- `cum_empty_net`：累積空櫃淨流量
- `export_pull = full_out / throughput`

### 2. 權重校準與回測

後續 `Pressure Index`、`Buffer Index` 與 `Matching Score` 都會用到權重。本專案的權重設計不是直接以主觀偏好決定，而是先以 ROC（Ranked Order Centroid）方法作為客觀起點，再透過回測比較多組權重情境，決定正式輸出方案。

ROC 的用途是把「哪個分量較重要」的排序轉換為一組基礎權重；回測的用途則是檢查這組權重在下一期狀態辨識上是否穩定。正式權重因此不是單純理論值，也不是任意調整，而是同時考量管理意義、可解釋性與 T+1 回測結果。

回測採 T+1 expanding window 架構：每個評估時間點只使用當期以前已知資料計算歷史分位數、滾動特徵與指標分數，再檢查模型對下一期狀態的辨識能力。這樣可以避免未來資料洩漏，並讓後續三層權重選擇具備可驗證依據：

- `Pressure`：比較不同壓力分量權重，選出對下一期高壓狀態較具辨識力的方案。
- `Buffer`：在固定正式 `Pressure` 後，比較承接能力權重。
- `Matching`：在已定案的 `Pressure` 與 `Buffer` 基礎上，比較跨港排序權重。

README 僅保留正式採用的指標邏輯；完整權重情境、ROC 推導與回測結果請參考 [`context/weight_backtest_plan.md`](context/weight_backtest_plan.md) 與 [`context/app_formula_design.md`](context/app_formula_design.md)。

### 3. Pressure Index

`Pressure Index` 用來衡量港口是否出現空櫃累積壓力。它不是只看絕對櫃量，而是將港口當期狀態放回該港自身歷史序列中比較，判斷是否相對偏高。此設計可降低不同港口規模差異造成的誤判。

正式輸出目前採用下列設計：

```text
pressure_index =
0.22 * pct(pressure_flow_raw) +
0.60 * pct(pressure_net_raw) +
0.18 * pct(pressure_roll_raw)
```

其中：

- `pressure_flow_raw`：當月空櫃進港占整體作業規模比重
- `pressure_net_raw`：當月空櫃是否淨增加
- `pressure_roll_raw`：近三個月是否持續淨增加
- `pct(x)`：該港當期值在自身歷史序列中的相對分位

### 4. Buffer Index

`Buffer Index` 用來衡量港口是否具備承接空櫃的緩衝能力。它不是 `1 - pressure`，而是同時考慮目前空櫃壓力、近期淨流出情況與出口活動。

正式輸出目前採用下列設計：

```text
buffer_index =
0.45 * pct(buffer_space_raw) +
0.40 * pct(buffer_net_raw) +
0.15 * pct(buffer_export_raw)
```

其中：

- `buffer_space_raw`：目前空櫃進港壓力相對低
- `buffer_net_raw`：近三月偏向淨流出，不易堆積
- `buffer_export_raw`：出口活動較強，可能較能吸收空櫃

### 5. 港口狀態分類

每個月份會在當期港口橫截面上重新計算相對門檻，並要求歷史期數達到基本判讀條件：

```text
高壓力港：pressure_index >= Q75 且 roll3_empty_net > 0 且 history_n >= 3
高緩衝港：buffer_index >= Q75 且 pressure_index <= Q50 且 history_n >= 3
正常港：其餘
```

這樣的設計可以讓判斷維持在「同一月份、不同港口之間」的相對比較，而不是使用全年固定門檻。

### 6. 跨港媒合排序

候選配對只會在下列條件成立時建立：

- 來源港為高壓力港
- 目標港為高緩衝港
- 來源港與目標港不同

媒合排序使用：

```text
matching_score =
0.45 * source_pressure_index +
0.35 * target_buffer_index +
0.20 * distance_factor
```

`Matching Score` 的用途是產生「哪些港對值得優先討論」的排序。它並不直接等於最終調度命令，而是把來源港壓力、目標港緩衝與距離阻力整合成決策前的候選清單。

### 7. 調度比例情境模擬

跨港排序只能回答「哪一組值得先看」，還不能回答「移轉之後會不會安全」。因此，本專案進一步模擬不同移轉比例下，來源港是否能解壓、目標港是否仍能維持安全，避免只是把壓力從一個港口轉移到另一個港口。

目前採用固定比例進行情境模擬：

```text
move_share in {0, 0.25, 0.50, 0.75}
move_amount = move_share * max(source_empty_net, 0)
```

模擬後會重新估計來源港與目標港的壓力、緩衝與狀態，並判斷：

- 來源港是否由高壓力狀態改善。
- 目標港接收空櫃後是否仍未轉為高壓力港。
- 哪個移轉比例可作為較合理的討論起點。
- 是否存在「來源港改善，但目標港變成高壓」的副作用。

### 8. Prophet 基準與 Forecast 預警

主線模型、Forecast 預警與 Prophet 基準各自回答不同問題，三者互補但不混用判定口徑：

| 模組 | 主要問題 | 用途 |
|---|---|---|
| `Pressure / Buffer / Matching / Simulation` 主線 | 當月哪些港口高壓、哪些港口可承接、哪些港對值得優先討論。 | 支援當期跨港調度決策。 |
| Forecast 預警實驗 | 下一期哪些港口較可能成為高壓力港。 | 提供前瞻 watchlist，讓使用者提前關注風險港口。 |
| Prophet 官方多年基準 | 今年某港是否偏離往年同月的季節性合理水準。 | 補充跨年縱向判讀，避免只看當月四港橫截面比較。 |

其中 Forecast 是主線壓力模型的前瞻延伸；Prophet 則使用官方多年統計建立季節性基準。Forecast 關注「下一期風險」，Prophet 關注「是否偏離多年季節性預期」，兩者都可放進 dashboard 輔助判讀，但不取代正式的 `Pressure / Buffer / Matching / Simulation` 主線。

## 資料來源

目前使用的主要資料包含：

| 資料 | 專案用途 |
|---|---|
| iMarine 航港發展資料庫－臺灣進口貨空櫃 | 建立 `empty_in` |
| iMarine 航港發展資料庫－臺灣出口貨空櫃 | 建立 `empty_out` |
| iMarine 航港發展資料庫－臺灣進口貨實櫃 | 建立 `full_in` |
| iMarine 航港發展資料庫－臺灣出口貨實櫃 | 建立 `full_out` |
| 臺灣地區國際商港貨櫃裝卸量 | 建立 `throughput` |
| 港口距離矩陣 | 建立跨港距離修正因子 |
| 國際商港貨櫃裝卸量(個數) | 建立官方多年空櫃、實櫃與總櫃基準 |
| 國際商港貨櫃裝卸量(TEU)─按進出港分 | 建立官方多年進出港 TEU 背景欄位 |

原始資料放置於 `context/input/`，經腳本處理後產出 `context/` 下的分析表、回測表與 dashboard 用資料。

## 專案結構

```text
.
├── app.R                         # Shiny dashboard 主要入口
├── iMarine_dashboard.R            # 可直接執行的 dashboard 入口
├── R/                             # Dashboard UI、server、模組與共用函式
├── www/                           # Dashboard 靜態資源
├── context/
│   ├── input/                     # 原始輸入資料
│   ├── *.R                        # 資料整併、回測、媒合與模擬腳本
│   ├── *.csv                      # 分析輸出與 dashboard 資料
│   └── *.md                       # 方法、公式、資料流與 dashboard 說明
└── index.html                     # 靜態展示頁
```

更完整的腳本與資料流說明請參考 [`context/file_guide.md`](context/file_guide.md)。

## 執行方式

### 環境需求

- R
- Shiny dashboard 所需套件：
  - `shiny`
  - `ggplot2`
  - `dplyr`
  - `readr`
  - `tidyr`
- 若要重跑完整資料處理與回測流程，另需：
  - `stringr`
  - `slider`
  - `tibble`
  - `purrr`

可用以下指令安裝常用套件：

```r
install.packages(c(
  "shiny", "ggplot2", "dplyr", "readr", "tidyr",
  "stringr", "slider", "tibble", "purrr"
))
```

### 啟動 dashboard

在專案根目錄執行：

```r
shiny::runApp(".")
```

或直接執行：

```bash
Rscript iMarine_dashboard.R
```

### 重跑正式輸出

若要由原始資料重建正式輸出，請依序執行：

```bash
Rscript context/build_master_table.R
Rscript context/check_master_table.R
Rscript context/build_analysis_table.R
Rscript context/build_history_features.R
Rscript context/build_pressure_scenarios.R
Rscript context/backtest_pressure_only.R
Rscript context/build_buffer_scenarios_fixed_pressure.R
Rscript context/backtest_buffer_fixed_pressure.R
Rscript context/build_status_final_2025.R
Rscript context/build_matching_final_2025.R
Rscript context/build_simulation_final_2025.R
Rscript context/build_simulation_recommendation_final_2025.R
```

官方多年統計基準與預警實驗屬於延伸流程，詳細說明請參考 [`context/file_guide.md`](context/file_guide.md)。

## 分析範圍與限制

本專案以公開資料建立港口層級的決策支援模型，因此結果應解讀為 proxy-based decision support，而不是航商營運層級的最適化調度解。

目前限制包括：

- 未掌握即時空櫃庫存。
- 未掌握真實堆場剩餘容量。
- 未納入航商、箱型、航線、船期與實際調度成本。
- 跨港距離因子是簡化規則，尚未等同於完整運輸成本。
- 目前正式媒合分析聚焦於基隆港、臺北港、臺中港與高雄港。

因此，`pressure_index`、`buffer_index` 與 `matching_score` 適合作為港口壓力辨識、承接能力比較與調度優先順序討論的依據；不應直接解讀為真實庫存、真實剩餘容量或最終執行排程。

## 延伸文件

- [`context/bigdata.md`](context/bigdata.md)：研究背景、核心問題、資料來源與研究架構。
- [`context/app_formula_design.md`](context/app_formula_design.md)：指標、狀態分類、媒合與模擬公式。
- [`context/weight_backtest_plan.md`](context/weight_backtest_plan.md)：權重回測設計、採用結果與限制。
- [`context/file_guide.md`](context/file_guide.md)：腳本、CSV 產物、資料依賴與執行順序。
- [`context/dashboard_guide.md`](context/dashboard_guide.md)：Dashboard 頁面設計、名詞定義與資料來源分工。
