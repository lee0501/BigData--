# iMarine Dashboard README

## 1. 目的

本 dashboard 是以 R 前處理後的正式 csv 輸出為資料來源，使用 `Shiny + ggplot2` 建立本地端可查閱的決策支援介面。

目前目標不是重做一個「看起來像真實營運系統」的前端，而是把研究已經定案的三層輸出穩定呈現：

- `status_final_2025.csv`
- `matching_final_2025.csv`
- `simulation_recommendation_final_2025.csv`

因此，dashboard 的內容必須和 [app_formula_design.md](/Users/lee/Documents/BigData/context/app_formula_design.md) 保持一致，不能再回頭使用 prototype 中不符合正式資料定義的欄位或文案。

## 2. 技術選擇

### 建議主體

- `Shiny`：資料讀取、月份篩選、反應式切換、分頁結構
- `ggplot2`：趨勢圖、壓力/緩衝圖、模擬比較圖
- `HTML/CSS`：卡片、色塊、版型

### 為何不用資料庫也適合用 Shiny

目前資料流程是：

1. 用 R 做資料清理與指標計算
2. 輸出正式 csv
3. 由 dashboard 直接讀 csv 顯示

這種情況下，Shiny 比純 `html/css/js` 更適合，原因是：

- 不需要把 R 端邏輯重寫成 JS
- 不需要資料庫也能穩定讀取本地 csv
- 研究公式或規則更新後，只要更新 csv 或 R script，不必維護第二套前端計算邏輯

## 3. 正式分析宇宙

### 正式納入港口

- `基隆港`
- `臺北港`
- `臺中港`
- `高雄港`

### 不納入正式分析的港口

- `安平港`
- `花蓮港`
- `蘇澳港`

### 排除原因

- `安平港`：2025 年有 throughput，但 flow 全零，缺乏有效流量訊號
- `花蓮港`：2025 年 throughput 與 flow 全零
- `蘇澳港`：未進入正式分析表，且目前無有效 flow 訊號支撐正式 status/matching/simulation

### 前端呈現規則

- 不在主要圖表與正式推薦中顯示上述三港
- 可在頁面底部或說明區塊加一段方法說明，交代排除原因

## 4. 原型頁面改版原則

以下規則是為了避免 prototype 與正式研究定義衝突。

### 4.1 不再使用真實容量語言

prototype 中的以下概念不能直接照抄：

- `capacity`
- `utilization`
- `調度後 +12% 使用率`

目前正式資料沒有：

- `yard_capacity`
- `available_capacity`
- `utilization`

因此必須改成：

- `throughput`
- `pressure_index`
- `buffer_index`
- `projected pressure`

### 4.2 不把花蓮港當正式分析港

prototype 中若出現花蓮港作為正式推薦、正式狀態港或正式承接端，必須移除。

花蓮港只能出現在：

- 方法說明
- 資料排除說明
- 未納入正式分析宇宙的備註

### 4.3 不假設每月固定有 3 個正式承接港

正式 matching 規則目前是：

- `高壓力港` × `高緩衝港`
- 再套 `matching_score`

在目前 2025 的正式輸出下，多數月份只有 `1` 個正式 pair，因此：

- 不能把頁面寫成「每月固定 3 個正式推薦」
- 若真的想保留 3 張卡，只有第 1 張可以是正式推薦
- 第 2、3 張必須明確標成 `探索性候選` 或 `備選觀察`

### 4.4 Simulation 必須使用三分類

Simulation 頁不應只分：

- `可調度`
- `不可調度`

應至少分三類：

1. `完全解壓`
2. `可緩解但未解壓`
3. `不建議調度`

這三類的正式來源是：

- [simulation_recommendation_final_2025.csv](/Users/lee/Documents/BigData/context/simulation_recommendation_final_2025.csv:1)

### 4.5 年份篩選先固定在 2025

prototype 若有：

- `2023`
- `2024`
- `2025`

三年切換，現階段必須改掉。

目前正式輸出只支撐 `2025`，因此前端應改成：

- 固定年度：`2025`
- 主要篩選：`月份`

## 5. 各頁面資料來源與規則

### 5.1 空櫃總覽

#### 目的

呈現單月港口整體狀態與正式調度摘要。

#### 主要資料表

- `status_final_2025.csv`
- `matching_final_2025.csv`
- `simulation_recommendation_final_2025.csv`

#### 主要元件

- 單月 KPI：
  - 高壓力港數
  - 高緩衝港數
  - 正常港數
  - 本月是否存在正式推薦 pair
- 各港口 `pressure_index` 橫條圖
- 各港口 `buffer_index` 橫條圖
- 港口狀態卡
- 本月正式推薦摘要卡

#### 文案規則

- 不寫「使用率」
- 不寫「容量剩餘 XX%」
- 不寫「調度後一定可降低總壓力 XX%」

### 5.2 港口分析

#### 目的

用時間序列看單港或多港的流量與指標變化。

#### 主要資料表

- `analysis_table_2025.csv`
- `status_final_2025.csv`

#### 主要元件

- 空櫃進港 / 出港趨勢
- `empty_net` 趨勢
- `pressure_index` 趨勢
- `buffer_index` 趨勢
- 單月摘要表

#### 規則

- 只允許分析四港
- 若多港一起看，建議用 facet，而不是把所有線硬疊在一起

### 5.3 跨港媒合

#### 目的

呈現正式 matching 規則下的 source-target 推薦。

#### 主要資料表

- `matching_final_2025.csv`
- `simulation_recommendation_final_2025.csv`

#### 主要元件

- 本月正式推薦卡
- 月別 matching score 時序圖
- 全年度正式 pair 摘要表

#### 規則

- 若本月沒有正式 pair，直接顯示「本月無正式調度建議」
- 不要硬塞成 3 個正式承接港
- 若要顯示額外備選，必須和正式推薦分區

### 5.4 Simulation

#### 目的

呈現正式推薦 pair 在模擬移轉下的結果分類。

#### 主要資料表

- `simulation_recommendation_final_2025.csv`
- `simulation_final_2025.csv`

#### 主要元件

- 本月推薦模擬摘要卡
- 三分類結果 badge
- `move_share` 對 source/target 壓力變化圖
- 4 個情境明細表：
  - `no_move`
  - `move_25pct`
  - `move_50pct`
  - `move_75pct`

#### 三分類規則

- `完全解壓`：`pair_success = TRUE`
- `可緩解但未解壓`：`target_stays_safe = TRUE` 且 `pair_success = FALSE`
- `不建議調度`：`max_safe_move_share = 0`

## 6. 前端取值分工

### 建議分層

#### 明細表

- `status_final_2025.csv`
- `matching_final_2025.csv`
- `simulation_final_2025.csv`

#### dashboard 摘要表

- `simulation_recommendation_final_2025.csv`

### 原則

- 複雜判斷盡量留在 R 前處理端
- 前端盡量只做篩選、呈現、切換
- 不在前端重新實作一整套 `status / matching / simulation` 判斷

## 7. 未來可延伸，但目前不應硬做的部分

- 真實堆場剩餘容量
- 真實使用率
- 真實承接上限
- 真實調度成本最佳化
- 船期與艙位可行性
- 航商層級調度限制

這些在目前資料條件下，不能在 dashboard 中寫成已被精準量化的結果。

## 8. 本地端檔案

### 規格文件

- [dashboard README.md](/Users/lee/Documents/BigData/dashboard%20README.md)

### Shiny app

- [iMarine_dashboard.R](/Users/lee/Documents/BigData/iMarine_dashboard.R)

### 正式資料表

- [status_final_2025.csv](/Users/lee/Documents/BigData/context/status_final_2025.csv:1)
- [matching_final_2025.csv](/Users/lee/Documents/BigData/context/matching_final_2025.csv:1)
- [simulation_final_2025.csv](/Users/lee/Documents/BigData/context/simulation_final_2025.csv:1)
- [simulation_recommendation_final_2025.csv](/Users/lee/Documents/BigData/context/simulation_recommendation_final_2025.csv:1)

## 9. 本地端執行方式

### 方法一：R console

```r
source("/Users/lee/Documents/BigData/iMarine_dashboard.R")
shiny::runApp(app, launch.browser = TRUE)
```

### 方法二：Terminal

```bash
Rscript /Users/lee/Documents/BigData/iMarine_dashboard.R
```

若使用 terminal 啟動，Shiny 會在 console 顯示本地網址，再自行用瀏覽器打開。

### 啟動後怎麼找到探索模式

1. 請確認你啟動的是 [iMarine_dashboard.R](/Users/lee/Documents/BigData/iMarine_dashboard.R)，不是其他舊版 `stable` / `fixed` 檔案。
2. 進入首頁後，左側 sidebar 會顯示 `儀表板模式`。
3. 在 `儀表板模式` 可切換：
   - `正式模式`
   - `探索模式`
4. 切到 `探索模式` 後，sidebar 內可再切：
   - `策略模板`
   - `使用者自訂`
5. `進階設定` 視窗也會跟著模式切換，顯示正式說明、策略模板或自訂權重滑桿。

### 關於高壓港標註

首頁卡片上的百分比是 `pressure_index * 100` 的視覺化顯示；`正式高壓港 / 正式正常 / 正式高緩衝` 則依研究正式規則判定：

- 當月 `pressure_index` 的 `Q75`
- 當月 `buffer_index` 的 `Q75`
- 當月 `pressure_index` 的 `Q50`
- 以及 `roll3_empty_net > 0`

因此某港口顯示 `80%`，不一定就會被標成 `正式高壓`；它還要同時滿足月內相對門檻與近三月淨增加條件。
