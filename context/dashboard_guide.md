# iMarine Dashboard 設計指南

本文件整合 dashboard 的設計規範、各頁面功能定義、資料來源分工與執行方式，是 dashboard 開發與維護的主要參考文件。

---

## 1. 目的與定位

本 dashboard 以 R 前處理後的正式 CSV 輸出為資料來源，使用 Shiny + ggplot2 建立本地端可查閱的決策支援介面。

**主線定位**：呈現研究已定案的三層輸出（壓力辨識、媒合建議、調度模擬）。

**輔助定位**：`官方基準` 頁讀取港務公司多年官方統計與 Prophet baseline 輸出，把 iMarine 2025 壓力結果放回多年官方統計背景中判讀，不取代正式 status / matching / simulation。

---

## 2. 技術架構

```text
iMarine_dashboard.R          啟動入口
R/
  dashboard_globals.R        全域常數、palette、正式權重、港口清單
  dashboard_utils.R          format/date/plot helpers、UI 小元件
  dashboard_data.R           CSV 載入、欄位標準化、共用資料物件
  dashboard_compute.R        指標重算函數（探索模式用）
  dashboard_port_baseline.R  官方基準資料載入與 bridge 計算
  modules/
    mod_overview.R / _server.R
    mod_analysis.R / _server.R
    mod_matching.R / _server.R
    mod_simulation.R / _server.R
    mod_explore.R / _server.R
    mod_forecast.R / _server.R   官方基準頁（已啟用）
    mod_shell_server.R           sidebar / 進階設定 modal
    mod_provenance_server.R      資料來歷頁
www/
  dashboard.css
  dashboard.js
```

---

## 3. 正式分析宇宙

### 正式納入港口（4 港）

- `基隆港`
- `臺北港`
- `臺中港`
- `高雄港`

### 不納入正式分析

| 港口 | 排除原因 |
|------|---------|
| 安平港 | 2025 年有 throughput，但 flow 全零，缺乏有效流量訊號 |
| 花蓮港 | 2025 年 throughput 與 flow 全零 |
| 蘇澳港 | 未進入正式分析表，且目前無有效 flow 訊號 |

上述三港只能出現在方法說明或資料排除備註，不得出現在正式推薦或狀態卡中。

---

## 4. 官方基準頁判讀規則

官方基準頁使用三種口徑，三者回答不同問題，不能混用：

| 口徑 | 用途 |
|------|------|
| iMarine 月內 Q75 / Q50 | 同月四港相對比較，判定高壓 / 高緩衝港 |
| Prophet `yhat`（Bridge） | iMarine 高壓港當月是否高於往年同月季節性預期 |
| 同港多年歷史 Q75（清單門檻） | 優先檢查清單分級：偏高 / 觀察 / 正常 |

**Bridge**：若實際 > yhat，代表高於往年同期預期，需搭配 iMarine 明細判斷是否為短期作業壓力。非結構性異常的直接確認，不能單獨下結論。

**優先檢查清單**：分三段——
- 淡藍色列：2025 OOS 驗證段（訓練截止 2024-12，有實際值可對照，誤差欄有顏色標示）
- 白色列（2026-01~04）：橋接段（使用回測資料填補，有實際值）
- 白色列（2026-05 以後）：預測段（無實際值，依預測值估算等級，供提前關注）

---

## 5. 各頁面設計規範

### 5.1 空櫃總覽

**角色**：單月正式狀態摘要，讓使用者快速知道本月哪裡有壓力、哪裡可承接、是否有正式推薦。

**主要資料表**：`status_final_2025.csv`、`matching_final_2025.csv`、`simulation_recommendation_final_2025.csv`

**必備元件**：
1. Featured KPI（高壓力港數、高緩衝港數、正常港數、本月是否存在正式 pair）
2. 各港 `pressure_index` 橫條圖
3. 港口狀態卡
4. 本月調度推薦摘要
5. 下月優先檢查排序（標示「非正式預測」）
6. 原始資料表

**規則**：
- 正式分析宇宙只顯示四港
- `觀察候選` 不能和 `正式推薦` 混在同一摘要主區塊
- 若本月無正式 pair，直接顯示「本月無正式調度建議」

### 5.2 港口分析

**角色**：單港或多港時間序列檢視，支援趨勢解讀，不直接做正式推薦。

**主要資料表**：`analysis_table_2025.csv`、`status_final_2025.csv`（補正式分類）

**必備元件**：
1. `empty_in / empty_out` 趨勢圖
2. `empty_net` 趨勢圖
3. `pressure_index` 趨勢圖
4. `buffer_index` 趨勢圖
5. 單月摘要表

**規則**：只允許四港；多港比較優先採 facet，不硬疊折線。

### 5.3 跨港媒合

**角色**：呈現正式 matching 規則下的 source-target 建議。

**主要資料表**：`matching_final_2025.csv`、`simulation_recommendation_final_2025.csv`

**必備元件**：
1. 本月正式推薦卡
2. 月別 `matching_score` 時序圖
3. 全年度正式 pair 摘要表

**規則**：
- 若本月沒有正式 pair，直接顯示「本月無正式調度建議」
- 額外備選若顯示，必須分區為「觀察候選」，不得冒充正式推薦
- 頁面標示「以下為候選方案排序，供研究參考，不代表正式唯一調度決策」

### 5.4 Simulation

**角色**：呈現正式推薦 pair 的情境模擬結果。

**主要資料表**：`simulation_final_2025.csv`、`simulation_recommendation_final_2025.csv`

**必備元件**：
1. 本月推薦模擬摘要卡
2. 三分類結果 badge（完全解壓 / 可緩解但未解壓 / 不建議調度）
3. `move_share` 對 source / target 壓力變化圖
4. 四個情境明細表（`no_move`、`move_25pct`、`move_50pct`、`move_75pct`）

**三分類規則**：
- `完全解壓`：`pair_success = TRUE`
- `可緩解但未解壓`：`target_stays_safe = TRUE` 且 `pair_success = FALSE`
- `不建議調度`：`max_safe_move_share = 0`

**規則**：語言固定為「模擬 / 推演」，不能寫成「預測」。

### 5.5 探索模式

**角色**：敏感度分析，補充模型透明度，不覆蓋正式研究結論。

**兩個子模式**：
1. `策略模板`：只調 Pressure 權重（Buffer / Matching 維持正式值）
2. `使用者自訂`：允許同時調 Pressure / Buffer / Matching

**規則**：永遠標示為「不覆蓋正式研究結論」。

### 5.6 官方基準

**角色**：以港務公司多年統計 + Prophet 建立 iMarine 結果的背景判讀層，不產生正式調度建議。

**主要資料表**：`port_stat_prophet_base.csv`、`port_stat_prophet_forecast.csv`、`port_stat_prophet_backtest.csv`、`port_stat_prophet_latest_watchlist.csv`

**必備元件**：
1. 趨勢圖（各港多年空櫃占比 + Prophet 歷史擬合 + 基準延伸）
2. Bridge 對照（iMarine 高壓港 × 往年同月預期）
3. 優先檢查清單（含 OOS 驗證段、橋接段、預測段）
4. 模型可信度說明（MAE / MAPE 回測摘要，置於頁面底部）

---

## 6. 名詞規範

**固定使用**：

| 名詞 | 意義 |
|------|------|
| 正式模式 | 研究定稿輸出 |
| 探索模式 | 敏感度分析 |
| 模擬 | 以既有月份資料做 `move_share` 情境重算 |
| 推演 | 模擬的中文敘事詞，不與「預測」混用 |
| 往年同月預期 | Prophet yhat，官方基準頁 Bridge 對照用 |
| 優先檢查分數 | 下月優先關注排序的評分 |

**禁止用語**：

- `capacity`、`utilization`、`剩餘容量 xx%`
- `預測推演`（混用）
- `預警分數`（請用「優先檢查分數」）
- `Prophet 季節基準`（請用「往年同月預期」）
- 花蓮港 / 安平港 / 蘇澳港 出現在正式推薦

---

## 7. 資料來源分工

| 頁面 | 主要 CSV |
|------|---------|
| 空櫃總覽 | `status_final_2025.csv`、`matching_final_2025.csv`、`simulation_recommendation_final_2025.csv` |
| 港口分析 | `analysis_table_2025.csv`、`status_final_2025.csv` |
| 跨港媒合 | `matching_final_2025.csv`、`simulation_recommendation_final_2025.csv` |
| Simulation | `simulation_final_2025.csv`、`simulation_recommendation_final_2025.csv` |
| 官方基準 | `port_stat_prophet_base.csv`、`port_stat_prophet_forecast.csv`、`port_stat_prophet_backtest.csv`、`port_stat_prophet_latest_watchlist.csv` |

---

## 8. 原型改版禁忌

從 prototype 遺留下來、正式版不能沿用的做法：

1. **不使用真實容量語言**：`capacity`、`utilization`、`調度後 +12% 使用率` 均不合正式資料定義
2. **花蓮港不進正式分析**：只能出現在方法說明 / 排除備註
3. **不假設每月固定 3 個承接港**：多數月份只有 1 個正式 pair
4. **Simulation 必須三分類**：不能只分「可調度 / 不可調度」兩類
5. **年份篩選固定 2025**：前端以月份為主要篩選維度

---

## 9. 執行方式

### 方法一：R console

```r
source("/Users/lee/Documents/BigData/iMarine_dashboard.R")
shiny::runApp(app, launch.browser = TRUE)
```

### 方法二：Terminal

```bash
Rscript /Users/lee/Documents/BigData/iMarine_dashboard.R
```

啟動後瀏覽器打開本地網址即可。左側 sidebar 可切換正式模式 / 探索模式；官方基準頁為獨立頁面，在導覽列中選取。
