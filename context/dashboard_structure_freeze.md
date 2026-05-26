# Dashboard Structure Freeze

## 1. 目的

本文件的用途，不是做最後的視覺 polish，而是先把 dashboard 的資訊架構、命名、資料分工與官方多年基準頁的角色定住。

這一版的目標是：

1. 讓 dashboard 先有穩定的頁面邊界與欄位契約。
2. 讓目前的 Shiny app 可以先做模組化重整，而不是繼續把所有邏輯堆在單一檔案。
3. 讓 Prophet 不作為另一套調度模型，而是作為官方多年統計基準與季節性合理性驗證。

本文件明確不處理：

- full polish
- 最終視覺細節
- 完整動畫與品牌層設計
- Prophet 模型本身的回測公式

---

## 2. 凍結原則

### 2.1 先定結構，再定模型，再做整合

推進順序固定為：

1. dashboard 結構凍結
2. Shiny 模組化重整
3. 官方多年統計基準建表與 Prophet baseline check
4. 將官方基準頁整合進 dashboard，作為 iMarine 結果的背景判讀層
5. 最後才做 full polish

### 2.2 正式模式與延伸模組分離

- `正式模式` 只呈現目前研究主線已定稿的輸出。
- `探索模式` 只做敏感度分析，不覆蓋正式結論。
- `官方基準` 已進主導覽，但定位是輔助判讀，不覆蓋正式 iMarine 結論。

### 2.3 正式 dashboard 不回頭使用 prototype 語言

禁止用語：

- `capacity`
- `utilization`
- `剩餘容量 xx%`
- `預測推演`

允許用語：

- `pressure_index`
- `buffer_index`
- `matching_score`
- `模擬`
- `推演`
- `預警`

---

## 3. 頁面保留決策

本案正式保留五個 iMarine 主頁面，外加一個官方多年基準頁。

### 3.1 保留頁面

1. `空櫃總覽`
2. `港口分析`
3. `跨港媒合`
4. `Simulation`
5. `探索模式`

### 3.2 官方多年基準頁

6. `官方基準`

規則：

- `官方基準` 使用臺灣港務股份有限公司多年官方統計與 Prophet baseline。
- 此頁不產生正式調度建議，分兩層：
  - **Bridge**：以 Prophet `yhat` 對照 iMarine 高壓港，回答是否高於往年同期預期。
  - **優先檢查清單**：以同港多年歷史 Q75 為分級門檻，供提前關注使用。
- 頁面文字必須明確區分 `iMarine 月內 Q75/Q50`、`yhat（Bridge）`、與 `多年歷史 Q75（清單門檻）` 三種不同口徑。

---

## 4. 各頁面必備元件

### 4.1 空櫃總覽

角色：

- 單月正式狀態摘要頁
- 讓使用者快速知道本月哪裡有壓力、哪裡可承接、是否有正式推薦

必備元件：

1. 單月 KPI
   - 高壓力港數
   - 高緩衝港數
   - 正常港數
   - 本月是否存在正式推薦 pair
2. 各港口 `pressure_index` 橫條圖
3. 各港口 `buffer_index` 橫條圖
4. 港口狀態卡
5. 本月正式推薦摘要卡
6. 原始資料表

顯示規則：

- 正式分析宇宙只顯示四港。
- `觀察候選` 不能和 `正式推薦` 混在同一個摘要主區塊。
- 若本月無正式 pair，要直接顯示 `本月無正式調度建議`。

Prophet 預留位置：

- 總覽頁右上角保留一個未來可插入的 `下月風險預警` 卡槽。
- 在 Prophet 未通過前，此卡槽不顯示。

### 4.2 港口分析

角色：

- 單港或多港時間序列檢視頁
- 支援趨勢解讀，不直接做正式推薦

必備元件：

1. `empty_in / empty_out` 趨勢圖
2. `empty_net` 趨勢圖
3. `pressure_index` 趨勢圖
4. `buffer_index` 趨勢圖
5. 單月摘要表

顯示規則：

- 只允許四港。
- 多港比較優先採 `facet` 或明確分面，不用所有折線硬疊。
- 這頁的主資料來源應回到 `analysis_table_2025.csv`，`status_final_2025.csv` 只負責補正式分類。

Prophet 預留位置：

- 之後若導入預警，可在 `pressure_index` 趨勢圖上加 `forecast overlay`。
- 在 Prophet 未通過前，不先做預測線。

### 4.3 跨港媒合

角色：

- 呈現正式 matching 規則下的 source-target 建議

必備元件：

1. 本月正式推薦卡
2. 月別 `matching_score` 時序圖
3. 全年度正式 pair 摘要表

顯示規則：

- 若本月沒有正式 pair，直接顯示 `本月無正式調度建議`。
- 額外備選若顯示，必須分區為 `觀察候選`，不得冒充正式推薦。

不在本頁處理：

- Prophet 預警結果
- 使用者自訂權重敏感度分析

### 4.4 Simulation

角色：

- 呈現正式推薦 pair 的情境模擬結果

必備元件：

1. 本月推薦模擬摘要卡
2. 三分類結果 badge
3. `move_share` 對 source / target 壓力變化圖
4. 四個情境明細表
   - `no_move`
   - `move_25pct`
   - `move_50pct`
   - `move_75pct`

顯示規則：

- 此頁的語言固定為 `模擬 / 推演`，不能寫成 `預測`。
- `預計執行日` 若只是展示欄位、不進入計算，就不應在正式模式中表現成會改變結果的參數。

### 4.5 探索模式

角色：

- 敏感度分析
- 補充模型透明度

保留兩個子模式：

1. `策略模板`
2. `使用者自訂`

顯示規則：

- 永遠標示為 `不覆蓋正式研究結論`。
- `策略模板` 只調 `Pressure` 權重。
- `使用者自訂` 才允許同時調 `Pressure / Buffer / Matching`。

### 4.6 Forecast / 預警

目前狀態：

- 只保留插槽，不對外公開。

未來若啟用，角色應為：

- 下一月港口風險預警
- 不是正式 matching 建議
- 不是 simulation 替代品

必備元件：

1. 下月風險排名卡
2. 各港 `pressure_index forecast` 圖
3. 預警說明文字
4. 模型版本與回測表現摘要

---

## 5. 名詞凍結

為避免頁面語言混亂，以下名詞固定使用：

- `正式模式`
  - 研究定稿輸出
- `探索模式`
  - 敏感度分析
- `模擬`
  - 以既有月份資料做 `move_share` 情境重算
- `推演`
  - 可作為 `模擬` 的中文敘事詞，但不與 `預測` 混用
- `預警`
  - 時間序列模型對未來月份的風險提示
- `預測`
  - 僅在 Prophet / forecast 模組出現

固定禁用：

- `預測推演`

---

## 6. 資料來源分工凍結

### 6.1 正式模式

`空櫃總覽`

- `status_final_2025.csv`
- `matching_final_2025.csv`
- `simulation_recommendation_final_2025.csv`

`港口分析`

- `analysis_table_2025.csv`
- `status_final_2025.csv`

`跨港媒合`

- `matching_final_2025.csv`
- `simulation_recommendation_final_2025.csv`

`Simulation`

- `simulation_final_2025.csv`
- `simulation_recommendation_final_2025.csv`

### 6.2 探索模式

允許在 app 內重算，但應集中到共用計算層，不應把計算散落在各 page module 中。

### 6.3 Forecast / 預警

未來預留的資料表名稱先凍結如下：

- `forecast_base_2025.csv`
- `forecast_backtest_detail.csv`
- `forecast_backtest_summary.csv`
- `forecast_alert_next_month.csv`

規則：

- 在這些表尚未建立前，不做 forecast 頁。
- 在 backtest 未通過前，不把 forecast 寫進正式總覽主區塊。

---

## 7. 表格策略凍結

目前 app 內部有手刻 HTML table、排序、分頁與搜尋邏輯。這一層不再擴寫。

正式決策：

- 所有主要資料表改用穩定元件實作。
- 優先採 `reactable`。
- 若環境相依性不允許，再退回 `DT`。
- 不再繼續擴充手刻 table pagination / sorting。

適用範圍：

1. 總覽原始資料表
2. 港口分析摘要表
3. 跨港媒合年度摘要表
4. Simulation 四情境表
5. 探索模式候選配對表
6. 未來 Forecast 回測摘要表

表格規則：

- 長欄位允許橫向卷動
- 數值欄固定右對齊
- 關鍵欄位可固定在左側
- 不讓欄位名稱與數值在窄寬度下互相擠壓跑版

---

## 8. Shiny 模組化重整方案

這一輪不追求 full polish，但要先把單檔 app 拆成可維護的架構。

### 8.1 目標檔案結構

```text
iMarine_dashboard.R
R/
  dashboard_globals.R
  dashboard_utils.R
  dashboard_data.R
  dashboard_compute.R
  modules/
    mod_overview.R
    mod_analysis.R
    mod_matching.R
    mod_simulation.R
    mod_explore.R
    mod_forecast.R
www/
  dashboard.css
  dashboard.js
```

### 8.2 各檔職責

`iMarine_dashboard.R`

- 只保留啟動入口
- 載入 modules 與共用資源

`dashboard_globals.R`

- 全域常數
- palette
- 正式權重
- 港口清單

`dashboard_utils.R`

- format helpers
- date helpers
- plot save helpers
- UI 小元件 helpers

`dashboard_data.R`

- CSV 載入
- 欄位標準化
- 共用資料物件建立

`dashboard_compute.R`

- `build_metric_history()`
- `build_status_table()`
- `build_candidate_table()`
- explore mode 所需的重算函數

`mod_overview.R`

- 總覽頁 UI / server

`mod_analysis.R`

- 港口分析頁 UI / server

`mod_matching.R`

- 跨港媒合頁 UI / server

`mod_simulation.R`

- Simulation 頁 UI / server

`mod_explore.R`

- 探索模式頁 UI / server

`mod_forecast.R`

- 先建 placeholder
- 待 Prophet 通過後再啟用正式內容

`www/dashboard.css`

- 所有樣式集中

`www/dashboard.js`

- 導覽、互動、少量前端狀態控制

### 8.3 重整原則

- 先拆檔，不改研究邏輯。
- explore mode 的重算保留，但集中到共用函數。
- simulation 與 matching 頁分離，不再長期混在同一頁。

---

## 9. Prophet 接入條件凍結

Prophet 不以「想做就接」為原則，而以回測 gate 決定。

### 9.1 允許接入的條件

至少滿足以下兩點：

1. Prophet 在 `pressure_index` 的整體表現，不明顯差於簡單 baseline。
   - baseline 至少包含 `naive_last` 與 `ma3`
2. Prophet 在 `高壓力預警` 上，不能只是畫出一條平滑線而已，必須能提供可解釋的 alert signal。

### 9.2 不接入正式 dashboard 的條件

若出現以下情況之一，forecast 不進正式導覽：

- 比 baseline 更差
- 預測區間過寬，無法形成有用預警
- 高壓月命中過少
- 只能提供看似平滑但不可操作的數值

### 9.3 若未通過 gate 的處理方式

- Prophet 可保留為研究附錄
- 可保留 `forecast_backtest_summary.csv`
- 不在正式 dashboard 顯示 forecast page

---

## 10. 執行順序

### Phase A：結構凍結

1. 完成本文件
2. 確認名詞與頁面保留範圍

### Phase B：模組化重整

1. 拆出 `R/` 與 `www/`
2. 拆出五個正式頁面 module
3. 將表格元件策略改成 `reactable`

### Phase C：Prophet backtest

1. 建立 forecast base table
2. 跑 baseline 與 Prophet 比較
3. 產出 backtest summary

### Phase D：決策

1. 若 Prophet 通過 gate，啟用 `Forecast / 預警` 模組
2. 若 Prophet 未通過 gate，維持 dashboard 無 forecast 正式頁

### Phase E：最後整合與 polish

1. 補 forecast UI 或正式確認不接
2. 再做 full polish

---

## 11. 本文件的實際意義

這份 freeze 的目的，是讓後續不會陷入以下重工：

- 先把 dashboard 做得很滿，結果 Prophet 可行後又得重排導覽
- 先做 forecast 卡，結果模型根本不值得接
- 一邊改頁面、一邊改資料契約，最後每頁都互相牽動

因此本文件的結論是：

- 先把 dashboard 做成可模組化、可插 forecast 的穩定骨架
- Prophet 是否正式上線，交由 backtest 結果決定
- full polish 放到最後
