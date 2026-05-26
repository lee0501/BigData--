branch

| name | 用途 |
| :---- | :---- |
| main | GitHub 主線，透過 PR 合併 |
| feature/prophet-port-stat-extension | 官方統計基準 + Prophet baseline + dashboard 文件整併（本 branch）|
| feature/prophet-seasonal-yhat-baseline | Prophet yhat Bridge 對照、OOS 驗證、優先檢查清單 UI 改版 |
| feature/dashboard-r | dashboard Shiny app 模組化版本 |
| feature/dashboard-formula | dashboard 公式設計階段 |
| feature/data-preprocessing | 資料清洗與前處理 |
| feature/original-data | 原始開放資料下載（未清洗）|
| feature/history-features | 2.3 歷史資料對齊 |
| codex/safe-edit-20260407 | 測試用 |

---

### feature/prophet-port-stat-extension（2026-05）

**目的**：將港務公司多年官方統計與 Prophet 模型整合進 dashboard，建立「官方基準」頁作為 iMarine 結果的背景判讀層；同步完成文件整併與術語統一。

**主要內容**：

- `build_port_stat_history.R`：整合港務公司官方月別統計（空實櫃個數 + TEU 方向別）
- `build_port_stat_prophet_forecast.R`：Prophet 模型訓練、預測、回測、優先檢查清單建立
- 官方基準頁（`mod_forecast.R / _server.R`）：
  - 趨勢圖（多年空櫃占比 + 歷史擬合 + 延伸預測）
  - Bridge 對照（iMarine 高壓港 × Prophet yhat 往年同月預期）
  - 優先檢查清單（含 2025 OOS 驗證段 + 2026-01~04 橋接段 + 2026-05 以後預測段）
  - 模型可信度說明（MAE / MAPE，頁面底部）
- UI 術語統一：「預警分數」→「優先檢查分數」、「Prophet 季節基準」→「往年同月預期」
- 文件整併：
  - 刪除：`D.資料分析模式與方法.md`、`E.研究流程及推論邏輯.md`、`context/table.md`、`資料運用來源細節表.md`
  - 合併：`dashboard README.md` + `context/dashboard_structure_freeze.md` → `context/dashboard_guide.md`
  - 吸收：`context/權重資料.md` → `context/app_formula_design.md`（附錄 A）
  - 更新：`context/bigdata.md`（加入官方統計 Layer 1 + Layer 6）、`context/app_formula_design.md`（更名 + 吸收）

### 0407 commit history

create branch : 2

1. feature/dashboard-r  
2. feature/dashboard-formula

commit & PR : 2

1. feature/dashboard-r  
* app.R  
* new\_dashboard.R  
2. feature/dashboard-formula  
* app\_formula\_design.md

### 0408 commit history

create branch : 2

1. feature/original-data  
2. feature/data-preprocessing

commit & PR

1. feature/original-data  
   (5 files)  
* create mode 100644 context/enter-empty.csv  
* create mode 100644 context/enter-full.csv  
* create mode 100644 context/exit-empty.csv  
* create mode 100644 context/exit-full.csv  
* create mode 100644 context/throughput.csv  
2. feature/data-preprocessing  
   (8 files)  
* create mode 100644 context/analysis\_table\_2025.csv  
* create mode 100644 context/build\_analysis\_table.R  
* create mode 100644 context/build\_master\_table.R   
* create mode 100644 context/check\_master\_table.R  
* create mode 100644 context/check\_port\_summary.csv  
* create mode 100644 context/check\_summary.csv  
* create mode 100644 context/check\_zero\_throughput.csv  
* create mode 100644 context/master\_table\_2025.csv

### markdown reading order

1. `bigdata.md`
2. `app_formula_design.md`
3. `權重資料.md`
4. `weight_backtest_plan.md`
5. `develop log.md`
6. `table.md`

### markdown direction summary

#### `bigdata.md`

整個專案的主軸文件。

大方向：

- 說明研究背景、研究問題、研究邏輯與系統架構
- 定義目前主分析目標是 `T+1` 的港口壓力辨識與跨港調度支援
- 建立資料來源、研究方法、預期輸出與研究價值

#### `app_formula_design.md`

執行層的公式與欄位設計文件。

大方向：

- 把 `bigdata.md` 的研究主軸轉成可計算的欄位與公式
- 說明 `2.1`、`2.2`、`2.3` 的資料欄位結構
- 補充 `pressure_index`、`buffer_index`、`status`、`matching_score` 的設計原理

#### `權重資料.md`

權重設計依據文件。

大方向：

- 用交通部資料說明權重構面的背景
- 用 ROC 權重作為理論基準
- 用自治權重表達研究者對研究問題的重點排序
- 說明權重文件本身不是結論，而是後續比較的依據

#### `weight_backtest_plan.md`

正式的權重情境回測方法文件。

大方向：

- 說明 ROC 與自治權重如何在同一個 `T+1` 架構下比較
- 定義 `expanding window`、評估時點與驗證月份
- 對應目前的 R 腳本、輸出表與回測邏輯

#### `develop log.md`

開發與版控歷程文件。

大方向：

- 記錄 branch 用途
- 記錄 commit / PR 歷程
- 幫助回頭理解目前專案是如何一步一步建立出來的

#### `table.md`

資料表補充與操作備註文件。

大方向：

- 補充資料整併、欄位處理與表格層面的觀察
- 作為操作與資料理解上的輔助說明

