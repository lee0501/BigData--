# File Guide

## 1. 先看這份文件的目的

這份文件是給目前專案使用者的「檔案導覽表」。

目的只有三個：

- 區分哪些檔案是 **正式主線**
- 區分哪些檔案只是 **探索性或整體版**
- 告訴你報告時應該依序執行哪些 R 檔、看哪些 csv

目前若要報告 **3.2 Pressure 權重定案**，不要一次看全部檔案，只要跟著本文件的正式主線走即可。

---

## 2. 正式主線：3.2 Pressure 定案

這一條線是目前最重要的分析流程。

邏輯是：

1. 先整理出歷史特徵表
2. 定義 Pressure 權重情境
3. 用不同權重算出各港各月的 Pressure Index
4. 用 T+1 回測比較 `roc` 與 `custom`
5. 確認 `roc` 較佳後，保留前一輪最佳候選，並比較 ROC 上下兩側與橋接延伸方案
6. 重新回測後目前由 `roc_prev_best` 排名最高，擬採用它

### 2.1 正式主線要看的檔案

- [build_history_features.R](/Users/lee/Documents/BigData/context/build_history_features.R)
  - 作用：建立歷史特徵欄位

- [analysis_table_history_2025.csv](/Users/lee/Documents/BigData/context/analysis_table_history_2025.csv)
  - 作用：正式主線的基底表

- [build_weight_scenarios.R](/Users/lee/Documents/BigData/context/build_weight_scenarios.R)
  - 作用：建立要比較的 Pressure 權重情境

- [weight_scenarios.csv](/Users/lee/Documents/BigData/context/weight_scenarios.csv)
  - 作用：權重設定表

- [build_pressure_scenarios.R](/Users/lee/Documents/BigData/context/build_pressure_scenarios.R)
  - 作用：將 Pressure 權重套入歷史資料，算出各港各月的 `pressure_index`

- [pressure_scenarios_2025.csv](/Users/lee/Documents/BigData/context/pressure_scenarios_2025.csv)
  - 作用：Pressure-only 的中間結果表

- [backtest_pressure_only.R](/Users/lee/Documents/BigData/context/backtest_pressure_only.R)
  - 作用：用 T+1 方式回測 Pressure 權重

- [pressure_backtest_summary.csv](/Users/lee/Documents/BigData/context/pressure_backtest_summary.csv)
  - 作用：正式總結表，回答哪組權重比較好

- [pressure_backtest_port_month.csv](/Users/lee/Documents/BigData/context/pressure_backtest_port_month.csv)
  - 作用：細節驗證表，檢查結果是否穩定

### 2.2 報告時正式主線的執行順序

若你要在 RStudio 重新跑一次完整流程，建議依序執行：

```r
source("/Users/lee/Documents/BigData/context/build_history_features.R")
source("/Users/lee/Documents/BigData/context/build_weight_scenarios.R")
source("/Users/lee/Documents/BigData/context/build_pressure_scenarios.R")
source("/Users/lee/Documents/BigData/context/backtest_pressure_only.R")
```

### 2.3 每一步在做什麼

#### Step 1

- R 檔：[build_history_features.R](/Users/lee/Documents/BigData/context/build_history_features.R)
- 輸出：[analysis_table_history_2025.csv](/Users/lee/Documents/BigData/context/analysis_table_history_2025.csv)

用途：

- 在分析表上補齊歷史欄位
- 建立後面回測會用到的歷史特徵

#### Step 2

- R 檔：[build_weight_scenarios.R](/Users/lee/Documents/BigData/context/build_weight_scenarios.R)
- 輸出：[weight_scenarios.csv](/Users/lee/Documents/BigData/context/weight_scenarios.csv)

用途：

- 定義 `roc`
- 定義 `custom`
- 定義 `roc_prev_best`
- 定義 `roc_down_1`
- 定義 `roc_down_2`
- 定義 `roc_down_3`
- 定義 `roc_bridge_1`
- 定義 `roc_bridge_2`
- 定義 `roc_bridge_3`
- 定義 `roc_up_1`
- 定義 `roc_up_2`
- 定義 `roc_up_3`

目前正式主線的理解方式是：

- 第一輪比較：`roc` vs `custom`
- 第二輪比較：只在 `roc` 較佳時，比較 `roc_prev_best`、`roc_down_*`、`roc_bridge_*` 與 `roc_up_*`

#### Step 3

- R 檔：[build_pressure_scenarios.R](/Users/lee/Documents/BigData/context/build_pressure_scenarios.R)
- 輸出：[pressure_scenarios_2025.csv](/Users/lee/Documents/BigData/context/pressure_scenarios_2025.csv)

用途：

- 把不同 Pressure 權重套進歷史資料
- 產生每個港口、每個月份、每個情境的 `pressure_index`

這是一份中間結果表，不是最後哪個方案最好的結論表。

#### Step 4

- R 檔：[backtest_pressure_only.R](/Users/lee/Documents/BigData/context/backtest_pressure_only.R)
- 輸出：
  - [pressure_backtest_summary.csv](/Users/lee/Documents/BigData/context/pressure_backtest_summary.csv)
  - [pressure_backtest_port_month.csv](/Users/lee/Documents/BigData/context/pressure_backtest_port_month.csv)

用途：

- 用 T+1 做正式回測
- 回答 `roc` 是否優於 `custom`
- 回答 ROC 上下兩側延伸方案中哪個最好

---

## 3. 目前正式主線的結論

現階段正式主線的結論應寫成：

1. 先比較 `roc` 與 `custom`
2. 結果顯示 `roc` 較佳
3. 因此以 `roc` 作為正式基準
4. 再對 `roc_prev_best`、`roc_down_*`、`roc_bridge_*` 與 `roc_up_*` 做敏感度分析
5. 由 `pressure_backtest_summary.csv` 判斷 ROC 鄰近方案中表現最佳者
6. 目前結果顯示 `roc_prev_best` 排名最高，因此擬採 `roc_prev_best`

---

## 4. 目前各 csv 的最簡單理解

### 正式主線必看

- [analysis_table_history_2025.csv](/Users/lee/Documents/BigData/context/analysis_table_history_2025.csv)
  - 歷史特徵基底表

- [weight_scenarios.csv](/Users/lee/Documents/BigData/context/weight_scenarios.csv)
  - 權重設定表

- [pressure_scenarios_2025.csv](/Users/lee/Documents/BigData/context/pressure_scenarios_2025.csv)
  - 套權重後的 Pressure 中間結果表

- [pressure_backtest_summary.csv](/Users/lee/Documents/BigData/context/pressure_backtest_summary.csv)
  - 正式總結表

- [pressure_backtest_port_month.csv](/Users/lee/Documents/BigData/context/pressure_backtest_port_month.csv)
  - 細節驗證表

### 一句話記憶

- `weight_scenarios.csv`：我測了哪些權重
- `pressure_scenarios_2025.csv`：這些權重算出了哪些 Pressure Index
- `pressure_backtest_summary.csv`：最後哪個權重最好
- `pressure_backtest_port_month.csv`：這個結果穩不穩

---

## 5. 探索性 / 整體版檔案

以下檔案不是錯，但不是目前 3.2 正式主線的第一優先：

- [build_index_scenarios.R](/Users/lee/Documents/BigData/context/build_index_scenarios.R)
- [analysis_table_scenarios_2025.csv](/Users/lee/Documents/BigData/context/analysis_table_scenarios_2025.csv)
- [backtest_weight_scenarios.R](/Users/lee/Documents/BigData/context/backtest_weight_scenarios.R)
- [weight_backtest_summary.csv](/Users/lee/Documents/BigData/context/weight_backtest_summary.csv)
- [weight_backtest_port_month.csv](/Users/lee/Documents/BigData/context/weight_backtest_port_month.csv)

原因是：

- 這些檔案把 Pressure 與 Buffer 一起看
- 適合當整體參考
- 但不適合直接拿來決定 3.2 Pressure 正式權重

因此目前建議：

- 報告 3.2 時先不以這組檔案為主
- 等 3.3 Buffer 補強後，再回來看整體 scenario

---

## 6. 報告時最簡單的口頭版本

若要快速說明目前流程，可以直接講：

1. 我們先用歷史資料建立歷史特徵表
2. 接著建立 Pressure 權重情境，先比較 ROC 與 custom
3. 之後把各情境套進各港各月資料，算出 pressure index
4. 再用 T+1 方式回測各情境的前瞻辨識能力
5. 結果顯示 ROC 優於 custom
6. 因此保留前一輪最佳候選，並針對 ROC 周圍做上下兩側與橋接敏感度分析
7. 最後由目前回測排名最高的 `roc_prev_best` 作為目前擬採方案
