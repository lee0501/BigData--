# 權重情境回測規劃與執行說明

## 1. 目的

本文件整理 `ROC 權重` 與 `自治權重` 的比較方法、資料來源、R 語言執行流程，以及目前在專案中的檔案分工。

但需要先說明：現階段最應優先定案的是 [app_formula_design.md](/Users/lee/Documents/BigData/context/app_formula_design.md) 的 **3.2 Pressure Index 權重設計**。  
`3.3 Buffer Index` 雖然已先有 proxy 公式與初步情境比較，但其外部參考來源與可驗證性仍弱於 Pressure，因此目前不宜把 `Pressure + Buffer` 的整體 scenario 排名，直接當成 3.2 的最終定案依據。

這份規劃對應以下三份主文件：

- [bigdata.md](/Users/lee/Documents/BigData/context/bigdata.md)：研究主軸與決策支援目標
- [app_formula_design.md](/Users/lee/Documents/BigData/context/app_formula_design.md)：指標公式與欄位定義
- [權重資料.md](/Users/lee/Documents/BigData/context/權重資料.md)：權重背景、ROC 權重與自治權重說明

本次新增的回測流程，不是要找出唯一正確答案，而是分階段比較不同權重設計在歷史資料下是否能更好地支撐研究目標：

- 第一階段：更早辨識高壓力港
- 第二階段：再補強高緩衝港的辨識與排序
- 最後階段：整合成可解釋的跨港媒合排序基礎

現階段本研究聚焦於 **T+1（月後一期）** 的短期決策問題，也就是：

- 在 `t` 月底用當時可得資料判斷港口壓力與緩衝狀態
- 再檢查該判斷是否對 `t+1` 月具前瞻辨識能力

因此本文件目前不以跨年度同月份季節性預測作為主要分析目標。若未來能取得更長期的多年度資料，再擴充為季節性分析會更合適。

## 2. 方法概念

### 2.1 為什麼用歷史資料回測

目前資料來源是月別公開資料，沒有真實的「最佳調度決策標籤」。  
因此這裡採用的是 **policy backtest / ranking backtest**，不是傳統監督式預測模型評估。

目前第一階段的核心問題改寫為：

- 若在 `t` 月用某組權重判定高壓力港，這些港口在 `t+1` 是否仍呈現壓力累積跡象？
- 哪套 **Pressure 權重** 在歷史資料下，整體表現更符合研究敘事？

換句話說，這裡比較的不是「哪一組權重較能預測明年同月份」，而是：

> 哪一組 Pressure 權重在只使用當下以前資料的前提下，更適合回答 T+1 的港口壓力辨識問題？

### 2.2 第一階段：3.2 Pressure 權重設計

#### ROC 權重（目前 3.2 正式基準）

來源：[權重資料.md](/Users/lee/Documents/BigData/context/權重資料.md)

當只知道重要性排序、不確定精準加權時，使用 Ranked Order Centroid 作為理論基準。

3 個構面時的 ROC 權重為：

- 第 1 名：`0.611`
- 第 2 名：`0.278`
- 第 3 名：`0.111`

由於目前 Pressure-only 的第一輪比較結果顯示 `roc > custom`，因此在 3.2 正式定案上，現階段應以 ROC 權重作為主要基準，再於其附近做局部敏感度分析。

#### ROC 鄰近敏感度分析

既然第一輪比較是 `roc` 優於初始 `custom`，則後續更合理的做法不是直接沿著 `custom` 家族繼續微調，而是先在 `roc` 周圍做局部敏感度分析，檢查：

- ROC 附近是否存在更穩定的壓力權重
- 這些權重是否仍保留 ROC 作為理論基準的排序邏輯

目前納入以下 ROC 鄰近情境：

- `roc_prev_best`
- `roc_down_1`
- `roc_down_2`
- `roc_down_3`
- `roc_bridge_1`
- `roc_bridge_2`
- `roc_bridge_3`
- `roc_up_1`
- `roc_up_2`
- `roc_up_3`

這些情境的共同特徵是：

- 仍以 `pressure_net` 為明確最高權重
- 保留 ROC 的排序導向邏輯
- 保留上一輪最佳候選，避免新一輪測試忽略既有最佳解
- 同時測試 `pressure_net` 低於與高於 ROC baseline 的可能性
- 在 `roc_prev_best` 與 `roc_up_3` 之間補做橋接測試，避免兩個高分方案之間缺少連續比較
- 所有情境權重加總皆為 `1`

本研究目前使用的 Pressure 權重數值如下：

- `roc`
  - `pressure_net_pct = 0.611`
  - `pressure_flow_pct = 0.278`
  - `pressure_roll_pct = 0.111`
- `roc_prev_best`
  - `pressure_net_pct = 0.600`
  - `pressure_flow_pct = 0.220`
  - `pressure_roll_pct = 0.180`
- `roc_down_1`
  - `pressure_net_pct = 0.580`
  - `pressure_flow_pct = 0.270`
  - `pressure_roll_pct = 0.150`
- `roc_down_2`
  - `pressure_net_pct = 0.560`
  - `pressure_flow_pct = 0.280`
  - `pressure_roll_pct = 0.160`
- `roc_down_3`
  - `pressure_net_pct = 0.540`
  - `pressure_flow_pct = 0.290`
  - `pressure_roll_pct = 0.170`
- `roc_bridge_1`
  - `pressure_net_pct = 0.625`
  - `pressure_flow_pct = 0.218`
  - `pressure_roll_pct = 0.157`
- `roc_bridge_2`
  - `pressure_net_pct = 0.650`
  - `pressure_flow_pct = 0.215`
  - `pressure_roll_pct = 0.135`
- `roc_bridge_3`
  - `pressure_net_pct = 0.675`
  - `pressure_flow_pct = 0.213`
  - `pressure_roll_pct = 0.112`
- `roc_up_1`
  - `pressure_net_pct = 0.640`
  - `pressure_flow_pct = 0.250`
  - `pressure_roll_pct = 0.110`
- `roc_up_2`
  - `pressure_net_pct = 0.670`
  - `pressure_flow_pct = 0.230`
  - `pressure_roll_pct = 0.100`
- `roc_up_3`
  - `pressure_net_pct = 0.700`
  - `pressure_flow_pct = 0.210`
  - `pressure_roll_pct = 0.090`

可以把這六組理解成：

- `roc_prev_best`：保留前一輪 ROC 鄰近測試的最佳候選，作為比較基準
- `roc_down_*`：測試降低 `pressure_net`、提高其他構面的效果
- `roc_bridge_*`：測試 `roc_prev_best` 到 `roc_up_3` 之間的連續路徑
- `roc_up_*`：測試進一步提高 `pressure_net`、降低其他構面的效果
- 這樣可以避免只往單一方向調整造成敏感度分析偏誤

#### 自治權重（Pressure 初始假設版）

來源：[app_formula_design.md](/Users/lee/Documents/BigData/context/app_formula_design.md)

依研究敘事給權重：

Pressure Index：

- `pressure_flow_pct`：`0.35`
- `pressure_net_pct`：`0.40`
- `pressure_roll_pct`：`0.25`

#### 自治權重鄰近敏感度分析（探索性）

由於 `custom` 是研究者依研究敘事提出的初始假設，因此仍可保留其鄰近權重情境，作為探索性參考。

設計原則：

- 不做無限制全空間搜尋
- 只測試與 `custom` 接近、且仍符合研究邏輯的權重
- 維持 `pressure_net` 仍為核心構面
- 只在 `pressure_flow` 與 `pressure_roll` 間做小幅調整

目前納入以下 5 組鄰近情境：

- `custom_nearby_a`
- `custom_nearby_b`
- `custom_nearby_c`
- `custom_nearby_d`
- `custom_nearby_e`

但這裡要明確說明：在目前正式流程下，`custom_nearby_*` 已不是主線分析。

它們保留在檔案中的原因只有兩個：

1. 保留研究過程紀錄
   - 表示曾經檢查過研究者自訂權重及其鄰近版本
2. 保留探索性附錄材料
   - 若未來要補附錄，可說明 `custom` 路線曾被評估，但不是本階段主結論

因此現在應這樣理解：

- 正式主線：`roc`、`custom`、`roc_prev_best`、`roc_down_*`、`roc_bridge_*`、`roc_up_*`
- 探索性保留：`custom_nearby_*`
- 正式結論不再依賴 `custom_nearby_*`

### 2.3 第二階段：3.3 Buffer 權重補強

`Buffer Index` 目前雖已有 proxy 公式，但其權重與外部支撐仍較弱，因此本研究暫不把 Buffer 權重定案與 Pressure 權重綁在同一輪完成。

目前定位如下：

- `buffer_space_raw`、`buffer_net_raw`、`buffer_export_raw` 可先做為承接能力 proxy
- `Buffer` 的整體 ranking 可先作 exploratory reference
- 但不宜直接用整體 scenario 的高低，回頭決定 3.2 Pressure 權重

換句話說：

- `Pressure` 權重先獨立定案
- `Buffer` 權重後續再補資料來源、研究依據與敏感度分析
- 最後才重新整合 3.2 + 3.3 + 3.5

#### Buffer 小實驗：固定 `pressure = roc_prev_best`

為避免 `Pressure` 與 `Buffer` 同時變動而難以解讀，本次先補做一個小實驗：

- `pressure` 固定採用目前 3.2 擬採方案 `roc_prev_best`
- 只變動 `buffer` 權重情境
- 仍使用相同的 **expanding window + T+1** 架構

本次納入比較的 `buffer` 情境為：

- `roc`
- `custom`
- `custom_nearby_a`
- `custom_nearby_b`
- `custom_nearby_c`
- `custom_nearby_d`
- `custom_nearby_e`

本次比較重點改為只看 Buffer 指標：

- `buffer_lead_corr`
- `buffer_next_pressure_corr`
- `high_buffer_safety_rate`
- `high_buffer_nonpositive_roll_rate`

其中：

- `buffer_lead_corr` 越負越好，表示當期 `buffer_index` 越高者，下期越不容易出現較高 `empty_net`
- `buffer_next_pressure_corr` 越低越好，表示當期 `buffer_index` 越高者，下期越不容易轉成較高 `pressure_index`
- `high_buffer_safety_rate` 越高越好，表示當期被判為高緩衝港者，下期仍維持低壓力的比例較高
- `high_buffer_nonpositive_roll_rate` 越高越好，表示當期被判為高緩衝港者，下期較不容易出現正的 `roll3_empty_net`

#### Buffer 小實驗結果紀錄

本次小實驗輸出於：

- [buffer_fixed_scenarios_2025.csv](/Users/lee/Documents/BigData/context/buffer_fixed_scenarios_2025.csv)
- [buffer_backtest_summary.csv](/Users/lee/Documents/BigData/context/buffer_backtest_summary.csv)
- [buffer_backtest_port_month.csv](/Users/lee/Documents/BigData/context/buffer_backtest_port_month.csv)

目前在 `pressure = roc_prev_best` 固定下，Buffer 情境排名如下：

1. `custom_nearby_c`
   - `buffer_space_pct = 0.45`
   - `buffer_net_pct = 0.40`
   - `buffer_export_pct = 0.15`
   - `buffer_overall_score = 1.3148`
2. `custom_nearby_b`
   - `buffer_space_pct = 0.40`
   - `buffer_net_pct = 0.40`
   - `buffer_export_pct = 0.20`
   - `buffer_overall_score = 1.3112`
3. `custom`
   - `buffer_space_pct = 0.45`
   - `buffer_net_pct = 0.35`
   - `buffer_export_pct = 0.20`
   - `buffer_overall_score = 1.2526`
4. `custom_nearby_e`
   - `buffer_space_pct = 0.40`
   - `buffer_net_pct = 0.35`
   - `buffer_export_pct = 0.25`
   - `buffer_overall_score = 1.2476`
5. `custom_nearby_a`
   - `buffer_space_pct = 0.50`
   - `buffer_net_pct = 0.30`
   - `buffer_export_pct = 0.20`
   - `buffer_overall_score = 1.1523`
6. `custom_nearby_d`
   - `buffer_space_pct = 0.50`
   - `buffer_net_pct = 0.35`
   - `buffer_export_pct = 0.15`
   - `buffer_overall_score = 1.1523`
7. `roc`
   - `buffer_space_pct = 0.611`
   - `buffer_net_pct = 0.278`
   - `buffer_export_pct = 0.111`
   - `buffer_overall_score = 1.0976`

這組小實驗目前可先做以下解讀：

- 在 `pressure` 固定後，Buffer 表現最佳者不再是 `roc`，而是 `custom_nearby_c`
- 目前較佳方案集中在 **提高 `buffer_net` 權重**，並適度降低 `buffer_space` 或 `buffer_export`
- `roc` 在本輪比較中排名最後，表示若只看目前資料下的高緩衝港穩定性，過度偏重 `buffer_space` 可能不足以支撐 3.3 的辨識需求
- `custom` 本身仍有競爭力，但 `custom_nearby_c` 與 `custom_nearby_b` 顯示 `buffer_net_pct` 從 `0.35` 提高到 `0.40` 後，整體表現略優

但這裡仍要保留方法上的限制：

- 本次只是 **Buffer 段落的小實驗**
- `pressure` 雖已固定，但 `buffer` 目前仍是公開資料 proxy
- 因此這份結果可作為 3.3 的暫定方向，不宜直接宣稱已完成 Buffer 最終定案

### 2.4 第一階段回測評估指標

第一階段以 Pressure 為主，因此優先看與壓力辨識直接相關的指標。

#### 壓力前瞻能力

- `pressure_lead_corr`：當月 `pressure_index` 與下月 `empty_net` 的相關性
- `pressure_roll_lead_corr`：當月 `pressure_index` 與下月 `roll3_empty_net` 的相關性

#### 分類合理性

- `high_pressure_hit_rate`：當月被標為高壓力港者，下月仍 `empty_net > 0` 的比例
- `high_pressure_roll_hit_rate`：當月被標為高壓力港者，下月仍 `roll3_empty_net > 0` 的比例
- `high_buffer_safety_rate`：目前保留於整體情境表，僅作第二階段 Buffer 補強的參考，不作為第一階段定案 Pressure 權重的主指標

### 2.5 第三階段：3.5 Matching Score 初步回測

在 3.2 與 3.3 已各自取得暫定主方案後，3.5 不宜直接跳到大量組合搜尋，而應先做一輪簡化且可解釋的父方案比較：

- `pressure` 固定為 `roc_prev_best`
- `buffer` 固定為 `custom_nearby_c`
- `distance_factor` 固定採目前規則型版本
  - `distance_class = 1 -> 1.00`
  - `distance_class = 2 -> 0.85`
  - `distance_class = 3 -> 0.70`
- 候選配對只保留：
  - `source.status == 高壓力港`
  - `target.status == 高緩衝港`
  - `source != target`

#### 3.5 的方法調整

由於目前沒有真實跨港調度量與實際執行結果，因此 3.5 不能把「來源港在 `t+1` 自然下降」直接當成成功指標。否則會反而獎勵那些本來就沒有那麼迫切需要調度的來源港。

因此本輪回測改採以下邏輯：

- `source` 端：看它在 **未干預** 條件下，`t+1` 是否仍維持高壓力或持續累積  
  目的：確認這個來源港確實需要優先處理
- `target` 端：看它在 **未干預** 條件下，`t+1` 是否仍維持相對低壓力與非正向累積  
  目的：確認這個承接港本身足夠穩定

這表示 3.5 回測評估的不是「真實調過之後改善多少」，而是：

- 目前的排序規則是否把真正需要優先釋放的來源港排前面
- 目前的排序規則是否把本來就較穩定的承接港排前面

#### 3.5 第一輪比較：`roc vs custom`

本輪父方案定義如下：

- `matching_roc`
  - `source_pressure_pct = 0.611`
  - `target_buffer_pct = 0.278`
  - `distance_pct = 0.111`
- `matching_custom`
  - `source_pressure_pct = 0.45`
  - `target_buffer_pct = 0.35`
  - `distance_pct = 0.20`

若 `custom` 較佳，再沿 `custom` 家族做鄰近微調；若 `roc` 較佳，才再沿 `roc` 家族做延伸。

#### 3.5 第一輪結果

輸出檔案：

- [matching_candidate_pairs_fixed_2025.csv](/Users/lee/Documents/BigData/context/matching_candidate_pairs_fixed_2025.csv)
- [matching_parent_backtest_summary.csv](/Users/lee/Documents/BigData/context/matching_parent_backtest_summary.csv)
- [matching_parent_backtest_pair_month.csv](/Users/lee/Documents/BigData/context/matching_parent_backtest_pair_month.csv)

目前第一輪 `roc vs custom` 的結果為：

- `custom`
  - `global_top1_pair_viable_rate = 0.6667`
  - `global_top1_pair_stable_rate = 0.6667`
  - `global_top1_source_need_rate = 1.0000`
  - `global_top1_mean_outcome_score = 3.3333`
  - `pooled_outcome_corr = 0.1873`
- `roc`
  - `global_top1_pair_viable_rate = 0.6667`
  - `global_top1_pair_stable_rate = 0.6667`
  - `global_top1_source_need_rate = 0.8889`
  - `global_top1_mean_outcome_score = 3.2222`
  - `pooled_outcome_corr = 0.1297`

初步解讀：

- `custom` 與 `roc` 在 top-1 的 target 安全率上相近
- 但 `custom` 較能把「下期仍維持高壓力」的來源港排到更前面
- 因此 3.5 的父方案比較由 `custom` 勝出

其中最明顯的差異出現在 `202510`：

- `custom` 的全月 top pair 為 `臺中港 -> 臺北港`
- `roc` 的全月 top pair 則轉成 `高雄港 -> 臺北港`
- 但在觀察值下，`高雄港` 到 `t+1` 已不再是高壓力港，因此 `roc` 在這個月份把相對不那麼迫切的來源港排到了前面

#### 3.5 第二輪：沿 `custom` 家族做鄰近小實驗

由於第一輪結果為 `custom > roc`，因此第二輪只沿 `custom` 家族比較：

- `custom`
- `custom_nearby_a`
- `custom_nearby_b`
- `custom_nearby_c`
- `custom_nearby_d`
- `custom_nearby_e`

輸出檔案：

- [matching_weight_scenarios.csv](/Users/lee/Documents/BigData/context/matching_weight_scenarios.csv)
- [matching_backtest_summary.csv](/Users/lee/Documents/BigData/context/matching_backtest_summary.csv)
- [matching_backtest_pair_month.csv](/Users/lee/Documents/BigData/context/matching_backtest_pair_month.csv)

目前第二輪結果顯示：

- `custom_nearby_b` 排名第 1
  - `source_pressure_pct = 0.40`
  - `target_buffer_pct = 0.40`
  - `distance_pct = 0.20`
  - `pooled_outcome_corr = 0.2060`
- `custom_nearby_d` 排名第 2
  - `source_pressure_pct = 0.45`
  - `target_buffer_pct = 0.30`
  - `distance_pct = 0.25`
  - `pooled_outcome_corr = 0.1896`
- 原始 `custom` 排名第 3
  - `pooled_outcome_corr = 0.1873`

但要特別注意：

- `custom` 家族各組在 top-1 決策指標上目前幾乎完全相同
- 差異主要只出現在 `pooled_outcome_corr`
- 這表示目前 3.5 在合理的 `custom` 鄰近區間內，排序結果對小幅權重變動相對穩定

因此這一輪更保守的結論應是：

- 3.5 的父方案主線可先採 `custom`
- 若一定要指定目前的暫定優先候選，則可先記為 `custom_nearby_b`
- 但不宜把 `custom_nearby_b` 與 `custom` 的差距解讀得過度確定

## 3. R 檔案分工

### 3.1 既有檔案

- [build_master_table.R](/Users/lee/Documents/BigData/context/build_master_table.R)
  負責 `2.1`、`2.2` 的基礎欄位

- [build_analysis_table.R](/Users/lee/Documents/BigData/context/build_analysis_table.R)
  負責篩出分析範圍

- [build_history_features.R](/Users/lee/Documents/BigData/context/build_history_features.R)
  負責 `2.3` 的歷史欄位

### 3.2 本次新增檔案

- [build_weight_scenarios.R](/Users/lee/Documents/BigData/context/build_weight_scenarios.R)
  建立 ROC、ROC 鄰近情境、初始自治權重與探索性鄰近情境

- [build_index_scenarios.R](/Users/lee/Documents/BigData/context/build_index_scenarios.R)
  依不同權重計算 `pressure_index`、`buffer_index`、`status`

- [backtest_weight_scenarios.R](/Users/lee/Documents/BigData/context/backtest_weight_scenarios.R)
  用歷史資料比較不同整體情境的前瞻與分類表現

- [build_pressure_scenarios.R](/Users/lee/Documents/BigData/context/build_pressure_scenarios.R)
  只針對 `Pressure` 權重做 expanding window 計算，避免 3.2 與 3.3 混在一起解讀

- [backtest_pressure_only.R](/Users/lee/Documents/BigData/context/backtest_pressure_only.R)
  只用 `Pressure` 指標做 T+1 回測，作為 3.2 正式定案的主分析腳本

- [build_buffer_scenarios_fixed_pressure.R](/Users/lee/Documents/BigData/context/build_buffer_scenarios_fixed_pressure.R)
  固定 `pressure = roc_prev_best`，只變動 `buffer` 情境，作為 3.3 的小型補強實驗

- [backtest_buffer_fixed_pressure.R](/Users/lee/Documents/BigData/context/backtest_buffer_fixed_pressure.R)
  只比較固定 pressure 下各 `buffer` 情境的 T+1 表現

- [build_matching_weight_scenarios.R](/Users/lee/Documents/BigData/context/build_matching_weight_scenarios.R)
  建立 3.5 `matching_score` 的父方案與鄰近權重情境

- [backtest_matching_fixed_indices.R](/Users/lee/Documents/BigData/context/backtest_matching_fixed_indices.R)
  固定 `pressure = roc_prev_best`、`buffer = custom_nearby_c`，比較 3.5 的 `roc vs custom` 與勝方鄰近情境

## 4. 執行流程

### Step 1：建立權重情境表

執行：

```r
source("/Users/lee/Documents/BigData/context/build_weight_scenarios.R")
```

輸出：

- [weight_scenarios.csv](/Users/lee/Documents/BigData/context/weight_scenarios.csv)

這個 csv 的用途不是看誰最好，而是確認「有哪些權重方案被拿來測」。

閱讀方式：

- 一列代表一個 `scenario + component`
- 先只看 `index_type == "pressure"`
- 再確認每個 `scenario` 底下三個 component 的權重是否加總為 `1`

### Step 2：若要看整體 scenario，再建立整體指標表

執行：

```r
source("/Users/lee/Documents/BigData/context/build_index_scenarios.R")
```

輸出：

- [analysis_table_scenarios_2025.csv](/Users/lee/Documents/BigData/context/analysis_table_scenarios_2025.csv)

表內會包含：

- 各港各月在不同情境下的 `pressure_index`
- `buffer_index`
- `status`
- 相關原始構面欄位

這個 csv 的用途是：

- 看每個 `scenario` 在每個港口、每個月份算出來的 `pressure_index` / `buffer_index`
- 它是「中間結果表」，不是最後排名表
- 因為混有 Buffer，所以不適合作為 3.2 的正式主判斷依據

這一步已改為 **expanding window** 架構：

- 在每個月份 `t`，只使用 `1 ~ t` 月資料建立當期指標
- 不再使用完整年度資料一次計算所有分位數
- 以避免未來資訊污染當下判斷

### Step 3：若要看整體 scenario，再做整體回測比較

執行：

```r
source("/Users/lee/Documents/BigData/context/backtest_weight_scenarios.R")
```

輸出：

- [weight_backtest_summary.csv](/Users/lee/Documents/BigData/context/weight_backtest_summary.csv)
- [weight_backtest_port_month.csv](/Users/lee/Documents/BigData/context/weight_backtest_port_month.csv)

這兩個 csv 的用途是：

- `weight_backtest_summary.csv`
  - 看整體 scenario 的總結分數
- `weight_backtest_port_month.csv`
  - 看整體 scenario 在各港、各月的細部表現

但因為這兩份資料混有 Buffer，所以目前只作整體參考，不作為 3.2 主結論。

正式回測時，採用：

- `t` 月作為評估時點
- `t+1` 月作為驗證月份

例如：

- 用 `1~3 月` 資料建立 `3 月` 指標，驗證 `4 月`
- 用 `1~9 月` 資料建立 `9 月` 指標，驗證 `10 月`

### Step 4：3.2 正式流程改用 Pressure-only 分析

執行：

```r
source("/Users/lee/Documents/BigData/context/build_pressure_scenarios.R")
source("/Users/lee/Documents/BigData/context/backtest_pressure_only.R")
```

輸出：

- [pressure_scenarios_2025.csv](/Users/lee/Documents/BigData/context/pressure_scenarios_2025.csv)
- [pressure_backtest_summary.csv](/Users/lee/Documents/BigData/context/pressure_backtest_summary.csv)
- [pressure_backtest_port_month.csv](/Users/lee/Documents/BigData/context/pressure_backtest_port_month.csv)

這一步的目的很明確：

- 先把 `3.2 Pressure` 從 `3.3 Buffer` 獨立出來
- 先比較 `roc` 與 `custom`
- 若 `roc` 較佳，再保留上一輪最佳候選，並在 `roc` 周圍做上下兩側與橋接敏感度分析

這三個 csv 的角色要分清楚：

- `pressure_scenarios_2025.csv`
  - 中間結果表
  - 內容是每個 `pressure scenario` 在每個港口、每個月份算出的 `pressure_index`
- `pressure_backtest_summary.csv`
  - 正式總結表
  - 用來回答 `roc` 跟 `custom` 誰比較好，以及 `roc_nearby_*` 誰最好
- `pressure_backtest_port_month.csv`
  - 細節檢查表
  - 用來看每個港、每個月的回測表現是否穩定

### Step 5：第一階段判讀邏輯

在目前分階段架構下，3.2 應優先根據以下指標討論 Pressure 權重：

- `pressure_lead_corr`
- `pressure_roll_lead_corr`
- `high_pressure_hit_rate`
- `high_pressure_roll_hit_rate`

而不應直接用包含 Buffer 的整體 `overall_score` 當成 3.2 的唯一決策依據。

在正式定案流程上，應依序進行：

1. 先比較 `roc` 與 `custom`
2. 若 `roc` 較佳，則以 `roc` 為正式基準，保留 `roc_prev_best`，再做 `roc_down_*`、`roc_bridge_*` 與 `roc_up_*`
3. 若 `custom` 較佳，才沿 `custom` 家族做進一步微調

### Step 6：3.3 補做固定 Pressure 的 Buffer 小實驗

執行：

```r
source("/Users/lee/Documents/BigData/context/build_buffer_scenarios_fixed_pressure.R")
source("/Users/lee/Documents/BigData/context/backtest_buffer_fixed_pressure.R")
```

輸出：

- [buffer_fixed_scenarios_2025.csv](/Users/lee/Documents/BigData/context/buffer_fixed_scenarios_2025.csv)
- [buffer_backtest_summary.csv](/Users/lee/Documents/BigData/context/buffer_backtest_summary.csv)
- [buffer_backtest_port_month.csv](/Users/lee/Documents/BigData/context/buffer_backtest_port_month.csv)

這一步的目的與 3.2 不同，重點是：

- 不再重選 `pressure`
- 直接把 `pressure` 固定為 `roc_prev_best`
- 只比較各 `buffer` 情境的前瞻與分類穩定性

這樣做的好處是：

- 可以把 3.3 的解讀從 3.2 主線中拆開
- 可以直接回答「若 Pressure 已固定，哪一組 Buffer 較合理」
- 可以避免整體 `overall_score` 混入壓力權重差異

### Step 7：3.5 補做固定 Pressure / Buffer 的 Matching Score 實驗

執行：

```r
source("/Users/lee/Documents/BigData/context/build_matching_weight_scenarios.R")
source("/Users/lee/Documents/BigData/context/backtest_matching_fixed_indices.R")
```

輸出：

- [matching_weight_scenarios.csv](/Users/lee/Documents/BigData/context/matching_weight_scenarios.csv)
- [matching_candidate_pairs_fixed_2025.csv](/Users/lee/Documents/BigData/context/matching_candidate_pairs_fixed_2025.csv)
- [matching_parent_backtest_summary.csv](/Users/lee/Documents/BigData/context/matching_parent_backtest_summary.csv)
- [matching_parent_backtest_pair_month.csv](/Users/lee/Documents/BigData/context/matching_parent_backtest_pair_month.csv)
- [matching_backtest_summary.csv](/Users/lee/Documents/BigData/context/matching_backtest_summary.csv)
- [matching_backtest_pair_month.csv](/Users/lee/Documents/BigData/context/matching_backtest_pair_month.csv)

這一步的重點是：

- `pressure` 不再重選，而是固定為 `roc_prev_best`
- `buffer` 不再重選，而是固定為 `custom_nearby_c`
- 先只比較 `matching_score` 的 `roc vs custom`
- 再沿勝出的父方案做鄰近小實驗

## 5. 各腳本在做什麼

### 5.1 `build_weight_scenarios.R`

把權重設定寫成正式資料表，而不是散落在文件裡。

好處：

- 比較不同權重時可重複執行
- 後續若加第三套、第四套權重很方便
- 可直接保留為研究附錄或方法文件

### 5.2 `build_index_scenarios.R`

先根據 [app_formula_design.md](/Users/lee/Documents/BigData/context/app_formula_design.md) 建構各 component：

Pressure：

- `pressure_flow_raw`
- `pressure_net_raw`
- `pressure_roll_raw`

Buffer：

- `buffer_space_raw`
- `buffer_net_raw`
- `buffer_export_raw`

之後在港口內做歷史分位數轉換：

- `pressure_flow_pct`
- `pressure_net_pct`
- `pressure_roll_pct`
- `buffer_space_pct`
- `buffer_net_pct`
- `buffer_export_pct`

最後依 scenario 的權重組合成：

- `pressure_index`
- `buffer_index`

再用月內橫截面門檻判定：

- `高壓力港`
- `高緩衝港`
- `正常港`

### 5.3 `backtest_weight_scenarios.R`

在每個 `scenario × port × ym` 上，建立下一期欄位：

- `next_empty_net`
- `next_roll3_empty_net`
- `next_pressure_index`
- `next_buffer_index`

再彙總成每個情境的比較指標。

但在目前分階段研究策略下：

- `pressure_signal_score` 與高壓力港辨識指標，優先用於 3.2 的主方案決定
- `buffer_signal_score` 與 `high_buffer_safety_rate`，先視為 3.3 的補充觀察
- `overall_score` 則作為整體 scenario 的參考，不作為 3.2 的單獨定案依據

### 5.4 `build_pressure_scenarios.R`

這支腳本的用途是把 3.2 Pressure 權重分析獨立出來。

它會：

- 讀入歷史特徵表
- 只保留 `index_type == "pressure"` 的權重情境
- 在每個月 `t` 用 `1 ~ t` 月資料計算當期 percent-rank
- 產生各港各月在不同 Pressure 權重下的 `pressure_index`

這樣做的好處是：

- 可以避免 Buffer 權重尚未定案時，干擾 Pressure 主方案判斷
- 可以讓 3.2 的比較邏輯更乾淨
- 可以直接對應 `roc vs custom` 與 ROC 上下兩側敏感度分析的正式流程

### 5.5 `backtest_pressure_only.R`

這支腳本會把 `pressure_scenarios_2025.csv` 做成 T+1 的 Pressure-only 回測結果。

它會：

- 建立 `next_empty_net`
- 建立 `next_roll3_empty_net`
- 計算各情境的壓力前瞻能力與分類命中率
- 輸出每個情境的 summary 與每月逐港結果

這支腳本是目前 3.2 正式決策的主要依據。

## 6. 如何解讀結果

### 6.1 第一階段：只解讀 Pressure

若目前要決定的是 3.2 Pressure 權重，應優先看：

- `pressure_lead_corr`
- `pressure_roll_lead_corr`
- `high_pressure_hit_rate`
- `high_pressure_roll_hit_rate`

若某組權重在這些指標上較穩，則可先作為 Pressure 主方案。

就目前這版 Pressure-only 回測結果而言，正式解讀順序應為：

1. 先比較 `roc` 與 `custom`
2. 結果顯示 `roc` 優於 `custom`
3. 因此先把 `roc` 定為 3.2 的正式基準
4. 再納入 `roc_prev_best`、`roc_down_*`、`roc_bridge_*` 與 `roc_up_*` 做敏感度分析
5. 依回測結果選擇 ROC 鄰近家族中表現最佳者，作為擬採方案

因此現階段的正式寫法應是：

- `roc` 是基準方案
- `roc_prev_best` 是前一輪最佳候選，必須保留作為比較基準
- `roc_bridge_*` 是補足 `roc_prev_best` 與 `roc_up_3` 之間的橋接測試
- `roc_down_*` 與 `roc_up_*` 是 ROC 基準下的局部敏感度分析
- 目前重新回測後，`roc_prev_best` 在 ROC 鄰近情境中排名最高
- `custom` 與 `custom_nearby_*` 目前保留為探索性參考，不作為本階段正式主方案

如果要用一句最簡單的話說明目前檔案角色，可以寫成：

- `weight_scenarios.csv`：我測了哪些權重
- `pressure_scenarios_2025.csv`：這些權重算出了什麼 pressure 指標
- `pressure_backtest_summary.csv`：最後哪個權重比較好
- `pressure_backtest_port_month.csv`：這個結論是不是穩定，不是只靠少數港口或少數月份撐起來

### 6.2 第二階段：Buffer 後續再補

目前已先補做一輪固定 `pressure = roc_prev_best` 的 Buffer 小實驗。

若依這一輪結果看，暫時較值得優先保留的 Buffer 候選為：

- `custom_nearby_c`
- `custom_nearby_b`
- `custom`

其中目前 `custom_nearby_c` 排名最高，代表在現有資料下：

- `buffer_space_pct = 0.45`
- `buffer_net_pct = 0.40`
- `buffer_export_pct = 0.15`

這組配置比 `roc` 與原始 `custom` 更能維持高緩衝港的下一期穩定性。

但因為這仍是小實驗，因此正式寫法應保守一些：

- `custom_nearby_c` 是目前 3.3 的暫定優先候選
- `custom_nearby_b` 與 `custom` 可保留作為鄰近敏感度對照
- `roc` 則作為理論基準，但目前在固定 Pressure 條件下表現不如前述幾組

後續若要繼續補強 3.3，再回頭用：

- `buffer_lead_corr`
- `buffer_next_pressure_corr`
- `high_buffer_safety_rate`
- `high_buffer_nonpositive_roll_rate`

去決定 Buffer 權重是否需要微調。

### 6.3 主方案與敏感度分析一起看

建議把結果分成三層解讀：

1. `roc`
   - 理論基準方案
2. `roc_prev_best`
   - 前一輪最佳候選
3. `roc_down_*` / `roc_up_*`
   - 基準方案的上下兩側局部敏感度分析
4. `roc_bridge_*`
   - 兩個高分候選間的橋接敏感度分析
5. `custom`
   - 研究者初始假設方案
6. `custom_nearby_*`
   - 研究導向方案的探索性比較

若 `roc` 在第一輪比較中較佳，且 ROC 上下兩側敏感度分析中又存在更穩定版本，則可主張：

- ROC 作為基準方向是合理的
- 在不偏離基準邏輯的前提下，權重仍可做有限度優化
- 3.2 正式方案可採 `roc` 或 ROC 鄰近情境中表現較佳者

重新回測後，正式敘事應寫成：

- 第一輪比較：`roc > custom`
- 第二輪敏感度分析：比較 `roc_prev_best`、`roc_down_*`、`roc_bridge_*` 與 `roc_up_*`
- 目前 `roc_prev_best` 排名最高，因此擬採 `roc_prev_best` 作為 3.2 Pressure 正式權重
- 這表示上一輪找到的 `0.600 / 0.220 / 0.180` 仍比本輪新增的上下延伸與橋接方案更合適
- `roc_bridge_1`、`roc_bridge_2`、`roc_bridge_3` 緊接在後，代表較佳區域確實靠近 `roc_prev_best`
- `roc_up_3` 雖然仍表現良好，但橋接結果顯示繼續把 `pressure_net` 提高到 `0.700` 並沒有優於 `roc_prev_best`

### 6.4 如果鄰近權重差異很大

若 `custom_nearby_*` 的表現差異很大，則代表：

- 權重設定對結果非常敏感
- 目前資料可能不足以穩定支持精細權重差異
- 後續應更保守地把 ROC 作為基準，並將自治權重描述為暫定方案

### 若 ROC 權重分數較高

代表：

- 僅用排序資訊推得的理論權重，已足夠支撐你的資料結構
- 研究可以主張 ROC 為較中立、較方法論化的基準方案
- 若 ROC 鄰近情境更佳，則可進一步主張是在 ROC 基準邏輯下做有限度微調，而不是完全改走另一套方向

### 若自治權重分數較高

代表：

- 依研究敘事強調「實際堆積」與「近期累積」的設計，更符合台灣港口歷史資料特性
- 可以支持你在 [權重資料.md](/Users/lee/Documents/BigData/context/權重資料.md) 裡對權重邏輯的論述

### 若兩者差異不大

代表：

- 權重排序的重要性大於精準數值
- 研究可以主張 ROC 提供理論基準，而自治權重提供情境彈性
- 若鄰近權重情境也落在相近區間，則可進一步說明研究結論對合理小幅權重變動具有穩健性

## 7. 限制與注意事項

### 7.1 目前資料期間偏短

如果目前主要是 2025 年資料，樣本期數有限。  
這代表：

- 分位數與相關性指標較容易受少數月份影響
- 回測結果應解讀為「初步比較」，不是最終定論

### 7.2 這不是實際調度效果驗證

目前沒有真實跨港調度量、實際成本與政策執行結果資料，因此本回測評估的是：

- 指標辨識能力
- 分類穩定性
- 排序合理性

不是直接驗證「真的調了之後改善多少」。

### 7.3 Buffer 仍是公開資料 proxy

如 [app_formula_design.md](/Users/lee/Documents/BigData/context/app_formula_design.md) 所述，`buffer_index` 目前仍是公開資料下的承接潛力 proxy，而不是真實堆場剩餘容量。

這也代表目前若把 Pressure 與 Buffer 完全綁在一起做主方案決定，會讓 3.2 的定案受到 3.3 尚未穩定部分的干擾。因此目前更合理的做法是先處理 3.2，再補 3.3。

### 7.4 本版不處理跨年度季節性

理論上，港口空櫃壓力可能存在「不同年但同月份」的季節特性，例如 `2025/09` 與 `2026/09` 的可比性。  
但目前資料條件仍較適合支撐 `T+1` 的短期辨識問題，因此本版回測不將跨年度季節性作為主分析主題。未來若能取得更長期資料，再另行建立季節性比較或月別固定效果模型會更穩健。

## 8. 建議下一步

目前較合理的下一步不是再回頭混合搜尋整體 scenario，而是依照目前已完成的分階段結果繼續寫作與定案：

1. 在文件中明確寫出第一輪比較為 `roc vs custom`
2. 說明第一輪結果為 `roc` 較佳，因此選 `roc` 作為正式基準
3. 接著說明第二輪納入 `roc_prev_best`、`roc_down_*`、`roc_bridge_*` 與 `roc_up_*` 做敏感度分析
4. 目前 `pressure_backtest_summary.csv` 顯示 `roc_prev_best` 排名最高，因此擬採 `roc_prev_best`
5. 目前 `buffer_backtest_summary.csv` 顯示 `custom_nearby_c` 排名最高，因此 3.3 可暫採 `custom_nearby_c`
6. 目前 `matching_parent_backtest_summary.csv` 顯示 3.5 父方案由 `custom` 勝出，而 `matching_backtest_summary.csv` 顯示 `custom_nearby_b` 為暫定優先候選
7. 下一步應回寫 [app_formula_design.md](/Users/lee/Documents/BigData/context/app_formula_design.md)，決定 3.5 要採：
   - 較保守的 `custom`
   - 或較積極的暫定候選 `custom_nearby_b`
