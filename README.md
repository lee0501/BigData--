# 航港大數據：空櫃失衡辨識與跨港媒合決策機制

## 專案定位

本專案要解決的不是「台灣整體空櫃夠不夠」，而是「空櫃在不同港口之間分布不均，導致某些港口堆積、某些港口仍有承接空間」的問題。  
因此，專案的核心不是單純做一個看板，而是建立一套可以被解釋、被回測、被落地使用的決策支援流程：

1. 辨識哪些港口的空櫃壓力已經偏高，應優先釋放。
2. 辨識哪些港口目前仍有承接緩衝，不會因接收部分空櫃就快速轉為高壓力港。
3. 建立高壓力港與高緩衝港的跨港候選組合，並給出具優先順序的調度建議。
4. 進一步模擬不同移轉比例下，來源港是否能解壓、目標港是否仍能維持安全。

換句話說，這個專案是在把原本偏經驗判斷的空櫃調度問題，轉成一套以公開資料支撐的港口層級決策模型。

[詳細資料請參考 `context/bigdata.md`](context/bigdata.md) 與 [詳細資料請參考 `context/app_formula_design.md`](context/app_formula_design.md)。

## 為什麼這個專案有說服力

這個專案的說服力來自四個層面。

- 它回答的是實務上真正需要被回答的問題。港口管理單位與決策者在意的不是漂亮圖表，而是「哪裡已經有壓力」、「哪裡還能接」、「哪一組最值得先處理」。
- 它使用的是公開資料可重現的流程。即使沒有航商內部資料，仍然可以建立一套可比較、可解釋的 proxy-based decision model。
- 它不是用一組主觀權重直接下結論，而是把 `Pressure`、`Buffer`、`Matching` 都放進一致的 `T+1` 回測框架裡比較，檢查哪一種設計對下一期更有辨識力。
- 它的產出不是停在理論公式，而是直接落到 `status_final_2025.csv`、`matching_final_2025.csv`、`simulation_recommendation_final_2025.csv`，可以接上儀表板、簡報與決策討論。

## 這個專案到底在回答什麼

本專案目前聚焦回答三個問題。

1. 哪些港口已經出現空櫃累積壓力，應優先釋放空櫃。
2. 哪些港口在現況下仍具有承接能力，可作為候選承接端。
3. 在目前公開資料與簡化距離規則下，哪些跨港組合最值得優先列入調度清單。

此外，本專案新增一層「官方多年統計基準」作為輔助判讀。這一層不取代 iMarine 的 2025 明細壓力模型，而是回答第四個問題：

4. iMarine 判出的高壓港，是不是同時也接近該港自己的多年官方統計高位，或只是 2025 明細資料下的短期作業壓力？

這個邏輯採用的是 **push-based** 思維。起點不是先問「哪裡缺櫃」，而是先問「哪裡的空櫃已經累積到不處理不行」。這樣的設計比較符合本專案目前可取得的資料條件，也更貼近港口壓力釋放的管理情境。

## 核心方法

### 1. 先把公開資料整合成港口月別主表

專案先把各種公開資料整合成 `港口 × 年月` 的主表，包含：

- `empty_in`：空櫃進港量
- `empty_out`：空櫃出港量
- `full_in`：實櫃進口量
- `full_out`：實櫃出口量
- `throughput`：港口總貨櫃裝卸量

接著再衍生：

- `empty_net = empty_in - empty_out`
- `empty_total_flow = empty_in + empty_out`
- `roll3_empty_net`：近三月平均空櫃淨流量
- `cum_empty_net`：累積空櫃淨流量
- `export_pull = full_out / throughput`

這一步的目的，是把原本分散的流量資料轉成一張後續所有分析都共用的基底表。

### 1.1 以港務公司多年統計建立官方基準

由於 iMarine 明細資料目前主要支撐 2025 年的方向性壓力判讀，本專案另外整合交通統計查詢網中的臺灣港務股份有限公司官方統計：

- `國際商港貨櫃裝卸量(個數)`：提供各港空櫃、實櫃與總櫃個數。
- `國際商港貨櫃裝卸量(TEU)─按進出港分`：提供各港進港、出港與總 TEU。

其中 Prophet 主模型只使用官方原始表中直接存在的「空櫃個數、實櫃個數、總櫃個數、空櫃占比」，避免以 TEU 換算值作為主要結論。這一層輸出的用途是建立同一港口跨多年月份的基準線，用於判斷某月是否接近該港自己的多年高位。

需要注意：iMarine 狀態分類使用的是「同月四港之間的相對門檻（月內 Q75 / Q50）」；官方基準頁使用兩條不同的比較線：

- **Bridge 對照**：使用 Prophet `yhat`（同月份多年季節性預期），回答 iMarine 高壓港當月是否高於往年同期預期。
- **優先檢查清單**：使用同港多年歷史 Q75 當分級門檻，供提前關注使用，非正式高壓判定。

三者口徑各不相同，不能混用。

### 2. 用 `Pressure Index` 辨識「需要先釋放」的港口

`Pressure Index` 的重點不是看絕對櫃量，而是看某港口相對於自己的歷史狀態，是否已經偏高。

目前正式輸出層採用的是 `roc_prev_best`：

```text
pressure_index =
0.22 * pct(pressure_flow_raw) +
0.60 * pct(pressure_net_raw) +
0.18 * pct(pressure_roll_raw)
```

其中：

- `pressure_flow_raw`：當月空櫃進港占整體作業規模比重
- `pressure_net_raw`：當月是否真的淨增加
- `pressure_roll_raw`：近三個月是否持續淨增加
- `pct(x)`：把某港口當期值放回該港口自己的歷史序列，看它落在自己歷史的相對分位

這組權重的意思很清楚：本專案目前最重視的是「這個港本月是否真的在堆積」，其次才是「今天進來的量大不大」，最後才是「這個趨勢持續多久」。

### 3. 用 `Buffer Index` 辨識「可以承接」的港口

`Buffer Index` 不是 `1 - pressure`。  
承接能力的重點不只是「現在壓力低」，而是「承接一些空櫃後，是否仍不容易快速轉為高壓力港」。

目前 `status_final_2025.csv` 採用的 `Buffer` 權重是 `custom_nearby_c`：

```text
buffer_index =
0.45 * pct(buffer_space_raw) +
0.40 * pct(buffer_net_raw) +
0.15 * pct(buffer_export_raw)
```

其中：

- `buffer_space_raw`：目前空櫃進港壓力相對低
- `buffer_net_raw`：近三月偏向淨流出，不易堆積
- `buffer_export_raw`：出口活動較強，代表可能較能吸收空櫃

這代表目前的承接能力判斷，不是只看「還有沒有空間」，而是同時看「近期是不是本來就不容易累積」。

### 4. 用 `Status` 把港口分成三類

專案不是直接把所有港口排成一條線，而是先做狀態分類：

```text
高壓力港：pressure_index >= Q75 且 roll3_empty_net > 0
高緩衝港：buffer_index >= Q75 且 pressure_index <= Q50
正常港：其餘
```

這裡的 `Q75` 與 `Q50` 不是全年固定值，而是每個月份在當期港口橫截面上重新計算的相對門檻。  
目的很明確：高壓力港要真的處在相對高壓區段，而且近期仍在累積；高緩衝港則要同時具備高緩衝、且目前不高壓。

### 5. 用 `Matching Score` 產出跨港候選排序

只有在下列條件同時成立時，才建立候選配對：

- `source.status == 高壓力港`
- `target.status == 高緩衝港`
- `source_port != target_port`

目前正式輸出層的 `matching_final_2025.csv` 採用較保守的 `custom` 權重：

```text
matching_score =
0.45 * source_pressure_index +
0.35 * target_buffer_index +
0.20 * distance_factor
```

這代表排序邏輯是：

- 先看來源港有沒有迫切需要釋放
- 再看目標港能不能穩定承接
- 最後用距離阻力作為執行難度的修正

補充說明：  
`Matching` 的探索性回測中，`custom_nearby_b` 的表現略優於 `custom`，但差距不大，因此目前正式輸出腳本仍保留較保守、較容易說明的 `custom = 0.45 / 0.35 / 0.20`。

### 6. 用模擬把排序變成「可討論的方案」

排序只能回答「哪一組值得先看」，模擬則進一步回答「如果真的移一部分，會發生什麼事」。

目前模擬採：

```text
move_share ∈ {0, 0.25, 0.50, 0.75}
move_amount = move_share * max(source_empty_net, 0)
```

然後重新估計來源港與目標港的：

- `pressure_index_after`
- `buffer_index_after`
- `status_after`
- `pair_success`

因此，專案最後不是只給一個高分港對，而是能進一步給出：

- 可完全解壓的建議比例
- 可部分緩解但未完全解壓的比例
- 不建議調度的組合

[詳細資料請參考 `context/weight_backtest_plan.md`](context/weight_backtest_plan.md) 與 [詳細資料請參考 `context/app_formula_design.md`](context/app_formula_design.md)。

## 目前專案做到哪裡

截至目前，專案已完成從資料整併、特徵建構、權重回測、狀態分類、跨港排序到情境模擬的完整主線，並且已有可直接供儀表板讀取的最終輸出。

目前正式輸出層的關鍵設定如下：

- `Pressure`：採 `roc_prev_best = 0.22 / 0.60 / 0.18`
- `Buffer`：採 `custom_nearby_c = 0.45 / 0.40 / 0.15`
- `Matching`：正式輸出採 `custom = 0.45 / 0.35 / 0.20`
- `Simulation`：採固定比例移轉 `{0, 25%, 50%, 75%}` 的基準模擬

目前主要成果檔案如下：

- [`context/status_final_2025.csv`](context/status_final_2025.csv)：每月各港口的最終狀態表，回答哪些港口是高壓力港、哪些是高緩衝港。
- [`context/matching_final_2025.csv`](context/matching_final_2025.csv)：各月份的跨港候選組合與排序結果。
- [`context/simulation_recommendation_final_2025.csv`](context/simulation_recommendation_final_2025.csv)：每組候選港對的最終模擬建議與訊息文字。
- [`iMarine_dashboard_fixed_R3_compatible.R`](iMarine_dashboard_fixed_R3_compatible.R)、[`iMarine_dashboard_local_fixed_v3.R`](iMarine_dashboard_local_fixed_v3.R)、[`iMarine_dashboard_stable_local.R`](iMarine_dashboard_stable_local.R)：讀取上述結果表並呈現在本地儀表板。

## 正式分析宇宙與目前取捨

本專案在資料層保留的港口不只四個，但正式比較與媒合分析目前聚焦在：

- 基隆港
- 臺北港
- 臺中港
- 高雄港

這樣做不是任意挑選，而是有資料品質上的理由：

- 花蓮港與蘇澳港在 `throughput` 上存在全年或長期接近零的情況，不適合作為正式分析樣本。
- 安平港雖保留在上游資料表中，但 2025 年 `empty_in`、`empty_out`、`full_in`、`full_out` 與 `empty_net` 幾乎全為零，因此在 `status_final_2025.csv` 與 `matching_final_2025.csv` 中被排除，以避免扭曲橫截面門檻。

這樣的取捨，讓目前主線更聚焦於真正有代表性、且具有比較意義的國際商港。

## 如果被老師提問我們專案就是不離開以下面向

（先講管理問題，再講決策流程）

### 回覆

本專案用公開航港資料，把原本依賴經驗的空櫃調度問題，轉成一套可辨識、可排序、可模擬的跨港決策支援流程。它先找出哪些港口空櫃已經累積過多，再找出哪些港口仍有承接緩衝，最後產出具優先順序的跨港調度建議與模擬結果。

台灣港口的空櫃問題，很多時候不是總量不足，而是分布不均。某些港口會累積過多空櫃，造成堆場壓力與作業負擔，另一些港口則還有承接空間。這個專案的重點，就是用公開資料建立 `Pressure Index`、`Buffer Index` 與 `Matching Score`，把「哪裡該先釋放」、「哪裡可以承接」、「先移哪一組」轉成具體而且可回測的決策流程。它的價值不只是看趨勢，而是把跨港調度從經驗判斷，提升成可解釋、可比較的資料驅動支援工具。

### 重點

- 這不是在做一個靜態看板，而是在做一套港口層級的空櫃調度決策模型。
- 這套模型可以用公開資料重現，且已經透過 `T+1` 回測比較權重方案，而不是只靠主觀設定。
- 它最後輸出的不是抽象分數，而是港口狀態、跨港候選清單與調度比例建議，能直接支援溝通與管理。

## 資料來源

目前主資料源包括：

- `臺灣進口貨空櫃`
- `臺灣出口貨空櫃`
- `臺灣進口貨實櫃`
- `臺灣出口貨實櫃`
- `臺灣地區國際商港貨櫃裝卸量`
- `Distance Matrix` 距離矩陣（依照距離給予權重分數）

這些資料在專案中被標準化為 `ym × port` 結構，並進一步轉為分析欄位與決策輸出表。

[詳細資料請參考 `context/bigdata.md`](context/bigdata.md)。

## 目前限制

本專案的強項是「辨識、排序與決策支援」，沒有真實掌握真實庫存、真實堆場容量或真實最適調度成本（因為政府公開資料庫沒有）。

- `pressure_index`：公開資料下的港口空櫃壓力 proxy
- `buffer_index`：公開資料下的港口承接潛力 proxy
- `matching_score`：港口層級的跨港調度優先順序建議

下面的講法要著重避免（跟之前查閱的論文講法不能一樣）：

- 真實空櫃庫存
- 真實剩餘容量
- 最佳化調度解
- 已可直接落地到航商營運層的執行方案

[詳細資料請參考 `context/app_formula_design.md`](context/app_formula_design.md) 與 [詳細資料請參考 `context/weight_backtest_plan.md`](context/weight_backtest_plan.md)。

## 專案檔案閱讀順序

1. [`README.md`](README.md)：先建立整體專案故事與價值主張。
2. [`context/bigdata.md`](context/bigdata.md)：看研究背景、核心問題、資料來源與研究架構。
3. [`context/app_formula_design.md`](context/app_formula_design.md)：看指標、狀態分類、媒合與模擬公式。
4. [`context/file_guide.md`](context/file_guide.md)：看所有 `R` 腳本、`csv` 產物、彼此依賴關係與執行順序。
5. [`context/weight_backtest_plan.md`](context/weight_backtest_plan.md)：看權重回測方法、目前結果與限制。
6. [`context/權重資料.md`](context/%E6%AC%8A%E9%87%8D%E8%B3%87%E6%96%99.md)：看權重設計的理論與研究依據。

## 執行與輸出主線

如果要重跑目前的正式輸出，主線大致如下：

1. 建立主表：`build_master_table.R`
2. 檢查資料品質：`check_master_table.R`
3. 篩出正式分析樣本：`build_analysis_table.R`
4. 建立歷史特徵：`build_history_features.R`
5. 產出權重情境：`build_weight_scenarios.R`
6. 先定案 `Pressure`：`build_pressure_scenarios.R` -> `backtest_pressure_only.R`
7. 固定 `Pressure` 後比較 `Buffer`：`build_buffer_scenarios_fixed_pressure.R` -> `backtest_buffer_fixed_pressure.R`
8. 產出正式狀態表：`build_status_final_2025.R`
9. 產出正式媒合表：`build_matching_final_2025.R`
10. 產出模擬表與建議表：`build_simulation_final_2025.R` -> `build_simulation_recommendation_final_2025.R`

## Forecast 預警實驗

這條支線是 **預警實驗**，不是正式主線的替代品。

- 目標：提早一個月預警哪些正式四港，下一月較可能成為 `高壓力港`。
- 定位：補在 `Pressure -> Matching -> Simulation` 之前，作為前瞻 watchlist。
- 原則：正式口徑與暖機口徑分開回測，不混成同一組結論。

目前已新增：

- `context/backtest_forecast_high_pressure.R`
- `context/forecast_high_pressure_backtest_summary.csv`
- `context/forecast_high_pressure_backtest_detail.csv`
- `context/forecast_high_pressure_latest_watchlist.csv`

本輪實驗固定使用正式 `Pressure` 方案 `roc_prev_best` 與正式四港，並比較兩種簡單可解釋的排序方式：

- `baseline_pressure_index`：直接用當月 `pressure_index` 做下月風險排序
- `pressure_index_streak_boost`：在 `pressure_index` 上補一個連續堆積月數加權，用來強化早期預警

其中：

- `official`：來源月 `history_n >= 3`
- `warmup_included`：允許 `2025-02 -> 2025-03` 納入，但仍要求目標月已進入 `history_n >= 3`

所有腳本與輸出對照，請直接看 [`context/file_guide.md`](context/file_guide.md)。

## 總結

這個專案最重要的價值，不在於它把多少公式堆進去，而在於它把一個原本很難說清楚、很容易落回經驗判斷的港口問題，整理成一套有邏輯順序的決策流程：

先辨識壓力，再辨識緩衝，再建立候選組合，再做排序，最後用模擬補上可執行性的討論。

因此，這份專案的定位著重在：

> 一套以公開資料為基礎、以港口壓力釋放為核心、可回測且可視覺化的空櫃跨港媒合決策支援機制。
