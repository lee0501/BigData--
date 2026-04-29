# `app.R` 指標公式設計草案

本文件用於重新整理 `app.R` 目前的指標邏輯，但**不修改原始 `app.R`**。  
設計原則以 [bigdata.md](/Users/lee/Documents/BigData/context/bigdata.md) 為準，目標是讓儀表板從「展示型 prototype」往「公開資料支撐的跨港決策支援工具」靠近。

---

## 一、設計原則

依照 [bigdata.md](/Users/lee/Documents/BigData/context/bigdata.md) 的研究訴求，公式設計需符合以下原則：

1. 採 **push-based** 邏輯  
   先找出「需釋放空櫃」的港口，再找「可承接」港口。

2. 同時保留三個分析構面  
   - 空櫃流動構面
   - 港口壓力構面
   - 承接緩衝構面

3. 優先使用 **公開資料可直接取得或可推導** 的欄位  
   避免依賴航商內部資料。

4. 分類採 **相對門檻**  
   以歷史分位數、標準化分數或相對容量判斷，不建議只用單一固定門檻。

5. 媒合排序要可解釋  
   不追求黑盒最佳化，而是能說明「為什麼這一組港對優先」。

---

## 二、欄位來源整理

### 2.1 可直接由公開資料取得的欄位

下列欄位可直接來自 [bigdata.md](/Users/lee/Documents/BigData/context/bigdata.md) 規劃的資料集：

| 欄位 | 說明 | 來源 |
|---|---|---|
| `ym` | 年月 | 各資料集原始欄位 |
| `port` | 港口名稱 | 各資料集原始欄位 |
| `empty_in` | 空櫃進港量 | 臺灣進口貨空櫃 |
| `empty_out` | 空櫃出港量 | 臺灣出口貨空櫃 |
| `full_in` | 實櫃進口量 | 臺灣進口貨實櫃 |
| `full_out` | 實櫃出口量 | 臺灣出口貨實櫃 |
| `throughput` | 港口總貨櫃裝卸量 | 國際商港貨櫃裝卸量（TEU）或主計總處備援資料 |

### 2.2 可由公開資料直接推導的衍生欄位

| 欄位 | 公式 | 說明 |
|---|---|---|
| `empty_net` | `empty_in - empty_out` | 當月空櫃淨流量 |
| `empty_total_flow` | `empty_in + empty_out` | 當月空櫃活動量 |
| `full_total_flow` | `full_in + full_out` | 當月實櫃活動量 |
| `empty_share` | `empty_total_flow / throughput` | 空櫃活動占整體作業比重 |
| `net_pressure_raw` | `empty_net / throughput` | 當月淨累積壓力的相對值 |
| `export_pull` | `full_out / throughput` | 出口活動強度，作為空櫃需求背景 |

### 2.3 需由歷史資料計算的欄位

這些欄位仍可完全用公開資料算，但必須先建立 `港口 × 年月` 歷史序列：

| 欄位 | 概念 |
|---|---|
| `cum_empty_net` | 空櫃淨流量累積值 |
| `roll3_empty_net` | 近 3 個月平均淨流量 |
| `roll3_pressure_raw` | 近 3 個月平均相對壓力 |
| `pressure_z` | 港口內歷史標準化壓力分數 |
| `buffer_z` | 港口內歷史標準化緩衝分數 |
| `pressure_pct` | 壓力分位數 |
| `buffer_pct` | 緩衝分位數 |
| `positive_net_streak` | 連續淨流入月數 |

### 2.4 非公開資料或需額外人工建置的欄位

| 欄位 | 取得方式 | 用途 |
|---|---|---|
| `distance_class` | 人工建立港口對照表 | 南北距離阻力 |
| `region_pair_weight` | 人工規則設定 | 區域/政策修正因子 |
| `port_role_weight` | 人工規則設定 | 若後續要納入樞紐港/支線港角色 |

---

## 三、公式設計草案

## 3.1 空櫃流動構面

這一層負責回答「這個港最近是不是一直在累積空櫃」。

### 基礎公式

```text
empty_net = empty_in - empty_out
cum_empty_net_t = Σ(empty_net_1 ... empty_net_t)
roll3_empty_net = mean(empty_net_t, empty_net_t-1, empty_net_t-2)
```

### 解讀方式

- `empty_net > 0`：當月空櫃流入大於流出，偏向累積
- `cum_empty_net` 持續上升：長期累積風險
- `roll3_empty_net > 0`：近期仍有持續累積趨勢

### 對應 markdown 訴求

這一層直接對應 [bigdata.md](/Users/lee/Documents/BigData/context/bigdata.md#L245) 的空櫃流動構面，以及 [bigdata.md](/Users/lee/Documents/BigData/context/bigdata.md#L370) 要求的淨流量與累積趨勢分析。

---

## 3.2 Pressure Index 設計

`app.R` 目前用 `empty_in / throughput` 當壓力，過於單薄。  
較合理的做法是把「當月累積壓力」與「近期持續累積」一起納入。

### 建議分兩層

#### 第一步：先算原始壓力成分

```text
pressure_flow_raw = empty_in / throughput
pressure_net_raw  = pmax(empty_net, 0) / throughput
pressure_roll_raw = pmax(roll3_empty_net, 0) / throughput
```

說明：

- `pressure_flow_raw`：空櫃進港占港口作業規模比重
- `pressure_net_raw`：當月是否「淨增加」
- `pressure_roll_raw`：近 3 個月是否持續淨增加

#### 第二步：標準化後組合

建議以「同港歷史」做標準化，避免不同港口因規模差異直接硬比。

### `baseline`、`custom` 與目前擬採值

[權重資料.md](/Users/lee/Documents/BigData/context/權重資料.md:12) 到 [權重資料.md](/Users/lee/Documents/BigData/context/權重資料.md:18) 已將權重來源分為三類：

- 交通部資料：作為研究與營運背景依據
- `ROC`：作為只知道重要性排序時的理論基準情境
- `custom`：作為研究導向的自訂情境

其中 [權重資料.md](/Users/lee/Documents/BigData/context/權重資料.md:59) 到 [權重資料.md](/Users/lee/Documents/BigData/context/權重資料.md:76) 也明確說明：

- `baseline` 應理解為方法論上的比較基準，而非直接定案值
- `custom` 應理解為研究敘事下的初始預設值，最終仍需由 `T+1` 回測決定

因此 3.2 目前應區分三個層次：

1. `baseline (ROC)`  
   若僅知道 `pressure_net > pressure_flow > pressure_roll` 的重要性排序，則可採三構面 ROC 權重 `0.611 / 0.278 / 0.111`。  
   其情境定義與實作可見 [build_weight_scenarios.R](/Users/lee/Documents/BigData/context/build_weight_scenarios.R:15) 到 [build_weight_scenarios.R](/Users/lee/Documents/BigData/context/build_weight_scenarios.R:18)、[build_weight_scenarios.R](/Users/lee/Documents/BigData/context/build_weight_scenarios.R:49) 到 [build_weight_scenarios.R](/Users/lee/Documents/BigData/context/build_weight_scenarios.R:51)。

   ```text
   pressure_index_roc =
   0.278 * pct(pressure_flow_raw) +
   0.611 * pct(pressure_net_raw) +
   0.111 * pct(pressure_roll_raw)
   ```

2. `custom (研究初始預設值)`  
   依研究敘事，初始假設版將 `pressure_net_raw` 設為核心構面，並保留 `pressure_flow_raw` 與 `pressure_roll_raw` 的輔助角色。  
   其預設值可見 [build_weight_scenarios.R](/Users/lee/Documents/BigData/context/build_weight_scenarios.R:82) 到 [build_weight_scenarios.R](/Users/lee/Documents/BigData/context/build_weight_scenarios.R:84)，研究定位說明則見 [權重資料.md](/Users/lee/Documents/BigData/context/權重資料.md:65) 到 [權重資料.md](/Users/lee/Documents/BigData/context/權重資料.md:76)。

   ```text
   pressure_index_custom =
   0.35 * pct(pressure_flow_raw) +
   0.40 * pct(pressure_net_raw) +
   0.25 * pct(pressure_roll_raw)
   ```

3. `目前擬採正式值`  
   目前 `Pressure-only` 回測結果中，`roc_prev_best` 排名第 1，見 [pressure_backtest_summary.csv](/Users/lee/Documents/BigData/context/pressure_backtest_summary.csv:2)；其鄰近 ROC 家族的比較邏輯與流程紀錄見 [weight_backtest_plan.md](/Users/lee/Documents/BigData/context/weight_backtest_plan.md:140) 到 [weight_backtest_plan.md](/Users/lee/Documents/BigData/context/weight_backtest_plan.md:205)。  
   因此若以目前研究主線的暫定正式版本來看，3.2 應採 `roc_prev_best`：

   ```text
   pressure_index =
   0.22 * pct(pressure_flow_raw) +
   0.60 * pct(pressure_net_raw) +
   0.18 * pct(pressure_roll_raw)
   ```

其中 `pct(x)` 表示該港口在自身歷史序列中的分位數，範圍 `0~1`。<br>
**pct(x)**
把某港口某指標，放回它自己的歷史序列裡看
看它目前落在自己歷史的第幾分位
<br>
若此公式要用於 **T+1 驗證**，則 `pct(x)` 不得用完整年度資料一次計算，而必須在每一個評估時點 `t` 僅用 `1 ~ t` 月可得資料重新估計。換言之，當要評估 `t+1` 月時，只能用 `t` 月以前已觀察到的資料來算當期分位數。

這組結果代表：在目前資料條件下，`pressure_net_raw` 對辨識下期仍持續累積之高壓力港的解釋力高於 `pressure_flow_raw` 與 `pressure_roll_raw`。因此正式設計上應優先強調「當月是否真的淨增加」，再輔以「流入占比」與「近三月趨勢」。

### 解讀方式

- 越接近 `1`：代表該港目前空櫃壓力相對自身歷史偏高
- 越接近 `0`：代表壓力相對低

### 為什麼這樣設計

- 保留 `throughput` 正規化概念，符合 [bigdata.md](/Users/lee/Documents/BigData/context/bigdata.md#L176) 到 [bigdata.md](/Users/lee/Documents/BigData/context/bigdata.md#L194)
- 不只看「進港量」，也看「是否真的累積」
- 不只看單月，也看短期持續性

---

## 3.3 Buffer Index 設計

`buffer` 不應該只是 `1 - pressure`。  
承接能力的重點不是「現在壓力低而已」，而是「承接一些外移空櫃後仍不容易變成高壓力港」。

因此，本研究不將 `buffer` 視為 `pressure` 的反函數，而將其定義為港口在當期條件下承接外移空櫃後，仍不易快速轉為高壓力狀態之相對緩衝能力指標。換言之，`buffer_index` 回答的不是「這個港現在壓力有多低」，而是「這個港若承接部分外移空櫃，是否仍較不容易在下一階段快速進入高壓力狀態」。

在目前公開資料限制下，`buffer_index` 應解讀為港口承接潛力的 proxy，而非真實剩餘容量、真實堆場可用空間或可直接執行的承接上限。

### 建議原始成分

```text
buffer_space_raw = 1 - pressure_flow_raw
buffer_net_raw   = pmax(-roll3_empty_net, 0) / throughput
buffer_export_raw = export_pull
```

說明：

- `buffer_space_raw`：目前作業中空櫃進港壓力相對低
- `buffer_net_raw`：近 3 個月偏向淨流出，代表不易堆積
- `buffer_export_raw`：出口活動較強，可能較能吸收空櫃

### 標準化後組合

由於 `buffer_index` 是由 `buffer_space_raw`、`buffer_net_raw` 與 `buffer_export_raw` 三個子構面共同組成的複合指標，而非單一函數，因此其權重設計也應比照 3.2 區分 `baseline`、`custom` 與目前擬採值。

1. `baseline (ROC)`  
   若僅知道 `buffer_space > buffer_net > buffer_export` 的重要性排序，則可採三構面 ROC 權重 `0.611 / 0.278 / 0.111`。  
   其理論定位可參考 [權重資料.md](/Users/lee/Documents/BigData/context/權重資料.md:42) 到 [權重資料.md](/Users/lee/Documents/BigData/context/權重資料.md:63)，情境實作則見 [build_weight_scenarios.R](/Users/lee/Documents/BigData/context/build_weight_scenarios.R:100) 到 [build_weight_scenarios.R](/Users/lee/Documents/BigData/context/build_weight_scenarios.R:102)。

   ```text
   buffer_index_roc =
   0.611 * pct(buffer_space_raw) +
   0.278 * pct(buffer_net_raw) +
   0.111 * pct(buffer_export_raw)
   ```

2. `custom (研究初始預設值)`  
   依研究敘事，初始假設版保留 `buffer_space_raw` 為主要構面，並讓 `buffer_net_raw` 與 `buffer_export_raw` 作為承接穩定性與出口吸納能力的輔助構面。  
   其預設值可見 [build_weight_scenarios.R](/Users/lee/Documents/BigData/context/build_weight_scenarios.R:103) 到 [build_weight_scenarios.R](/Users/lee/Documents/BigData/context/build_weight_scenarios.R:105)，研究定位則可參考 [權重資料.md](/Users/lee/Documents/BigData/context/權重資料.md:65) 到 [權重資料.md](/Users/lee/Documents/BigData/context/權重資料.md:76)。

   ```text
   buffer_index_custom =
   0.45 * pct(buffer_space_raw) +
   0.35 * pct(buffer_net_raw) +
   0.20 * pct(buffer_export_raw)
   ```

3. `目前擬採正式值`  
   為避免 `Pressure` 與 `Buffer` 同時變動而難以解讀，本研究先固定 `pressure = roc_prev_best`，再單獨比較 `buffer` 情境，這一輪小實驗流程與評估指標可見 [weight_backtest_plan.md](/Users/lee/Documents/BigData/context/weight_backtest_plan.md:207) 到 [weight_backtest_plan.md](/Users/lee/Documents/BigData/context/weight_backtest_plan.md:290)。  
   目前 `custom_nearby_c` 排名第 1，見 [buffer_backtest_summary.csv](/Users/lee/Documents/BigData/context/buffer_backtest_summary.csv:2)；`custom_nearby_b` 為次佳，見 [buffer_backtest_summary.csv](/Users/lee/Documents/BigData/context/buffer_backtest_summary.csv:3)；`roc` 則在本輪比較中排名最後，見 [buffer_backtest_summary.csv](/Users/lee/Documents/BigData/context/buffer_backtest_summary.csv:8)。  
   因此若以目前 3.3 的暫定正式版本來看，應採 `custom_nearby_c`：

   ```text
   buffer_index =
   0.45 * pct(buffer_space_raw) +
   0.40 * pct(buffer_net_raw) +
   0.15 * pct(buffer_export_raw)
   ```

若後續要比較不同權重方案，則 `buffer_index` 應與 `pressure_index` 採同樣規則：每一個月份只用當時以前的歷史資料重新估計 `pct(x)`，避免未來月份影響當下的緩衝判定。

### 解讀方式

- 越接近 `1`：代表越有承接緩衝
- 越接近 `0`：代表承接後可能很快進入壓力狀態
- 若 `roll3_empty_net` 為正，表示最近也在累積，則 `buffer_net_raw` 會趨近於低值。也就是說，這港即使現在壓力沒那麼高，也不代表它適合接。

### 為什麼這樣設計

- 把「低壓力」和「不易持續累積」分開處理
- 納入 `full_out` 對應的出口活躍程度，較符合「空櫃不足/需求背景」概念
- 仍維持只用公開資料或可直接推導的欄位

---

## 3.4 Status 設計

依 [bigdata.md](/Users/lee/Documents/BigData/context/bigdata.md#L262) 到 [bigdata.md](/Users/lee/Documents/BigData/context/bigdata.md#L276)，分類應採相對門檻，而非固定 TEU 或固定比例。

在港口壓力衡量上，理論上較理想的做法應是以實際吞吐量相對於設計容量或堆場容量作為基準；但目前公開資料缺乏一致且可追溯的 `capacity`、`yard_capacity` 或 `occupancy_ratio`，因此本研究無法直接建立傳統意義上的 utilization ratio。這一點的補充說明可見 [3.4 | 0.5 _ 0.75 定義.md](</Users/lee/Downloads/3.4 | 0.5 _ 0.75 定義.md:7>) 到 [3.4 | 0.5 _ 0.75 定義.md](</Users/lee/Downloads/3.4 | 0.5 _ 0.75 定義.md:18>)。

因此，本研究改以 `throughput` 作為容量代理變數，先在各港口內部用歷史 percent-rank 建立 `pressure_index` 與 `buffer_index`，再於每個月份 `t` 的港口橫截面上，以 percentile-based threshold 進行分類。這裡要特別區分兩層：

- 第一層：`pressure_index`、`buffer_index` 本身，是港口內歷史相對位置
- 第二層：`high_pressure_cut`、`high_buffer_cut`、`low_pressure_cut`，是當月橫截面分類門檻

### 為何採用分位數門檻

[3.4 | 0.5 _ 0.75 定義.md](</Users/lee/Downloads/3.4 | 0.5 _ 0.75 定義.md:11>) 到 [3.4 | 0.5 _ 0.75 定義.md](</Users/lee/Downloads/3.4 | 0.5 _ 0.75 定義.md:61>) 已整理三個核心理由：

- 港口規模高度異質，固定絕對門檻不利跨港比較
- 公開資料缺乏真實 capacity，無法建立一致的 occupancy ratio
- percentile threshold 適合用來辨識相對高區段與相對低區段

### 建議分類規則

先定義：

```text
high_pressure_cut = t 月橫截面 pressure_index 的 0.75 分位
high_buffer_cut   = t 月橫截面 buffer_index 的 0.75 分位
low_pressure_cut  = t 月橫截面 pressure_index 的 0.50 分位
```

其門檻意義如下：

- `Q75` 用於 `high_pressure_cut` 與 `high_buffer_cut`  
  代表當月相對位置落在前 25% 的高區段港口，適合作為 early warning 與優先辨識的 baseline。此點可見 [3.4 | 0.5 _ 0.75 定義.md](</Users/lee/Downloads/3.4 | 0.5 _ 0.75 定義.md:38>) 到 [3.4 | 0.5 _ 0.75 定義.md](</Users/lee/Downloads/3.4 | 0.5 _ 0.75 定義.md:52>)。
- `Q50` 用於 `low_pressure_cut`  
  代表「不高於常態」的上界，用來避免只有極低壓港才能成為承接候選，導致候選港口過少。此點可見 [3.4 | 0.5 _ 0.75 定義.md](</Users/lee/Downloads/3.4 | 0.5 _ 0.75 定義.md:54>) 到 [3.4 | 0.5 _ 0.75 定義.md](</Users/lee/Downloads/3.4 | 0.5 _ 0.75 定義.md:61>)。

若採 `T+1` 架構，則這些 cut 不應以全年度資料回頭統一設定，而應在每個月 `t` 用當月可得的港口橫截面重新計算。

### 狀態判定

```text
if pressure_index >= high_pressure_cut
   and roll3_empty_net > 0
=> 高壓力港

else if buffer_index >= high_buffer_cut
        and pressure_index <= low_pressure_cut
=> 高緩衝港

else
=> 正常港
```

### 補充說明

- `高壓力港` 不能只看 `pressure_index` 高，還需確認近期確實在累積，即 `roll3_empty_net > 0`
- `高緩衝港` 不能只看 `buffer_index` 高，也要排除已經接近高壓力的港口，因此需同時滿足 `buffer_index >= Q75` 與 `pressure_index <= Q50`
- 這套設計的目的是建立 `source（高壓） -> target（高 buffer 且不高壓）` 的分類基礎，避免形成「把櫃子從一個高壓港移到另一個即將高壓的港」的連環問題。這個邏輯可見 [3.4 | 0.5 _ 0.75 定義.md](</Users/lee/Downloads/3.4 | 0.5 _ 0.75 定義.md:48>) 到 [3.4 | 0.5 _ 0.75 定義.md](</Users/lee/Downloads/3.4 | 0.5 _ 0.75 定義.md:61>)。
- `Q75` 與 `Q50` 目前應視為 baseline threshold，而非最終不可調整值；待 3.2 與 3.3 穩定後，仍可在相同 expanding window 與 `T+1` 架構下再做有限度微調。

---

## 3.5 Matching Score 設計

依 [bigdata.md](/Users/lee/Documents/BigData/context/bigdata.md#L278) 到 [bigdata.md](/Users/lee/Documents/BigData/context/bigdata.md#L293)，媒合排序至少要考慮：

1. 釋放端壓力程度
2. 承接端緩衝程度
3. 港對之間的調度阻力

### 候選組合建立

只建立：

```text
source_port.status == "高壓力港"
target_port.status == "高緩衝港"
source_port != target_port
```

也就是說，只有「相對需要優先釋放」的港口，才會與「具相對承接能力且目前不高壓」的港口形成候選調度對。

目前 3.5 的正式分析宇宙限定為 `基隆港、臺北港、臺中港、高雄港`。`安平港` 雖保留於原始資料層，但因 2025 年 12 個月的 `empty_in`、`empty_out`、`full_in`、`full_out` 與 `empty_net` 全為 `0`，可見 [analysis_table_2025.csv](/Users/lee/Documents/BigData/context/analysis_table_2025.csv:14) 到 [analysis_table_2025.csv](/Users/lee/Documents/BigData/context/analysis_table_2025.csv:25)，因此不納入 `status_final_2025.csv` 與 `matching_final_2025.csv` 的正式比較與媒合分析，以避免結構性全零港扭曲當月橫截面分位數門檻與配對排序。

### 距離矩陣與 `distance_class`

本研究目前不直接使用真實運價或完整成本模型，而是先以港對距離作為調度阻力 proxy。距離分級可參考：

- [Distance_Matrix - 距離.csv](</Users/lee/Downloads/Distance_Matrix - 距離.csv:1>)
- [Distance_Matrix - 說明.csv](</Users/lee/Downloads/Distance_Matrix - 說明.csv:1>) 到 [Distance_Matrix - 說明.csv](</Users/lee/Downloads/Distance_Matrix - 說明.csv:9>)

[Distance_Matrix - 說明.csv](</Users/lee/Downloads/Distance_Matrix - 說明.csv:1>) 說明目前級距是以最遠距離除以 3 作為簡化切分基礎，因此可定義：

```text
distance_class = 1 : 0 - 195 km
distance_class = 2 : 196 - 390 km
distance_class = 3 : 390 km 以上
```

以目前距離矩陣來看：

- 基隆港 - 台北港：`130.8 km`，屬 `distance_class = 1`
- 基隆港 - 台中港：`262.0 km`，屬 `distance_class = 2`
- 基隆港 - 高雄港：`583.7 km`，屬 `distance_class = 3`
- 台北港 - 台中港：`205.8 km`，屬 `distance_class = 2`
- 台北港 - 高雄港：`527.4 km`，屬 `distance_class = 3`
- 台中港 - 高雄港：`420.2 km`，屬 `distance_class = 3`

對應矩陣則可見 [Distance_Matrix - 距離.csv](</Users/lee/Downloads/Distance_Matrix - 距離.csv:2>) 到 [Distance_Matrix - 距離.csv](</Users/lee/Downloads/Distance_Matrix - 距離.csv:5>)。

### `distance_factor` 映射

為了讓距離矩陣可以直接進入媒合分數，本研究先將 `distance_class` 映射為 `distance_factor`：

```text
distance_class = 1 -> distance_factor = 1.00
distance_class = 2 -> distance_factor = 0.85
distance_class = 3 -> distance_factor = 0.70
```

其中 `distance_factor` 越高，代表調度阻力越低、越容易執行。此處應解讀為港口層級的簡化調度 proxy，不代表真實里程成本、船期成本或最佳化成本函數。

### 媒合分數

3.5 的權重設計同樣應區分 `baseline`、`custom` 與目前擬採值。

1. `baseline (ROC)`  
   若僅知道 `source_pressure > target_buffer > distance` 的重要性排序，則可採三構面 ROC 權重 `0.611 / 0.278 / 0.111`。其父方案比較結果可見 [matching_parent_backtest_summary.csv](/Users/lee/Documents/BigData/context/matching_parent_backtest_summary.csv:2) 到 [matching_parent_backtest_summary.csv](/Users/lee/Documents/BigData/context/matching_parent_backtest_summary.csv:3)。

   ```text
   matching_score_roc =
   0.611 * source_pressure_index +
   0.278 * target_buffer_index +
   0.111 * distance_factor
   ```

2. `custom (規則型預設值)`  
   依研究敘事，初始假設版將來源港壓力置於首位，承接端緩衝次之，距離阻力作為第三構面。這組設定也是目前 3.5 的規則型設計主線。

   ```text
   matching_score_custom =
   0.45 * source_pressure_index +
   0.35 * target_buffer_index +
   0.20 * distance_factor
   ```

3. `目前擬採正式值`  
   在固定 `pressure = roc_prev_best`、`buffer = custom_nearby_c` 的條件下，3.5 先比較 `roc` 與 `custom` 兩個父方案。結果顯示 `custom` 與 `roc` 在 top-1 target 安全率上相近，但 `custom` 較能把下期仍維持高壓力的來源港排到更前面，因此父方案比較由 `custom` 勝出，見 [matching_parent_backtest_summary.csv](/Users/lee/Documents/BigData/context/matching_parent_backtest_summary.csv:2) 到 [matching_parent_backtest_summary.csv](/Users/lee/Documents/BigData/context/matching_parent_backtest_summary.csv:3)。
   
   之後沿 `custom` 家族做鄰近小實驗，`custom_nearby_b = 0.40 / 0.40 / 0.20` 的 `pooled_outcome_corr` 略優於原始 `custom`，見 [matching_backtest_summary.csv](/Users/lee/Documents/BigData/context/matching_backtest_summary.csv:2) 到 [matching_backtest_summary.csv](/Users/lee/Documents/BigData/context/matching_backtest_summary.csv:4)。但 `custom_nearby_b` 與 `custom` 在 top-1 決策指標上目前幾乎一致，因此主文較保守的寫法仍應採 `custom` 作為 3.5 的正式版本，並將 `custom_nearby_b` 保留為敏感度分析下的暫定優先候選。

   ```text
   matching_score =
   0.45 * source_pressure_index +
   0.35 * target_buffer_index +
   0.20 * distance_factor
   ```

三個構面的角色如下：

- `source_pressure_index` 越高：代表來源港越需要優先釋放
- `target_buffer_index` 越高：代表目標港越具承接緩衝
- `distance_factor` 越高：代表該港對在簡化規則下越容易執行

這代表目前 3.5 的採用邏輯應寫成：

- `custom = 0.45 / 0.35 / 0.20`：正式主方案
- `custom_nearby_b = 0.40 / 0.40 / 0.20`：敏感度分析下的微調候選

換言之，3.5 已不只是規則型預設值，而是已有一輪固定 `Pressure / Buffer` 條件下的初步回測支撐；但由於 `custom` 與 `custom_nearby_b` 的實際推薦結果差距仍小，因此目前不宜把兩者差異解讀得過度確定。

若需要更保守，可加入承接安全檢查：

```text
projected_target_pressure =
target_pressure_index + move_ratio
```

若預估承接後目標港會超過高壓力門檻，則可降低分數或直接排除該候選組合。

若要先產出一版可直接接到 app 的 `simulation_final_2025.csv`，可採以下基準模擬規則：

```text
move_share ∈ {0, 0.25, 0.50, 0.75}
move_amount = move_share * max(source_empty_net, 0)
```

其中：

- `move_share` 表示模擬移轉來源港當月正向 `empty_net` 的比例
- `move_amount` 為該比例對應的模擬移轉量
- `source` 端假設本月 `empty_out` 增加 `move_amount`
- `target` 端假設本月 `empty_in` 增加 `move_amount`
- `source/target` 的 `empty_net` 會分別減少/增加 `move_amount`
- `roll3_empty_net` 則在僅調整當月值的假設下，分別減少/增加 `move_amount / 3`

在此基準下，可重新估計 source 與 target 當期的 `pressure_index`、`buffer_index`、`status` 與跨港 cut，形成 Before/After 比較表。此處仍應保守解讀：它是港口層級的簡化承接模擬，不代表真實可調度量或實際營運可行性。

### 解讀方式

- 分數越高：越值得優先列入調度清單
- 高分不代表必定執行，只代表在目前公開資料與簡化距離規則下較合理
- 目前 `matching_score` 應解讀為港口層級的決策支援排序，而非真實成本最佳化解

---

## 3.6 權重比較與定案順序

若後續要正式比較 `pressure` 與 `buffer` 的不同權重方案，建議不要一開始就同時微調兩者，而應採分階段方式進行。原因是若兩個構面同時變動，將難以判斷回測結果的改善究竟來自 `pressure`、`buffer`，還是僅是某一組組合的偶然表現。

較合理的執行順序如下：

1. **先獨立定案 `Pressure` 權重**  
   先只比較 `pressure_index` 的不同權重方案，並以相同的 expanding window 與 `T+1` 回測架構，檢查哪一組權重較能辨識下期仍持續累積的高壓力港。  
   目前此步驟的結果可見 [pressure_backtest_summary.csv](/Users/lee/Documents/BigData/context/pressure_backtest_summary.csv:2) 到 [pressure_backtest_summary.csv](/Users/lee/Documents/BigData/context/pressure_backtest_summary.csv:12)，其中 `roc_prev_best` 排名第 1。

2. **再固定 `Pressure`，單獨比較 `Buffer` 權重**  
   當 `pressure` 已有擬採方案後，將其固定，再只變動 `buffer_index` 的各組權重，檢查哪一組較能維持高緩衝港在下一期仍不易轉為高壓力狀態。  
   目前此步驟的流程與結果可見 [weight_backtest_plan.md](/Users/lee/Documents/BigData/context/weight_backtest_plan.md:207) 到 [weight_backtest_plan.md](/Users/lee/Documents/BigData/context/weight_backtest_plan.md:290)、[buffer_backtest_summary.csv](/Users/lee/Documents/BigData/context/buffer_backtest_summary.csv:2) 到 [buffer_backtest_summary.csv](/Users/lee/Documents/BigData/context/buffer_backtest_summary.csv:8)，其中 `custom_nearby_c` 排名第 1。

3. **最後才做 `Pressure + Buffer` 的整合檢查**  
   待 `pressure` 與 `buffer` 各自都有暫定主方案後，再進一步檢查兩者組合下的 `status`、候選調度對與 `matching_score` 是否仍合理，並將此步驟作為整體方案的穩健性檢查，而非第一輪主定案依據。

這樣的好處是：

- 可清楚辨識每一輪結果究竟是哪個構面帶來改善
- 比較容易撰寫研究方法與回測邏輯
- 可避免 `buffer` 尚未穩定時，反過來干擾 `pressure` 的正式定案

這個次序也與 [權重資料.md](/Users/lee/Documents/BigData/context/權重資料.md:80) 到 [權重資料.md](/Users/lee/Documents/BigData/context/權重資料.md:97) 的原則一致：`baseline` 與 `custom` 都只是候選設計依據，最終仍需回到一致的 `expanding window + T+1` 回測架構來決定採用值。

因此，本研究較建議採：

`Pressure-only 定案 -> Pressure fixed 下比較 Buffer -> 最後整合 Pressure + Buffer`

---

## 3.7 探索模式（Exploration Mode）定位與設計

### 為什麼需要探索模式

正式模式的目的，是固定研究已驗證的權重與分類邏輯，輸出可直接對應研究敘事的正式建議。  
因此正式模式不應讓使用者任意覆寫 `pressure`、`buffer` 或 `matching` 權重，否則會削弱研究方法的可解釋性與正式推薦的說服力。

但另一方面，若 dashboard 完全不提供互動調整，使用者就很難理解：

- 為什麼正式模式會強調 `net` 而不是 `flow`
- 若決策者偏好「提早預警」或「先看當月壓力」，推薦結果會如何改變
- 正式推薦是穩健的，還是只在某一組權重下才成立

因此探索模式的定位不應是「讓使用者自己決定正確答案」，而應是：

1. **做敏感度分析**  
   比較不同權重偏好下，`status`、`matching_score` 與調度推薦是否改變。

2. **支援不同決策偏好**  
   例如有些管理者更在意即時壓力，有些更在意中期堆積風險，有些則更在意趨勢預警。

3. **提高模型透明度**  
   讓使用者知道 dashboard 並非黑盒，而是由可解釋的 proxy 組成。

因此應明確區分：

- `正式模式`：固定研究正式權重與正式分類規則
- `探索模式`：允許以受控方式比較不同偏好與不同權重，但**不覆蓋正式模式之研究結論**

建議 UI 上明確標示：

> 探索模式僅供敏感度分析與情境模擬，不覆蓋正式模式之研究建議。

### 探索模式的兩個子分頁

探索模式建議再細分為兩個 tab：

#### 1. Strategy Mode（策略模板）

目的：  
不是讓使用者直接改數字，而是提供可解釋的決策偏好模板，方便不熟悉公式或權重含義的使用者快速比較。

建議至少提供三種模板：

- `即時壓力型（Flow 重）`  
  提高 `pressure_flow_raw` 權重，優先辨識「這個月空櫃流入壓力是否明顯升高」。

- `堆積風險型（Net 重）`  
  提高 `pressure_net_raw` 權重，優先辨識「這個月是否真的在淨累積」。

- `趨勢預警型（Trend 重）`  
  提高 `pressure_roll_raw` 權重，優先辨識「近三月是否持續累積」。

這些模板的作用，是把專業權重選擇翻譯成使用者容易理解的策略語言，降低 dashboard 的使用門檻。

若要避免同時變動太多構面而難以解讀，建議：

- `Strategy Mode` 主要調整 `Pressure` 三構面
- `Buffer` 與 `Matching` 先維持正式模式預設值

這樣比較容易解釋：

- 是因為壓力定義改變，才導致 `source` 改變
- 而不是 `Pressure / Buffer / Matching` 三組權重同時變動造成的混合效果

#### 2. User Custom（使用者自訂）

目的：  
提供進階使用者做更細緻的敏感度分析，直接自訂三組權重：

- `Pressure`：`flow / net / roll`
- `Buffer`：`space / netout / export`
- `Matching`：`source / target / distance`

建議限制如下：

- 每組權重總和 `= 1`
- 每個單一權重 `∈ [0.1, 0.6]`
- 由 UI slider 控制

此限制的原因不是技術方便，而是研究解釋性考量：

- 避免某一個構面被設成接近 `0` 而失去原模型意義
- 避免某一個構面被設成接近 `1` 而使結果退化成單一指標排序
- 保留模型作為「多構面 proxy-based decision model」的本質

### 探索模式下必須同步提供的參數說明

若 dashboard 允許自訂權重，則不應只提供 slider，而必須同步說明各參數代表的意義。至少應在 UI 中或說明文件中清楚定義：

#### Pressure 構面

- `Flow`  
  `pressure_flow_raw = empty_in / throughput`  
  代表當月空櫃流入相對於港口作業規模的壓力。

- `Net`  
  `pressure_net_raw = pmax(empty_net, 0) / throughput`  
  代表當月是否真的在正向累積空櫃。

- `Trend`  
  `pressure_roll_raw = pmax(roll3_empty_net, 0) / throughput`  
  代表近三月是否持續累積。

#### Buffer 構面

- `Space`  
  `buffer_space_raw = 1 - pressure_flow_raw`  
  代表相對空間餘裕。

- `Net Out`  
  `buffer_net_raw = pmax(-roll3_empty_net, 0) / throughput`  
  代表近期是否較偏淨流出，較不易累積。

- `Export`  
  `buffer_export_raw = export_pull`  
  代表出口活動與空櫃吸納需求背景。

#### Matching 構面

- `Source`  
  來源港壓力越高，越有優先疏解必要。

- `Target`  
  目標港緩衝越高，越具承接潛力。

- `Distance`  
  距離阻力越低，調度越容易執行。

### 與正式模式的關係

探索模式不是研究正式主線的替代品，而是正式模式之外的輔助層。  
因此 dashboard 的輸出邏輯應為：

- `正式模式`：固定正式權重與正式分類規則
- `探索模式 / Strategy Mode`：比較不同策略偏好
- `探索模式 / User Custom`：做進階敏感度分析

若三者結果一致，可作為正式模式穩健性的補充證據；  
若三者結果差異大，則應在研究敘述中解釋：

- 模型對哪些構面敏感
- 哪些港口的分類或推薦排序最容易受權重改變影響

這樣探索模式的存在才有方法論意義，而不是單純增加一組可以拖動的 UI 元件。

---

## 四、建議實作欄位清單

如果後續要在新版本 Shiny 或資料前處理流程落地，建議至少整理出以下欄位：

| 類別 | 欄位 |
|---|---|
| 主鍵 | `ym`, `port` |
| 核心公開欄位 | `empty_in`, `empty_out`, `full_in`, `full_out`, `throughput` |
| 流動衍生欄位 | `empty_net`, `cum_empty_net`, `roll3_empty_net` |
| 壓力欄位 | `pressure_flow_raw`, `pressure_net_raw`, `pressure_roll_raw`, `pressure_index` |
| 緩衝欄位 | `buffer_space_raw`, `buffer_net_raw`, `buffer_export_raw`, `buffer_index` |
| 分類欄位 | `status` |
| 媒合欄位 | `source_port`, `target_port`, `distance_factor`, `matching_score` |
| 模擬欄位 | `simulation_scenario`, `move_share`, `move_amount`, `source_pressure_index_after`, `target_pressure_index_after`, `pair_success` |

### 正式分析宇宙

為避免結構性全零港影響相對門檻與媒合排序，正式輸出層應區分：

- 原始資料層：保留 `安平港`
- 正式分析層：僅納入 `基隆港、臺北港、臺中港、高雄港`

因此後續 app 或最終分析表若要產出：

- `status_final_2025.csv`
- `matching_final_2025.csv`
- `simulation_final_2025.csv`

應以四港作為正式分析宇宙；`安平港` 保留於 `master_table_2025.csv` 與 `analysis_table_2025.csv`，但不納入 3.2 到 3.5 的正式分類與媒合結果。

---

## 五、與目前 `app.R` 的對照

### `app.R` 目前保留即可的概念

- `empty_in`
- `empty_out`
- `empty_net`
- `throughput`
- 壓力/緩衝/媒合/模擬四個頁面架構

### `app.R` 目前不建議沿用的公式

```text
pressure = empty_in / throughput
buffer = 1 - pressure
```

原因：

- `pressure` 只反映流入，不反映實際累積
- `buffer` 與 `pressure` 完全鏡像，沒有獨立分析意義
- 不符合 markdown 要求的「流動、壓力、緩衝」三構面分離

### `app.R` 目前需改成動態生成的部分

- `status`：應由歷史相對門檻計算
- `matching_data`：應由高壓力港 × 高緩衝港自動生成
- `score`：應由壓力、緩衝、距離因素計算

---

## 六、最小可行版本建議

若要先做一版最小可行、又符合目前已完成的權重回測與正式輸出，建議公式收斂為：

### Pressure Index

```text
pressure_index =
0.22 * pct(pressure_flow_raw) +
0.60 * pct(pressure_net_raw) +
0.18 * pct(pressure_roll_raw)
```

### Buffer Index

```text
buffer_index =
0.45 * pct(buffer_space_raw) +
0.40 * pct(buffer_net_raw) +
0.15 * pct(buffer_export_raw)
```

### Status

```text
高壓力港：pressure_index >= Q75 且 roll3_empty_net > 0
高緩衝港：buffer_index >= Q75 且 pressure_index <= Q50
正常港：其餘
```

### Matching Score

```text
matching_score =
0.45 * source_pressure_index +
0.35 * target_buffer_index +
0.20 * distance_factor
```

### Simulation Baseline

```text
move_share ∈ {0, 0.25, 0.50, 0.75}
move_amount = move_share * max(source_empty_net, 0)
```

並在每個情境下重算：

- `source_pressure_index_after`
- `target_pressure_index_after`
- `source_status_after`
- `target_status_after`
- `pair_success`

這版的優點是：

- 全部可用公開資料加上簡單人工對照表完成
- 能直接對應 markdown 的三大研究問題
- 已可直接對應 `status_final_2025.csv`、`matching_final_2025.csv`、`simulation_final_2025.csv`
- 可先在 Shiny 上落地，再逐步加 Prophet/ARIMA 預警模組

若要進一步比較不同權重設計，則應回到前述 `3.2`、`3.3`、`3.5` 的 baseline/custom/正式採用值架構；本節不再使用舊版示意權重，而以目前正式輸出層已採用的版本作為最小可行落地方案。

---

## 七、結論

若完全以 [bigdata.md](/Users/lee/Documents/BigData/context/bigdata.md) 為準，則後續不應再把：

```text
buffer = 1 - pressure
```

視為正式研究公式。

較合理的方向是：

1. 用 `empty_in`, `empty_out`, `throughput`, `full_out` 建立公開資料主表
2. 用 `empty_net` 與 `roll3_empty_net` 反映空櫃是否持續累積
3. 用相對門檻建立 `pressure_index` 與 `buffer_index`
4. 用 `status` 辨識高壓力港與高緩衝港
5. 若要回答 T+1 問題，則各月份的分位數、cutoff 與分類均應只使用當時以前資料估計
6. 用 `matching_score` 形成可解釋的跨港調度優先清單

這樣才真正符合研究書中的「辨識 -> 分類 -> 候選組合 -> 媒合排序 -> 視覺化」主線。

---

## 八、可行性分析表

本節目的不是重新發明公式，而是回答一個更實際的問題：

> 以目前 [bigdata.md](/Users/lee/Documents/BigData/context/bigdata.md) 規劃的資料表來看，哪些指標或模組可以直接做，哪些需要補人工規則，哪些在現階段資料不足下不宜硬做。

分類原則如下：

- `可直接做`：現有公開資料表已足夠，經清洗與整併後即可計算
- `可做但需人工規則`：公開資料可支撐主體，但還要自行建立對照表、分類規則或政策權重
- `目前資料不足`：若沒有新增資料表，現階段只能做 proxy，不能宣稱已精準衡量

### 8.1 可直接做

| 項目 | 可行性判斷 | 目前可用資料表 | 一定需要的欄位 | 說明 |
|---|---|---|---|---|
| 月別港口主表 | 可直接做 | 臺灣進口貨空櫃、臺灣出口貨空櫃、臺灣進口貨實櫃、臺灣出口貨實櫃、國際商港貨櫃裝卸量 | `年月`, `港口`, `20呎標準貨櫃` 或對應櫃量欄位 | 先整併成 `ym × port` 主表，是後續所有分析基礎 |
| `empty_in` | 可直接做 | 臺灣進口貨空櫃 | `年月`, `臺灣港`, `20呎標準貨櫃` | 直接對應空櫃進港量 |
| `empty_out` | 可直接做 | 臺灣出口貨空櫃 | `年月`, `臺灣港`, `20呎標準貨櫃` | 直接對應空櫃出港量 |
| `full_in` | 可直接做 | 臺灣進口貨實櫃 | `年月`, `臺灣港`, `20呎標準貨櫃` | 可作為背景活動量 |
| `full_out` | 可直接做 | 臺灣出口貨實櫃 | `年月`, `臺灣港`, `20呎標準貨櫃` | 可作為出口拉力或需求背景 |
| `throughput` | 可直接做 | 國際商港貨櫃裝卸量（TEU）或主計總處備援資料 | `年月`, `港口名稱` 或 `港別`, `貨櫃裝卸量` | 可做為港口作業規模分母 |
| `empty_net` | 可直接做 | 由主表衍生 | `empty_in`, `empty_out` | `empty_in - empty_out` |
| `empty_total_flow` | 可直接做 | 由主表衍生 | `empty_in`, `empty_out` | 可衡量空櫃活動量 |
| `export_pull` | 可直接做 | 由主表衍生 | `full_out`, `throughput` | 可作為承接需求背景 proxy |
| `cum_empty_net` | 可直接做 | 由主表衍生 | `ym`, `port`, `empty_net` | 可看長期累積趨勢 |
| `roll3_empty_net` | 可直接做 | 由主表衍生 | `ym`, `port`, `empty_net` | 可看近 3 月持續累積或流失 |
| `pressure_index` 基礎版 | 可直接做 | 主表加歷史序列即可 | `empty_in`, `empty_out`, `throughput`, `ym`, `port` | 可用相對分位數做壓力辨識 |
| `buffer_index` 基礎版 | 可直接做 | 主表加歷史序列即可 | `empty_in`, `empty_out`, `full_out`, `throughput`, `ym`, `port` | 可做成承接潛力 proxy |
| `status` | 可直接做 | 由指標衍生 | `pressure_index`, `buffer_index`, `roll3_empty_net` | 可分類高壓力港、正常港、高緩衝港 |
| 趨勢圖與分類圖 | 可直接做 | 主表與衍生欄位 | `ym`, `port`, `empty_net`, `pressure_index`, `buffer_index`, `status` | 對應研究中的視覺化模組 |

### 8.2 可做但需人工規則

| 項目 | 可行性判斷 | 還需補什麼 | 一定需要的欄位 | 說明 |
|---|---|---|---|---|
| `distance_factor` | 可做但需人工規則 | 建立港口對港口的距離/區域權重表 | `source_port`, `target_port`, `distance_factor` | 公開資料通常不會直接給港對阻力，需自行定義 |
| 區域分類 | 可做但需人工規則 | 建立港口區域對照表 | `port`, `region` | 例如北部、中部、南部、東部 |
| 相鄰港或跨區港規則 | 可做但需人工規則 | 建立港口配對規則表 | `source_port`, `target_port`, `pair_type` | 例如相鄰、跨區、遠距 |
| `matching_score` | 可做但需人工規則 | 補距離或區域阻力規則 | `source_port`, `target_port`, `source_pressure_index`, `target_buffer_index`, `distance_factor` | 核心分數可做，但距離因子必須人工建置 |
| 候選調度對排序 | 可做但需人工規則 | 定義 Top-N、每港最多幾個承接端等規則 | `source_port`, `target_port`, `matching_score` | 屬於決策支援規則，不是資料表直接給 |
| 港口角色修正 | 可做但需人工規則 | 建立港口角色表 | `port`, `port_role`, `role_weight` | 若後續要區分樞紐港、支線港，可加入修正值 |
| 模擬調度量規則 | 可做但需人工規則 | 定義每次模擬量或比例 | `source_port`, `target_port`, `move_amount` 或 `move_ratio` | 目前公開資料不會直接告訴你合理調度量 |
| 預測風險門檻 | 可做但需人工規則 | 定義何謂預警、警戒、危險 | `pressure_index`, `buffer_index`, `forecast_value` | 若導入 ARIMA/Prophet，警戒門檻仍需研究者設定 |

### 8.3 目前資料不足

| 項目 | 可行性判斷 | 為何不足 | 若要做到需補搜集的資料表 | 一定需要的欄位 | 說明 |
|---|---|---|---|---|---|
| 真實空櫃庫存 | 目前資料不足 | 現有資料是流量，不是期初/期末庫存 | 港口或場站月末空櫃存量表 | `ym`, `port`, `empty_stock_end`，最好另有 `empty_stock_begin` | `cum_empty_net` 只能當 proxy，不能等同真實存量 |
| 真實堆場剩餘容量 | 目前資料不足 | `throughput` 不等於堆場可用空間 | 港口堆場容量/使用率資料表 | `ym`, `port`, `yard_capacity`, `yard_utilization`, `empty_yard_usage` | 若沒有這些欄位，`buffer_index` 只能解釋為相對承接潛力 |
| 真實承接上限 | 目前資料不足 | 無法知道港口到底還能接多少空櫃 | 港口或場站承載上限資料表 | `ym`, `port`, `available_capacity`, `safety_capacity` | 目前最多只能做「承接傾向」而非「可承接量上限」 |
| 航商層級媒合可行性 | 目前資料不足 | 港口層級資料無法反映航商間的櫃權、責任與契約限制 | 航商別空櫃流動或交換資料 | `carrier`, `port`, `ym`, `container_type`, `empty_in`, `empty_out` | 現階段只能做到港口層級，不宜宣稱已可落實到航商營運層 |
| 櫃型別精細分析 | 目前資料不足 | 目前欄位大多是總量或 20 呎標準化，不足以分櫃型 | 櫃型別貨櫃資料表 | `ym`, `port`, `container_type`, `qty_teu` 或 `qty_box` | 若實務差異來自 20 呎/40 呎/冷藏櫃，現資料無法拆解 |
| 真實調度成本最佳化 | 目前資料不足 | 缺乏運價、船期、艙位、陸運成本等資料 | 成本與運輸網路資料表 | `source_port`, `target_port`, `sea_cost`, `land_cost`, `schedule`, `capacity` | 現階段可排序，不足以做最佳化求解 |
| 船期與艙位可行性 | 目前資料不足 | 現有月資料不含班期與艙位 | 船期/航線/艙位資料表 | `date`, `route`, `source_port`, `target_port`, `slot_capacity` | 無法確認建議調度對在實務上何時可執行 |
| 高精度缺櫃預測 | 目前資料不足 | 目前資料偏港口流量背景，缺乏需求端細節 | 出口訂單、報關、航商提櫃需求資料 | `ym` 或 `date`, `port`, `export_demand`, `booking_demand` | 目前較適合做風險預警，不適合講精準需求預測 |

### 8.4 研究表述建議

若以目前資料條件撰寫報告或簡報，建議用以下說法：

- `pressure_index`：公開資料下的港口空櫃壓力 proxy
- `buffer_index`：公開資料下的港口承接潛力 proxy
- `matching_score`：港口層級之跨港調度優先順序建議

不建議直接寫成：

- 真實庫存
- 真實剩餘容量
- 最佳調度解
- 可直接執行之航商調度方案

### 8.5 一句話總結

目前資料條件足以支撐：

- 辨識型模型
- 排序型模型
- 決策支援型儀表板

目前資料條件不足以直接支撐：

- 真實庫存模型
- 精準容量模型
- 營運最佳化模型
