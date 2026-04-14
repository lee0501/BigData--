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

建議以「同港歷史」做標準化，避免高雄港與花蓮港因規模差異直接硬比。

```text
pressure_index =
0.35 * pct(pressure_flow_raw) +
0.40 * pct(pressure_net_raw) +
0.25 * pct(pressure_roll_raw)
```

其中 `pct(x)` 表示該港口在自身歷史序列中的分位數，範圍 `0~1`。<br>
**pct(x)**
把某港口某指標，放回它自己的歷史序列裡看
看它目前落在自己歷史的第幾分位
<br>
權重上讓 pressure_net_raw 稍高，是因為你的研究重點不是「進來很多」而已，而是「堆起來了」。
所以這個公式的本質不是數學漂亮，而是：
把「當月壓力」和「持續累積」合併成一個更接近研究問題的指標。

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

```text
buffer_index =
0.45 * pct(buffer_space_raw) +
0.35 * pct(buffer_net_raw) +
0.20 * pct(buffer_export_raw)
```

### 解讀方式

- 越接近 `1`：代表越有承接緩衝
- 越接近 `0`：代表承接後可能很快進入壓力狀態
- buffer_net_raw：
- 如果 roll3_empty_net 是正的，表示最近也在累積，那 buffer_net_raw 就是 0。
也就是說，這港即使現在壓力沒那麼高，也不代表它適合接。

### 為什麼這樣設計

- 把「低壓力」和「不易持續累積」分開處理
- 納入 `full_out` 對應的出口活躍程度，較符合「空櫃不足/需求背景」概念
- 仍維持只用公開資料或可直接推導的欄位

---

## 3.4 Status 設計

依 [bigdata.md](/Users/lee/Documents/BigData/context/bigdata.md#L262) 到 [bigdata.md](/Users/lee/Documents/BigData/context/bigdata.md#L276)，分類應採相對門檻。

### 建議分類規則

先定義：

```text
high_pressure_cut = pressure_index 的歷史或當月橫截面 0.75 分位
high_buffer_cut   = buffer_index 的歷史或當月橫截面 0.75 分位
low_pressure_cut  = pressure_index 的 0.50 分位
```

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

- 高壓力港不能只看高壓力，還要確認近期確實在累積
- 高緩衝港不能只看 buffer 高，也要排除已經接近高壓力的港口
- 這樣可避免 `app.R` 現在「高壓力」與「高緩衝」互相重疊的問題

---

## 3.5 Matching Score 設計

依 [bigdata.md](/Users/lee/Documents/BigData/context/bigdata.md#L278) 到 [bigdata.md](/Users/lee/Documents/BigData/context/bigdata.md#L293)，媒合排序至少要考慮：

1. 釋放端壓力程度
2. 承接端緩衝程度
3. 區域／距離阻力

### 候選組合建立

只建立：

```text
source_port.status == "高壓力港"
target_port.status == "高緩衝港"
source_port != target_port
```

### 距離／區域阻力建議

先用簡單規則，不需要一開始就做真實公里數。

```text
same_region = 1.00
west_coast_adjacent = 0.85
north_south_cross_region = 0.70
west_east_cross_region = 0.60
```

此值可命名為 `distance_factor`，範圍越高表示越容易執行。

### 媒合分數

```text
matching_score =
0.45 * source_pressure_index +
0.35 * target_buffer_index +
0.20 * distance_factor
```

若需要更保守，可加入承接安全檢查：

```text
projected_target_pressure =
target_pressure_index + move_ratio
```

若預估承接後目標港會超過高壓力門檻，則降低分數或排除。

### 解讀方式

- 分數越高：越值得優先列入調度清單
- 高分不代表必定執行，只代表在目前公開資料條件下較合理

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

若要先做一版最小可行、又符合研究書寫方向，建議公式先收斂為：

### Pressure Index

```text
pressure_index =
0.4 * pct(empty_in / throughput) +
0.4 * pct(pmax(empty_net, 0) / throughput) +
0.2 * pct(pmax(roll3_empty_net, 0) / throughput)
```

### Buffer Index

```text
buffer_index =
0.5 * pct(1 - empty_in / throughput) +
0.3 * pct(pmax(-roll3_empty_net, 0) / throughput) +
0.2 * pct(full_out / throughput)
```

### Status

```text
高壓力港：pressure_index >= Q75 且 roll3_empty_net > 0
高緩衝港：buffer_index >= Q75 且 pressure_index < Q50
正常港：其餘
```

### Matching Score

```text
matching_score =
0.45 * source_pressure_index +
0.35 * target_buffer_index +
0.20 * distance_factor
```

這版的優點是：

- 全部可用公開資料加上簡單人工對照表完成
- 能直接對應 markdown 的三大研究問題
- 可先在 Shiny 上落地，再逐步加 Prophet/ARIMA 預警模組

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
5. 用 `matching_score` 形成可解釋的跨港調度優先清單

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
