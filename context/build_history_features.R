library(readr)
library(dplyr)
library(slider)

input_path <- "/Users/lee/Documents/BigData/context/analysis_table_2025.csv"
output_path <- "/Users/lee/Documents/BigData/context/analysis_table_history_2025.csv"

analysis <- read_csv(input_path, show_col_types = FALSE) %>%
  arrange(port, ym)  #我擬定先照港口年月排序


calc_streak <- function(x) {  #自定義函數,用來計算有「連續幾個月 empty_net > 0」
  streak <- integer(length(x)) #先建立一個跟 x 一樣長度的整數向量 方便映射
  current <- 0L #目前連續月數起始值先設 0
  
  # 這邊我要逐月檢查
  for (i in seq_along(x)) {
    if (!is.na(x[i]) && x[i] > 0) { #如果這個月 x[i] 不是空值，而且大於 0 (這表示當月是淨流入)
      current <- current + 1L #那current就+1
    } else { #這裡否則是表示中斷，沒有繼續>0
      current <- 0L #把current歸0
    }
    streak[i] <- current #把每個月當下的連續月數記到 streak[i]
  }
  
  streak #函數最後回傳 streak 這個向量
}

history_table <- analysis %>%
  group_by(port) %>% #每個港口分開算
  arrange(ym, .by_group = TRUE) %>% #在每個港口內依年月排序
  #因為歷史指標一定要「每個港口自己和自己比」
  
  mutate(
    cum_empty_net = cumsum(empty_net),#變數表示「空櫃淨流量累積值」，是把每個港口每個月的 empty_net 一路加總
    roll3_empty_net = slide_dbl( #近 3 個月平均空櫃淨流量 #做滾動計算並回傳數值
      empty_net,#拿這欄來滾
      ~ mean(.x, na.rm = TRUE),#每個視窗取平均
      .before = 2,#往前抓 2 期，加上當期，共 3 期
      .complete = FALSE #即使前兩個月即使不滿 3 期，也照算現有資料
    ),
    roll3_pressure_raw = slide_dbl( #把單月壓力平滑化，避免只看某一個月的偶然波動
      net_pressure_raw,
      ~ mean(.x, na.rm = TRUE),
      .before = 2,
      .complete = FALSE
    ),
    positive_net_streak = calc_streak(empty_net), #呼前自訂函數，目標要得到「這個港口到這個月為止，已經連續幾個月空櫃淨流入」
    
    # 先用可直接取得的欄位近似緩衝能力原始值
    #這不是最終 Buffer Index，只是先做一個「可用的原始緩衝能力指標」
    buffer_raw = pmax(throughput - empty_total_flow, 0) / throughput,
    
    pressure_pct = percent_rank(net_pressure_raw), #某港口各月份的壓力值，在自己歷史中相對高低
    buffer_pct = percent_rank(buffer_raw), #某港口各月份的緩衝值，在自己歷史中相對高低
    
    pressure_z = ifelse( #標準分數
      sd(net_pressure_raw, na.rm = TRUE) == 0, #先算這個值和平均值差多少
      0,
      (net_pressure_raw - mean(net_pressure_raw, na.rm = TRUE)) /
        sd(net_pressure_raw, na.rm = TRUE) #再除以標準差
    ),#目標是看出某月壓力相對於歷史平均是高還是低
    
    #得到緩衝能力的歷史相對位置
    buffer_z = ifelse(
      sd(buffer_raw, na.rm = TRUE) == 0,
      0,
      (buffer_raw - mean(buffer_raw, na.rm = TRUE)) /
        sd(buffer_raw, na.rm = TRUE)
    )
  ) %>%
  ungroup() #mutate() 做完後，用 ungroup() 把分組狀態解除，避免後面寫檔或後續操作還保留 group_by(port)

write_csv(history_table, output_path)

message("History feature table written to: ", output_path)

