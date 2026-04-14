library(readr)
library(dplyr)

master_path <- "/Users/lee/Documents/BigData/context/master_table_2025.csv"
output_path <- "/Users/lee/Documents/BigData/context/analysis_table_2025.csv"

master <- read_csv(master_path, show_col_types = FALSE)

analysis <- master %>%
  filter(
    port %in% c("基隆港", "臺中港", "高雄港", "臺北港", "安平港"),
    !is.na(throughput),
    throughput > 0
  )

write_csv(analysis, output_path)

message("Analysis table written to: ", output_path)
