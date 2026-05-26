`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

safe_divide <- function(x, y) {
  ifelse(is.na(y) | y == 0, NA_real_, x / y)
}

current_percent_rank <- function(x) {
  valid <- !is.na(x)
  if (sum(valid) <= 1) {
    return(0)
  }

  ranks <- rank(x[valid], ties.method = "min")
  (ranks[length(ranks)] - 1) / (length(ranks) - 1)
}

normalize_port <- function(x) {
  gsub("台", "臺", x)
}

display_port <- function(x) {
  gsub("臺", "台", x)
}

fmt_ym <- function(x) {
  sprintf("%04d-%02d", as.integer(x) %/% 100, as.integer(x) %% 100)
}

fmt_num <- function(x, digits = 0) {
  ifelse(
    is.na(x),
    "—",
    format(round(x, digits), big.mark = ",", nsmall = digits, trim = TRUE)
  )
}

fmt_idx <- function(x, digits = 4) {
  ifelse(is.na(x), "—", sprintf(paste0("%.", digits, "f"), x))
}

fmt_pct <- function(x, digits = 0) {
  ifelse(is.na(x), "—", paste0(round(x * 100, digits), "%"))
}

month_to_ym <- function(month_num, year_num = 2025) {
  year_num * 100 + as.integer(month_num)
}

month_to_date <- function(month_num, year_num = 2025) {
  as.Date(sprintf("%04d-%02d-20", year_num, as.integer(month_num)))
}

formal_ports <- c("基隆港", "臺北港", "臺中港", "高雄港")
display_ports <- c("基隆港", "台北港", "台中港", "高雄港")
table_ports <- formal_ports
port_codes <- c(
  "基隆港" = "KEE",
  "臺北港" = "TPE",
  "臺中港" = "TAI",
  "高雄港" = "KAO",
  "花蓮港" = "HUA"
)

status_palette <- c(
  "高壓力港" = "#dc2626",
  "高緩衝港" = "#16a34a",
  "正常港" = "#2563eb",
  "資料暖機中" = "#f59e0b"
)

official_pressure_weights <- c(flow = 0.22, net = 0.60, roll = 0.18)
official_buffer_weights <- c(space = 0.45, netout = 0.40, export = 0.15)
official_matching_weights <- c(source = 0.45, target = 0.35, distance = 0.20)
custom_weight_bounds <- c(min = 0.10, max = 0.60)

strategy_templates <- list(
  flow_focus = list(
    label = "即時壓力型",
    short = "Flow 重",
    description = "提高當月空櫃進港比重，優先辨識短期即時壓力。",
    pressure_weights = c(flow = 0.60, net = 0.25, roll = 0.15)
  ),
  net_focus = list(
    label = "堆積風險型",
    short = "Net 重",
    description = "提高當月淨增加比重，優先辨識已經開始累積的港口。",
    pressure_weights = c(flow = 0.20, net = 0.60, roll = 0.20)
  ),
  trend_focus = list(
    label = "趨勢預警型",
    short = "Trend 重",
    description = "提高近三月趨勢比重，優先辨識持續累積的早期訊號。",
    pressure_weights = c(flow = 0.15, net = 0.25, roll = 0.60)
  )
)

weight_help_text <- list(
  flow = "拉高後會更重視當月空櫃流入占吞吐量的比例，適合觀察短期即時壓力。",
  net = "拉高後會更重視當月空櫃淨增加，適合辨識已經開始堆積的港口。",
  roll = "拉高後會更重視近三月連續累積趨勢，適合提早發現持續升壓的訊號。",
  space = "拉高後會更看重港口剩餘空間，代表系統更偏好能承接新空櫃的港口。",
  netout = "拉高後會更看重歷史淨流出能力，代表系統更偏好過去有消化空櫃能力的港口。",
  export = "拉高後會更看重出口拉力，代表系統更偏好後續可能把空櫃帶走的港口。",
  source = "拉高後會優先處理壓力最高的來源港，讓推薦更偏向先解決最緊急問題。",
  target = "拉高後會優先選擇緩衝較高的目標港，讓調度結果更保守穩定。",
  distance = "拉高後會更在意距離阻力與運輸成本，讓推薦更偏向近距離方案。"
)
