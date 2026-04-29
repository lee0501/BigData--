# iMarine Dashboard - Stable Local Version
# Compatible with older R versions. Uses only base R + shiny.
# Place this file beside a folder named "context", then run:
# source("iMarine_dashboard_stable_local.R", local = FALSE)

required_packages <- c("shiny")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing required packages: ", paste(missing_packages, collapse = ", "),
       ". Please run install.packages('shiny') first.")
}

library(shiny)

options(stringsAsFactors = FALSE)

# -----------------------------
# Utilities
# -----------------------------
locate_app_dir <- function() {
  candidates <- unique(c(
    getwd(),
    dirname(normalizePath(sys.frame(1)$ofile %||% getwd(), winslash = "/", mustWork = FALSE)),
    "/Users/lee/Documents/BigData"
  ))
  hits <- candidates[file.exists(file.path(candidates, "context", "status_final_2025.csv"))]
  if (length(hits) < 1) {
    stop("Cannot locate context/status_final_2025.csv. Put this R file beside the context folder, or setwd() to the project folder first.")
  }
  normalizePath(hits[1], winslash = "/", mustWork = TRUE)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

safe_read <- function(path) {
  if (!file.exists(path)) {
    warning("Missing file: ", path)
    return(data.frame())
  }
  read.csv(path, fileEncoding = "UTF-8-BOM", check.names = FALSE, stringsAsFactors = FALSE)
}

normalize_port <- function(x) gsub("台", "臺", x)
display_port <- function(x) gsub("臺", "台", x)
fmt_ym <- function(x) sprintf("%04d-%02d", as.integer(x) %/% 100, as.integer(x) %% 100)
fmt_num <- function(x, digits = 0) {
  ifelse(is.na(x), "—", format(round(as.numeric(x), digits), big.mark = ",", nsmall = digits, trim = TRUE))
}
fmt_pct <- function(x, digits = 0) {
  ifelse(is.na(x), "—", paste0(round(as.numeric(x) * 100, digits), "%"))
}

status_color <- function(status) {
  ifelse(status == "高壓力港", "#fee2e2",
         ifelse(status == "高緩衝港", "#dcfce7", "#dbeafe"))
}
status_text_color <- function(status) {
  ifelse(status == "高壓力港", "#b91c1c",
         ifelse(status == "高緩衝港", "#15803d", "#1d4ed8"))
}

# -----------------------------
# Load data
# -----------------------------
app_dir <- locate_app_dir()
context_dir <- file.path(app_dir, "context")

status_final <- safe_read(file.path(context_dir, "status_final_2025.csv"))
matching_final <- safe_read(file.path(context_dir, "matching_final_2025.csv"))
simulation_reco <- safe_read(file.path(context_dir, "simulation_recommendation_final_2025.csv"))
analysis_table <- safe_read(file.path(context_dir, "analysis_table_2025.csv"))
analysis_history <- safe_read(file.path(context_dir, "analysis_table_history_2025.csv"))
master_table <- safe_read(file.path(context_dir, "master_table_2025.csv"))
distance_lookup <- safe_read(file.path(context_dir, "port_pair_distance_lookup.csv"))

for (nm in c("status_final", "matching_final", "simulation_reco", "analysis_table", "analysis_history", "master_table", "distance_lookup")) {
  df <- get(nm)
  if (nrow(df) > 0) {
    if ("ym" %in% names(df)) df$ym <- as.integer(df$ym)
    port_cols <- intersect(c("port", "source_port", "target_port"), names(df))
    for (pc in port_cols) df[[pc]] <- normalize_port(df[[pc]])
    assign(nm, df)
  }
}

all_ym <- sort(unique(status_final$ym))
if (length(all_ym) == 0) stop("status_final_2025.csv has no ym values.")
default_ym <- max(all_ym, na.rm = TRUE)

years <- sort(unique(all_ym %/% 100))
months <- 1:12
ports <- sort(unique(status_final$port))

# -----------------------------
# CSS
# -----------------------------
app_css <- paste(c(
  "body { background:#eef2f7; font-family:-apple-system,BlinkMacSystemFont,'Segoe UI','Noto Sans TC',Arial,sans-serif; color:#111827; }",
  ".container-fluid { padding: 22px 26px 40px 26px; }",
  ".title-row { display:flex; align-items:center; gap:12px; margin-bottom:18px; }",
  ".brand-icon { width:44px; height:44px; border-radius:12px; background:#2563eb; color:white; display:flex; align-items:center; justify-content:center; font-weight:800; }",
  ".page-title { font-size:24px; font-weight:800; margin:0; }",
  ".sidebar-card { background:white; border:1px solid #dbe3ef; border-radius:16px; padding:18px; margin-bottom:16px; box-shadow:0 1px 2px rgba(15,23,42,.04); }",
  ".main-card { background:white; border:1px solid #dbe3ef; border-radius:16px; padding:18px; margin-bottom:16px; box-shadow:0 1px 2px rgba(15,23,42,.04); }",
  ".card-title { font-size:16px; font-weight:800; margin:0 0 10px 0; }",
  ".card-sub { color:#64748b; font-size:13px; margin-bottom:12px; }",
  ".kpi-grid { display:grid; grid-template-columns:repeat(4, minmax(130px, 1fr)); gap:12px; margin-bottom:16px; }",
  ".kpi { border-radius:14px; padding:14px; background:#f8fafc; border:1px solid #e2e8f0; }",
  ".kpi-label { font-size:12px; color:#64748b; font-weight:700; }",
  ".kpi-value { font-size:26px; font-weight:900; margin-top:4px; color:#0f172a; }",
  ".port-grid { display:grid; grid-template-columns:repeat(2, minmax(240px, 1fr)); gap:12px; }",
  ".port-card { border-radius:14px; padding:14px; border:1px solid #e2e8f0; }",
  ".port-name { font-weight:900; font-size:16px; margin-bottom:4px; }",
  ".badge-status { display:inline-block; border-radius:999px; padding:3px 10px; font-size:12px; font-weight:800; margin-left:6px; }",
  ".metric-row { display:flex; justify-content:space-between; font-size:13px; margin-top:8px; color:#475569; }",
  ".bar-track { height:8px; background:rgba(15,23,42,.12); border-radius:999px; overflow:hidden; margin-top:8px; }",
  ".bar-fill { height:100%; background:#2563eb; border-radius:999px; }",
  ".note { font-size:13px; color:#64748b; line-height:1.6; }",
  ".table { font-size:13px; }",
  ".nav-tabs > li > a { font-weight:700; color:#475569; }",
  ".nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover { color:#2563eb; font-weight:800; }",
  ".form-group label { font-size:13px; color:#334155; font-weight:800; }",
  "@media (max-width: 1100px) { .kpi-grid { grid-template-columns:repeat(2, 1fr); } .port-grid { grid-template-columns:1fr; } }",
  "@media (max-width: 760px) { .container-fluid { padding:14px; } .kpi-grid { grid-template-columns:1fr; } }"
), collapse = "\n")

# -----------------------------
# UI components
# -----------------------------
kpi_box <- function(label, value) {
  div(class = "kpi", div(class = "kpi-label", label), div(class = "kpi-value", value))
}

ui <- fluidPage(
  tags$head(tags$title("iMarine 空櫃失衡決策平台"), tags$style(HTML(app_css))),
  div(class = "title-row", div(class = "brand-icon", "IM"), h1(class = "page-title", "iMarine 空櫃失衡決策平台")),
  sidebarLayout(
    sidebarPanel(width = 3,
      div(class = "sidebar-card",
        h4("篩選條件"),
        selectInput("year", "年份", choices = years, selected = default_ym %/% 100),
        selectInput("month", "月份", choices = months, selected = default_ym %% 100),
        sliderInput("pressure_cut", "高壓力門檻（顯示用）", min = 0, max = 100, value = 75, step = 5, post = "%"),
        selectInput("port", "港口", choices = setNames(ports, display_port(ports)), selected = ports[1])
      ),
      div(class = "sidebar-card",
        h4("進階設定"),
        p(class = "note", "此版本優先確保本地端穩定執行。進階權重會用於『配對候選』頁面的自訂分數顯示，不會覆蓋原始 CSV。"),
        sliderInput("w_source", "來源港壓力權重", min = 0, max = 100, value = 45, step = 5, post = "%"),
        sliderInput("w_target", "目標港緩衝權重", min = 0, max = 100, value = 35, step = 5, post = "%"),
        sliderInput("w_distance", "距離因子權重", min = 0, max = 100, value = 20, step = 5, post = "%")
      )
    ),
    mainPanel(width = 9,
      tabsetPanel(id = "main_tabs",
        tabPanel("空櫃總覽",
          br(),
          uiOutput("overview_kpis"),
          div(class = "main-card", h3(class = "card-title", "各港口壓力狀態"), uiOutput("port_cards")),
          div(class = "main-card", h3(class = "card-title", "各港口壓力指標"), plotOutput("pressure_plot", height = "300px"))
        ),
        tabPanel("港口分析",
          br(),
          div(class = "main-card", h3(class = "card-title", "港口月度趨勢"), p(class="card-sub", "顯示所選港口的 pressure_index 與 buffer_index 歷史變化。"), plotOutput("port_trend_plot", height = "320px")),
          div(class = "main-card", h3(class = "card-title", "所選港口資料"), tableOutput("port_table"))
        ),
        tabPanel("跨港配合",
          br(),
          div(class = "main-card", h3(class = "card-title", "正式推薦調度"), tableOutput("simulation_table")),
          div(class = "main-card", h3(class = "card-title", "候選配對與自訂權重分數"), tableOutput("matching_table"))
        ),
        tabPanel("原始資料",
          br(),
          div(class = "main-card", h3(class = "card-title", "status_final_2025.csv"), tableOutput("raw_status")),
          div(class = "main-card", h3(class = "card-title", "資料檔案檢查"), verbatimTextOutput("file_check"))
        )
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  selected_ym <- reactive({ as.integer(input$year) * 100 + as.integer(input$month) })
  month_status <- reactive({
    df <- status_final[status_final$ym == selected_ym(), , drop = FALSE]
    df[order(df$port), , drop = FALSE]
  })

  output$overview_kpis <- renderUI({
    df <- month_status()
    if (nrow(df) == 0) {
      return(div(class="main-card", h3("此月份沒有資料"), p("請切換年份或月份。")))
    }
    hp <- sum(df$status == "高壓力港", na.rm = TRUE)
    hb <- sum(df$status == "高緩衝港", na.rm = TRUE)
    normal <- sum(df$status == "正常港", na.rm = TRUE)
    total_net <- sum(as.numeric(df$empty_net), na.rm = TRUE)
    div(class = "kpi-grid",
      kpi_box("高壓力港", hp),
      kpi_box("高緩衝港", hb),
      kpi_box("正常港", normal),
      kpi_box("全港 empty_net", fmt_num(total_net, 0))
    )
  })

  output$port_cards <- renderUI({
    df <- month_status()
    if (nrow(df) == 0) return(p("無資料"))
    cards <- lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      pidx <- as.numeric(row$pressure_index)
      bidx <- as.numeric(row$buffer_index)
      stat <- as.character(row$status)
      div(class = "port-card", style = paste0("background:", status_color(stat), ";"),
        div(class = "port-name", display_port(row$port),
          span(class = "badge-status", style = paste0("color:", status_text_color(stat), ";background:white;"), stat)
        ),
        div(class = "metric-row", span("壓力指標"), strong(fmt_pct(pidx, 0))),
        div(class = "bar-track", div(class = "bar-fill", style = paste0("width:", max(0, min(100, round(pidx*100))), "%;background:", status_text_color(stat), ";"))),
        div(class = "metric-row", span("緩衝指標"), strong(fmt_pct(bidx, 0))),
        div(class = "metric-row", span("empty_net"), strong(fmt_num(as.numeric(row$empty_net), 0))),
        div(class = "metric-row", span("throughput"), strong(fmt_num(as.numeric(row$throughput), 0)))
      )
    })
    do.call(div, c(list(class = "port-grid"), cards))
  })

  output$pressure_plot <- renderPlot({
    df <- month_status()
    if (nrow(df) == 0) return(NULL)
    vals <- as.numeric(df$pressure_index) * 100
    names(vals) <- display_port(df$port)
    op <- par(mar = c(5, 4, 3, 1))
    on.exit(par(op))
    barplot(vals, ylim = c(0, 100), las = 1, ylab = "Pressure Index (%)", main = paste0(fmt_ym(selected_ym()), " 各港壓力"))
    abline(h = input$pressure_cut, lty = 2)
  })

  output$port_trend_plot <- renderPlot({
    p <- normalize_port(input$port)
    df <- status_final[status_final$port == p, , drop = FALSE]
    if (nrow(df) == 0) return(NULL)
    df <- df[order(df$ym), ]
    x <- seq_len(nrow(df))
    y1 <- as.numeric(df$pressure_index) * 100
    y2 <- as.numeric(df$buffer_index) * 100
    op <- par(mar = c(6, 4, 3, 1))
    on.exit(par(op))
    plot(x, y1, type = "o", ylim = c(0, 100), xaxt = "n", xlab = "月份", ylab = "Index (%)", main = paste0(display_port(p), " 趨勢"))
    lines(x, y2, type = "o", lty = 2)
    axis(1, at = x, labels = fmt_ym(df$ym), las = 2, cex.axis = .8)
    legend("topleft", legend = c("Pressure", "Buffer"), lty = c(1,2), pch = 1, bty = "n")
  })

  output$port_table <- renderTable({
    p <- normalize_port(input$port)
    df <- status_final[status_final$port == p, , drop = FALSE]
    if (nrow(df) == 0) return(data.frame(Message = "No data"))
    df <- df[order(-df$ym), ]
    out <- data.frame(
      ym = fmt_ym(df$ym),
      port = display_port(df$port),
      status = df$status,
      empty_in = fmt_num(df$empty_in),
      empty_out = fmt_num(df$empty_out),
      empty_net = fmt_num(df$empty_net),
      throughput = fmt_num(df$throughput),
      pressure_index = fmt_pct(df$pressure_index, 1),
      buffer_index = fmt_pct(df$buffer_index, 1),
      stringsAsFactors = FALSE
    )
    head(out, 12)
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$simulation_table <- renderTable({
    df <- simulation_reco[simulation_reco$ym == selected_ym(), , drop = FALSE]
    if (nrow(df) == 0) return(data.frame(Message = "此月份沒有正式推薦資料"))
    df <- df[order(df$rank_global), ]
    data.frame(
      rank = df$rank_global,
      source = display_port(df$source_port),
      target = display_port(df$target_port),
      move_amount = fmt_num(df$recommended_move_amount, 0),
      result = df$simulation_result_class,
      source_before = df$source_status_before,
      source_after = df$source_status_after,
      target_before = df$target_status_before,
      target_after = df$target_status_after,
      message = df$display_message,
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$matching_table <- renderTable({
    df <- matching_final[matching_final$ym == selected_ym(), , drop = FALSE]
    if (nrow(df) == 0) return(data.frame(Message = "此月份沒有候選配對資料"))
    sw <- as.numeric(input$w_source); tw <- as.numeric(input$w_target); dw <- as.numeric(input$w_distance)
    total <- sw + tw + dw
    if (total <= 0) total <- 1
    custom_score <- (sw/total) * as.numeric(df$source_pressure_index) +
      (tw/total) * as.numeric(df$target_buffer_index) +
      (dw/total) * as.numeric(df$distance_factor)
    df$custom_score <- custom_score
    df <- df[order(-df$custom_score), ]
    data.frame(
      rank = seq_len(nrow(df)),
      source = display_port(df$source_port),
      source_status = df$source_status,
      target = display_port(df$target_port),
      target_status = df$target_status,
      distance_class = df$distance_class,
      original_score = round(as.numeric(df$matching_score), 3),
      custom_score = round(df$custom_score, 3),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$raw_status <- renderTable({
    df <- status_final[status_final$ym == selected_ym(), , drop = FALSE]
    if (nrow(df) == 0) return(data.frame(Message = "No data"))
    keep <- intersect(c("ym", "port", "empty_in", "empty_out", "empty_net", "throughput", "pressure_index", "buffer_index", "status"), names(df))
    out <- df[, keep, drop = FALSE]
    if ("port" %in% names(out)) out$port <- display_port(out$port)
    out
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$file_check <- renderPrint({
    files <- c(
      "status_final_2025.csv",
      "matching_final_2025.csv",
      "simulation_recommendation_final_2025.csv",
      "analysis_table_2025.csv",
      "analysis_table_history_2025.csv",
      "master_table_2025.csv",
      "port_pair_distance_lookup.csv"
    )
    cat("Project directory:\n", app_dir, "\n\n", sep = "")
    for (f in files) {
      p <- file.path(context_dir, f)
      cat(sprintf("%-45s %s\n", f, ifelse(file.exists(p), "OK", "MISSING")))
    }
    cat("\nAvailable ym values:\n")
    print(fmt_ym(all_ym))
  })
}

app <- shinyApp(ui = ui, server = server)

if (sys.nframe() == 0) {
  runApp(app, launch.browser = TRUE)
} else {
  app
}
