library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

# ----------------------------
# 模擬資料：正式版改成讀你的清洗後資料
# ----------------------------
port_data <- data.frame(
  ym = rep(c("2025-01", "2025-02", "2025-03"), each = 5),
  port = rep(c("基隆港", "台北港", "台中港", "高雄港", "花蓮港"), times = 3),
  empty_in = c(120, 90, 150, 400, 40,
               130, 95, 155, 420, 35,
               110, 85, 140, 450, 30),
  empty_out = c(100, 80, 130, 200, 20,
                90, 85, 120, 180, 18,
                95, 75, 125, 160, 15),
  throughput = c(800, 600, 1000, 3000, 200,
                 820, 610, 980, 3050, 190,
                 790, 590, 970, 3100, 180)
) %>%
  mutate(
    empty_net = empty_in - empty_out,
    pressure = empty_in / throughput,
    buffer = 1 - pressure
  )

matching_data <- data.frame(
  source_port = c("高雄港", "高雄港", "台中港"),
  target_port = c("台中港", "台北港", "基隆港"),
  source_pressure = c(0.145, 0.145, 0.144),
  target_buffer = c(0.856, 0.856, 0.861),
  distance_weight = c(2, 3, 2),
  score = c(0.81, 0.72, 0.68)
)

theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  primary = "#2563eb",
  secondary = "#64748b",
  success = "#16a34a",
  warning = "#f59e0b",
  danger = "#dc2626",
  base_font = font_google("Noto Sans TC"),
  heading_font = font_google("Noto Sans TC")
)

ui <- page_fillable(
  theme = theme,
  title = "空櫃失衡與跨港媒合儀表板",
  gap = "1rem",
  
  tags$head(
    tags$style(HTML("
      .app-title {
        font-size: 1.6rem;
        font-weight: 700;
        margin-bottom: 0.25rem;
      }
      .app-subtitle {
        color: #64748b;
        margin-bottom: 0;
      }
      .metric-label {
        color: #64748b;
        font-size: 0.9rem;
      }
      .sidebar-card {
        min-width: 280px;
      }
      .plot-card .card-body {
        min-height: 420px;
      }
      .summary-block {
        font-size: 1rem;
        line-height: 1.8;
        white-space: pre-line;
      }
    "))
  ),
  
  layout_column_wrap(
    width = 1/1,
    
    card(
      full_screen = FALSE,
      card_body(
        div(class = "app-title", "空櫃失衡與跨港媒合決策平台"),
        div(class = "app-subtitle", "Prototype Dashboard for Port Pressure, Buffer, and Cross-port Matching")
      )
    )
  ),
  
  layout_sidebar(
    sidebar = card(
      class = "sidebar-card",
      card_header("條件篩選"),
      selectInput("ym_select", "選擇年月", choices = unique(port_data$ym), selected = "2025-03"),
      selectInput("port_select", "選擇港口", choices = unique(port_data$port), selected = "高雄港"),
      sliderInput("pressure_threshold", "高壓力門檻", min = 0.05, max = 0.30, value = 0.12, step = 0.01),
      hr(),
      p("這個版本使用模擬資料，正式版可替換為政府公開資料。", class = "text-muted")
    ),
    
    navset_card_tab(
      id = "main_tabs",
      
      nav_panel(
        "總覽",
        layout_column_wrap(
          width = 1/3,
          value_box(title = "高壓力港數", value = textOutput("high_pressure_box"), theme = "danger"),
          value_box(title = "高承接港數", value = textOutput("high_buffer_box"), theme = "success"),
          value_box(title = "候選調度對", value = textOutput("matching_box"), theme = "primary")
        ),
        layout_column_wrap(
          width = 1/2,
          card(class = "plot-card", card_header("各港口壓力指標"), plotlyOutput("pressure_bar")),
          card(class = "plot-card", card_header("壓力 vs 承接能力"), plotlyOutput("pressure_buffer_scatter"))
        ),
        card(
          card_header("港口狀態總覽"),
          DTOutput("overview_table")
        )
      ),
      
      nav_panel(
        "港口分析",
        layout_column_wrap(
          width = 1/2,
          card(class = "plot-card", card_header("空櫃進出趨勢"), plotlyOutput("in_out_trend")),
          card(class = "plot-card", card_header("淨流量與壓力趨勢"), plotlyOutput("net_pressure_trend"))
        ),
        card(
          card_header("港口分析摘要"),
          div(class = "summary-block", textOutput("port_summary"))
        )
      ),
      
      nav_panel(
        "跨港媒合",
        card(
          card_header("跨港媒合優先順序"),
          DTOutput("matching_table")
        ),
        card(
          card_header("媒合建議摘要"),
          div(class = "summary-block", textOutput("matching_summary"))
        )
      ),
      
      nav_panel(
        "情境模擬",
        layout_column_wrap(
          width = 1/3,
          card(
            card_header("模擬參數"),
            numericInput("move_amount", "外移空櫃量", value = 50, min = 0, step = 10),
            selectInput("sim_source", "釋放港", choices = unique(port_data$port), selected = "高雄港"),
            selectInput("sim_target", "承接港", choices = unique(port_data$port), selected = "台中港")
          ),
          card(
            class = "plot-card",
            card_header("調度前後壓力比較"),
            plotlyOutput("simulation_plot")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  current_data <- reactive({
    port_data %>% filter(ym == input$ym_select)
  })
  
  output$high_pressure_box <- renderText({
    current_data() %>% filter(pressure >= input$pressure_threshold) %>% nrow()
  })
  
  output$high_buffer_box <- renderText({
    current_data() %>% filter(buffer >= 0.85) %>% nrow()
  })
  
  output$matching_box <- renderText({
    nrow(matching_data)
  })
  
  output$pressure_bar <- renderPlotly({
    p <- ggplot(current_data(), aes(x = reorder(port, pressure), y = pressure, fill = pressure)) +
      geom_col() +
      coord_flip() +
      labs(x = "港口", y = "壓力指標") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$pressure_buffer_scatter <- renderPlotly({
    p <- ggplot(current_data(), aes(x = pressure, y = buffer, label = port)) +
      geom_point(size = 4, color = "#2563eb") +
      geom_text(vjust = -0.8) +
      labs(x = "壓力指標", y = "承接緩衝") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$overview_table <- renderDT({
    df <- current_data() %>%
      mutate(
        status = case_when(
          pressure >= input$pressure_threshold ~ "高壓力港",
          buffer >= 0.85 ~ "高緩衝港",
          TRUE ~ "正常港"
        )
      ) %>%
      select(ym, port, empty_in, empty_out, empty_net, throughput, pressure, buffer, status)
    
    datatable(df, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  output$in_out_trend <- renderPlotly({
    df <- port_data %>% filter(port == input$port_select)
    p <- ggplot(df, aes(x = ym, group = 1)) +
      geom_line(aes(y = empty_in, color = "空櫃進港"), linewidth = 1.1) +
      geom_line(aes(y = empty_out, color = "空櫃出港"), linewidth = 1.1) +
      labs(x = "年月", y = "TEU", color = "指標") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$net_pressure_trend <- renderPlotly({
    df <- port_data %>% filter(port == input$port_select)
    p <- ggplot(df, aes(x = ym, group = 1)) +
      geom_line(aes(y = empty_net, color = "淨流量"), linewidth = 1.1) +
      geom_line(aes(y = pressure * 100, color = "壓力指標(×100)"), linewidth = 1.1) +
      labs(x = "年月", y = "值", color = "指標") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$port_summary <- renderText({
    df <- port_data %>% filter(port == input$port_select)
    latest <- df %>% tail(1)
    
    paste0(
      "港口：", latest$port, "\n",
      "最新月份：", latest$ym, "\n",
      "空櫃進港：", latest$empty_in, "\n",
      "空櫃出港：", latest$empty_out, "\n",
      "淨流量：", latest$empty_net, "\n",
      "壓力指標：", round(latest$pressure, 3), "\n",
      "承接緩衝：", round(latest$buffer, 3)
    )
  })
  
  output$matching_table <- renderDT({
    datatable(
      matching_data %>% arrange(desc(score)),
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
  output$matching_summary <- renderText({
    top1 <- matching_data %>% arrange(desc(score)) %>% slice(1)
    paste0(
      "目前建議優先調度對：", top1$source_port, " → ", top1$target_port, "\n",
      "綜合分數：", top1$score, "\n",
      "此排序綜合來源港壓力、承接港緩衝與距離權重。"
    )
  })
  
  output$simulation_plot <- renderPlotly({
    df <- current_data()
    
    source_row <- df %>% filter(port == input$sim_source)
    target_row <- df %>% filter(port == input$sim_target)
    
    before_after <- data.frame(
      port = c(input$sim_source, input$sim_target),
      before = c(source_row$pressure, target_row$pressure),
      after = c(
        max(source_row$pressure - input$move_amount / source_row$throughput, 0),
        target_row$pressure + input$move_amount / target_row$throughput
      )
    )
    
    long_df <- rbind(
      data.frame(port = before_after$port, status = "調度前", pressure = before_after$before),
      data.frame(port = before_after$port, status = "調度後", pressure = before_after$after)
    )
    
    p <- ggplot(long_df, aes(x = port, y = pressure, fill = status)) +
      geom_col(position = "dodge") +
      labs(x = "港口", y = "壓力指標") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
}

shinyApp(ui, server)