library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

# ----------------------------
# Demo data
# Keep mock data, but apply revised indicator logic
# ----------------------------
port_data <- data.frame(
  ym = rep(c("2024-10", "2024-11", "2024-12", "2025-01", "2025-02", "2025-03"), each = 5),
  port = rep(c("基隆港", "台北港", "台中港", "高雄港", "花蓮港"), times = 6),
  empty_in = c(
    105, 82, 135, 350, 45,
    112, 86, 142, 365, 43,
    118, 88, 148, 380, 40,
    120, 90, 150, 400, 40,
    130, 95, 155, 420, 35,
    110, 85, 140, 450, 30
  ),
  empty_out = c(
    100, 88, 132, 250, 26,
    98, 90, 130, 235, 24,
    95, 85, 128, 220, 22,
    100, 80, 130, 200, 20,
    90, 85, 120, 180, 18,
    95, 75, 125, 160, 15
  ),
  full_in = c(
    290, 240, 360, 1200, 50,
    300, 245, 370, 1220, 48,
    305, 250, 375, 1250, 46,
    310, 255, 380, 1280, 45,
    315, 258, 390, 1300, 44,
    300, 250, 385, 1320, 42
  ),
  full_out = c(
    320, 260, 390, 1350, 30,
    325, 268, 395, 1380, 28,
    330, 275, 405, 1420, 27,
    340, 280, 410, 1450, 25,
    350, 290, 420, 1480, 24,
    345, 285, 415, 1500, 22
  ),
  throughput = c(
    820, 620, 980, 3000, 180,
    830, 625, 995, 3020, 178,
    840, 630, 1005, 3050, 175,
    800, 600, 1000, 3000, 200,
    820, 610, 980, 3050, 190,
    790, 590, 970, 3100, 180
  ),
  stringsAsFactors = FALSE
)

distance_lookup <- data.frame(
  source_port = c(
    "基隆港", "基隆港", "基隆港", "基隆港",
    "台北港", "台北港", "台北港", "台北港",
    "台中港", "台中港", "台中港", "台中港",
    "高雄港", "高雄港", "高雄港", "高雄港",
    "花蓮港", "花蓮港", "花蓮港", "花蓮港"
  ),
  target_port = c(
    "台北港", "台中港", "高雄港", "花蓮港",
    "基隆港", "台中港", "高雄港", "花蓮港",
    "基隆港", "台北港", "高雄港", "花蓮港",
    "基隆港", "台北港", "台中港", "花蓮港",
    "基隆港", "台北港", "台中港", "高雄港"
  ),
  distance_factor = c(
    1.00, 0.85, 0.70, 0.60,
    1.00, 0.85, 0.70, 0.60,
    0.85, 0.85, 0.85, 0.60,
    0.70, 0.70, 0.85, 0.60,
    0.60, 0.60, 0.60, 0.60
  ),
  stringsAsFactors = FALSE
)

percent_rank_safe <- function(x) {
  dplyr::percent_rank(replace(x, is.na(x), min(x, na.rm = TRUE)))
}

port_data <- port_data %>%
  arrange(port, ym) %>%
  group_by(port) %>%
  mutate(
    empty_net = empty_in - empty_out,
    empty_total_flow = empty_in + empty_out,
    full_total_flow = full_in + full_out,
    pressure_flow_raw = empty_in / throughput,
    pressure_net_raw = pmax(empty_net, 0) / throughput,
    roll3_empty_net = (
      empty_net +
      lag(empty_net, 1, default = first(empty_net)) +
      lag(empty_net, 2, default = first(empty_net))
    ) / 3,
    pressure_roll_raw = pmax(roll3_empty_net, 0) / throughput,
    export_pull = full_out / throughput,
    buffer_space_raw = 1 - pressure_flow_raw,
    buffer_net_raw = pmax(-roll3_empty_net, 0) / throughput,
    buffer_export_raw = export_pull,
    pressure_flow_pct = percent_rank_safe(pressure_flow_raw),
    pressure_net_pct = percent_rank_safe(pressure_net_raw),
    pressure_roll_pct = percent_rank_safe(pressure_roll_raw),
    buffer_space_pct = percent_rank_safe(buffer_space_raw),
    buffer_net_pct = percent_rank_safe(buffer_net_raw),
    buffer_export_pct = percent_rank_safe(buffer_export_raw),
    pressure_index = 0.35 * pressure_flow_pct +
      0.40 * pressure_net_pct +
      0.25 * pressure_roll_pct,
    buffer_index = 0.45 * buffer_space_pct +
      0.35 * buffer_net_pct +
      0.20 * buffer_export_pct,
    cum_empty_net = cumsum(empty_net)
  ) %>%
  ungroup()

latest_month <- max(port_data$ym)

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
  title = "空櫃失衡與跨港媒合決策平台 Demo",
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
      .sidebar-card {
        min-width: 290px;
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
      card_body(
        div(class = "app-title", "空櫃失衡與跨港媒合決策平台"),
        div(
          class = "app-subtitle",
          "Demo with revised Pressure Index / Buffer Index / Status / Matching Score on mock data"
        )
      )
    )
  ),
  layout_sidebar(
    sidebar = card(
      class = "sidebar-card",
      card_header("條件篩選"),
      selectInput("ym_select", "選擇年月", choices = sort(unique(port_data$ym)), selected = latest_month),
      selectInput("port_select", "選擇港口", choices = unique(port_data$port), selected = "高雄港"),
      sliderInput("top_quantile", "高指標分位門檻", min = 0.60, max = 0.90, value = 0.75, step = 0.05),
      numericInput("move_amount", "模擬外移空櫃量", value = 50, min = 0, step = 10),
      hr(),
      p("本版仍使用假資料，但指標邏輯已改為研究草案版。", class = "text-muted")
    ),
    navset_card_tab(
      id = "main_tabs",
      nav_panel(
        "總覽",
        layout_column_wrap(
          width = 1/3,
          value_box(title = "高壓力港數", value = textOutput("high_pressure_box"), theme = "danger"),
          value_box(title = "高緩衝港數", value = textOutput("high_buffer_box"), theme = "success"),
          value_box(title = "候選調度對", value = textOutput("matching_box"), theme = "primary")
        ),
        layout_column_wrap(
          width = 1/2,
          card(class = "plot-card", card_header("各港口 Pressure Index"), plotlyOutput("pressure_bar")),
          card(class = "plot-card", card_header("Pressure vs Buffer"), plotlyOutput("pressure_buffer_scatter"))
        ),
        card(card_header("港口狀態總覽"), DTOutput("overview_table")),
        card(
          card_header("資料適用範圍說明"),
          div(
            class = "summary-block",
            textOutput("data_scope_summary")
          )
        )
      ),
      nav_panel(
        "港口分析",
        layout_column_wrap(
          width = 1/2,
          card(class = "plot-card", card_header("空櫃淨流量與累積趨勢"), plotlyOutput("net_cum_trend")),
          card(class = "plot-card", card_header("Pressure / Buffer Index 趨勢"), plotlyOutput("index_trend"))
        ),
        card(card_header("港口分析摘要"), div(class = "summary-block", textOutput("port_summary")))
      ),
      nav_panel(
        "跨港媒合",
        card(card_header("跨港媒合優先順序"), DTOutput("matching_table")),
        card(card_header("媒合建議摘要"), div(class = "summary-block", textOutput("matching_summary"))),
        card(card_header("目前未納入資訊"), div(class = "summary-block", textOutput("missing_data_summary")))
      ),
      nav_panel(
        "情境模擬",
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("模擬參數"),
            selectInput("sim_source", "釋放港", choices = unique(port_data$port), selected = "高雄港"),
            selectInput("sim_target", "承接港", choices = unique(port_data$port), selected = "台中港")
          ),
          card(class = "plot-card", card_header("調度前後 Index 比較"), plotlyOutput("simulation_plot"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  current_data <- reactive({
    df <- port_data %>% filter(ym == input$ym_select)
    high_cut_pressure <- quantile(df$pressure_index, probs = input$top_quantile, na.rm = TRUE)
    high_cut_buffer <- quantile(df$buffer_index, probs = input$top_quantile, na.rm = TRUE)
    low_cut_pressure <- quantile(df$pressure_index, probs = 0.50, na.rm = TRUE)

    df %>%
      mutate(
        status = case_when(
          pressure_index >= high_cut_pressure & roll3_empty_net > 0 ~ "高壓力港",
          buffer_index >= high_cut_buffer & pressure_index <= low_cut_pressure ~ "高緩衝港",
          TRUE ~ "正常港"
        )
      )
  })

  matching_data <- reactive({
    df <- current_data()
    sources <- df %>%
      filter(status == "高壓力港") %>%
      select(source_port = port, source_pressure_index = pressure_index, source_empty_net = empty_net)
    targets <- df %>%
      filter(status == "高緩衝港") %>%
      select(target_port = port, target_buffer_index = buffer_index, target_pressure_index = pressure_index)

    if (nrow(sources) == 0 || nrow(targets) == 0) {
      return(data.frame())
    }

    merge(sources, targets, by = NULL) %>%
      filter(source_port != target_port) %>%
      left_join(distance_lookup, by = c("source_port", "target_port")) %>%
      mutate(
        distance_factor = ifelse(is.na(distance_factor), 0.60, distance_factor),
        matching_score = 0.45 * source_pressure_index +
          0.35 * target_buffer_index +
          0.20 * distance_factor
      ) %>%
      arrange(desc(matching_score))
  })

  output$high_pressure_box <- renderText({
    current_data() %>% filter(status == "高壓力港") %>% nrow()
  })

  output$high_buffer_box <- renderText({
    current_data() %>% filter(status == "高緩衝港") %>% nrow()
  })

  output$matching_box <- renderText({
    nrow(matching_data())
  })

  output$pressure_bar <- renderPlotly({
    df <- current_data()
    p <- ggplot(df, aes(x = reorder(port, pressure_index), y = pressure_index, fill = status)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("高壓力港" = "#dc2626", "正常港" = "#9ca3af", "高緩衝港" = "#16a34a")) +
      labs(x = "港口", y = "Pressure Index", fill = "狀態") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })

  output$pressure_buffer_scatter <- renderPlotly({
    df <- current_data()
    p <- ggplot(df, aes(x = pressure_index, y = buffer_index, color = status, label = port)) +
      geom_point(size = 4) +
      geom_text(vjust = -0.8, show.legend = FALSE) +
      scale_color_manual(values = c("高壓力港" = "#dc2626", "正常港" = "#9ca3af", "高緩衝港" = "#16a34a")) +
      labs(x = "Pressure Index", y = "Buffer Index", color = "狀態") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })

  output$overview_table <- renderDT({
    df <- current_data() %>%
      select(
        ym, port, empty_in, empty_out, full_out, throughput,
        empty_net, roll3_empty_net, pressure_index, buffer_index, status
      ) %>%
      rename(
        "年月" = ym,
        "港口" = port,
        "空櫃進港量" = empty_in,
        "空櫃出港量" = empty_out,
        "實櫃出口量" = full_out,
        "港口總裝卸量" = throughput,
        "空櫃淨流量" = empty_net,
        "近三月平均淨流量" = roll3_empty_net,
        "空櫃壓力指數" = pressure_index,
        "承接緩衝指數" = buffer_index,
        "港口狀態" = status
      ) %>%
      mutate(
        `空櫃壓力指數` = round(`空櫃壓力指數`, 3),
        `承接緩衝指數` = round(`承接緩衝指數`, 3),
        `近三月平均淨流量` = round(`近三月平均淨流量`, 1)
      )

    datatable(df, options = list(pageLength = 10, autoWidth = TRUE))
  })

  output$net_cum_trend <- renderPlotly({
    df <- port_data %>% filter(port == input$port_select)
    p <- ggplot(df, aes(x = ym, group = 1)) +
      geom_line(aes(y = empty_net, color = "當月淨流量"), linewidth = 1.1) +
      geom_line(aes(y = cum_empty_net, color = "累積淨流量"), linewidth = 1.1) +
      labs(x = "年月", y = "TEU", color = "指標") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })

  output$index_trend <- renderPlotly({
    df <- port_data %>% filter(port == input$port_select)
    p <- ggplot(df, aes(x = ym, group = 1)) +
      geom_line(aes(y = pressure_index, color = "Pressure Index"), linewidth = 1.1) +
      geom_line(aes(y = buffer_index, color = "Buffer Index"), linewidth = 1.1) +
      labs(x = "年月", y = "Index", color = "指標") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })

  output$port_summary <- renderText({
    df <- current_data() %>% filter(port == input$port_select)
    if (nrow(df) == 0) {
      return("查無資料")
    }

    latest <- df %>% slice(1)
    paste0(
      "港口：", latest$port, "\n",
      "月份：", latest$ym, "\n",
      "空櫃淨流量：", latest$empty_net, "\n",
      "近三月平均淨流量：", round(latest$roll3_empty_net, 1), "\n",
      "Pressure Index：", round(latest$pressure_index, 3), "\n",
      "Buffer Index：", round(latest$buffer_index, 3), "\n",
      "狀態：", latest$status, "\n",
      "解讀：",
      if (latest$status == "高壓力港") {
        "近期累積壓力偏高，屬優先釋放端。"
      } else if (latest$status == "高緩衝港") {
        "相對低壓且具承接潛力，可作候選承接端。"
      } else {
        "目前處於中間狀態，建議持續觀察。"
      }
    )
  })

  output$matching_table <- renderDT({
    df <- matching_data()
    if (nrow(df) == 0) {
      return(datatable(data.frame(訊息 = "本月沒有符合條件的高壓力港 / 高緩衝港配對")))
    }

    datatable(
      df %>%
        transmute(
          `釋放港` = source_port,
          `承接港` = target_port,
          `釋放港壓力指數` = round(source_pressure_index, 3),
          `承接港緩衝指數` = round(target_buffer_index, 3),
          `距離因子` = round(distance_factor, 2),
          `媒合分數` = round(matching_score, 3)
        ),
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })

  output$data_scope_summary <- renderText({
    paste0(
      "本儀表板目前能直接呈現的是：空櫃流入/流出、淨流量、近月累積趨勢、相對壓力、相對緩衝，以及依此產生的跨港候選排序。\n",
      "這些都可由公開月資料或其衍生欄位支持，因此可以做成可比較、可解釋的 proxy 指標。\n\n",
      "本儀表板目前沒有直接呈現的，是公開資料不足以支撐的內容，例如：真實空櫃庫存、真實堆場剩餘容量、航商別可調度限制、實際成本與船期可行性。\n",
      "因此這裡的『承接緩衝指數』代表相對承接潛力，不等於真實剩餘空間；『媒合分數』代表優先順序建議，不等於最終可執行排程。"
    )
  })

  output$matching_summary <- renderText({
    df <- matching_data()
    if (nrow(df) == 0) {
      return("本月沒有形成可解釋的高壓力港與高緩衝港配對，可能代表門檻過高或假資料差異不足。")
    }

    top1 <- df %>% slice(1)
    paste0(
      "目前建議優先調度對：", top1$source_port, " → ", top1$target_port, "\n",
      "來源港 Pressure Index：", round(top1$source_pressure_index, 3), "\n",
      "承接港 Buffer Index：", round(top1$target_buffer_index, 3), "\n",
      "距離因子：", round(top1$distance_factor, 2), "\n",
      "媒合分數：", round(top1$matching_score, 3), "\n",
      "說明：此排序是根據公開資料可觀察到的相對壓力、相對緩衝與距離規則產生，屬決策支援建議。"
    )
  })

  output$missing_data_summary <- renderText({
    paste0(
      "目前未納入但若要更接近真實營運，仍需補蒐集的資料包括：\n",
      "1. 港口或場站空櫃月末存量：用來辨識真實空櫃庫存，而不是只看流入流出。\n",
      "2. 堆場容量與使用率：用來判斷真實剩餘空間，而不是只用 throughput 推估。\n",
      "3. 航商別或櫃型別資料：用來處理不同航商間不能直接互換、不同櫃型不能混用的限制。\n",
      "4. 船期、艙位與調度成本：用來判斷建議配對是否真的能在某月執行，並進一步做最佳化。\n\n",
      "所以目前看起來『都做得出來』，是因為這版儀表板刻意只展示公開資料足以支撐的層次；不足的部分不是硬做，而是暫時不呈現，或以 proxy 指標帶過。"
    )
  })

  output$simulation_plot <- renderPlotly({
    df <- current_data()
    source_row <- df %>% filter(port == input$sim_source)
    target_row <- df %>% filter(port == input$sim_target)

    validate(need(nrow(source_row) == 1, "找不到釋放港資料"))
    validate(need(nrow(target_row) == 1, "找不到承接港資料"))

    source_delta <- input$move_amount / source_row$throughput
    target_delta <- input$move_amount / target_row$throughput

    sim_df <- data.frame(
      port = c(input$sim_source, input$sim_target),
      metric = rep(c("Pressure Index", "Buffer Index"), each = 2),
      before = c(
        source_row$pressure_index, target_row$pressure_index,
        source_row$buffer_index, target_row$buffer_index
      ),
      after = c(
        pmax(source_row$pressure_index - source_delta, 0),
        pmin(target_row$pressure_index + target_delta, 1),
        pmin(source_row$buffer_index + source_delta, 1),
        pmax(target_row$buffer_index - target_delta, 0)
      )
    )

    long_df <- rbind(
      data.frame(port = sim_df$port, metric = sim_df$metric, status = "調度前", value = sim_df$before),
      data.frame(port = sim_df$port, metric = sim_df$metric, status = "調度後", value = sim_df$after)
    )

    p <- ggplot(long_df, aes(x = port, y = value, fill = status)) +
      geom_col(position = "dodge") +
      facet_wrap(~metric, scales = "free_y") +
      labs(x = "港口", y = "Index") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
}

shinyApp(ui, server)
