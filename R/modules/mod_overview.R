overview_page_ui <- function() {
  div(
    class = "page",
    page_head("空櫃總覽"),
    div(
      class = "overview-hero-grid",
      div(class = "card feat-card home-block overview-hero-card", div(class = "feat-inner", uiOutput("overview_featured"))),
      div(
        class = "card home-block overview-hero-card",
        div(
          class = "c-inner",
          div(class = "c-head", span(class = "c-title", "各港口壓力指標"), uiOutput("overview_month_chip")),
          div(class = "chart-wrap overview-pressure-wrap", imageOutput("overview_pressure_chart", height = "210px"))
        )
      )
    ),
    div(
      class = "overview-content-grid",
      div(
        class = "card home-block overview-status-card",
        div(
          class = "c-inner",
          div(
            class = "c-head",
            span(class = "c-title", "各港口狀態"),
            tags$a(href = "#", class = "feat-action", onclick = "switchPage('analysis'); return false;", "前往港口分析")
          ),
          uiOutput("overview_port_cards")
        )
      ),
      div(
        class = "card home-block overview-pairs-card",
        div(
          class = "c-inner",
          div(
            class = "c-head",
            span(class = "c-title", "調度推薦"),
            tags$a(href = "#", class = "feat-action", onclick = "switchPage('matching'); return false;", "前往跨港媒合")
          ),
          div(class = "pair-list", uiOutput("overview_pair_rows"))
        )
      )
    ),
    div(
      class = "ds-section",
      div(
        class = "dt-card",
        div(
          class = "dt-card-head",
          span(class = "dt-card-title", "原始數據"),
          uiOutput("overview_table_meta")
        ),
        div(
          class = "dt-controls",
          div(
            class = "dt-show-wrap",
            "Show",
            tags$select(
              id = "dt-show",
              class = "dt-show-select",
              onchange = "Shiny.setInputValue('dt_show', this.value, {priority:'event'})",
              tags$option(value = "5", "5"),
              tags$option(value = "10", selected = "selected", "10"),
              tags$option(value = "20", "20")
            ),
            "entries"
          ),
          div(
            class = "dt-search-wrap",
            "Search:",
            tags$input(
              id = "dt-search",
              class = "dt-search-input",
              type = "text",
              oninput = "Shiny.setInputValue('dt_search', this.value, {priority:'event'})"
            )
          )
        ),
        div(class = "dt-scroll-note", "欄位較多，手機版可左右滑動查看更多欄位。"),
        uiOutput("overview_table_block"),
        div(
          class = "dt-footer",
          span(class = "dt-info", uiOutput("overview_table_info")),
          div(class = "dt-pagination", uiOutput("overview_table_pager"))
        ),
        div(
          class = "dt-legend",
          div(class = "dt-legend-title", "欄位對照（上表欄位標題即為 CSV 欄位的中文化名稱）"),
          div(class = "dt-legend-note", "下方列出原始 CSV 欄位名稱，方便直接對照資料來源與欄位意義。"),
          span(class = "dt-legend-item", HTML("<code>ym</code> 年月")),
          span(class = "dt-legend-item", HTML("<code>port</code> 港口")),
          span(class = "dt-legend-item", HTML("<code>empty_in</code> 空櫃進港量")),
          span(class = "dt-legend-item", HTML("<code>empty_out</code> 空櫃出港量")),
          span(class = "dt-legend-item", HTML("<code>empty_net</code> 空櫃淨流量")),
          span(class = "dt-legend-item", HTML("<code>throughput</code> 港口總貨櫃裝卸量")),
          span(class = "dt-legend-item", HTML("<code>pressure_index</code> 壓力指數")),
          span(class = "dt-legend-item", HTML("<code>buffer_index</code> 緩衝指數"))
        )
      )
    )
  )
}
