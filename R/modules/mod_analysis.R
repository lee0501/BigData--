analysis_page_ui <- function() {
  div(
    class = "page",
    page_head("港口分析"),
    div(
      class = "analysis-chart-grid",
      div(
        class = "card home-block analysis-chart-card",
        div(
          class = "c-inner",
          div(class = "c-head", span(class = "c-title", "空櫃進出趨勢"), uiOutput("analysis_chip1")),
          div(class = "chart-wrap analysis-chart-wrap", imageOutput("analysis_chart1", height = "360px"))
        )
      ),
      div(
        class = "card home-block analysis-chart-card",
        div(
          class = "c-inner",
          div(class = "c-head", span(class = "c-title", "淨流量與壓力趨勢"), uiOutput("analysis_chip2")),
          div(class = "chart-wrap analysis-chart-wrap", imageOutput("analysis_chart2", height = "360px"))
        )
      )
    ),
    div(
      class = "card home-block",
      div(
        class = "c-inner",
        div(
          class = "c-head",
          style = "align-items:flex-start;",
          div(
            style = "display:flex; flex-direction:column; gap:4px;",
            span(class = "c-title", "港口分析摘要"),
            span(style = "font-size:0.7rem; color:var(--muted);", "各港口動態數據檢視")
          ),
          div(class = "port-selectors", uiOutput("analysis_port_buttons"))
        ),
        uiOutput("analysis_summary_boxes"),
        uiOutput("analysis_insight_note")
      )
    )
  )
}
