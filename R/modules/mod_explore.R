explore_page_ui <- function() {
  div(
    class = "page",
    page_head("探索模式"),
    div(
      class = "explore-top-grid",
      div(
        class = "card home-block",
        div(
          class = "c-inner",
          div(class = "c-head", span(class = "c-title", "探索設定")),
          div(
            class = "sf-group",
            span(class = "sf-label", "探索類型"),
            selectInput(
              inputId = "explore_mode_select",
              label = NULL,
              choices = c("策略模板" = "strategy", "使用者自訂" = "custom"),
              selected = "strategy",
              width = "100%"
            )
          ),
          uiOutput("explore_control_panel")
        )
      ),
      div(
        class = "card home-block",
        div(
          class = "c-inner",
          div(class = "c-head", span(class = "c-title", "探索結果預覽"), uiOutput("overview_month_chip")),
          uiOutput("explore_port_cards")
        )
      )
    ),
    div(
      class = "card home-block",
      div(
        class = "c-inner",
        div(class = "c-head", span(class = "c-title", "探索模式候選配對"), span(class = "c-chip", "即時重算")),
        div(class = "table-caption", "調整上方模板或權重後，下表會立刻更新排序與模擬結果。"),
        uiOutput("explore_candidates_table")
      )
    )
  )
}
