matching_page_ui <- function() {
  div(
    class = "page",
    page_head("跨港媒合"),
    fluidRow(
      column(
        width = 12,
        div(
          class = "card home-block",
          div(
            class = "c-inner",
              div(
                class = "c-head",
                div(
                  div(class = "c-title", "調度配對設定"),
                div(class = "c-subtitle", "設定來源港口與調度參數後，下方候選方案會依目前公式即時重算")
              ),
              div(
                style = "display:flex;align-items:center;gap:12px;",
                span(style = "display:inline-flex;align-items:center;gap:4px;font-size:0.67rem;color:var(--muted);white-space:nowrap;opacity:0.85;", uiOutput("matching_update_stamp")),
                span(class = "c-chip", "即時推演")
              )
            ),
            uiOutput("matching_settings_bar"),
            uiOutput("matching_logic_note"),
            uiOutput("matching_results_state")
          )
        )
      )
    )
  )
}
