simulation_page_ui <- function() {
  div(
    class = "page",
    page_head("Simulation"),
    div(
      class = "card home-block",
      div(
        class = "c-inner",
        div(class = "c-head", span(class = "c-title", "Simulation 結構預留")),
        div(
          class = "mode-note",
          "目前 dashboard 正在進行模組化重整。正式 simulation 元件後續會從 matching 工作流中拆分到這個模組。"
        )
      )
    )
  )
}
