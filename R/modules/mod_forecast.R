forecast_page_ui <- function() {
  div(
    class = "page",
    page_head("Forecast / 預警"),
    div(
      class = "card home-block",
      div(
        class = "c-inner",
        div(class = "c-head", span(class = "c-title", "Forecast 插槽保留")),
        div(
          class = "mode-note",
          "Forecast 模組保留給 Prophet backtest 通過後再啟用。在此之前不接入正式 dashboard。"
        )
      )
    )
  )
}
