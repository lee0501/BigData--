provenance_page_ui <- function() {
  div(
    class = "page",
    page_head("資料來歷"),
    div(
      class = "card home-block",
      div(
        class = "c-inner",
        div(class = "c-head", span(class = "c-title", "資料檔案檢查"), uiOutput("provenance_meta_chip")),
        div(class = "table-caption", "直接列出目前 app 讀取的正式資料檔、路徑位置與可用月份。"),
        uiOutput("provenance_report_ui")
      )
    )
  )
}
