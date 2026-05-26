forecast_page_ui <- function() {
  div(
    class = "page",
    page_head("官方統計基準"),
    div(
      class = "card home-block",
      div(
        class = "c-inner",
        div(class = "c-head", span(class = "c-title", "多年空櫃結構基準"), span(class = "c-chip", "港務公司公開統計")),
        div(
          class = "mode-note",
          "此頁不取代 iMarine 的 2025 明細壓力與媒合邏輯；它使用港務公司多年官方統計（民國 110–115 年）建立空櫃結構基準，用來檢查 iMarine 訊號是否高於多年常態與季節性背景。"
        )
      )
    ),
    div(
      class = "card home-block",
      div(
        class = "c-inner",
        div(class = "c-head", span(class = "c-title", "基準資料狀態")),
        uiOutput("baseline_kpis")
      )
    ),
    div(
      class = "card home-block",
      div(
        class = "c-inner",
        div(
          class = "c-head",
          div(
            span(class = "c-title", "空櫃占比多年基準線"),
            div(class = "c-subtitle", "實線為官方實際資料，虛線為 Prophet 往後延伸的預測基準。")
          )
        ),
        div(class = "port-toggle", uiOutput("baseline_port_buttons")),
        uiOutput("baseline_trend_result"),
        div(class = "chart-wrap analysis-chart-wrap", imageOutput("baseline_trend_chart", height = "360px"))
      )
    ),
    div(
      class = "card home-block",
      div(
        class = "c-inner",
        div(
          class = "c-head",
          div(
            span(class = "c-title", "iMarine 2025 壓力與多年基準對照"),
            div(class = "c-subtitle", "Bridge 對照 Prophet 同月預期值（yhat）；iMarine 使用同月港口相對壓力門檻。兩者回答不同問題。")
          ),
          uiOutput("baseline_month_chip")
        ),
        uiOutput("baseline_bridge_result"),
        uiOutput("baseline_bridge_table")
      )
    ),
    div(
      class = "card home-block",
      div(
        class = "c-inner",
        div(
          class = "c-head",
          div(
            span(class = "c-title", "優先檢查清單"),
            div(class = "c-subtitle", "列出未來月份中，空櫃結構偏高、更新 iMarine 明細後應優先確認的港口。非正式高壓預測，僅供提前關注使用。")
          )
        ),
        uiOutput("baseline_watchlist_legend"),
        uiOutput("baseline_watchlist_table")
      )
    ),
    div(
      class = "card home-block",
      div(
        class = "c-inner",
        div(
          class = "c-head",
          span(class = "c-title", "模型可信度說明"),
          span(class = "c-chip", "輔助參考")
        ),
        div(
          class = "mode-note subtle",
          "以下回測指標說明 Prophet 基準的準確程度，用來判斷上方對照結論是否值得採信，不是分析主線。"
        ),
        uiOutput("baseline_reliability_result"),
        uiOutput("baseline_backtest_table")
      )
    )
  )
}
