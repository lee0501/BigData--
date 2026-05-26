ui <- fluidPage(
  tags$head(
    tags$title("iMarine 空櫃失衡決策平台"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Noto+Sans+TC:wght@400;500;700&display=swap"
    ),
    tags$link(rel = "stylesheet", href = "dashboard-static/dashboard.css"),
    tags$script(src = "dashboard-static/dashboard.js")
  ),
  div(
    class = "app",
    div(class = "sidebar-overlay", id = "sidebarOverlay"),
    sidebar_ui(default_month = 3),
    div(
      class = "main-area",
      tabsetPanel(
        id = "main_nav",
        type = "hidden",
        selected = "overview",
        tabPanel(title = NULL, value = "overview", overview_page_ui()),
        tabPanel(title = NULL, value = "analysis", analysis_page_ui()),
        tabPanel(title = NULL, value = "matching", matching_page_ui()),
        tabPanel(title = NULL, value = "explore", explore_page_ui()),
        tabPanel(title = NULL, value = "provenance", provenance_page_ui())
      )
    )
  )
)
