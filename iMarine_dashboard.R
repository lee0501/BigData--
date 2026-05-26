locate_source_root <- function() {
  candidates <- unique(c(
    getwd(),
    "/Users/lee/Documents/BigData"
  ))

  hits <- candidates[
    vapply(
      candidates,
      function(x) file.exists(file.path(x, "R", "dashboard_setup.R")),
      logical(1)
    )
  ]

  if (!length(hits)) {
    stop("Cannot locate the dashboard source root containing R/dashboard_setup.R")
  }

  normalizePath(hits[[1]], winslash = "/", mustWork = TRUE)
}

source_root <- locate_source_root()
shiny::addResourcePath("dashboard-static", file.path(source_root, "www"))

source_files <- c(
  "R/dashboard_setup.R",
  "R/dashboard_globals.R",
  "R/dashboard_utils.R",
  "R/dashboard_compute.R",
  "R/dashboard_data.R",
  "R/dashboard_port_baseline.R",
  "R/dashboard_matching_compute.R",
  "R/dashboard_ui_helpers.R",
  "R/modules/mod_overview.R",
  "R/modules/mod_analysis.R",
  "R/modules/mod_matching.R",
  "R/modules/mod_explore.R",
  "R/modules/mod_provenance.R",
  "R/modules/mod_simulation.R",
  "R/modules/mod_forecast.R",
  "R/modules/mod_shell_server.R",
  "R/modules/mod_explore_server.R",
  "R/modules/mod_overview_server.R",
  "R/modules/mod_analysis_server.R",
  "R/modules/mod_matching_server.R",
  "R/modules/mod_forecast_server.R",
  "R/modules/mod_provenance_server.R",
  "R/dashboard_ui.R",
  "R/dashboard_server.R"
)

for (rel_path in source_files) {
  source(file.path(source_root, rel_path), local = FALSE)
}

app <- shinyApp(ui, server)

if (sys.nframe() == 0) {
  shiny::runApp(app, launch.browser = interactive())
} else {
  app
}
