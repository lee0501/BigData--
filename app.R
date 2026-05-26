source_root <- getwd()
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

shinyApp(ui, server)
