register_provenance_outputs <- function(server_env) {
  local({
      output$provenance_meta_chip <- renderUI({
        span(class = "c-chip", paste0(length(dashboard_context_files), " 份正式資料"))
      })

      output$provenance_report_ui <- renderUI({
        tags$pre(
          class = "lineage-pre",
          paste(build_data_origin_report(), collapse = "\n")
        )
      })
  }, envir = server_env)
}
