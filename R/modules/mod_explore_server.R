register_explore_outputs <- function(server_env) {
  local({
      output$explore_control_panel <- renderUI({
        slider_value <- function(id, fallback) {
          isolate({
            val <- suppressWarnings(as.numeric(input[[id]]))
            if (is.null(val) || length(val) == 0 || all(is.na(val))) fallback else val[[1]]
          })
        }
    
        if (active_explore_tab() == "strategy") {
          div(
            class = "sf-group",
            span(class = "sf-label", "策略模板"),
            selectInput(
              inputId = "strategy_template_select",
              label = NULL,
              choices = setNames(names(strategy_templates), vapply(strategy_templates, `[[`, character(1), "label")),
              selected = active_strategy_key(),
              width = "100%"
            ),
            div(class = "mode-note", active_strategy()$description)
          )
        } else {
          tagList(
            div(
              class = "adv-col",
              div(class = "adv-col-head", span(class = "adv-col-dot red"), span(class = "adv-col-title", "Pressure 權重")),
              div(class = "mode-note inline-note", "調整的是決策權重，不是直接修改港口狀態；Pressure 權重會先影響高壓辨識，再連動下方排序與候選配對。"),
              div(class = "mode-note subtle inline-note", "3 組權重都會自動維持總和 100%，且每一項限制在 10% 到 60%。"),
              custom_slider_card("flow", "空櫃進港比重", "Flow", slider_value("sl-flow", 22), "pressure", weight_help_text$flow),
              custom_slider_card("net", "當月淨增加", "Net", slider_value("sl-net", 60), "pressure", weight_help_text$net),
              custom_slider_card("roll", "近三月淨增加", "Trend", slider_value("sl-roll", 18), "pressure", weight_help_text$roll)
            ),
            div(class = "adv-col", div(class = "adv-col-head", span(class = "adv-col-dot green"), span(class = "adv-col-title", "Buffer 權重")), custom_slider_card("space", "空間餘裕", "Space", slider_value("sl-space", 45), "buffer", weight_help_text$space), custom_slider_card("netout", "歷史淨流出", "Net Out", slider_value("sl-netout", 40), "buffer", weight_help_text$netout), custom_slider_card("export", "出口拉力", "Export", slider_value("sl-export", 15), "buffer", weight_help_text$export)),
            div(class = "adv-col", div(class = "adv-col-head", span(class = "adv-col-dot green"), span(class = "adv-col-title", "Matching 權重")), custom_slider_card("source", "來源港壓力", "Source", slider_value("sl-source", 45), "matching", weight_help_text$source), custom_slider_card("target", "目標港緩衝", "Target", slider_value("sl-target", 35), "matching", weight_help_text$target), custom_slider_card("distance", "距離阻力", "Distance", slider_value("sl-distance", 20), "matching", weight_help_text$distance)),
            tags$script(HTML("window.refreshAllWeightGroups && window.refreshAllWeightGroups();"))
          )
        }
      })
    
      output$explore_port_cards <- renderUI({
        snapshot <- explore_month_status()
    
        if (nrow(snapshot) == 0) {
          return(div(class = "feat-desc", "本月份沒有可供探索模式顯示的資料。"))
        }
    
        ordered_ports <- snapshot %>%
          arrange(desc(pressure_index), desc(empty_net)) %>%
          pull(port)
    
        div(class = "port-cards", lapply(ordered_ports, function(port_name) {
          row <- snapshot %>% filter(port == port_name) %>% slice(1)
          pct_val <- round(row$pressure_index[[1]] * 100)
          lvl <- status_card_class(row$status[[1]])
    
          div(
            class = paste("port-card", lvl),
            div(
              class = "pc-header",
              div(
                class = "pc-name-group",
                div(class = paste("pc-dot", lvl)),
                div(
                  div(class = "pc-name", display_port(port_name)),
                  div(class = "pc-code", port_codes[[port_name]])
                )
              ),
              span(class = paste("pc-badge", lvl), status_badge_label(row$status[[1]]))
            ),
            div(class = "pc-stats", span("探索壓力 / 緩衝"), span(class = "pc-stat-val", paste0(fmt_idx(row$pressure_index[[1]], 3), " / ", fmt_idx(row$buffer_index[[1]], 3)))),
            div(class = "pc-bar-row", span(style = "font-size:0.72rem;color:var(--muted);white-space:nowrap;", "壓力指標"), div(class = "pc-bar-track", div(class = paste("pc-bar-fill", lvl), style = paste0("width:", pct_val, "%"))), span(class = paste("pc-pct", lvl), paste0(pct_val, "%")))
          )
        }))
      })
    
      output$explore_candidates_table <- renderUI({
        rows <- explore_candidates()
    
        if (nrow(rows) == 0) {
          return(div(class = "feat-desc", "目前探索模式下沒有形成有效候選配對。"))
        }
    
        div(
          class = "dt-table-wrap has-scroll explore-table-wrap",
          tags$table(
            class = "dt-table explore-table",
            tags$thead(
              tags$tr(
                tags$th("來源港"),
                tags$th("建議承接港"),
                tags$th("建議調度量"),
                tags$th("模擬結果"),
                tags$th("說明")
              )
            ),
            tags$tbody(
              lapply(seq_len(nrow(rows)), function(i) {
                row <- rows[i, ]
                tags$tr(
                  tags$td(display_port(row$source_port[[1]])),
                  tags$td(display_port(row$target_port[[1]])),
                  tags$td(paste0(fmt_num(row$move_amount[[1]], 0), " TEU")),
                  tags$td(row$simulation_result[[1]]),
                  tags$td(class = "desc-cell", row$summary_text[[1]])
                )
              })
            )
          )
        )
      })
  }, envir = server_env)
}
