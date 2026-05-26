register_matching_outputs <- function(server_env) {
  local({
      output$matching_update_stamp <- renderUI({
        HTML(
          if (active_mode() == "formal") {
            "正式模式・官方權重・即時模擬"
          } else {
            paste0("探索模式・", if (active_explore_tab() == "strategy") "策略模板" else "使用者自訂", "・即時重算")
          }
        )
      })

      output$matching_logic_note <- renderUI({
        source_name <- display_port(match_current_source())
        move_amount <- fmt_num(match_current_volume(), 0)
        mode_text <- if (active_mode() == "formal") {
          "使用正式權重與正式 status 歷史"
        } else {
          paste0("使用探索模式目前的 ", if (active_explore_tab() == "strategy") "策略模板" else "使用者自訂", " 權重")
        }

        div(
          class = "formula-note",
          div(class = "formula-title", "即時計算邏輯"),
          div(class = "formula-line", paste0("1. ", mode_text, "，以 ", source_name, " 作為來源港。")),
          div(class = "formula-line", paste0("2. 將目前設定的 ", move_amount, " TEU 套入推演，重算來源港與各目標港的壓力 / 緩衝變化。")),
          div(class = "formula-line", "3. 依來源港壓力、目標港緩衝與距離阻力綜合排序；日期欄位僅作情境註記，不進入公式。")
        )
      })
    
      output$matching_settings_bar <- renderUI({
        choices <- match_source_choices()
        current_source <- display_port(match_current_source())
        current_volume <- round(match_current_volume(), 0)
        current_date <- match_current_date()
    
        div(
          class = "cross-settings-bar",
          div(
            class = "csb-group csb-narrow",
            tags$label(`for` = "sim-src", "來源港口 — 高壓力"),
            tags$select(
              id = "sim-src",
              class = "csb-select-danger",
              onchange = "Shiny.setInputValue('sim_src', this.value, {priority:'event'})",
              if (nrow(choices) == 0) {
                tags$option(value = "", "本月無可用來源港")
              } else {
                lapply(seq_len(nrow(choices)), function(i) {
                  choice <- choices[i, ]
                  tags$option(
                    value = display_port(choice$value[[1]]),
                    selected = if (display_port(choice$value[[1]]) == current_source) "selected" else NULL,
                    choice$label[[1]]
                  )
                })
              }
            )
          ),
          div(
            class = "csb-group csb-md",
            tags$label(`for` = "sim-volume", "調度數量 (TEU)"),
            tags$input(
              id = "sim-volume",
              type = "number",
              min = "1",
              step = "100",
              value = current_volume,
              inputmode = "numeric",
              onchange = "Shiny.setInputValue('sim_volume', this.value, {priority:'event'})",
              onfocus = "this.select()"
            )
          ),
          div(
            class = "csb-group csb-md",
            tags$label(`for` = "sim-date", "預計執行日"),
            tags$input(
              id = "sim-date",
              type = "date",
              value = current_date,
              onchange = "Shiny.setInputValue('sim_date', this.value, {priority:'event'})"
            )
          ),
          div(
            class = "csb-actions",
            tags$button(
              type = "button",
              class = "live-pill-btn",
              onclick = "Shiny.setInputValue('matching_refresh', Date.now(), {priority:'event'})",
              "重新推演"
            )
          )
        )
      })

      output$matching_results_state <- renderUI({
        cards <- match_candidates()
        selected_card <- match_selected_card()
    
        if (nrow(cards) == 0 || nrow(selected_card) == 0) {
          return(
            div(
              class = "cross-empty-state",
              div(class = "ces-title", "目前沒有可用候選"),
              div(class = "ces-sub", "該月份沒有可組成的 target 候選。")
            )
          )
        }
    
        source_drop <- selected_card$source_pressure[[1]] - selected_card$source_pressure_after[[1]]
        source_relief_pct <- ifelse(selected_card$source_pressure[[1]] <= 0, 0, round(source_drop / selected_card$source_pressure[[1]] * 100))
        selected_class <- selected_card$simulation_result[[1]]
    
        card_ui <- lapply(seq_len(nrow(cards)), function(i) {
          row <- cards[i, ]
          active_cls <- if (row$candidate_key[[1]] == selected_card$candidate_key[[1]]) "msc-active" else ""
          bar_cls <- if (row$pressure_after_pct[[1]] >= 75) "red" else if (row$pressure_after_pct[[1]] >= 45) "amber" else "green"
          rag_cls <- if (row$pressure_before_pct[[1]] >= 75) "rag-red" else if (row$pressure_before_pct[[1]] >= 45) "rag-amber" else "rag-green"
          scenario_label <- if ("scenario_label" %in% names(row)) row[["scenario_label"]][[1]] else NULL
          code_suffix <- if (!is.null(scenario_label) && nzchar(scenario_label)) {
            paste0("・", scenario_label)
          } else if (i == 1) {
            "・最優先"
          } else if (i == 2) {
            "・次選"
          } else {
            "・備選"
          }
    
          tags$div(
            class = paste("match-sugg-card", active_cls),
            onclick = sprintf("selectMatchingTarget('%s')", row$candidate_key[[1]]),
            div(
              class = "msc-header",
              div(class = "msc-rank", HTML(c("①", "②", "③")[i])),
              div(
                style = "flex:1;min-width:0;",
                div(class = "msc-name", display_port(row$target_port[[1]])),
                div(class = "msc-code", paste0(port_codes[[row$target_port[[1]]]], code_suffix))
              ),
              if (i == 1) span(class = "msc-best-badge", "系統首選")
            ),
            div(class = "msc-summary", row$summary_text[[1]]),
            div(
              class = "msc-tags",
              if (row$pair_tier[[1]] == "正式推薦") span(class = "msc-tag blue", "正式推薦"),
              if (row$target_status[[1]] == "高緩衝港") span(class = "msc-tag green", "可承接"),
              if (row$simulation_result[[1]] == "可緩解但未解壓") span(class = "msc-tag amber", "部分緩解"),
              if (row$simulation_result[[1]] == "不建議調度") span(class = "msc-tag amber", "需保守")
            ),
            div(
              class = "msc-usage-row",
              span(class = "msc-util-label", HTML(sprintf("<span class='rag-dot %s'></span>壓力", rag_cls))),
              span(
                style = paste0("color:", if (bar_cls == "red") "var(--danger)" else if (bar_cls == "amber") "var(--warning)" else "var(--success)", ";font-weight:700;"),
                paste0(row$pressure_before_pct[[1]], "%")
              )
            ),
            div(
              class = "msc-prog-track",
              div(class = paste("msc-prog-fill", bar_cls), style = paste0("width:", row$pressure_before_pct[[1]], "%")),
              div(
                class = "msc-prog-increase",
                style = paste0("left:", row$pressure_before_pct[[1]], "%;width:", row$pressure_increase_pct[[1]], "%;background-color:",
                               if (bar_cls == "red") "rgba(220,38,38,.35)" else if (bar_cls == "amber") "rgba(217,119,6,.35)" else "rgba(22,163,74,.35)", ";")
              )
            )
          )
        })
    
        div(
          id = "cross-results",
          div(
            class = "cross-rec-area",
            div(
              class = "match-right-header",
              div(
                div(class = "mh-title", "可承接港口建議"),
                div(
                  class = "mh-sub",
                  paste0(
                    "目前模擬：",
                    display_port(match_current_source()),
                    " 調移 ",
                    fmt_num(match_current_volume(), 0),
                    " TEU，以下方案依容量・距離・成本即時重排。"
                  )
                )
              ),
              span(class = "c-chip", paste0(nrow(cards), " 個方案"))
            ),
            div(
              id = "sim-result-content",
              class = "sim-kpi-row",
              div(
                class = "sim-kpi-card",
                div(class = "sk-label", paste0(display_port(selected_card$source_port[[1]]), " 壓力指數")),
                div(class = "sk-val", style = "color:var(--success);", round(selected_card$source_pressure_after[[1]] * 100)),
                div(class = "sk-diff", HTML(paste0("↓ ", round(source_drop * 100), " <span style='font-size:0.68rem'>(原 ", round(selected_card$source_pressure[[1]] * 100), ")</span>")))
              ),
              div(
                class = "sim-kpi-card",
                div(class = "sk-label", "整體疏解率"),
                div(class = "sk-val", style = "color:var(--blue);", paste0(source_relief_pct, "%")),
                div(class = "sk-diff", paste0("調移 ", fmt_num(match_current_volume(), 0), " TEU"))
              )
            ),
            div(class = "match-sugg-grid", card_ui)
          )
        )
      })
  }, envir = server_env)
}
