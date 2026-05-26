register_shell_outputs <- function(server_env) {
  local({
      output$sidebar_nav <- renderUI({
        current_page <- input$main_nav %||% rv$main_nav %||% "overview"
    
        div(
          class = "nav-tabs",
          nav_tab(
            "overview",
            "空櫃總覽",
            identical(current_page, "overview"),
            "<svg width='14' height='14' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><rect x='3' y='3' width='7' height='7' /><rect x='14' y='3' width='7' height='7' /><rect x='14' y='14' width='7' height='7' /><rect x='3' y='14' width='7' height='7' /></svg>"
          ),
          nav_tab(
            "analysis",
            "港口分析",
            identical(current_page, "analysis"),
            "<svg width='14' height='14' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><circle cx='12' cy='12' r='10' /><line x1='2' y1='12' x2='22' y2='12' /><path d='M12 2a15.3 15.3 0 0 1 4 10 15.3 15.3 0 0 1-4 10 15.3 15.3 0 0 1-4-10 15.3 15.3 0 0 1 4-10z' /></svg>"
          ),
          nav_tab(
            "matching",
            "跨港媒合",
            identical(current_page, "matching"),
            "<svg width='14' height='14' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><path d='M8 3v3a2 2 0 0 1-2 2H3m18 0h-3a2 2 0 0 1-2-2V3m0 18v-3a2 2 0 0 1 2-2h3M3 16h3a2 2 0 0 1 2 2v3' /></svg>"
          ),
          nav_tab(
            "explore",
            "探索模式",
            identical(current_page, "explore"),
            "<svg width='14' height='14' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><path d='M12 3v18M3 12h18'/></svg>"
          ),
          nav_tab(
            "provenance",
            "資料來歷",
            identical(current_page, "provenance"),
            "<svg width='14' height='14' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><path d='M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z'/><path d='M14 2v6h6'/><path d='M8 13h8'/><path d='M8 17h6'/></svg>"
          )
        )
      })
    
      output$sidebar_context_panel <- renderUI({
        if (active_mode() == "formal") {
          div(
            class = "mode-note",
            tags$strong("正式模式"),
            tags$br(),
            "高壓 / 高緩衝採月內相對門檻（Q75 / Q50）分類，再搭配近三月淨增加條件；因此 80% 或 100% 只是相對指標值，不保證一定是正式高壓。"
          )
        } else {
          div(
            class = "mode-note",
            tags$strong("探索模式"),
            tags$br(),
            "探索模式只用於敏感度分析。策略模板改變 Pressure 權重重心；使用者自訂則允許在受限範圍內重配權重，不覆蓋正式研究結論。"
          )
        }
      })
    
      output$advanced_modal_sub <- renderUI({
        if (active_mode() == "formal") {
          div(class = "adv-modal-sub", "正式模式鎖定研究正式權重，這裡只提供公式與參數說明。")
        } else if (active_explore_tab() == "strategy") {
          div(class = "adv-modal-sub", "探索模式中的策略模板會改變壓力指標的重點，方便非技術使用者比較結果。")
        } else {
          div(class = "adv-modal-sub", "探索模式中的使用者自訂會即時重算 Pressure / Buffer / Matching，僅供敏感度分析。")
        }
      })
    
      output$advanced_modal_body <- renderUI({
        pressure_labels <- c(flow = "Flow", net = "Net", roll = "Trend")
        buffer_labels <- c(space = "Space", netout = "NetOut", export = "Export")
        matching_labels <- c(source = "Source", target = "Target", distance = "Distance")
    
        if (active_mode() == "formal") {
          return(tagList(
            div(
              class = "adv-help",
              "正式模式不允許直接覆寫研究正式權重。若要比較不同決策偏好，請切換到探索模式；探索結果只作敏感度分析，不覆蓋正式結論。"
            ),
            div(
              class = "adv-col",
              div(class = "adv-col-head", span(class = "adv-col-dot red"), span(class = "adv-col-title", "Pressure 正式權重")),
              div(class = "strategy-sub", "壓力指標以當月淨增加為主，流入占比與近三月趨勢為輔。"),
              weight_chip_set(official_pressure_weights, pressure_labels)
            ),
            div(
              class = "adv-col",
              div(class = "adv-col-head", span(class = "adv-col-dot green"), span(class = "adv-col-title", "Buffer 正式權重")),
              div(class = "strategy-sub", "緩衝能力以空間餘裕與歷史淨流出為主，再參考出口拉力。"),
              weight_chip_set(official_buffer_weights, buffer_labels)
            ),
            div(
              class = "adv-col",
              div(class = "adv-col-head", span(class = "adv-col-dot green"), span(class = "adv-col-title", "Matching 正式權重")),
              div(class = "strategy-sub", "媒合排序先看來源港壓力，再看目標港緩衝與距離阻力。"),
              weight_chip_set(official_matching_weights, matching_labels)
            )
          ))
        }
    
        if (active_explore_tab() == "strategy") {
          return(tagList(
            div(
              class = "adv-help",
              "策略模板不讓使用者直接拉數字，而是以研究可解釋的三種偏好預設跑結果，幫助不熟公式的人理解不同決策立場會如何改變推薦。"
            ),
            div(
              class = "adv-col",
              div(class = "adv-col-head", span(class = "adv-col-dot red"), span(class = "adv-col-title", "Strategy Mode")),
              div(
                class = "strategy-stack",
                lapply(names(strategy_templates), function(key) {
                  strategy <- strategy_templates[[key]]
                  div(
                    class = "strategy-card",
                    div(class = paste("strategy-btn", if (active_strategy_key() == key) "active" else ""), strategy$label),
                    div(class = "strategy-sub", strategy$description),
                    weight_chip_set(strategy$pressure_weights, pressure_labels)
                  )
                })
              )
            ),
            div(
              class = "adv-col",
              div(class = "adv-col-head", span(class = "adv-col-dot green"), span(class = "adv-col-title", "固定參數")),
              div(class = "strategy-sub", "策略模板僅調整 Pressure 三構面；Buffer 與 Matching 維持正式權重，避免一次同時改動過多構面而難以解釋。"),
              weight_chip_set(official_buffer_weights, buffer_labels),
              weight_chip_set(official_matching_weights, matching_labels)
            ),
            tags$script(HTML("window.refreshAllWeightGroups && window.refreshAllWeightGroups();"))
          ))
        }
    
        tagList(
          div(
            class = "adv-help",
            "使用者自訂模式的滑桿已移到「探索模式」功能頁，避免在 sidebar / modal 內重複出現。這裡只保留規則說明：每組權重總和固定 100%，單一權重限制在 10% 到 60%。"
          ),
          div(
            class = "adv-col",
            div(class = "adv-col-head", span(class = "adv-col-dot red"), span(class = "adv-col-title", "Pressure 自訂規則")),
            weight_chip_set(custom_pressure_weights(), pressure_labels)
          ),
          div(
            class = "adv-col",
            div(class = "adv-col-head", span(class = "adv-col-dot green"), span(class = "adv-col-title", "Buffer 自訂規則")),
            weight_chip_set(custom_buffer_weights(), buffer_labels)
          ),
          div(
            class = "adv-col",
            div(class = "adv-col-head", span(class = "adv-col-dot green"), span(class = "adv-col-title", "Matching 自訂規則")),
            weight_chip_set(custom_matching_weights(), matching_labels)
          )
        )
      })
  }, envir = server_env)
}
