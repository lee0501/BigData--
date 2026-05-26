register_overview_outputs <- function(server_env) {
  local({
      output$overview_pressure_chart <- renderImage({
        chart_df <- overview_month_status() %>%
          mutate(
            port_display = display_port(port),
            high_flag = pressure_index >= high_pressure_cut
          )
    
        validate(need(nrow(chart_df) > 0, "本月份沒有可顯示的壓力資料。"))
    
        plot_obj <- ggplot(chart_df, aes(x = reorder(port_display, pressure_index), y = pressure_index, fill = high_flag)) +
          geom_col(width = 0.72, show.legend = FALSE) +
          coord_flip() +
          scale_fill_manual(values = c(`TRUE` = "#dc2626", `FALSE` = "#2563EB")) +
          labs(x = NULL, y = NULL) +
          theme_minimal(base_size = 12, base_family = "") +
          theme(
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "#f3f4f6"),
            panel.grid.minor = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_text(color = "#111827", size = 12, margin = margin(r = 12)),
            axis.text.x = element_text(color = "#6b7280"),
            plot.margin = margin(0, 8, 0, 18)
          )
        file <- save_ggplot_png(plot_obj, width = 920, height = 430, res = 144)
        list(src = file, contentType = "image/png", width = "100%", height = "210px")
      }, deleteFile = TRUE)
    
      output$overview_port_cards <- renderUI({
        snapshot <- overview_month_status()
        card_order <- c("基隆港", "臺北港", "高雄港", "臺中港")
    
        cards <- lapply(card_order, function(port_name) {
          row <- snapshot %>% filter(port == port_name) %>% slice(1)
          pct_val <- round(row$pressure_index[[1]] * 100)
          lvl <- status_card_class(row$status[[1]])
          lbl <- status_badge_label(row$status[[1]])
    
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
              span(class = paste("pc-badge", lvl), lbl)
            ),
            div(
              class = "pc-stats",
              span("正式狀態 / 壓力"),
              span(class = "pc-stat-val", paste0(display_port(row$status[[1]]), " / ", fmt_idx(row$pressure_index[[1]], 3)))
            ),
            div(
              class = "pc-bar-row",
              span(style = "font-size:0.72rem;color:var(--muted);white-space:nowrap;", "壓力指標"),
              div(class = "pc-bar-track", div(class = paste("pc-bar-fill", lvl), style = paste0("width:", pct_val, "%"))),
              span(class = paste("pc-pct", lvl), paste0(pct_val, "%"))
            ),
            div(
              class = "pc-data-grid",
              div(class = "pc-data-item", div(class = "pc-data-label", "空櫃進港量"), div(class = "pc-data-value", fmt_num(row$empty_in[[1]], 0))),
              div(class = "pc-data-item", div(class = "pc-data-label", "港口總貨櫃裝卸量"), div(class = "pc-data-value", fmt_num(row$throughput[[1]], 0)))
            ),
            div(
              class = "pc-note",
              if (identical(row$status[[1]], "資料暖機中")) {
                "目前歷史樣本未滿 3 個月，正式 status 暫不判高壓 / 高緩衝。"
              } else {
                "壓力判讀主看空櫃進港量與港口總貨櫃裝卸量。"
              }
            )
          )
        })
    
        summary_card <- {
          sum_pressure <- round(mean(snapshot$pressure_index, na.rm = TRUE) * 100)
          div(
            class = "port-card summary",
            div(
              class = "pc-header",
              div(
                class = "pc-name-group",
                div(class = "pc-dot", style = "background:var(--blue);"),
                div(
                  div(class = "pc-name", "全港總覽"),
                  div(class = "pc-code", "ALL · 4 PORTS")
                )
              ),
              span(class = "pc-badge", style = "color:var(--blue);border-color:var(--blue-mid);", "總計")
            ),
            div(
              class = "summary-totals",
              div(class = "pc-stats", span("高壓 / 緩衝"), span(class = "pc-stat-val", paste0(sum(snapshot$status == '高壓力港'), " / ", sum(snapshot$status == '高緩衝港')))),
              div(
                class = "summary-row",
                span(class = "summary-row-label", "正式分析宇宙"),
                span(class = "summary-row-val", "4 港")
              )
            ),
            div(
              class = "pc-bar-row",
              span(style = "font-size:0.72rem;color:var(--blue);white-space:nowrap;font-weight:600;", "平均壓力"),
              div(class = "pc-bar-track", style = "background:var(--blue-mid);", div(class = "pc-bar-fill", style = paste0("width:", sum_pressure, "%;background:var(--blue);"))),
              span(class = "pc-pct", style = "color:var(--blue);", paste0(sum_pressure, "%"))
            )
          )
        }
    
        div(class = "port-cards", tagList(cards, summary_card))
      })
    
      output$overview_pair_rows <- renderUI({
        rows <- overview_candidates()
    
        if (nrow(rows) == 0) {
          return(
            div(class = "pair-row", "本月沒有可顯示的候選配對。")
          )
        }
    
        lapply(seq_len(nrow(rows)), function(i) {
          row <- rows[i, ]
          tag_class <- if (row$simulation_result[[1]] == "不建議調度") "danger" else "success"
          move_amount_val <- if ("move_amount" %in% names(row)) row$move_amount[[1]] else overview_default_move_amount()
          scenario_label <- if ("scenario_label" %in% names(row)) row[["scenario_label"]][[1]] else NULL
          target_tag <- if (row$pair_tier[[1]] == "觀察候選") {
            if (!is.null(scenario_label) && nzchar(scenario_label)) paste0("觀察方案 ", scenario_label) else "觀察候選"
          } else {
            "可承接"
          }
          div(
            class = "pair-row",
            div(
              class = "pair-port",
              span(class = "pair-name", display_port(row$source_port[[1]])),
              span(class = "pair-tag danger", row$source_status[[1]])
            ),
            HTML("<svg class='pair-arrow' width='16' height='16' viewBox='0 0 24 24' fill='none' stroke='#9ca3af' stroke-width='2'><path d='M5 12h14M13 6l6 6-6 6' /></svg>"),
            div(
              class = "pair-port",
              span(class = "pair-name", display_port(row$target_port[[1]])),
              span(class = paste("pair-tag", tag_class), target_tag)
            ),
            div(
              class = "pair-teu",
              span(class = "pair-teu-num", fmt_num(move_amount_val, 0)),
              span(class = "pair-teu-unit", "TEU")
            )
          )
        })
      })
    
      output$overview_table_meta <- renderUI({
        tbl <- raw_table_data()
        span(
          class = "c-chip",
          paste0(nrow(tbl), " 筆資料・", length(unique(tbl$port)), " 港口・", length(unique(tbl$ym)), " 個月份")
        )
      })

      sort_icon_node <- function(col_name) {
        cls <- "dt-sort-icon"
        if (identical(rv$dt_sort_col, col_name)) {
          cls <- paste(cls, rv$dt_sort_dir)
        }
        div(class = cls, span(class = "sa"), span(class = "sd"))
      }

      output$overview_table_block <- renderUI({
        page_info <- paged_raw_table()
        rows <- page_info$data
        columns <- list(
          list(col = "ym", label = "年月"),
          list(col = "port", label = "港口"),
          list(col = "empty_in", label = "空櫃進港量"),
          list(col = "empty_out", label = "空櫃出港量"),
          list(col = "empty_net", label = "空櫃淨流量"),
          list(col = "throughput", label = "港口總貨櫃裝卸量"),
          list(col = "pressure_index", label = "壓力指數"),
          list(col = "buffer_index", label = "緩衝指數")
        )

        div(
          class = "dt-table-wrap has-scroll",
          tags$table(
            class = "dt-table data-wide",
            id = "dt-main",
            tags$thead(
              tags$tr(
                tags$th(div(class = "th-inner", HTML("&nbsp;"))),
                lapply(columns, function(x) {
                  tags$th(
                    onclick = sprintf("sortRawTable('%s')", x$col),
                    div(
                      class = "th-inner",
                      x$label,
                      sort_icon_node(x$col)
                    )
                  )
                })
              )
            ),
            tags$tbody(
              if (nrow(rows) == 0) {
                tags$tr(tags$td(colspan = length(columns) + 1, class = "dt-empty", "找不到符合條件的資料"))
              } else {
                tagList(
                  lapply(seq_len(nrow(rows)), function(i) {
                    row <- rows[i, ]
                    tags$tr(
                      tags$td(class = "dt-row-num", page_info$start + i - 1),
                      tags$td(row$ym),
                      tags$td(row$port),
                      tags$td(fmt_num(row$empty_in, 0)),
                      tags$td(fmt_num(row$empty_out, 0)),
                      tags$td(fmt_num(row$empty_net, 0)),
                      tags$td(fmt_num(row$throughput, 0)),
                      tags$td(class = "dt-mono", fmt_idx(row$pressure_index, 6)),
                      tags$td(class = "dt-mono", fmt_idx(row$buffer_index, 6))
                    )
                  })
                )
              }
            )
          )
        )
      })
    
      output$overview_table_info <- renderUI({
        page_info <- paged_raw_table()
    
        if (page_info$total == 0) {
          HTML("找不到符合條件的資料")
        } else {
          HTML(sprintf("顯示第 %d 到 %d 筆，共 %d 筆資料", page_info$start, page_info$end, page_info$total))
        }
      })
    
      output$overview_table_pager <- renderUI({
        page_info <- paged_raw_table()
    
        tagList(
          tags$button(
            type = "button",
            class = "dt-page-btn",
            disabled = if (page_info$page <= 1) "disabled" else NULL,
            onclick = "rawTableAction('prev')",
            "Previous"
          ),
          span(class = "dt-page-num", paste0("Page ", page_info$page, " / ", page_info$pages)),
          tags$button(
            type = "button",
            class = "dt-page-btn",
            disabled = if (page_info$page >= page_info$pages) "disabled" else NULL,
            onclick = "rawTableAction('next')",
            "Next"
          )
        )
      })
      output$overview_next_month_watchlist <- renderUI({
        if (!next_month_available) {
          return(div(
            class = "mode-note",
            "尚未產生下月優先檢查資料，請執行 context/backtest_forecast_high_pressure.R。"
          ))
        }

        rows <- next_month_forecast %>% filter(origin_ym == selected_ym())

        if (nrow(rows) == 0) {
          avail <- sort(unique(next_month_forecast$origin_ym))
          return(div(
            class = "mode-note",
            paste0(
              fmt_ym(selected_ym()),
              " 無次月預測資料（可用月份：",
              paste(fmt_ym(avail), collapse = "、"),
              "）。"
            )
          ))
        }

        target_ym <- rows$target_ym[[1]]

        priority_class <- function(p) {
          switch(p, "高預警" = "danger", "觀察" = "highlight", "")
        }

        tagList(
          div(
            class = "mode-note subtle",
            paste0("來源月：", fmt_ym(selected_ym()), "　→　預測目標月：", fmt_ym(target_ym))
          ),
          div(
            class = "dt-table-wrap has-scroll",
            tags$table(
              class = "dt-table",
              tags$thead(tags$tr(
                tags$th("排序"),
                tags$th("港口"),
                tags$th("風險等級"),
                tags$th("優先檢查分數"),
                tags$th("連續淨流入月數"),
                tags$th("空櫃淨流量")
              )),
              tags$tbody(
                tagList(lapply(seq_len(nrow(rows)), function(i) {
                  row <- rows[i, ]
                  tags$tr(
                    tags$td(class = "dt-row-num", row$rank),
                    tags$td(display_port(row$port)),
                    tags$td(class = priority_class(row$forecast_priority), row$forecast_priority),
                    tags$td(class = "dt-mono", fmt_idx(row$score, 3)),
                    tags$td(row$positive_net_streak),
                    tags$td(fmt_num(row$empty_net, 0))
                  )
                }))
              )
            )
          )
        )
      })

  }, envir = server_env)
}
