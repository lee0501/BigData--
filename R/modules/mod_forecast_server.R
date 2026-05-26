register_forecast_outputs <- function(server_env) {
  local({
      observeEvent(input$baseline_port_click, {
        clicked <- normalize_port(sub("::.*$", "", input$baseline_port_click))
        if (clicked %in% formal_ports) {
          rv$baseline_port <- clicked
        }
      })

      baseline_selected_port <- reactive({
        rv$baseline_port %||% "高雄港"
      })

      baseline_bridge_rows <- reactive({
        build_port_baseline_bridge(status_formal_display, selected_ym())
      })

      output$baseline_port_buttons <- renderUI({
        tagList(lapply(formal_ports, function(port_name) {
          tags$button(
            type = "button",
            class = paste("port-btn", if (identical(port_name, baseline_selected_port())) "active" else ""),
            onclick = sprintf("Shiny.setInputValue('baseline_port_click', '%s::' + Date.now(), {priority:'event'})", display_port(port_name)),
            display_port(port_name)
          )
        }))
      })

      output$baseline_month_chip <- renderUI({
        span(class = "c-chip", fmt_ym(selected_ym()))
      })

      output$baseline_kpis <- renderUI({
        if (!port_baseline_available) {
          return(div(class = "mode-note", "尚未產生官方統計基準資料，請先執行 context/build_port_stat_history.R 與 context/build_port_stat_prophet_forecast.R。"))
        }

        latest_hist <- max(port_baseline_base$ym, na.rm = TRUE)
        latest_forecast <- max(port_baseline_watchlist$ym, na.rm = TRUE)
        high_watch <- port_baseline_watchlist %>%
          filter(proxy_risk_level %in% c("watch", "high")) %>%
          nrow()
        avg_mape <- mean(port_baseline_backtest_summary$mape, na.rm = TRUE)

        div(
          class = "summary-grid",
          div(class = "stat-box", div(class = "stat-label", "官方統計期間"), div(class = "stat-value", paste0(min(port_baseline_base$ym) %/% 100, "-", latest_hist %/% 100))),
          div(class = "stat-box", div(class = "stat-label", "預測延伸至"), div(class = "stat-value highlight", fmt_ym(latest_forecast))),
          div(class = "stat-box", div(class = "stat-label", "觀察月份"), div(class = "stat-value", nrow(port_baseline_watchlist))),
          div(class = "stat-box", div(class = "stat-label", "觀察 / 偏高"), div(class = "stat-value danger", high_watch)),
          div(class = "stat-box", div(class = "stat-label", "模型數量"), div(class = "stat-value", paste0(length(formal_ports) * length(port_baseline_metric_labels)))),
          div(class = "stat-box", div(class = "stat-label", "回測平均 MAPE"), div(class = "stat-value highlight", fmt_pct(avg_mape, 1))),
          div(class = "stat-box", div(class = "stat-label", "主模型欄位"), div(class = "stat-value", "個數")),
          div(class = "stat-box", div(class = "stat-label", "定位"), div(class = "stat-value", "基準"))
        )
      })

      output$baseline_trend_chart <- renderImage({
        validate(need(port_baseline_available, "尚未產生官方統計基準資料。"))

        selected_port <- baseline_selected_port()
        actual_df <- port_baseline_base %>%
          filter(port == selected_port) %>%
          transmute(
            ds,
            value = empty_share_count,
            series = "官方實際空櫃占比",
            source = "actual"
          )

        forecast_df <- port_baseline_forecast %>%
          filter(port == selected_port, metric == "empty_share_count") %>%
          transmute(
            ds,
            value = yhat,
            lower = yhat_lower,
            upper = yhat_upper,
            series = if_else(is_forecast, "Prophet 基準延伸", "Prophet 歷史擬合"),
            source = if_else(is_forecast, "forecast", "fit")
          )

        plot_obj <- ggplot() +
          geom_ribbon(
            data = forecast_df %>% filter(source == "forecast"),
            aes(x = ds, ymin = lower, ymax = upper),
            fill = "#bfdbfe",
            alpha = 0.35
          ) +
          geom_line(
            data = forecast_df,
            aes(x = ds, y = value, color = series, linetype = source),
            linewidth = 1.0
          ) +
          geom_line(
            data = actual_df,
            aes(x = ds, y = value, color = series),
            linewidth = 1.0
          ) +
          geom_point(
            data = actual_df %>% filter(ds >= as.Date("2025-01-01")),
            aes(x = ds, y = value),
            color = "#111827",
            size = 1.8
          ) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
          scale_color_manual(values = c("官方實際空櫃占比" = "#111827", "Prophet 歷史擬合" = "#2563eb", "Prophet 基準延伸" = "#dc2626")) +
          scale_linetype_manual(values = c(actual = "solid", fit = "solid", forecast = "dashed"), guide = "none") +
          labs(x = "月份", y = "空櫃占比", color = NULL) +
          theme_minimal(base_size = 11, base_family = "") +
          theme(
            panel.grid.major.x = element_line(color = "#eef2f7"),
            panel.grid.minor = element_blank(),
            legend.position = "bottom",
            axis.text.x = element_text(color = "#6b7280", size = 9),
            axis.text.y = element_text(color = "#6b7280"),
            plot.margin = margin(8, 16, 12, 8)
          )

        file <- save_ggplot_png(plot_obj, width = 1280, height = 620, res = 144)
        list(src = file, contentType = "image/png", width = "100%", height = "360px")
      }, deleteFile = TRUE)

      output$baseline_trend_result <- renderUI({
        validate(need(port_baseline_available, ""))

        selected_port <- baseline_selected_port()
        actual_latest <- port_baseline_base %>%
          filter(port == selected_port) %>%
          arrange(desc(ds)) %>%
          slice(1)

        forecast_future <- port_baseline_forecast %>%
          filter(port == selected_port, metric == "empty_share_count", is_forecast) %>%
          arrange(ds)

        hist_q75 <- port_baseline_base %>%
          filter(port == selected_port) %>%
          summarise(q75 = quantile(empty_share_count, 0.75, na.rm = TRUE)) %>%
          pull(q75)

        first_forecast <- forecast_future %>% slice(1)
        last_forecast <- forecast_future %>% slice(n())
        trend_delta <- last_forecast$yhat[[1]] - first_forecast$yhat[[1]]
        trend_label <- case_when(
          trend_delta > 0.01 ~ "上升",
          trend_delta < -0.01 ~ "下降",
          TRUE ~ "持平"
        )
        latest_ratio <- safe_divide(actual_latest$empty_share_count[[1]], hist_q75)
        future_peak <- max(forecast_future$yhat, na.rm = TRUE)
        future_peak_ratio <- safe_divide(future_peak, hist_q75)
        baseline_label <- case_when(
          future_peak_ratio >= 1.10 ~ "偏高",
          future_peak_ratio >= 0.95 ~ "接近高位",
          TRUE ~ "低於高位"
        )
        reason_text <- paste0(
          "依該港 110-115 年同月份型態與近期走勢延伸，",
          fmt_ym(first_forecast$ym[[1]]),
          " 至 ",
          fmt_ym(last_forecast$ym[[1]]),
          " 的預測空櫃占比為 ",
          fmt_pct(first_forecast$yhat[[1]], 1),
          " → ",
          fmt_pct(last_forecast$yhat[[1]], 1),
          "。"
        )
        threshold_text <- paste0(
          "「偏高」表示未來預測最高值超過同港多年歷史 Q75 的 1.10 倍；",
          "「接近高位」為 0.95 至 1.10 倍；目前最高為 ",
          fmt_idx(future_peak_ratio, 2),
          " 倍。"
        )

        div(
          tagList(
            div(
              class = "baseline-trend-result",
              div(
                class = "baseline-trend-pill",
                span(class = "baseline-trend-label", "最新實際"),
                strong(fmt_pct(actual_latest$empty_share_count[[1]], 1)),
                span(paste0("同港多年 Q75 ", fmt_idx(latest_ratio, 2), "x"))
              ),
              div(
                class = "baseline-trend-pill",
                span(class = "baseline-trend-label", "未來方向"),
                strong(trend_label),
                span(paste0(fmt_pct(first_forecast$yhat[[1]], 1), " → ", fmt_pct(last_forecast$yhat[[1]], 1)))
              ),
              div(
                class = "baseline-trend-pill",
                span(class = "baseline-trend-label", "未來高位判斷"),
                strong(baseline_label),
                span(paste0("最高為同港多年 Q75 ", fmt_idx(future_peak_ratio, 2), "x"))
              )
            ),
            div(
              class = "baseline-interpretation",
              div(strong("為什麼是這個方向："), reason_text),
              div(strong("高位判斷意思："), threshold_text)
            )
          )
        )
      })

      output$baseline_bridge_result <- renderUI({
        rows <- baseline_bridge_rows()
        if (nrow(rows) == 0) {
          return(NULL)
        }

        top_imarine <- rows %>% arrange(desc(pressure_index), desc(empty_net)) %>% slice(1)
        top_baseline <- rows %>% arrange(desc(empty_share_vs_yhat), desc(total_vs_yhat)) %>% slice(1)
        same_port <- identical(top_imarine$port[[1]], top_baseline$port[[1]])

        imarine_empty_ratio <- top_imarine$empty_share_vs_yhat[[1]]
        imarine_total_ratio <- top_imarine$total_vs_yhat[[1]]
        imarine_baseline_level <- case_when(
          imarine_empty_ratio >= 1.10 | imarine_total_ratio >= 1.10 ~ "超出季節性預期",
          imarine_empty_ratio >= 0.95 | imarine_total_ratio >= 0.95 ~ "接近季節性上界",
          TRUE ~ "符合季節性範圍"
        )

        result_label <- case_when(
          same_port && imarine_baseline_level != "符合季節性範圍" ~ "短期壓力 + 超出季節性預期",
          imarine_baseline_level != "符合季節性範圍" ~ "短期壓力，接近季節性上界",
          TRUE ~ "短期作業壓力"
        )
        response_text <- case_when(
          same_port && imarine_baseline_level != "符合季節性範圍" ~ "優先追蹤，適合列入本月重點說明。",
          imarine_baseline_level != "符合季節性範圍" ~ "依 iMarine 追蹤調度，同時標記為季節性觀察。",
          TRUE ~ "依 iMarine 明細追蹤，壓力屬於季節性正常範圍。"
        )

        decision_sentence <- paste0(
          fmt_ym(selected_ym()),
          "：",
          display_port(top_imarine$port[[1]]),
          " 是 iMarine 當月相對高壓港；與 Prophet 季節性基準相比，判定為「",
          imarine_baseline_level,
          "」，因此本月應採「",
          result_label,
          "」解讀。"
        )

        div(
          class = "baseline-decision-block",
          div(class = "baseline-decision-headline", decision_sentence),
          div(
            class = "baseline-decision-grid",
          div(
            class = "baseline-decision-card status",
            div(class = "baseline-decision-kicker", "1. iMarine 發現"),
            div(class = "baseline-decision-title", display_port(top_imarine$port[[1]])),
            div(class = "baseline-decision-sub", paste0("iMarine 壓力最高：", fmt_idx(top_imarine$pressure_index[[1]], 4)))
          ),
          div(
            class = "baseline-decision-card meaning",
            div(class = "baseline-decision-kicker", "2. 季節性基準檢查"),
            div(class = "baseline-decision-title", imarine_baseline_level),
            div(class = "baseline-decision-sub", paste0("對 Prophet 季節基準 yhat：空櫃占比 ", fmt_idx(imarine_empty_ratio, 2), "x；總量 ", fmt_idx(imarine_total_ratio, 2), "x"))
          ),
          div(
            class = "baseline-decision-card result",
            div(class = "baseline-decision-kicker", "3. 結論"),
            div(class = "baseline-decision-title", result_label),
            div(class = "baseline-decision-sub", response_text)
          ),
          div(
            class = "baseline-decision-card action",
            div(class = "baseline-decision-kicker", "4. 反應"),
            div(class = "baseline-decision-title", if (same_port) "優先處理" else "分層追蹤"),
            div(class = "baseline-decision-sub", paste0("季節基準最高：", display_port(top_baseline$port[[1]])))
          )
          )
        )
      })

      output$baseline_bridge_table <- renderUI({
        rows <- baseline_bridge_rows()
        div(
          class = "dt-table-wrap has-scroll",
            tags$table(
            class = "dt-table data-wide",
            tags$thead(tags$tr(
              tags$th("#"),
              tags$th("港口"),
              tags$th("iMarine"),
              tags$th("Prophet 季節基準"),
              tags$th("判讀")
            )),
            tags$tbody(
              if (nrow(rows) == 0) {
                tags$tr(tags$td(colspan = 5, class = "dt-empty", "沒有可顯示的對照資料"))
              } else {
                tagList(lapply(seq_len(nrow(rows)), function(i) {
                  row <- rows[i, ]
                  imarine_label <- paste0(row$status, "｜", fmt_idx(row$pressure_index, 4))
                  baseline_label <- paste0(fmt_pct(row$empty_share_count, 1), "｜季節預期 ", fmt_idx(row$empty_share_vs_yhat, 2), "x")
                  tags$tr(
                    tags$td(class = "dt-row-num", i),
                    tags$td(display_port(row$port)),
                    tags$td(imarine_label),
                    tags$td(baseline_label),
                    tags$td(row$baseline_signal)
                  )
                }))
              }
            )
          )
        )
      })

      output$baseline_watchlist_table <- renderUI({
        rows <- latest_port_baseline_watchlist(nrow(port_baseline_watchlist))
        risk_label <- c(normal = "正常", watch = "觀察", high = "偏高")
        div(
          class = "dt-table-wrap has-scroll",
          tags$table(
            class = "dt-table data-wide",
            tags$thead(tags$tr(
              tags$th("#"),
              tags$th("預測月份"),
              tags$th("港口"),
              tags$th("預測空櫃占比"),
              tags$th("預測空櫃個數"),
              tags$th("預測總個數"),
              tags$th("基準分數"),
              tags$th("等級")
            )),
            tags$tbody(
              if (nrow(rows) == 0) {
                tags$tr(tags$td(colspan = 8, class = "dt-empty", "尚未產生未來觀察清單"))
              } else {
                tagList(lapply(seq_len(nrow(rows)), function(i) {
                  row <- rows[i, ]
                  tags$tr(
                    tags$td(class = "dt-row-num", i),
                    tags$td(row$ym_label),
                    tags$td(display_port(row$port)),
                    tags$td(fmt_pct(row$pred_empty_share, 1)),
                    tags$td(fmt_num(row$empty_container_count, 0)),
                    tags$td(fmt_num(row$total_container_count, 0)),
                    tags$td(class = "dt-mono", fmt_idx(row$proxy_risk_score, 3)),
                    tags$td(risk_label[[as.character(row$proxy_risk_level)]] %||% as.character(row$proxy_risk_level))
                  )
                }))
              }
            )
          )
        )
      })

      output$baseline_watchlist_legend <- renderUI({
        div(
          class = "mode-note subtle",
          HTML(
            paste0(
              "<strong>等級定義：</strong>",
              "正常 = 基準分數低於 0.95；",
              "觀察 = 0.95 至 1.10，代表接近多年高位；",
              "偏高 = 高於 1.10，代表預測空櫃結構與作業量相對多年基準偏高。"
            )
          )
        )
      })

      output$baseline_backtest_table <- renderUI({
        rows <- port_baseline_backtest_summary
        div(
          class = "dt-table-wrap has-scroll",
          tags$table(
            class = "dt-table",
            tags$thead(tags$tr(
              tags$th("指標"),
              tags$th("MAE"),
              tags$th("MAPE")
            )),
            tags$tbody(
              if (nrow(rows) == 0) {
                tags$tr(tags$td(colspan = 3, class = "dt-empty", "尚未產生回測資料"))
              } else {
                tagList(lapply(seq_len(nrow(rows)), function(i) {
                  row <- rows[i, ]
                  tags$tr(
                    tags$td(row$metric_label),
                    tags$td(fmt_num(row$mae, if (row$metric == "empty_share_count") 3 else 0)),
                    tags$td(fmt_pct(row$mape, 1))
                  )
                }))
              }
            )
          )
        )
      })

      output$baseline_reliability_result <- renderUI({
        rows <- port_baseline_backtest_summary
        if (nrow(rows) == 0) {
          return(NULL)
        }

        empty_share <- rows %>% filter(metric == "empty_share_count") %>% slice(1)
        total_count <- rows %>% filter(metric == "total_container_count") %>% slice(1)
        empty_count <- rows %>% filter(metric == "empty_container_count") %>% slice(1)

        empty_share_level <- if (nrow(empty_share) && empty_share$mape[[1]] <= 0.10) {
          "可用"
        } else if (nrow(empty_share) && empty_share$mape[[1]] <= 0.15) {
          "輔助可用"
        } else {
          "需保守"
        }

        total_level <- if (nrow(total_count) && total_count$mape[[1]] <= 0.10) {
          "穩定"
        } else {
          "波動"
        }

        div(
          class = "baseline-reliability",
          div(
            class = "baseline-reliability-main",
            div(class = "baseline-reliability-label", "判讀結果"),
            div(class = "baseline-reliability-title", paste0("空櫃占比基準：", empty_share_level)),
            div(
              class = "baseline-reliability-text",
              HTML(paste0(
                "<strong>MAPE</strong> 是平均百分比誤差；空櫃占比 MAPE ",
                if (nrow(empty_share)) fmt_pct(empty_share$mape[[1]], 1) else "—",
                "，代表回測時預測占比平均約差這個比例。<br>",
                "<strong>MAE</strong> 是平均絕對誤差；空櫃占比 MAE ",
                if (nrow(empty_share)) fmt_idx(empty_share$mae[[1]], 3) else "—",
                "，約等於 ",
                if (nrow(empty_share)) fmt_pct(empty_share$mae[[1]], 1) else "—",
                " 個百分點。"
              ))
            )
          ),
          div(
            class = "baseline-reliability-side",
            div(class = "baseline-reliability-label", "總量基準"),
            div(class = "baseline-reliability-title", total_level),
            div(
              class = "baseline-reliability-text",
              HTML(paste0(
                "總櫃個數 <strong>MAPE</strong> ",
                if (nrow(total_count)) fmt_pct(total_count$mape[[1]], 1) else "—",
                "，表示總作業量預測相對穩定。空櫃個數 MAPE ",
                if (nrow(empty_count)) fmt_pct(empty_count$mape[[1]], 1) else "—",
                "，波動較大，因此不用它單獨下結論。"
              ))
            )
          )
        )
      })
  }, envir = server_env)
}
