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
          future_peak_ratio >= 1.10 ~ "高於往年高點",
          future_peak_ratio >= 0.95 ~ "接近往年高點",
          TRUE ~ "低於往年高點"
        )
        reason_text <- paste0(
          "根據 110-115 年同月份的歷史型態延伸，",
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
          "「高於往年高點」表示預測最高值已超過往年同期歷史高位；",
          "「接近往年高點」表示接近但尚未超過；目前預測最高值是往年高點的 ",
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
                span(paste0("往年高點的 ", fmt_idx(latest_ratio, 2), " 倍"))
              ),
              div(
                class = "baseline-trend-pill",
                span(class = "baseline-trend-label", "未來走向"),
                strong(trend_label),
                span(paste0(fmt_pct(first_forecast$yhat[[1]], 1), " → ", fmt_pct(last_forecast$yhat[[1]], 1)))
              ),
              div(
                class = "baseline-trend-pill",
                span(class = "baseline-trend-label", "未來走勢研判"),
                strong(baseline_label),
                span(paste0("預測最高為往年高點的 ", fmt_idx(future_peak_ratio, 2), " 倍"))
              )
            ),
            div(
              class = "baseline-interpretation",
              div(strong("預測走勢依據："), reason_text),
              div(strong("高點研判說明："), threshold_text)
            )
          )
        )
      })

      output$baseline_bridge_result <- renderUI({
        imarine_check <- status_formal_display %>%
          filter(ym == selected_ym(), port %in% formal_ports)

        if (nrow(imarine_check) > 0 && all(imarine_check$status == "資料暖機中")) {
          return(div(
            class = "mode-note",
            paste0(
              fmt_ym(selected_ym()),
              " 為 iMarine 資料暖機期（觀測月份不足三個月，壓力分類無統計意義），",
              "無法與往年同月資料對照。請選擇 2025 年 3 月以後的月份。"
            )
          ))
        }

        rows <- baseline_bridge_rows()
        if (nrow(rows) == 0) {
          return(NULL)
        }

        top_imarine <- rows %>% arrange(desc(pressure_index), desc(empty_net)) %>% slice(1)

        imarine_empty_ratio <- top_imarine$empty_share_vs_yhat[[1]]
        imarine_total_ratio <- top_imarine$total_vs_yhat[[1]]
        imarine_baseline_level <- case_when(
          imarine_empty_ratio >= 1.10 | imarine_total_ratio >= 1.10 ~ "高於往年同期",
          imarine_empty_ratio >= 0.95 | imarine_total_ratio >= 0.95 ~ "略高於往年同期",
          TRUE ~ "與往年同期相近"
        )

        result_label <- case_when(
          imarine_baseline_level == "高於往年同期" ~ "壓力偏高，且高於往年同期",
          imarine_baseline_level == "略高於往年同期" ~ "壓力偏高，往年同期本來就略高",
          TRUE ~ "壓力偏高，但往年同期屬正常水準"
        )
        response_text <- case_when(
          imarine_baseline_level == "高於往年同期" ~ "建議優先追蹤，這是今年出現的異常增加，不只是每年固定的季節起伏。",
          imarine_baseline_level == "略高於往年同期" ~ "持續追蹤，這個時間點歷來就偏高，留意是否繼續走高。",
          TRUE ~ "依 iMarine 追蹤調度；往年這個時期空櫃量本來就多，對外說明可以說屬正常季節性波動。"
        )

        decision_sentence <- paste0(
          fmt_ym(selected_ym()),
          "：",
          display_port(top_imarine$port[[1]]),
          " 是本月壓力最高的港口；對照往年同月份官方統計，判定為「",
          imarine_baseline_level,
          "」。"
        )

        div(
          class = "baseline-decision-block",
          div(class = "baseline-decision-headline", decision_sentence),
          div(
            class = "baseline-decision-grid",
            div(
              class = "baseline-decision-card status",
              div(class = "baseline-decision-kicker", "本月發現"),
              div(class = "baseline-decision-title", display_port(top_imarine$port[[1]])),
              div(class = "baseline-decision-sub", paste0(
                "壓力指數 ", fmt_idx(top_imarine$pressure_index[[1]], 4),
                "，歷史上只有 ", round((1 - top_imarine$pressure_index[[1]]) * 100),
                "% 的月份比這個月更高"
              ))
            ),
            div(
              class = "baseline-decision-card meaning",
              div(class = "baseline-decision-kicker", "同期判斷"),
              div(class = "baseline-decision-title", imarine_baseline_level),
              div(class = "baseline-decision-sub", paste0("今年此月空櫃占比為往年同月預期的 ", fmt_idx(imarine_empty_ratio, 2), " 倍"))
            ),
            div(
              class = "baseline-decision-card result",
              div(class = "baseline-decision-kicker", "處理建議"),
              div(class = "baseline-decision-title", result_label),
              div(class = "baseline-decision-sub", response_text)
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
              tags$th("往年同月預期（yhat）"),
              tags$th("判讀")
            )),
            tags$tbody(
              if (nrow(rows) == 0) {
                tags$tr(tags$td(colspan = 5, class = "dt-empty", "沒有可顯示的對照資料"))
              } else {
                tagList(lapply(seq_len(nrow(rows)), function(i) {
                  row <- rows[i, ]
                  imarine_label <- paste0(row$status, "｜", fmt_idx(row$pressure_index, 4))
                  baseline_label <- paste0(fmt_pct(row$empty_share_count, 1), "｜往年同月預期 ", fmt_idx(row$empty_share_vs_yhat, 2), "x")
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
        all_rows <- port_baseline_watchlist %>%
          filter(ym > selected_ym()) %>%
          arrange(ym, desc(proxy_risk_score))

        risk_label <- c(normal = "正常", watch = "觀察", high = "偏高")
        has_validation <- any(all_rows$is_validation, na.rm = TRUE)

        error_cell <- function(row) {
          if (!isTRUE(row$is_validation[[1]]) || is.na(row$actual_empty_share[[1]])) {
            return(tags$td(class = "dt-empty-cell", "—"))
          }
          err <- (row$actual_empty_share[[1]] - row$pred_empty_share[[1]]) * 100
          err_text <- sprintf("%+.1f%%pt", err)
          err_class <- if (abs(err) <= 1) "err-good" else if (abs(err) <= 3) "err-mid" else "err-high"
          tags$td(class = err_class, err_text)
        }

        div(
          class = "dt-table-wrap has-scroll",
          tags$table(
            class = "dt-table data-wide",
            tags$thead(tags$tr(
              tags$th("#"),
              tags$th("月份"),
              tags$th("港口"),
              tags$th("預測空櫃占比"),
              if (has_validation) tags$th("實際占比") else NULL,
              if (has_validation) tags$th("誤差") else NULL,
              tags$th("預測空櫃個數"),
              tags$th("預測總個數"),
              tags$th(if (has_validation) "實際等級" else "觀察等級")
            )),
            tags$tbody(
              if (nrow(all_rows) == 0) {
                tags$tr(tags$td(colspan = 9, class = "dt-empty", "此月份之後無可顯示資料"))
              } else {
                tagList(lapply(seq_len(nrow(all_rows)), function(i) {
                  row <- all_rows[i, ]
                  is_val <- isTRUE(row$is_validation[[1]])
                  actual_td <- if (has_validation) {
                    if (is_val && !is.na(row$actual_empty_share[[1]])) {
                      tags$td(fmt_pct(row$actual_empty_share[[1]], 1))
                    } else {
                      tags$td(class = "dt-empty-cell", "—")
                    }
                  } else NULL
                  level_val <- if (is_val && !is.na(row$actual_risk_level[[1]])) {
                    row$actual_risk_level[[1]]
                  } else {
                    as.character(row$proxy_risk_level[[1]])
                  }
                  tags$tr(
                    class = if (is_val) "row-validation" else "",
                    tags$td(class = "dt-row-num", i),
                    tags$td(row$ym_label),
                    tags$td(display_port(row$port)),
                    tags$td(fmt_pct(row$pred_empty_share, 1)),
                    actual_td,
                    if (has_validation) error_cell(row) else NULL,
                    tags$td(fmt_num(row$empty_container_count, 0)),
                    tags$td(fmt_num(row$total_container_count, 0)),
                    tags$td(risk_label[[level_val]] %||% level_val)
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
          HTML(paste0(
            "<strong>淡藍色列（已知結果）：</strong>",
            "Prophet 以 2024 年底前資料預測 2025 全年，實際值已知，可直接對照驗證。",
            "「誤差」欄為實際值 − 預測值（百分點）：",
            "<span style='color:#16a34a;font-weight:600;'>綠色</span> ±1%pt 以內，",
            "<span style='color:#d97706;font-weight:600;'>橘色</span> ±1–3%pt，",
            "<span style='color:#dc2626;font-weight:600;'>紅色</span> 超過 ±3%pt。",
            "「實際等級」依當月實際值判定。",
            "<br><strong>白色列（尚無實際值）：</strong>預測 2026 年，等級依預測值估算，供提前關注使用，更新 iMarine 明細後再行確認。",
            "<br><strong>觀察等級：</strong>正常 = 低於往年高點；觀察 = 接近往年高點；偏高 = 明顯高於往年高點。"
          ))
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
