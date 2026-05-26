register_analysis_outputs <- function(server_env) {
  local({
      output$analysis_chip1 <- renderUI({
        ports <- analysis_selected_ports()
        label <- if (length(ports) > 1) paste0("綜合比較 (", length(ports), " 港)") else display_port(ports[[1]])
        span(class = "c-chip", label)
      })
    
      output$analysis_chip2 <- renderUI({
        ports <- analysis_selected_ports()
        label <- if (length(ports) > 1) paste0("綜合比較 (", length(ports), " 港)") else display_port(ports[[1]])
        span(class = "c-chip", label)
      })
    
      output$analysis_port_buttons <- renderUI({
        buttons <- lapply(formal_ports, function(port_name) {
          tags$button(
            type = "button",
            class = paste("port-btn", if (port_name %in% analysis_selected_ports()) "active" else ""),
            onclick = sprintf("toggleAnalysisPort('%s')", display_port(port_name)),
            display_port(port_name)
          )
        })
    
        tagList(buttons)
      })
    
      output$analysis_chart1 <- renderImage({
        plot_df <- analysis_data_selected() %>%
          mutate(
            port_display = display_port(port),
            x_date = as.Date(sprintf("%04d-%02d-01", ym %/% 100, ym %% 100))
          ) %>%
          pivot_longer(
            cols = c(empty_in, empty_out),
            names_to = "metric",
            values_to = "value"
          ) %>%
          mutate(
            metric_label = ifelse(metric == "empty_in", "進港", "出港"),
            group_label = paste0(port_display, " (", metric_label, ")")
          )

        validate(need(nrow(plot_df) > 0, "沒有可顯示的港口分析資料。"))

        plot_obj <- ggplot(plot_df, aes(x = x_date, y = value, group = group_label, color = port_display, linetype = metric_label)) +
          geom_line(linewidth = 1.0) +
          geom_point(size = 2.0) +
          scale_linetype_manual(values = c("進港" = "solid", "出港" = "dashed")) +
          scale_x_date(date_breaks = "1 month", date_labels = "%m") +
          labs(x = "2025", y = "TEU", color = NULL, linetype = NULL) +
          theme_minimal(base_size = 11, base_family = "") +
          theme(
            panel.grid.major.x = element_line(color = "#eef2f7"),
            panel.grid.minor = element_blank(),
            legend.position = "bottom",
            legend.box = "vertical",
            axis.text.x = element_text(color = "#6b7280", size = 9),
            axis.title.x = element_text(color = "#94a3b8", size = 9, margin = margin(t = 8)),
            axis.text.y = element_text(color = "#6b7280"),
            plot.margin = margin(8, 16, 12, 8)
          )
        file <- save_ggplot_png(plot_obj, width = 1280, height = 620, res = 144)
        list(src = file, contentType = "image/png", width = "100%", height = "360px")
      }, deleteFile = TRUE)
    
      output$analysis_chart2 <- renderImage({
        plot_df <- analysis_data_selected() %>%
          mutate(
            port_display = display_port(port),
            pressure_scaled = pressure_index * 100,
            x_date = as.Date(sprintf("%04d-%02d-01", ym %/% 100, ym %% 100))
          ) %>%
          pivot_longer(
            cols = c(empty_net, pressure_scaled),
            names_to = "metric",
            values_to = "value"
          ) %>%
          mutate(
            metric_label = ifelse(metric == "empty_net", "淨流量", "壓力×100"),
            group_label = paste0(port_display, " (", metric_label, ")")
          )

        validate(need(nrow(plot_df) > 0, "沒有可顯示的港口分析資料。"))

        plot_obj <- ggplot(plot_df, aes(x = x_date, y = value, group = group_label, color = port_display, linetype = metric_label)) +
          geom_line(linewidth = 1.0) +
          geom_point(size = 2.0) +
          scale_linetype_manual(values = c("淨流量" = "solid", "壓力×100" = "dashed")) +
          scale_x_date(date_breaks = "1 month", date_labels = "%m") +
          labs(x = "2025", y = "值", color = NULL, linetype = NULL) +
          theme_minimal(base_size = 11, base_family = "") +
          theme(
            panel.grid.major.x = element_line(color = "#eef2f7"),
            panel.grid.minor = element_blank(),
            legend.position = "bottom",
            legend.box = "vertical",
            axis.text.x = element_text(color = "#6b7280", size = 9),
            axis.title.x = element_text(color = "#94a3b8", size = 9, margin = margin(t = 8)),
            axis.text.y = element_text(color = "#6b7280"),
            plot.margin = margin(8, 16, 12, 8)
          )
        file <- save_ggplot_png(plot_obj, width = 1280, height = 620, res = 144)
        list(src = file, contentType = "image/png", width = "100%", height = "360px")
      }, deleteFile = TRUE)
    
      output$analysis_summary_boxes <- renderUI({
        ports <- analysis_selected_ports()
        focus_rows <- analysis_data_selected() %>%
          filter(ym == selected_ym())
    
        sum_throughput <- sum(focus_rows$throughput, na.rm = TRUE)
        sum_in <- sum(focus_rows$empty_in, na.rm = TRUE)
        sum_out <- sum(focus_rows$empty_out, na.rm = TRUE)
        sum_net <- sum(focus_rows$empty_net, na.rm = TRUE)
        max_pressure <- max(focus_rows$pressure_index, na.rm = TRUE)
        avg_buffer <- mean(focus_rows$buffer_index, na.rm = TRUE)
    
        div(
          class = "summary-grid",
          div(class = "stat-box", div(class = "stat-label", "分析港口"), div(class = "stat-value", if (length(ports) > 1) paste0("綜合比較 (", length(ports), " 港)") else display_port(ports[[1]]))),
          div(class = "stat-box", div(class = "stat-label", "最新月份"), div(class = "stat-value", fmt_ym(selected_ym()))),
          div(class = "stat-box", div(class = "stat-label", "當月吞吐量"), div(class = "stat-value highlight", HTML(paste0(fmt_num(sum_throughput, 0), " <span style='font-size:0.7rem;color:var(--muted)'>TEU</span>")))),
          div(class = "stat-box", div(class = "stat-label", "當月淨流量"), div(class = "stat-value highlight", HTML(paste0(fmt_num(sum_net, 0), " <span style='font-size:0.7rem;color:var(--muted)'>TEU</span>")))),
          div(class = "stat-box", div(class = "stat-label", "空櫃進港"), div(class = "stat-value", HTML(paste0(fmt_num(sum_in, 0), " <span style='font-size:0.7rem;color:var(--muted)'>TEU</span>")))),
          div(class = "stat-box", div(class = "stat-label", "空櫃出港"), div(class = "stat-value", HTML(paste0(fmt_num(sum_out, 0), " <span style='font-size:0.7rem;color:var(--muted)'>TEU</span>")))),
          div(class = "stat-box", div(class = "stat-label", "壓力指標"), div(class = "stat-value danger", fmt_idx(max_pressure, 4))),
          div(class = "stat-box", div(class = "stat-label", "承接緩衝"), div(class = "stat-value", fmt_idx(avg_buffer, 4)))
        )
      })

      output$analysis_insight_note <- renderUI({
        focus_rows <- analysis_data_selected() %>%
          filter(ym == selected_ym()) %>%
          arrange(desc(pressure_index), desc(empty_net))

        if (nrow(focus_rows) == 0) {
          return(NULL)
        }

        top_pressure <- focus_rows %>% slice(1)
        top_buffer <- focus_rows %>% arrange(desc(buffer_index), pressure_index) %>% slice(1)
        net_direction <- sum(focus_rows$empty_net, na.rm = TRUE)

        direction_text <- if (net_direction > 0) {
          "整體呈現空櫃淨流入，代表本月有堆積壓力"
        } else if (net_direction < 0) {
          "整體呈現空櫃淨流出，代表本月消化量高於新增量"
        } else {
          "整體淨流量接近平衡，需搭配壓力與緩衝一起判讀"
        }

        div(
          class = "analysis-insight",
          div(class = "analysis-insight-title", "如何解讀這一區"),
          div(
            class = "analysis-insight-body",
            paste0(
              "這一區用來快速比較所選港口在 ",
              fmt_ym(selected_ym()),
              " 的供需方向與承接能力。當月壓力最高的是 ",
              display_port(top_pressure$port[[1]]),
              "（",
              fmt_idx(top_pressure$pressure_index[[1]], 4),
              "），承接緩衝最高的是 ",
              display_port(top_buffer$port[[1]]),
              "（",
              fmt_idx(top_buffer$buffer_index[[1]], 4),
              "）。",
              direction_text,
              "；若某港口同時出現淨增加為正且壓力上升，通常代表該港更需要優先處理。"
            )
          )
        )
      })
  }, envir = server_env)
}
