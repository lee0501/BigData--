server <- function(input, output, session) {
  rv <- reactiveValues(
    main_nav = "overview",
    analysis_ports = c("高雄港"),
    dt_page = 1L,
    dt_sort_col = NULL,
    dt_sort_dir = "asc",
    match_has_run = FALSE,
    match_selected_target = NULL
  )

  observeEvent(input$nav_page_click, {
    rv$main_nav <- sub("::.*$", "", input$nav_page_click)
    updateTabsetPanel(session, "main_nav", selected = rv$main_nav)
  })

  observeEvent(input$main_nav, {
    if (!is.null(input$main_nav) && nzchar(input$main_nav)) {
      rv$main_nav <- input$main_nav
    }
  }, ignoreInit = TRUE)


  selected_year <- reactive(as.integer(input$year_selector %||% 2025))
  selected_month <- reactive(as.integer(input$month_selector %||% 3))
  selected_ym <- reactive(month_to_ym(selected_month(), selected_year()))

  active_mode <- reactive(if ((input$main_nav %||% rv$main_nav %||% "overview") == "explore") "explore" else "formal")
  active_explore_tab <- reactive(input$explore_mode_select %||% "strategy")
  active_strategy_key <- reactive(input$strategy_template_select %||% "flow_focus")
  active_strategy <- reactive(strategy_templates[[active_strategy_key()]])

  read_weight_pct <- function(input_id, default_pct) {
    val <- suppressWarnings(as.numeric(input[[input_id]]))
    if (is.null(val) || length(val) == 0 || all(is.na(val))) {
      default_pct / 100
    } else {
      val[[1]] / 100
    }
  }

  custom_pressure_weights <- reactive({
    sanitize_weight_vector(c(
      flow = read_weight_pct("sl-flow", 22),
      net = read_weight_pct("sl-net", 60),
      roll = read_weight_pct("sl-roll", 18)
    ), official_pressure_weights)
  })

  custom_buffer_weights <- reactive({
    sanitize_weight_vector(c(
      space = read_weight_pct("sl-space", 45),
      netout = read_weight_pct("sl-netout", 40),
      export = read_weight_pct("sl-export", 15)
    ), official_buffer_weights)
  })

  custom_matching_weights <- reactive({
    sanitize_weight_vector(c(
      source = read_weight_pct("sl-source", 45),
      target = read_weight_pct("sl-target", 35),
      distance = read_weight_pct("sl-distance", 20)
    ), official_matching_weights)
  })

  active_pressure_weights <- reactive({
    if (active_mode() == "formal") {
      official_pressure_weights
    } else if (active_explore_tab() == "strategy") {
      active_strategy()$pressure_weights
    } else {
      custom_pressure_weights()
    }
  })

  active_buffer_weights <- reactive({
    if (active_mode() == "formal") {
      official_buffer_weights
    } else if (active_explore_tab() == "strategy") {
      official_buffer_weights
    } else {
      custom_buffer_weights()
    }
  })

  active_matching_weights <- reactive({
    if (active_mode() == "formal") {
      official_matching_weights
    } else if (active_explore_tab() == "strategy") {
      official_matching_weights
    } else {
      custom_matching_weights()
    }
  })

  computed_history_formal <- reactive({
    build_metric_history(
      history_tbl = base_history,
      ports = formal_ports,
      pressure_weights = active_pressure_weights(),
      buffer_weights = active_buffer_weights()
    )
  })

  computed_status_formal <- reactive({
    build_status_table(computed_history_formal())
  })

  computed_history_display <- reactive({
    build_metric_history(
      history_tbl = base_history,
      ports = table_ports,
      pressure_weights = active_pressure_weights(),
      buffer_weights = active_buffer_weights()
    )
  })

  overview_month_status <- reactive({
    if (active_mode() == "formal") {
      status_formal_display %>%
        filter(ym == selected_ym())
    } else {
      computed_status_formal() %>%
        filter(ym == selected_ym())
    }
  })

  overview_default_source <- reactive({
    if (active_mode() == "formal") {
      reco_row <- simulation_reco %>%
        filter(ym == selected_ym()) %>%
        slice(1)

      if (nrow(reco_row) > 0) {
        reco_row$source_port[[1]]
      } else {
        default_source_for_month(overview_month_status())
      }
    } else {
      default_source_for_month(overview_month_status())
    }
  })

  overview_default_move_amount <- reactive({
    if (active_mode() == "formal") {
      reco_row <- simulation_reco %>%
        filter(ym == selected_ym()) %>%
        slice(1)

      if (nrow(reco_row) > 0) {
        reco_row$recommended_move_amount[[1]]
      } else {
        default_move_amount(overview_month_status(), overview_default_source())
      }
    } else {
      default_move_amount(overview_month_status(), overview_default_source())
    }
  })

  overview_candidates <- reactive({
    if (nrow(overview_month_status()) == 0) {
      return(tibble())
    }

    if (active_mode() == "formal") {
      build_formal_candidate_rows(selected_ym())
    } else {
      build_candidate_table(
        ym_value = selected_ym(),
        source_port = overview_default_source(),
        move_amount = overview_default_move_amount(),
        status_table = computed_status_formal(),
        history_table = computed_history_formal(),
        matching_weights = active_matching_weights(),
        pressure_weights = active_pressure_weights(),
        buffer_weights = active_buffer_weights()
      ) %>%
        slice_head(n = 3)
    }
  })

  explore_month_status <- reactive({
    computed_status_formal() %>%
      filter(ym == selected_ym())
  })

  explore_candidates <- reactive({
    snapshot <- explore_month_status()

    if (nrow(snapshot) == 0) {
      return(tibble())
    }

    build_candidate_table(
      ym_value = selected_ym(),
      source_port = default_source_for_month(snapshot),
      move_amount = default_move_amount(snapshot, default_source_for_month(snapshot)),
      status_table = computed_status_formal(),
      history_table = computed_history_formal(),
      matching_weights = active_matching_weights(),
      pressure_weights = active_pressure_weights(),
      buffer_weights = active_buffer_weights()
    ) %>%
      slice_head(n = 5)
  })

  observeEvent(input$toggle_analysis_port, {
    port_clicked <- normalize_port(sub("::.*$", "", input$toggle_analysis_port))
    current <- rv$analysis_ports

    if (port_clicked %in% current) {
      if (length(current) > 1) {
        rv$analysis_ports <- setdiff(current, port_clicked)
      }
    } else {
      rv$analysis_ports <- c(current, port_clicked)
    }
  })

  analysis_selected_ports <- reactive({
    rv$analysis_ports %||% c("高雄港")
  })

  analysis_data_selected <- reactive({
    if (active_mode() == "formal") {
      status_formal_display %>%
        filter(port %in% analysis_selected_ports())
    } else {
      computed_status_formal() %>%
        filter(port %in% analysis_selected_ports())
    }
  })

  observeEvent(input$dt_show, {
    rv$dt_page <- 1L
  })

  observeEvent(input$dt_search, {
    rv$dt_page <- 1L
  })

  observeEvent(input$dt_sort_click, {
    col <- sub("::.*$", "", input$dt_sort_click)
    if (identical(rv$dt_sort_col, col)) {
      rv$dt_sort_dir <- if (identical(rv$dt_sort_dir, "asc")) "desc" else "asc"
    } else {
      rv$dt_sort_col <- col
      rv$dt_sort_dir <- "asc"
    }
    rv$dt_page <- 1L
  })

  observeEvent(input$dt_action, {
    action <- sub("::.*$", "", input$dt_action)
    total_rows <- nrow(filtered_raw_table())
    page_size <- as.integer(input$dt_show %||% 10)
    total_pages <- max(1L, ceiling(total_rows / page_size))

    if (action == "prev") {
      rv$dt_page <- max(1L, rv$dt_page - 1L)
    } else if (action == "next") {
      rv$dt_page <- min(total_pages, rv$dt_page + 1L)
    }
  })

  raw_table_data <- reactive({
    if (active_mode() == "formal") {
      status_formal_display %>%
        transmute(
          ym = ym_label,
          port = display_port(port),
          empty_in,
          empty_out,
          empty_net,
          throughput,
          pressure_index,
          buffer_index
        )
    } else {
      computed_history_display() %>%
        filter(port %in% normalize_port(table_ports)) %>%
        transmute(
          ym = ym_label,
          port = display_port(port),
          empty_in,
          empty_out,
          empty_net,
          throughput,
          pressure_index,
          buffer_index
        )
    }
  })

  filtered_raw_table <- reactive({
    data <- raw_table_data()
    query <- tolower(input$dt_search %||% "")

    if (nzchar(query)) {
      data <- data %>%
        filter(
          apply(., 1, function(row) any(grepl(query, tolower(as.character(row)), fixed = TRUE)))
        )
    }

    if (!is.null(rv$dt_sort_col)) {
      col_name <- rv$dt_sort_col
      desc_dir <- identical(rv$dt_sort_dir, "desc")

      if (col_name %in% names(data)) {
        if (is.numeric(data[[col_name]])) {
          data <- data %>% arrange(if (desc_dir) desc(.data[[col_name]]) else .data[[col_name]])
        } else {
          data <- data %>% arrange(if (desc_dir) desc(.data[[col_name]]) else .data[[col_name]])
        }
      }
    }

    data
  })

  paged_raw_table <- reactive({
    data <- filtered_raw_table()
    page_size <- as.integer(input$dt_show %||% 10)
    total_pages <- max(1L, ceiling(nrow(data) / page_size))
    page_now <- min(rv$dt_page, total_pages)
    rv$dt_page <- page_now
    start_idx <- (page_now - 1L) * page_size + 1L
    end_idx <- min(nrow(data), page_now * page_size)

    list(
      data = if (nrow(data) == 0) data[0, ] else data[start_idx:end_idx, , drop = FALSE],
      total = nrow(data),
      page = page_now,
      page_size = page_size,
      pages = total_pages,
      start = if (nrow(data) == 0) 0L else start_idx,
      end = if (nrow(data) == 0) 0L else end_idx
    )
  })

  match_source_choices <- reactive({
    snapshot <- overview_month_status() %>%
      arrange(desc(pressure_index), desc(empty_net))

    if (nrow(snapshot) == 0) {
      return(tibble(value = character(), label = character()))
    }

    choices <- snapshot %>%
      transmute(
        value = port,
        label = paste0(
          display_port(port),
          " (",
          port_codes[port],
          ") | ",
          display_port(status),
          " | 壓力 ",
          round(pressure_index * 100)
        )
      )

    choices
  })

  match_current_source <- reactive({
    available <- match_source_choices()$value
    default_source <- overview_default_source()
    src_value <- normalize_port(input$sim_src %||% display_port(default_source))

    if (!length(available)) {
      return(default_source)
    }

    if (!nzchar(src_value) || !(src_value %in% available)) {
      return(available[[1]])
    }

    if (!nzchar(src_value)) {
      overview_default_source()
    } else {
      src_value
    }
  })

  match_current_volume <- reactive({
    raw_value <- suppressWarnings(as.numeric(input$sim_volume %||% round(overview_default_move_amount(), 0)))
    default_value <- round(overview_default_move_amount(), 0)

    if (!is.finite(raw_value) || raw_value <= 0) {
      default_value
    } else {
      round(raw_value, 0)
    }
  })

  match_current_date <- reactive({
    input$sim_date %||% as.character(month_to_date(selected_month(), selected_year()))
  })

  observeEvent(selected_ym(), {
    rv$match_selected_target <- NULL
  })

  observeEvent(input$matching_target_click, {
    rv$match_selected_target <- sub("::.*$", "", input$matching_target_click)
  })

  observeEvent(input$matching_refresh, {
    rv$match_selected_target <- NULL
    showNotification("已依目前設定重新推演。", type = "message", duration = 1.2)
  })

  match_candidates <- reactive({
    if (nrow(overview_month_status()) == 0) {
      return(tibble())
    }

    build_candidate_table(
      ym_value = selected_ym(),
      source_port = match_current_source(),
      move_amount = match_current_volume(),
      status_table = computed_status_formal(),
      history_table = computed_history_formal(),
      matching_weights = active_matching_weights(),
      pressure_weights = active_pressure_weights(),
      buffer_weights = active_buffer_weights()
    ) %>%
      slice_head(n = 3)
  })

  match_selected_card <- reactive({
    cards <- match_candidates()
    if (nrow(cards) == 0) {
      return(tibble())
    }

    selected_key <- rv$match_selected_target
    if (is.null(selected_key) || length(selected_key) == 0 || !nzchar(selected_key) || !(selected_key %in% cards$candidate_key)) {
      return(cards %>% slice(1))
    }

    chosen <- cards %>%
      filter(candidate_key == selected_key) %>%
      slice(1)

    if (nrow(chosen) == 0) cards %>% slice(1) else chosen
  })

  output$overview_month_chip <- renderUI({
    span(class = "c-chip", fmt_ym(selected_ym()))
  })

  output$overview_featured <- renderUI({
    snapshot <- overview_month_status()
    candidates <- overview_candidates()

    if (nrow(snapshot) == 0) {
      return(div(class = "feat-desc", "本月份目前沒有可供顯示的正式分析資料。"))
    }

    urgent <- snapshot %>%
      arrange(desc(pressure_index), desc(empty_net)) %>%
      slice(1)

    high_count <- sum(snapshot$status == "高壓力港")
    buffer_count <- sum(snapshot$status == "高緩衝港")
    pair_count <- nrow(candidates)
    prev_snapshot <- if (active_mode() == "formal") {
      status_formal_display %>% filter(ym == selected_ym() - 1)
    } else {
      computed_status_formal() %>% filter(ym == selected_ym() - 1)
    }
    pressure_delta <- mean(snapshot$pressure_index, na.rm = TRUE) -
      mean(prev_snapshot$pressure_index, na.rm = TRUE)
    pressure_delta_pct <- ifelse(is.finite(pressure_delta), round(pressure_delta * 100), 0)

    urgent_text <- paste0(
      display_port(urgent$port[[1]]),
      " 壓力指標達 ",
      fmt_idx(urgent$pressure_index[[1]], 4),
      if (urgent$status[[1]] == "高壓力港") "，已進入正式高壓區。" else "，為本月相對壓力最高港。"
    )

    action_text <- if (nrow(candidates) > 0) {
      top_row <- candidates %>% slice(1)
      paste0(
        "建議優先檢查「",
        display_port(top_row$source_port[[1]]),
        " → ",
        display_port(top_row$target_port[[1]]),
        "」方案，預設調度量 ",
        fmt_num(if ("move_amount" %in% names(top_row)) top_row$move_amount[[1]] else overview_default_move_amount(), 0),
        " TEU。"
      )
    } else {
      "本月未形成有效候選配對，暫以觀察各港壓力變化為主。"
    }

    tagList(
      div(
        style = "flex-shrink:0;",
        div(
          style = "display:flex;align-items:center;gap:8px;margin-bottom:4px;",
          div(class = "feat-title", "現況總結")
        ),
        div(
          class = "feat-desc",
          paste0(
            "目前有 ",
            high_count,
            " 個港口處於高壓狀態，平均壓力指數較上月",
            ifelse(pressure_delta_pct >= 0, "上升 ", "下降 "),
            abs(pressure_delta_pct),
            "%。"
          )
        )
      ),
      div(
        class = "feat-alert-items",
        div(
          class = "feat-alert-item",
          span(class = "feat-alert-label", "最緊急事項"),
          span(class = "feat-alert-val", urgent_text)
        ),
        div(
          class = "feat-alert-item",
          span(class = "feat-alert-label", "建議行動"),
          span(class = "feat-alert-val", action_text)
        )
      ),
      div(
        class = "feat-kpi-row",
        div(class = "feat-kpi-item danger", span(class = "feat-kpi-num danger", high_count), span(class = "feat-kpi-lbl", "高壓力港口")),
        div(class = "feat-kpi-sep"),
        div(class = "feat-kpi-item success", span(class = "feat-kpi-num success", buffer_count), span(class = "feat-kpi-lbl", "可承接港口")),
        div(class = "feat-kpi-sep"),
        div(class = "feat-kpi-item neutral", span(class = "feat-kpi-num blue", pair_count), span(class = "feat-kpi-lbl", "候選配對"))
      )
    )
  })

  register_shell_outputs(environment())
  register_explore_outputs(environment())
  register_overview_outputs(environment())
  register_analysis_outputs(environment())
  register_matching_outputs(environment())
  register_provenance_outputs(environment())
}
