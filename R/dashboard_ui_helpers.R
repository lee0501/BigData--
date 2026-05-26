page_head <- function(title_text) {
  div(
    class = "page-head",
    tags$button(
      type = "button",
      class = "hamburger-btn js-hamburger",
      span(),
      span(),
      span()
    ),
    h1(class = "page-title", title_text)
  )
}

nav_tab <- function(page_id, label, active = FALSE, icon_svg) {
  tags$a(
    href = "#",
    class = paste("nav-tab", if (active) "active" else ""),
    `data-page` = page_id,
    onclick = sprintf("switchPage('%s'); return false;", page_id),
    HTML(icon_svg),
    label
  )
}

sidebar_ui <- function(default_month = 3) {
  tags$aside(
    class = "sidebar",
    id = "sidebar",
    div(
      class = "brand",
      div(
        class = "brand-icon",
        HTML("<svg width='16' height='16' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><path d='M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5' /></svg>")
      ),
      span(class = "brand-name", "iMarine")
    ),
    div(
      class = "nav-section",
      div(class = "nav-section-label", "功能導覽"),
      uiOutput("sidebar_nav")
    ),
    div(
      class = "sidebar-filter",
      div(
        class = "sf-header",
        div(
          class = "sf-header-title",
          HTML("<svg width='12' height='12' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><line x1='4' y1='6' x2='20' y2='6' /><line x1='8' y1='12' x2='16' y2='12' /><line x1='11' y1='18' x2='13' y2='18' /></svg>"),
          "篩選條件"
        )
      ),
      div(
        class = "sf-group",
        span(class = "sf-label", "年份"),
        tags$select(
          id = "year-selector",
          class = "sf-select",
          onchange = "Shiny.setInputValue('year_selector', this.value, {priority: 'event'})",
          tags$option(value = "2025", selected = "selected", "2025 年")
        )
      ),
      div(
        class = "sf-group",
        span(class = "sf-label", "月份"),
        tags$select(
          id = "month-selector",
          class = "sf-select",
          onchange = "Shiny.setInputValue('month_selector', this.value, {priority: 'event'})",
          lapply(1:12, function(m) {
            tags$option(
              value = as.character(m),
              selected = if (m == default_month) "selected" else NULL,
              paste0(m, " 月")
            )
          })
        )
      ),
      uiOutput("sidebar_context_panel")
    )
  )
}

advanced_modal_ui <- function() {
  div(
    class = "adv-backdrop",
    id = "advBackdrop",
    div(
      class = "adv-modal",
      div(
        class = "adv-modal-head",
        div(
          div(class = "adv-modal-title", "進階設定"),
          uiOutput("advanced_modal_sub")
        ),
        tags$button(
          type = "button",
          class = "adv-close",
          id = "advClose",
          HTML("<svg width='14' height='14' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2.5'><path d='M18 6 6 18M6 6l12 12' /></svg>")
        )
      ),
      div(class = "adv-modal-body", uiOutput("advanced_modal_body")),
      div(
        class = "adv-modal-foot",
        tags$button(type = "button", class = "adv-btn-cancel", id = "advCancel", "取消"),
        tags$button(type = "button", class = "adv-btn-apply", id = "advApply", "套用")
      )
    )
  )
}

weight_chip_set <- function(named_weights, labels) {
  div(
    class = "weight-mini-list",
    lapply(names(named_weights), function(key) {
      span(
        class = "weight-mini-chip",
        paste0(labels[[key]], " ", fmt_weight(named_weights[[key]]))
      )
    })
  )
}

custom_slider_card <- function(id, title, sub, value, group_name, help_text = NULL) {
  div(
    class = "adv-slider-card",
    div(
      class = "adv-slider-top",
      span(class = "adv-slider-name", HTML(sprintf("%s <span style='color:var(--muted);font-weight:500;'>(%s)</span>", title, sub))),
      span(class = "adv-pct-badge", id = paste0("pct-", id), paste0("實質占比: ", value, "%"))
    ),
    if (!is.null(help_text) && nzchar(help_text)) {
      div(class = "adv-slider-help", help_text)
    },
    div(
      class = "adv-slider-row",
      span(class = "adv-slider-label", "權重"),
      tags$input(
        id = paste0("sl-", id),
        class = "adv-slider",
        type = "range",
        min = 10,
        max = 60,
        step = 1,
        value = value,
        oninput = sprintf("rebalanceWeightGroup('%s', 'sl-%s')", group_name, id)
      ),
      span(class = "adv-slider-val", id = paste0("val-", id), value)
    )
  )
}
