sanitize_weight_vector <- function(values, defaults, min_value = 0.10, max_value = 0.60) {
  vals <- suppressWarnings(as.numeric(values))

  if (length(vals) != length(defaults) || any(is.na(vals))) {
    vals <- as.numeric(defaults)
  }

  vals <- pmin(pmax(vals, min_value), max_value)
  target_sum <- 1

  for (i in seq_len(10)) {
    diff <- target_sum - sum(vals)
    if (abs(diff) < 1e-8) {
      break
    }

    adjustable <- if (diff > 0) which(vals < (max_value - 1e-8)) else which(vals > (min_value + 1e-8))
    if (length(adjustable) == 0) {
      vals <- as.numeric(defaults)
      break
    }

    step <- diff / length(adjustable)
    vals[adjustable] <- vals[adjustable] + step
    vals <- pmin(pmax(vals, min_value), max_value)
  }

  if (abs(sum(vals) - target_sum) > 1e-8) {
    vals <- as.numeric(defaults)
  }

  names(vals) <- names(defaults)
  vals
}

level_class <- function(value) {
  if (is.na(value)) {
    "low"
  } else if (value >= 75) {
    "high"
  } else if (value >= 45) {
    "mid"
  } else {
    "low"
  }
}

level_label <- function(level) {
  if (level == "high") {
    "高壓"
  } else if (level == "mid") {
    "中壓"
  } else {
    "低壓"
  }
}

status_card_class <- function(status_value) {
  if (identical(status_value, "高壓力港")) {
    "high"
  } else if (identical(status_value, "高緩衝港")) {
    "low"
  } else if (identical(status_value, "資料暖機中")) {
    "mid"
  } else {
    "mid"
  }
}

status_badge_label <- function(status_value) {
  if (identical(status_value, "高壓力港")) {
    "正式高壓"
  } else if (identical(status_value, "高緩衝港")) {
    "正式高緩衝"
  } else if (identical(status_value, "資料暖機中")) {
    "資料暖機中"
  } else {
    "正式正常"
  }
}

simulation_result_class <- function(source_status_after, target_pressure_after, high_pressure_cut) {
  source_relieved <- source_status_after != "高壓力港"
  target_safe <- target_pressure_after < high_pressure_cut

  if (source_relieved && target_safe) {
    "完全解壓"
  } else if (target_safe) {
    "可緩解但未解壓"
  } else {
    "不建議調度"
  }
}

simulation_result_rank <- function(x) {
  case_when(
    x == "完全解壓" ~ 1L,
    x == "可緩解但未解壓" ~ 2L,
    TRUE ~ 3L
  )
}

fmt_weight <- function(x) {
  paste0(round(x * 100), "%")
}

save_ggplot_png <- function(plot_obj, width = 960, height = 420, res = 144) {
  file <- tempfile(fileext = ".png")
  device_type <- if (capabilities("cairo")) {
    "cairo"
  } else if (capabilities("aqua")) {
    "quartz"
  } else {
    getOption("bitmapType", "cairo")
  }

  tryCatch({
    grDevices::png(filename = file, width = width, height = height, res = res, type = device_type)
    print(plot_obj)
    grDevices::dev.off()
    file
  }, error = function(e) {
    try(grDevices::dev.off(), silent = TRUE)
    grDevices::png(filename = file, width = width, height = height, res = res)
    graphics::plot.new()
    graphics::text(0.5, 0.5, labels = paste("Plot render failed:", conditionMessage(e)), cex = 1)
    grDevices::dev.off()
    file
  })
}

expanding_percent_rank <- function(x) {
  vapply(
    seq_along(x),
    function(i) current_percent_rank(x[seq_len(i)]),
    numeric(1)
  )
}
