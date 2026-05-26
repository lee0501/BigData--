required_packages <- c("shiny", "ggplot2", "dplyr", "readr", "tidyr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    ". Please install them before running the dashboard."
  )
}

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

if (capabilities("cairo")) {
  options(bitmapType = "cairo")
} else if (capabilities("aqua")) {
  options(bitmapType = "quartz")
}

locate_app_dir <- function() {
  candidates <- unique(c(
    getwd(),
    "/Users/lee/Documents/BigData"
  ))

  hits <- candidates[
    vapply(
      candidates,
      function(x) file.exists(file.path(x, "context", "status_final_2025.csv")),
      logical(1)
    )
  ]

  if (!length(hits)) {
    stop("Cannot locate the project directory containing context/status_final_2025.csv")
  }

  normalizePath(hits[[1]], winslash = "/", mustWork = TRUE)
}
