#' Launch the glmnetUI Shiny Application
#'
#' Starts an interactive Shiny application for elastic net regression
#' modeling using \pkg{glmnet}. The application provides point-and-click
#' data import, variable selection, model fitting with cross-validation,
#' coefficient sign warnings, diagnostic plots, and report export.
#'
#' @param port Integer port number for the Shiny app. Defaults to 7879.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return This function does not return a value; it launches a Shiny
#'   application in the user's default browser.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   glmnetUI()
#' }
glmnetUI <- function(port = 7879L, ...) {
  required <- c("shiny", "glmnet", "readr", "readxl", "DT",
                "ggplot2", "officer", "rmarkdown", "jsonlite",
                "bslib")
  missing <- required[!vapply(required, requireNamespace,
                              quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) > 0L) {
    stop("The following required packages are not installed: ",
         paste(missing, collapse = ", "),
         "\nInstall them with: install.packages(c(",
         paste0('"', missing, '"', collapse = ", "), "))",
         call. = FALSE)
  }

  app_dir <- system.file("app", package = "glmnetUI")
  if (!nzchar(app_dir)) {
    stop("Could not find the glmnetUI app directory. ",
         "Is the package installed correctly?", call. = FALSE)
  }

  shiny::runApp(app_dir, port = port, ...)
}
