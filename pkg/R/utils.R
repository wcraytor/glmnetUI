#' Check Coefficient Sign Warnings
#'
#' Compares fitted coefficient signs against user-specified expected signs.
#' Returns a character vector of sign warning flags.
#'
#' @param coef_names Character vector of coefficient names (excluding intercept).
#' @param coef_values Numeric vector of coefficient values corresponding to
#'   \code{coef_names}.
#' @param expected_signs Named character vector where names are variable names
#'   and values are one of \code{"positive"}, \code{"negative"}, or
#'   \code{"either"}.
#'
#' @return A character vector the same length as \code{coef_names}. Each element
#'   is either \code{"OK"} or a descriptive warning string.
#'
#' @export
#' @examples
#' check_sign_warnings(
#'   coef_names = c("sqft", "age", "rooms"),
#'   coef_values = c(50, -2, -10),
#'   expected_signs = c(sqft = "positive", age = "negative", rooms = "positive")
#' )
check_sign_warnings <- function(coef_names, coef_values, expected_signs) {
  if (length(coef_names) != length(coef_values)) {
    stop("coef_names and coef_values must have the same length", call. = FALSE)
  }

  vapply(seq_along(coef_names), function(i) {
    nm <- coef_names[i]
    val <- coef_values[i]

    if (val == 0) return("OK")

    expected <- expected_signs[nm]
    if (is.null(expected) || is.na(expected) || expected == "either") {
      return("OK")
    }

    actual_sign <- if (val > 0) "positive" else "negative"
    if (actual_sign != expected) {
      paste0("Expected ", expected, ", got ", actual_sign)
    } else {
      "OK"
    }
  }, FUN.VALUE = character(1))
}

#' Build Coefficient Table from glmnet Model
#'
#' Extracts coefficients from a fitted glmnet or cv.glmnet model at a
#' given lambda value and returns a tidy data frame.
#'
#' @param model A fitted \code{glmnet} or \code{cv.glmnet} object.
#' @param lambda Numeric lambda value at which to extract coefficients.
#'   If \code{NULL} and model is a cv.glmnet object, uses \code{lambda.1se}.
#' @param gamma Numeric gamma value for relaxed fits (0 = fully relaxed,
#'   1 = regular glmnet). Only used for relaxed glmnet models. Default
#'   \code{NULL} (ignored for non-relaxed fits).
#'
#' @return A data frame with columns \code{variable} and \code{coefficient}.
#'
#' @export
#' @examples
#' x <- matrix(rnorm(100 * 5), 100, 5,
#'             dimnames = list(NULL, paste0("V", 1:5)))
#' y <- rnorm(100)
#' fit <- glmnet::cv.glmnet(x, y)
#' build_coef_table(fit)
build_coef_table <- function(model, lambda = NULL, gamma = NULL) {
  if (is.null(lambda)) {
    if (inherits(model, "cv.glmnet") || inherits(model, "cv.relaxed")) {
      lambda <- model$lambda.1se
    } else {
      stop("lambda must be specified for non-cv.glmnet models", call. = FALSE)
    }
  }

  coef_args <- list(model, s = lambda)
  if (!is.null(gamma)) coef_args$gamma <- gamma
  coefs <- do.call(stats::coef, coef_args)
  coef_df <- data.frame(
    variable = rownames(coefs),
    coefficient = as.numeric(coefs),
    stringsAsFactors = FALSE
  )
  coef_df[coef_df$coefficient != 0, , drop = FALSE]
}

#' Font Family Helper (internal)
#'
#' Returns \code{"Roboto Condensed"} if available via \pkg{sysfonts},
#' otherwise \code{"sans"}.
#'
#' @return A character string with the font family name.
#' @keywords internal
glmnet_font_family_ <- function() {
  if (requireNamespace("sysfonts", quietly = TRUE) &&
      "Roboto Condensed" %in% sysfonts::font_families()) {
    "Roboto Condensed"
  } else {
    "sans"
  }
}

#' Plot Dimension Helper (internal)
#'
#' Returns \code{width} and \code{height} functions that read the plot
#' container size from \code{session$clientData}. Used with
#' \code{renderPlot(width = d$width, height = d$height, res = 96)} to
#' normalise rendering across platforms (macOS, Windows, Linux).
#'
#' @param session The Shiny session object.
#' @param id Character output ID (un-namespaced; namespacing is handled
#'   automatically for modules).
#' @return A list with \code{width} and \code{height} functions.
#' @keywords internal
plot_dims_ <- function(session, id) {
  full_id <- if (!is.null(session$ns)) session$ns(id) else id
  w_key <- paste0("output_", full_id, "_width")
  h_key <- paste0("output_", full_id, "_height")
  list(
    width  = function() session$clientData[[w_key]],
    height = function() session$clientData[[h_key]]
  )
}

#' Null-coalescing operator (internal)
#'
#' Returns \code{a} unless it is \code{NULL}, in which case \code{b}. Defined
#' package-locally so the regProj helpers do not depend on \pkg{shiny} being
#' attached to the search path.
#'
#' @param a,b Values; \code{a} is returned unless \code{NULL}.
#' @return \code{a} if non-NULL, else \code{b}.
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Guess the default Special-column tag from a variable name (internal)
#'
#' Returns the appraisal Special tag most similar to \code{name} (by exact /
#' synonym / whole-token match), or \code{"no"} when no tag is a reasonable
#' match. Conservative on purpose: generic words (\code{"area"}, \code{"age"})
#' only match a whole name, never a sub-token, so columns like
#' \code{area_id} or \code{garage_spaces} stay \code{"no"}. The placeholder
#' tags \code{"no"} and \code{"display_only"} are never guessed. Kept identical
#' across the sibling apps (mgcvUI, earthUI).
#'
#' @param name Character scalar column name (already snake_cased).
#' @param tags Character vector of available Special options.
#' @return A single tag from \code{tags}, or \code{"no"}.
#' @noRd
special_default_for_ <- function(name, tags) {
  cand <- setdiff(tags, c("no", "display_only"))
  if (length(cand) == 0L || is.null(name) || !nzchar(name)) return("no")

  norm <- function(x) gsub("[^a-z0-9]+", "", tolower(x))
  tok <- function(x) {
    x <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", x)        # camelCase -> space
    parts <- strsplit(tolower(x), "[^a-z0-9]+")[[1L]]
    parts[nzchar(parts)]
  }
  # Known abbreviations / synonyms -> canonical tag.
  syn <- list(
    latitude        = c("lat"),
    longitude       = c("long", "lon", "lng"),
    living_area     = c("livingarea", "gla", "sqft", "livingsqft", "livingsf",
                        "grosslivingarea", "livarea", "livsf", "livingsq"),
    lot_size        = c("lotsize", "lotsf", "lotsqft", "lotarea"),
    site_dimensions = c("sitedimensions", "sitedim", "sitedims"),
    actual_age      = c("age", "actualage"),
    effective_age   = c("effectiveage", "effage"),
    sale_age        = c("saleage", "ageofsale", "daystosale"),
    sale_type       = c("saletype", "typeofsale"),
    contract_date   = c("contractdate", "kdate"),
    listing_date    = c("listingdate", "listdate"),
    dom             = c("daysonmarket", "daysmarket"),
    concessions     = c("concession", "conc", "sellerconcessions",
                        "saleconcessions"),
    weight          = c("wt", "wgt")
  )
  generic <- c("area", "age")  # match whole-name only, never a sub-token

  keys_for <- function(tag) {
    k <- unique(c(norm(tag), norm(syn[[tag]])))
    k[nzchar(k)]
  }
  nn   <- norm(name)
  toks <- norm(tok(name))

  # 1) whole-name equals a tag or synonym
  for (tag in cand) if (nn %in% keys_for(tag)) return(tag)
  # 2) a name token equals a specific (non-generic, >= 3 char) key
  for (tag in cand) {
    k <- setdiff(keys_for(tag), generic)
    k <- k[nchar(k) >= 3L]
    if (length(k) && any(toks %in% k)) return(tag)
  }
  "no"
}

#' Format the canonical fit timestamp for output filenames (internal)
#'
#' Every output produced from a single model fit (Excel exports, the sales grid,
#' and the qmd/docx/html/pdf reports) is named with the *fit* time rather than
#' each file's creation time, so a fit's outputs group together and a Trilogy
#' run can gather the three methods' files by fit time. Falls back to the
#' current time if no fit timestamp is available.
#'
#' @param ts A POSIXct fit time (e.g. `model_module$fit_ts()`), or NULL.
#' @return Character `"%Y%m%d_%H%M%S"`.
#' @noRd
fit_stamp_ <- function(ts = NULL) {
  if (is.null(ts) || length(ts) == 0L || all(is.na(ts))) ts <- Sys.time()
  format(ts, "%Y%m%d_%H%M%S")
}
