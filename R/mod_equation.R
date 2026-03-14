#' Model Equation Module UI
#'
#' @param id Module namespace ID.
#' @return A Shiny tagList.
#' @export
equationUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      style = "overflow-x: auto; padding: 10px 10px 10px 0;",
      shiny::uiOutput(ns("model_equation"))
    )
  )
}

#' Model Equation Module Server
#'
#' @param id Module namespace ID.
#' @param model_module Reactive list from [modelingServer()].
#' @param data_module Reactive list from [dataImportServer()].
#' @return A reactive list containing the equation LaTeX strings.
#' @export
equationServer <- function(id, model_module, data_module) {
  shiny::moduleServer(id, function(input, output, session) {

    equation <- shiny::reactive({
      shiny::req(model_module$fitted())
      model_module$fit_count()

      model  <- model_module$model()
      lambda <- model_module$lambda()
      gamma  <- model_module$gamma()
      resp   <- data_module$response()

      format_glmnet_equation_(model, lambda, gamma, resp)
    })

    output$model_equation <- shiny::renderUI({
      shiny::req(equation())
      eq <- equation()
      shiny::withMathJax(shiny::HTML(eq$latex_inline))
    })

    return(list(equation = equation))
  })
}

#' Format glmnet Model as LaTeX Equation
#'
#' @param model A glmnet or cv.glmnet model object.
#' @param lambda Selected lambda value.
#' @param gamma Selected gamma value (for relaxed models), or NULL.
#' @param response Name of the response variable.
#' @return A list with `latex` and `latex_inline` strings.
#' @noRd
format_glmnet_equation_ <- function(model, lambda, gamma, response) {
  # Extract coefficients
  coef_args <- list(model, s = lambda)
  if (!is.null(gamma)) coef_args$gamma <- gamma
  coef_sparse <- do.call(stats::coef, coef_args)
  coef_vec <- as.numeric(coef_sparse)
  coef_names <- rownames(coef_sparse)

  intercept <- coef_vec[1L]
  beta <- coef_vec[-1L]
  names(beta) <- coef_names[-1L]

  # Keep only non-zero coefficients
  nonzero <- beta[beta != 0]

  # Escape variable name for LaTeX \text{}
  esc <- function(x) gsub("_", "\\_", x, fixed = TRUE)

  # Build LaTeX lines
  lines <- character(0)

  # First line: response = intercept
  resp_tex <- esc(response)
  lines[1L] <- sprintf("\\text{%s} \\;=\\; & %s",
                        resp_tex, format_coef_(intercept))

  # Subsequent lines: + beta * x

  if (length(nonzero) > 0) {
    for (i in seq_along(nonzero)) {
      b <- nonzero[i]
      nm <- names(nonzero)[i]

      sign_str <- if (b >= 0) "+" else "-"
      abs_b <- abs(b)

      # Format the variable term
      if (grepl(":", nm, fixed = TRUE)) {
        # Interaction term: x1:x2 -> \text{x1} \times \text{x2}
        parts <- strsplit(nm, ":", fixed = TRUE)[[1L]]
        var_tex <- paste(sprintf("\\text{%s}", vapply(parts, esc, character(1))),
                         collapse = " \\times ")
      } else {
        var_tex <- sprintf("\\text{%s}", esc(nm))
      }

      lines[length(lines) + 1L] <- sprintf(
        "& %s \\; %s \\cdot %s",
        sign_str, format_coef_(abs_b), var_tex
      )
    }
  }

  # Assemble array
  body <- paste(lines, collapse = " \\\\\n")
  latex <- sprintf("\\begin{array}{rl}\n%s\n\\end{array}", body)
  latex_inline <- paste0("$$\n", latex, "\n$$")

  list(
    latex = latex,
    latex_inline = latex_inline
  )
}

#' Format Coefficient Value for LaTeX
#' @param x Numeric coefficient value.
#' @return Character string formatted for LaTeX.
#' @noRd
format_coef_ <- function(x) {
  if (abs(x) >= 1000) {
    formatC(x, format = "f", digits = 2, big.mark = ",")
  } else if (abs(x) >= 1) {
    formatC(x, format = "f", digits = 4)
  } else {
    formatC(x, format = "f", digits = 6)
  }
}
