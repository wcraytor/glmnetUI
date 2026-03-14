#' Contributions Module UI
#'
#' @param id Module namespace ID.
#' @return A Shiny tagList.
#' @export
contributionsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("contrib_var"), "Select Variable", choices = NULL),
    shiny::plotOutput(ns("contrib_plot"), height = "450px")
  )
}

# Internal: choose decimal places based on axis range
#' @noRd
contrib_auto_digits_ <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(0L)
  rng <- diff(range(x))
  if (rng < 1) 3L
  else if (rng < 10) 2L
  else if (rng < 100) 1L
  else 0L
}

# Internal: format slope label adapting units to x-axis range
#' @noRd
contrib_slope_label_ <- function(slope, x_breaks) {
  d <- contrib_auto_digits_(x_breaks)
  bm <- locale_big_mark_()
  dm <- locale_dec_mark_()
  unit_size <- 10^(-d)
  scaled <- slope * unit_size
  unit_label <- if (d == 0L) "/unit" else paste0(
    "/", format(unit_size, scientific = FALSE))
  paste0(
    ifelse(scaled >= 0, "+", "-"),
    formatC(abs(scaled), format = "f", digits = 2,
            big.mark = bm, decimal.mark = dm),
    unit_label
  )
}

#' Contributions Module Server
#'
#' @param id Module namespace ID.
#' @param model_module Reactive list from [modelingServer()].
#' @param data_module Reactive list from [dataImportServer()].
#' @export
contributionsServer <- function(id, model_module, data_module) {
  shiny::moduleServer(id, function(input, output, session) {

    # Build per-variable contributions reactive
    contrib_data <- shiny::reactive({
      shiny::req(model_module$fitted())
      model_module$fit_count()

      model  <- model_module$model()
      lambda <- model_module$lambda()
      gamma  <- model_module$gamma()
      x_mat  <- model_module$x_matrix()
      preds  <- data_module$predictors()
      df     <- data_module$data()

      # Extract coefficients (excluding intercept)
      coef_args <- list(model, s = lambda)
      if (!is.null(gamma)) coef_args$gamma <- gamma
      coef_sparse <- do.call(stats::coef, coef_args)
      coef_vec <- as.numeric(coef_sparse)[-1L]
      coef_names <- rownames(coef_sparse)[-1L]
      names(coef_vec) <- coef_names
      intercept <- as.numeric(coef_sparse)[1L]

      # Map model matrix columns to original predictor names
      col_to_pred <- character(length(coef_names))
      for (cn in coef_names) {
        # Interaction terms (contain ":") are their own group
        if (grepl(":", cn, fixed = TRUE)) {
          col_to_pred[which(coef_names == cn)] <- cn
          next
        }
        for (p in preds) {
          if (cn == p || startsWith(cn, paste0(p))) {
            suffix <- sub(paste0("^", p), "", cn)
            if (nzchar(suffix)) {
              longer_match <- FALSE
              for (op in setdiff(preds, p)) {
                if (startsWith(cn, paste0(op)) && nchar(op) > nchar(p)) {
                  longer_match <- TRUE
                  break
                }
              }
              if (longer_match) next
            }
            col_to_pred[which(coef_names == cn)] <- p
            break
          }
        }
      }

      # Compute contribution per predictor: sum of beta_k * x_k for all
      # model matrix columns belonging to that predictor
      unique_preds <- unique(col_to_pred)
      unique_preds <- unique_preds[nzchar(unique_preds)]

      contribs <- list()
      for (pred_name in unique_preds) {
        idx <- which(col_to_pred == pred_name)
        if (length(idx) == 0) next
        contrib_vec <- rep(0, nrow(x_mat))
        for (j in idx) {
          contrib_vec <- contrib_vec + coef_vec[j] * x_mat[, j]
        }
        contribs[[pred_name]] <- contrib_vec
      }

      # For the x-axis, use model matrix column directly for numeric predictors
      # Also store the coefficient (slope) for single-column numeric predictors
      x_values <- list()
      slopes <- list()
      for (pred_name in unique_preds) {
        idx <- which(col_to_pred == pred_name)
        if (length(idx) == 1L) {
          # Single column = numeric predictor; use x_mat values directly
          x_values[[pred_name]] <- x_mat[, idx]
          slopes[[pred_name]] <- coef_vec[idx]
        } else if (pred_name %in% names(df) && is.numeric(df[[pred_name]])) {
          # Multiple columns but original is numeric (e.g., interaction components)
          # Skip — will use histogram fallback
        }
      }

      list(
        contribs    = contribs,
        x_values    = x_values,
        slopes      = slopes,
        intercept   = intercept,
        preds       = unique_preds,
        col_to_pred = col_to_pred
      )
    })

    # Update variable selector when model changes
    shiny::observeEvent(contrib_data(), {
      cd <- contrib_data()
      shiny::updateSelectInput(session, "contrib_var",
                               choices = cd$preds,
                               selected = cd$preds[1L])
    })

    output$contrib_plot <- shiny::renderPlot({
      shiny::req(contrib_data(), input$contrib_var)

      cd <- contrib_data()
      var_name <- input$contrib_var
      shiny::req(var_name %in% names(cd$contribs))

      contrib_vec <- cd$contribs[[var_name]]
      font_fam <- glmnet_font_family_()

      # Use model matrix x-values for numeric predictors (perfectly aligned)
      if (var_name %in% names(cd$x_values)) {
        x_vals <- cd$x_values[[var_name]]
        plot_df <- data.frame(x = x_vals, contribution = contrib_vec)
        line_df <- plot_df[order(plot_df$x), ]

        # Slope label at midpoint of line
        slope_val <- cd$slopes[[var_name]]
        x_mid <- (min(x_vals) + max(x_vals)) / 2
        y_mid <- (min(contrib_vec) + max(contrib_vec)) / 2
        slope_lbl <- contrib_slope_label_(slope_val, x_vals)
        label_df <- data.frame(x = x_mid, y = y_mid, label = slope_lbl)

        ggplot2::ggplot(plot_df,
                        ggplot2::aes(x = .data$x,
                                     y = .data$contribution)) +
          ggplot2::geom_point(alpha = 0.5, color = "#5e81ac") +
          ggplot2::geom_line(
            data = line_df,
            color = "#bf616a", linewidth = 1, alpha = 0.8
          ) +
          ggplot2::geom_label(
            data = label_df,
            ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
            inherit.aes = FALSE,
            size = 4, fill = "white", alpha = 0.85,
            label.padding = ggplot2::unit(0.2, "lines"),
            label.size = 0.3, color = "#333333",
            family = font_fam
          ) +
          ggplot2::labs(
            title = paste("Contribution:", var_name),
            subtitle = paste0("Intercept (basis): ",
                              round(cd$intercept, 2)),
            x = var_name,
            y = "Contribution to Prediction"
          ) +
          ggplot2::scale_x_continuous(labels = glmnet_axis_labels_,
                                      n.breaks = 20) +
          ggplot2::scale_y_continuous(labels = glmnet_axis_labels_,
                                      n.breaks = 15) +
          glmnet_diag_theme_(font_fam) +
          ggplot2::theme(
            plot.subtitle = ggplot2::element_text(size = 13)
          )
      } else {
        # Factor/interaction: bar chart of mean contribution by group
        # Show contribution distribution (histogram-like)
        plot_df <- data.frame(
          index = seq_along(contrib_vec),
          contribution = contrib_vec
        )

        ggplot2::ggplot(plot_df,
                        ggplot2::aes(x = .data$contribution)) +
          ggplot2::geom_histogram(bins = 30, fill = "#5e81ac",
                                   color = "white", alpha = 0.8) +
          ggplot2::labs(
            title = paste("Contribution Distribution:", var_name),
            subtitle = paste0("Intercept (basis): ",
                              round(cd$intercept, 2)),
            x = "Contribution to Prediction",
            y = "Frequency"
          ) +
          ggplot2::scale_x_continuous(labels = glmnet_axis_labels_,
                                      n.breaks = 20) +
          glmnet_diag_theme_(font_fam) +
          ggplot2::theme(
            plot.subtitle = ggplot2::element_text(size = 13)
          )
      }
    })
  })
}
