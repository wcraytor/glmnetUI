#' Prepare Report Assets
#'
#' Pre-generates all plots and data for the glmnetUI model report.
#' Returns the path to a directory containing all assets. This
#' directory can be passed to [render_report()] to avoid re-computing
#' anything during rendering.
#'
#' @param model A glmnet or cv.glmnet model object.
#' @param lambda Selected lambda value.
#' @param gamma Selected gamma value (for relaxed models), or NULL.
#' @param x_mat The model matrix used for fitting.
#' @param y_vec The response vector.
#' @param coef_df Data frame of coefficients from [build_coef_table()].
#' @param predictors Character vector of predictor names.
#' @param response Character name of the response variable.
#' @param data The full data frame (for correlation and contributions).
#' @param col_types Named character vector of column types.
#' @param purpose Character: "general", "appraisal", or "market".
#' @param alpha The alpha value used.
#' @param family The family used.
#' @param standardize Logical: was standardize used.
#' @param relaxed Logical: was relaxed lasso used.
#' @param lambda_method Character: "cv" or "manual".
#' @param data_file_name Character: original data file name.
#' @param assets_dir Character path to write assets. If NULL, a
#'   temporary directory is created.
#' @return The path to the assets directory (invisibly).
#' @export
prepare_report_assets <- function(model, lambda, gamma, x_mat, y_vec,
                                  coef_df, predictors, response, data,
                                  col_types = NULL, purpose = "general",
                                  alpha = 1, family = "gaussian",
                                  standardize = TRUE, relaxed = FALSE,
                                  lambda_method = "cv",
                                  data_file_name = "",
                                  assets_dir = NULL) {
  if (is.null(assets_dir)) {
    assets_dir <- tempfile("glmnetUI_assets_")
  }
  dir.create(assets_dir, recursive = TRUE, showWarnings = FALSE)
  assets_dir <- normalizePath(assets_dir)
  plots_dir <- file.path(assets_dir, "plots")
  dir.create(plots_dir, showWarnings = FALSE)

  # --- Font setup ---
  font_fam <- glmnet_font_family_()
  if (requireNamespace("sysfonts", quietly = TRUE) &&
      requireNamespace("showtext", quietly = TRUE)) {
    if (!"Roboto Condensed" %in% sysfonts::font_families()) {
      tryCatch(
        sysfonts::font_add_google("Roboto Condensed", "Roboto Condensed"),
        error = function(e) NULL)
    }
    showtext::showtext_auto()
    ggplot2::theme_set(ggplot2::theme_minimal(base_family = font_fam))
  } else {
    ggplot2::theme_set(ggplot2::theme_minimal(base_family = "sans"))
  }

  # --- Predictions and residuals ---
  pred_args <- list(model, newx = x_mat, s = lambda, type = "response")
  if (!is.null(gamma)) pred_args$gamma <- gamma
  preds <- as.numeric(do.call(stats::predict, pred_args))
  resids <- y_vec - preds

  # --- Summary statistics ---
  ss_res <- sum(resids^2)
  ss_tot <- sum((y_vec - mean(y_vec))^2)
  n <- length(y_vec)
  p <- sum(coef_df$Coefficient != 0) - 1L  # exclude intercept
  r_sq <- 1 - ss_res / ss_tot
  adj_r_sq <- 1 - (1 - r_sq) * (n - 1) / (n - p - 1)
  rmse <- sqrt(mean(resids^2))
  mae <- mean(abs(resids))

  # Generalized R² (deviance ratio from cv.glmnet)
  gen_r_sq <- r_sq
  if (inherits(model, "cv.glmnet") || inherits(model, "cv.relaxed")) {
    fit_obj <- if (inherits(model, "cv.glmnet")) model$glmnet.fit else model
    dev_ratio <- fit_obj$dev.ratio
    idx <- which.min(abs(fit_obj$lambda - lambda))
    if (length(idx) == 1L) gen_r_sq <- dev_ratio[idx]
  }

  # CV R²
  cv_r_sq <- NA_real_
  if (inherits(model, "cv.glmnet") || inherits(model, "cv.relaxed")) {
    idx <- which.min(abs(model$lambda - lambda))
    if (length(idx) == 1L) {
      cv_mse <- model$cvm[idx]
      cv_r_sq <- 1 - cv_mse / stats::var(y_vec)
    }
  }

  # --- Equation ---
  eq <- format_glmnet_equation_(model, lambda, gamma, response)

  # --- Variable importance ---
  coef_args <- list(model, s = lambda)
  if (!is.null(gamma)) coef_args$gamma <- gamma
  coef_sparse <- do.call(stats::coef, coef_args)
  coef_vec <- as.numeric(coef_sparse)[-1L]
  coef_names <- rownames(coef_sparse)[-1L]
  names(coef_vec) <- coef_names

  imp_df <- compute_importance_(coef_vec, x_mat, predictors, col_types)

  # --- ANOVA ---
  anova_df <- compute_anova_(coef_vec, x_mat, y_vec, predictors, col_types)

  # --- Contribution data for plots ---
  contrib_info <- compute_contrib_for_report_(
    coef_vec, x_mat, predictors, data, col_types)

  # --- Model print output ---
  model_print <- utils::capture.output(print(model))

  # --- Save all pre-computed data ---
  saveRDS(list(
    data_file_name = data_file_name,
    n_obs          = n,
    response       = response,
    predictors     = predictors,
    purpose        = purpose,
    alpha          = alpha,
    lambda         = lambda,
    lambda_method  = lambda_method,
    family         = family,
    standardize    = standardize,
    relaxed        = relaxed,
    n_nonzero      = sum(coef_df$Coefficient != 0),
    n_predictors   = p,
    r_squared      = r_sq,
    adj_r_squared  = adj_r_sq,
    gen_r_squared  = gen_r_sq,
    cv_r_squared   = cv_r_sq,
    rmse           = rmse,
    mae            = mae,
    equation_latex = eq$latex,
    coef_df        = coef_df,
    importance_df  = imp_df,
    anova_df       = anova_df,
    contrib_names  = names(contrib_info$contribs),
    model_print    = model_print
  ), file.path(assets_dir, "report_data.rds"))

  # --- Generate plots ---
  save_ggplot_ <- function(name, p, width = 8, height = 5) {
    for (ext in c("png", "pdf")) {
      path <- file.path(plots_dir, paste0(name, ".", ext))
      tryCatch(
        ggplot2::ggsave(path, plot = p, width = width, height = height,
                        dpi = 150),
        error = function(e) {
          grDevices::png(path, width = width, height = height,
                         units = "in", res = 150)
          graphics::plot.new()
          graphics::text(0.5, 0.5, paste("Error:", e$message), cex = 0.8)
          grDevices::dev.off()
        }
      )
    }
  }

  # Variable importance plot
  if (nrow(imp_df) > 0) {
    p_imp <- ggplot2::ggplot(
      imp_df,
      ggplot2::aes(x = stats::reorder(.data$Variable, .data$Importance),
                   y = .data$Importance)) +
      ggplot2::geom_col(fill = "#5e81ac") +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "Importance (|coef| \u00d7 sd(x))",
                    title = "Variable Importance") +
      ggplot2::theme_minimal(base_family = font_fam)
    save_ggplot_("importance", p_imp)
  }

  # Correlation matrix
  numeric_preds <- predictors[vapply(predictors, function(p) {
    p %in% names(data) && is.numeric(data[[p]])
  }, logical(1))]
  if (length(numeric_preds) >= 2) {
    cor_mat <- stats::cor(data[, numeric_preds, drop = FALSE],
                          use = "pairwise.complete.obs")
    cor_long <- as.data.frame(as.table(cor_mat))
    names(cor_long) <- c("Var1", "Var2", "value")
    p_cor <- ggplot2::ggplot(cor_long,
               ggplot2::aes(x = .data$Var1, y = .data$Var2,
                            fill = .data$value)) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$value)),
                         size = 3) +
      ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white",
                                     high = "#B2182B", midpoint = 0) +
      ggplot2::labs(x = NULL, y = NULL, title = "Correlation Matrix") +
      ggplot2::theme_minimal(base_family = font_fam) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    save_ggplot_("correlation", p_cor, width = 10, height = 8)
  }

  # Contribution plots
  for (vname in names(contrib_info$contribs)) {
    safe_name <- gsub("[^a-zA-Z0-9_]", "_", vname)
    cv <- contrib_info$contribs[[vname]]
    xv <- contrib_info$x_values[[vname]]
    if (!is.null(xv) && length(xv) == length(cv)) {
      plot_df <- data.frame(x = xv, contribution = cv)
      plot_df <- plot_df[order(plot_df$x), ]
      p_c <- ggplot2::ggplot(plot_df,
               ggplot2::aes(x = .data$x, y = .data$contribution)) +
        ggplot2::geom_line(color = "#5e81ac", linewidth = 1) +
        ggplot2::geom_point(alpha = 0.3, size = 0.5) +
        ggplot2::labs(x = vname, y = "Contribution",
                      title = paste("Contribution:", vname)) +
        ggplot2::theme_minimal(base_family = font_fam)
      save_ggplot_(paste0("contrib_", safe_name), p_c)
    } else {
      # Histogram for factors/interactions
      plot_df <- data.frame(contribution = cv)
      p_c <- ggplot2::ggplot(plot_df,
               ggplot2::aes(x = .data$contribution)) +
        ggplot2::geom_histogram(fill = "#5e81ac", color = "white", bins = 30) +
        ggplot2::labs(x = "Contribution", y = "Count",
                      title = paste("Contribution Distribution:", vname)) +
        ggplot2::theme_minimal(base_family = font_fam)
      save_ggplot_(paste0("contrib_", safe_name), p_c)
    }
  }

  # Diagnostic plots
  fit_obj <- if (inherits(model, "cv.glmnet") ||
                   inherits(model, "cv.relaxed")) {
    model$glmnet.fit
  } else if (inherits(model, "relaxed")) {
    model$relaxed
  } else {
    model
  }

  # Coefficient path
  beta_mat <- as.matrix(fit_obj$beta)
  log_lambda <- log(fit_obj$lambda)
  df_long <- data.frame(
    log_lambda = rep(log_lambda, each = nrow(beta_mat)),
    coefficient = as.numeric(beta_mat),
    variable = rep(rownames(beta_mat), times = ncol(beta_mat)),
    stringsAsFactors = FALSE
  )
  p_coef <- ggplot2::ggplot(df_long,
              ggplot2::aes(x = .data$log_lambda,
                           y = .data$coefficient,
                           color = .data$variable)) +
    ggplot2::geom_line(show.legend = FALSE) +
    ggplot2::labs(x = "Log(Lambda)", y = "Coefficient",
                  title = "Coefficient Path") +
    ggplot2::theme_minimal(base_family = font_fam)
  save_ggplot_("coef_path", p_coef)

  # CV error plot
  if (inherits(model, "cv.glmnet") || inherits(model, "cv.relaxed")) {
    df_cv <- data.frame(
      log_lambda = log(model$lambda),
      cvm = model$cvm, cvup = model$cvup, cvlo = model$cvlo,
      stringsAsFactors = FALSE)
    p_cv <- ggplot2::ggplot(df_cv, ggplot2::aes(x = .data$log_lambda,
                                                  y = .data$cvm)) +
      ggplot2::geom_point(color = "#bf616a", size = 1.5) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$cvlo,
                                           ymax = .data$cvup), alpha = 0.4) +
      ggplot2::geom_vline(xintercept = log(model$lambda.min),
                           linetype = "dashed", color = "#5e81ac") +
      ggplot2::geom_vline(xintercept = log(model$lambda.1se),
                           linetype = "dashed", color = "#a3be8c") +
      ggplot2::labs(x = "Log(Lambda)", y = "CV Error",
                    title = "Cross-Validation Error") +
      ggplot2::theme_minimal(base_family = font_fam)
    save_ggplot_("cv_error", p_cv)
  }

  # Actual vs Predicted
  df_avp <- data.frame(actual = y_vec, predicted = preds)
  p_avp <- ggplot2::ggplot(df_avp,
             ggplot2::aes(x = .data$actual, y = .data$predicted)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                          color = "#bf616a", linetype = "dashed") +
    ggplot2::labs(x = "Actual", y = "Predicted",
                  title = "Actual vs Predicted") +
    ggplot2::theme_minimal(base_family = font_fam)
  save_ggplot_("actual_vs_predicted", p_avp)

  # Residuals vs Fitted
  df_resid <- data.frame(fitted = preds, residuals = resids)
  p_resid <- ggplot2::ggplot(df_resid,
               ggplot2::aes(x = .data$fitted, y = .data$residuals)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, color = "#bf616a",
                         linetype = "dashed") +
    ggplot2::labs(x = "Fitted Values", y = "Residuals",
                  title = "Residuals vs Fitted") +
    ggplot2::theme_minimal(base_family = font_fam)
  save_ggplot_("residuals_vs_fitted", p_resid)

  # Q-Q plot
  qq_data <- stats::qqnorm(resids, plot.it = FALSE)
  df_qq <- data.frame(theoretical = qq_data$x, sample = qq_data$y)
  p_qq <- ggplot2::ggplot(df_qq,
            ggplot2::aes(x = .data$theoretical, y = .data$sample)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_abline(slope = stats::sd(resids),
                          intercept = mean(resids),
                          color = "#bf616a", linetype = "dashed") +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
                  title = "Normal Q-Q Plot") +
    ggplot2::theme_minimal(base_family = font_fam)
  save_ggplot_("qq", p_qq)

  invisible(assets_dir)
}


#' Render glmnetUI Report
#'
#' Renders the Quarto template into HTML, PDF, or Word output using
#' pre-generated assets.
#'
#' @param output_format Character: "html", "pdf", or "docx".
#' @param output_file Character path for the output file.
#' @param assets_dir Character path to pre-generated assets from
#'   [prepare_report_assets()].
#' @return The output file path (invisibly).
#' @export
render_report <- function(output_format = "html", output_file,
                          assets_dir) {
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("Package 'quarto' is required for report rendering. ",
         "Install with: install.packages('quarto')", call. = FALSE)
  }

  template <- system.file("quarto", "glmnet_report.qmd",
                           package = "glmnetUI")
  if (!nzchar(template)) {
    stop("Report template not found in package.", call. = FALSE)
  }

  # Create temp rendering directory
  render_dir <- tempfile("glmnetUI_render_")
  dir.create(render_dir, recursive = TRUE)
  on.exit(unlink(render_dir, recursive = TRUE), add = TRUE)

  # Copy template and assets
  file.copy(template, file.path(render_dir, "glmnet_report.qmd"))
  file.copy(file.path(assets_dir, "report_data.rds"),
            file.path(render_dir, "report_data.rds"))
  plots_src <- file.path(assets_dir, "plots")
  if (dir.exists(plots_src)) {
    plots_dst <- file.path(render_dir, "plots")
    dir.create(plots_dst, showWarnings = FALSE)
    file.copy(list.files(plots_src, full.names = TRUE), plots_dst)
  }

  # Map format
  qmd_format <- switch(output_format,
    html = "html",
    pdf  = "pdf",
    docx = "docx",
    "html"
  )

  # Render
  old_env <- Sys.getenv("QUARTO_R")
  Sys.setenv(QUARTO_R = file.path(R.home("bin"), "R"))
  on.exit(Sys.setenv(QUARTO_R = old_env), add = TRUE)

  quarto::quarto_render(
    input = file.path(render_dir, "glmnet_report.qmd"),
    output_format = qmd_format,
    execute_params = list(
      data_file = file.path(render_dir, "report_data.rds")
    ),
    quiet = TRUE
  )

  # Find output file
  out_ext <- switch(output_format, html = ".html", pdf = ".pdf",
                    docx = ".docx", ".html")
  rendered <- file.path(render_dir, paste0("glmnet_report", out_ext))
  if (file.exists(rendered)) {
    file.copy(rendered, output_file, overwrite = TRUE)
  }

  invisible(output_file)
}


# --- Internal helpers for report asset generation ---

#' Compute variable importance for report
#' @noRd
compute_importance_ <- function(coef_vec, x_mat, predictors, col_types) {
  imp <- data.frame(
    Variable = character(0),
    Importance = numeric(0),
    Relative = numeric(0),
    stringsAsFactors = FALSE
  )

  col_names <- colnames(x_mat)
  for (pred in predictors) {
    # Find columns belonging to this predictor
    idx <- which(col_names == pred |
                   startsWith(col_names, paste0(pred)))
    # Filter to avoid matching longer predictor names
    idx <- Filter(function(j) {
      cn <- col_names[j]
      if (cn == pred) return(TRUE)
      suffix <- sub(paste0("^", pred), "", cn)
      !any(startsWith(cn, paste0(setdiff(predictors, pred))))
    }, idx)

    if (length(idx) == 0) next
    # Importance = sum of |coef| * sd(x) for each column
    total_imp <- 0
    for (j in idx) {
      b <- coef_vec[col_names[j]]
      if (!is.na(b) && b != 0) {
        total_imp <- total_imp + abs(b) * stats::sd(x_mat[, j])
      }
    }
    if (total_imp > 0) {
      imp <- rbind(imp, data.frame(
        Variable = pred, Importance = total_imp, Relative = 0,
        stringsAsFactors = FALSE
      ))
    }
  }

  if (nrow(imp) > 0) {
    max_imp <- max(imp$Importance)
    if (max_imp > 0) imp$Relative <- round(imp$Importance / max_imp * 100, 1)
    imp <- imp[order(-imp$Importance), ]
    rownames(imp) <- NULL
  }
  imp
}


#' Compute ANOVA decomposition for report
#' @noRd
compute_anova_ <- function(coef_vec, x_mat, y_vec, predictors, col_types) {
  col_names <- colnames(x_mat)
  n <- length(y_vec)
  intercept <- 0  # glmnet handles intercept separately

  rows <- list()
  for (pred in predictors) {
    idx <- which(col_names == pred |
                   startsWith(col_names, paste0(pred)))
    idx <- Filter(function(j) {
      cn <- col_names[j]
      cn == pred || !any(startsWith(cn, paste0(setdiff(predictors, pred))))
    }, idx)

    if (length(idx) == 0) next
    contrib <- rep(0, n)
    for (j in idx) {
      b <- coef_vec[col_names[j]]
      if (!is.na(b) && b != 0) {
        contrib <- contrib + b * x_mat[, j]
      }
    }
    ss <- sum(contrib^2)
    rows[[pred]] <- data.frame(
      Variable = pred,
      `Sum of Squares` = round(ss, 2),
      `% of Total` = 0,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  if (length(rows) == 0) {
    return(data.frame(Variable = character(0),
                      `Sum of Squares` = numeric(0),
                      `% of Total` = numeric(0),
                      stringsAsFactors = FALSE,
                      check.names = FALSE))
  }

  anova_df <- do.call(rbind, rows)
  total_ss <- sum(anova_df$`Sum of Squares`)
  if (total_ss > 0) {
    anova_df$`% of Total` <- round(anova_df$`Sum of Squares` / total_ss * 100, 1)
  }
  rownames(anova_df) <- NULL
  anova_df
}


#' Compute contribution data for report plots
#' @noRd
compute_contrib_for_report_ <- function(coef_vec, x_mat, predictors,
                                         data, col_types) {
  col_names <- colnames(x_mat)
  contribs <- list()
  x_values <- list()

  for (pred in predictors) {
    idx <- which(col_names == pred |
                   startsWith(col_names, paste0(pred)))
    idx <- Filter(function(j) {
      cn <- col_names[j]
      cn == pred || !any(startsWith(cn, paste0(setdiff(predictors, pred))))
    }, idx)

    if (length(idx) == 0) next
    contrib_vec <- rep(0, nrow(x_mat))
    has_nonzero <- FALSE
    for (j in idx) {
      b <- coef_vec[col_names[j]]
      if (!is.na(b) && b != 0) {
        contrib_vec <- contrib_vec + b * x_mat[, j]
        has_nonzero <- TRUE
      }
    }
    if (!has_nonzero) next

    contribs[[pred]] <- contrib_vec

    # X-axis values for scatter plot
    if (length(idx) == 1L && pred %in% names(data) &&
        is.numeric(data[[pred]])) {
      xv <- data[[pred]]
      if (length(xv) == nrow(x_mat)) {
        x_values[[pred]] <- xv
      }
    } else if (pred %in% names(data) && is.numeric(data[[pred]])) {
      xv <- data[[pred]]
      if (length(xv) == nrow(x_mat)) {
        x_values[[pred]] <- xv
      }
    }
  }

  list(contribs = contribs, x_values = x_values)
}
