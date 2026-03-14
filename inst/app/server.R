function(input, output, session) {

  # --- Nord theme switching ---
  shiny::observe({
    mode <- input$dark_mode
    shiny::req(mode)
    tryCatch(
      session$setCurrentTheme(
        if (mode == "dark") nord_dark else nord_light
      ),
      error = function(e) {
        message("Theme switch error (non-fatal): ", conditionMessage(e))
      }
    )
  })

  # --- Locale handling ---
  # Restore saved locale defaults from localStorage
  shiny::observeEvent(input$glmnet_locale_defaults, {
    ld <- input$glmnet_locale_defaults
    if (!is.null(ld$locale_country))
      shiny::updateSelectInput(session, "locale_country", selected = ld$locale_country)
    if (!is.null(ld$locale_paper))
      shiny::updateSelectInput(session, "locale_paper", selected = ld$locale_paper)
    if (!is.null(ld$locale_import))
      shiny::updateSelectInput(session, "data-locale_import", selected = ld$locale_import)
  })

  # When Settings country changes, sync import locale and paper
  shiny::observeEvent(input$locale_country, {
    country <- input$locale_country %||% "us"
    presets <- glmnetUI:::locale_country_presets_()
    preset <- presets[[country]] %||% presets[["us"]]
    shiny::updateSelectInput(session, "locale_paper", selected = preset$paper)
    shiny::updateSelectInput(session, "data-locale_import", selected = country)
    glmnetUI:::set_locale_(country)
  })

  # Sync import locale with settings locale
  shiny::observe({
    import_country <- input[["data-locale_import"]] %||%
      input$locale_country %||% "us"
    settings_country <- input$locale_country %||% "us"
    paper <- input$locale_paper %||% "letter"
    presets <- glmnetUI:::locale_country_presets_()
    import_preset <- presets[[import_country]] %||% presets[["us"]]
    settings_preset <- presets[[settings_country]] %||% presets[["us"]]
    glmnetUI:::set_locale_(settings_country,
                           csv_sep = import_preset$csv_sep,
                           csv_dec = import_preset$csv_dec,
                           big_mark = settings_preset$big_mark,
                           dec_mark = settings_preset$dec_mark,
                           date_fmt = import_preset$date_fmt,
                           paper = paper)
  })

  # Save locale as default
  shiny::observeEvent(input$locale_save_default, {
    session$sendCustomMessage("save_locale_defaults", list(
      locale_country = input$locale_country,
      locale_paper   = input$locale_paper,
      locale_import  = input[["data-locale_import"]]
    ))
    shiny::showNotification("Locale saved as default.",
                            type = "message", duration = 4)
  })

  # Wrap global inputs as reactives for module consumption
  purpose <- shiny::reactive(input$purpose)
  effective_date <- shiny::reactive(input$effective_date)

  data_out <- dataImportServer("data", purpose)
  model_out <- modelingServer("model", data_out, purpose, effective_date)
  coef_out <- coefficientsServer("coefs", model_out, data_out)
  equationServer("eq", model_out, data_out)
  summaryServer("summ", model_out, data_out, purpose)
  correlationServer("corr", data_out)
  importanceServer("imp", model_out, data_out)
  contributionsServer("contrib", model_out, data_out)
  anovaServer("anova", model_out, data_out)
  diagnosticsServer("diag", model_out)
  reportServer("report", model_out, coef_out, data_out)

  # --- Model fitted flag for conditionalPanel ---
  output$model_fitted <- shiny::reactive(isTRUE(model_out$fitted()))
  shiny::outputOptions(output, "model_fitted", suspendWhenHidden = FALSE)

  # --- RCA percentage data (stored after RCA export) ---
  rv_rca <- shiny::reactiveValues(pct_data = NULL, rca_df = NULL)

  output$rca_computed <- shiny::reactive(!is.null(rv_rca$pct_data))
  shiny::outputOptions(output, "rca_computed", suspendWhenHidden = FALSE)

  # --- Dynamic step headings ---
  output$download_heading <- shiny::renderUI({
    label <- if (identical(input$purpose, "general")) {
      "6. Download Estimated Target Variable(s) & Residuals"
    } else {
      "6. Download Estimated Sale Prices & Residuals"
    }
    shiny::h4(label, style = "display:inline;")
  })

  output$report_heading <- shiny::renderUI({
    n <- if (identical(input$purpose, "appraisal")) "9" else "7"
    shiny::h4(paste0(n, ". Download Report"), style = "display:inline;")
  })

  # ── Shared helper: build full model matrix & coefficients ──────────
  # Returns list(x_full, complete, coefs, intercept, term_labels, assign_attr)
  # or NULL on failure.
  build_export_matrix_ <- function() {
    export_df <- data_out$data()
    model     <- model_out$model()
    lambda    <- model_out$lambda()
    gamma     <- model_out$gamma()
    preds_col <- data_out$predictors()
    col_types <- data_out$col_types()
    x_train   <- model_out$x_matrix()
    train_colnames <- colnames(x_train)

    # --- Extract coefficients at selected lambda ---
    coef_args <- list(model, s = lambda)
    if (!is.null(gamma)) coef_args$gamma <- gamma
    coef_sparse <- do.call(stats::coef, coef_args)
    coef_vec    <- as.numeric(coef_sparse)
    coef_names  <- rownames(coef_sparse)
    intercept   <- coef_vec[1L]
    beta        <- coef_vec[-1L]
    names(beta) <- coef_names[-1L]

    # --- Prepare predictor data frame with aligned factor levels ---
    x_df <- export_df[, preds_col, drop = FALSE]
    for (p in preds_col) {
      if (!is.null(col_types[p])) {
        if (col_types[p] %in% c("factor", "character")) {
          prefix <- paste0(p)
          # Extract factor levels from training column names
          # Only match columns that start with this predictor and aren't
          # a longer predictor name (e.g., distinguish "area" from "area_id")
          matched <- grep(paste0("^", prefix), train_colnames, value = TRUE)
          # Filter out matches that belong to a longer predictor name
          other_preds <- setdiff(preds_col, p)
          for (op in other_preds) {
            matched <- matched[!grepl(paste0("^", op), matched)]
          }
          train_lvls <- sub(paste0("^", prefix), "", matched)
          # Include all levels (training + unseen) so unseen levels
          # get zero coefficients rather than NA
          all_lvls <- unique(c(train_lvls, as.character(x_df[[p]])))
          all_lvls <- all_lvls[!is.na(all_lvls) & nzchar(all_lvls)]
          x_df[[p]] <- factor(x_df[[p]], levels = all_lvls)
        } else if (col_types[p] == "numeric") {
          x_df[[p]] <- as.numeric(x_df[[p]])
        } else if (col_types[p] == "integer") {
          x_df[[p]] <- as.integer(x_df[[p]])
        }
      }
    }

    # --- Build formula (main effects; interactions detected from training) ---
    int_cols <- train_colnames[grepl(":", train_colnames)]
    # Extract unique interaction term labels (e.g., "x1:x2")
    int_terms <- unique(vapply(strsplit(int_cols, ":"), function(parts) {
      # Map back to original predictor names
      # Each part is either a predictor name or a factor dummy (predictor + level)
      map_part <- function(part) {
        # Find the predictor whose name is a prefix of this part
        for (p in preds_col) {
          if (grepl(paste0("^", p), part)) return(p)
        }
        part
      }
      paste(unique(vapply(parts, map_part, character(1))), collapse = ":")
    }, character(1)))
    formula_parts <- preds_col
    if (length(int_terms) > 0) {
      formula_parts <- c(formula_parts, int_terms)
    }
    formula_str <- paste("~", paste(formula_parts, collapse = " + "), "- 1")

    # --- Build model matrix for complete rows ---
    complete <- stats::complete.cases(x_df)
    x_full <- NULL
    assign_attr <- NULL

    if (any(complete)) {
      x_full <- stats::model.matrix(stats::as.formula(formula_str),
                                    data = x_df[complete, , drop = FALSE])
      assign_attr <- attr(x_full, "assign")

      # Align columns with training matrix
      missing_cols <- setdiff(train_colnames, colnames(x_full))
      if (length(missing_cols) > 0) {
        zero_mat <- matrix(0, nrow = nrow(x_full),
                           ncol = length(missing_cols),
                           dimnames = list(NULL, missing_cols))
        x_full <- cbind(x_full, zero_mat)
      }
      # Reorder and keep only training columns
      x_full <- x_full[, train_colnames, drop = FALSE]

      # Rebuild assign for reordered columns
      assign_attr <- match(train_colnames, colnames(
        stats::model.matrix(stats::as.formula(formula_str),
                            data = x_df[complete, , drop = FALSE])))
      # Safer: use the term labels from formula
    }

    # Term labels from the formula
    tt <- stats::terms(stats::as.formula(formula_str))
    term_labels <- attr(tt, "term.labels")

    list(x_full = x_full, complete = complete, beta = beta,
         intercept = intercept, term_labels = term_labels,
         formula_str = formula_str, x_df = x_df,
         train_colnames = train_colnames)
  }

  # ── Compute per-term contributions ───────────────────────────────
  # Returns a named list: term_label -> numeric vector (length = nrow(x_full))
  compute_contributions_ <- function(x_full, beta, train_colnames,
                                     term_labels, formula_str, x_df_complete) {
    # Get assign attribute from a fresh model.matrix (before column reorder)
    x_ref <- stats::model.matrix(stats::as.formula(formula_str),
                                 data = x_df_complete)
    ref_assign <- attr(x_ref, "assign")
    ref_colnames <- colnames(x_ref)

    # Map each training column to its term index
    col_to_term <- integer(length(train_colnames))
    for (j in seq_along(train_colnames)) {
      idx <- match(train_colnames[j], ref_colnames)
      if (!is.na(idx)) {
        col_to_term[j] <- ref_assign[idx]
      } else {
        col_to_term[j] <- NA_integer_  # missing column (zero-padded)
      }
    }

    # Compute contribution per term
    contribs <- list()
    for (ti in seq_along(term_labels)) {
      cols_idx <- which(col_to_term == ti)
      if (length(cols_idx) == 0) {
        contribs[[term_labels[ti]]] <- rep(0, nrow(x_full))
      } else {
        # contribution = sum over columns of beta[j] * x[,j]
        term_contrib <- rep(0, nrow(x_full))
        for (j in cols_idx) {
          b <- beta[train_colnames[j]]
          if (!is.na(b) && b != 0) {
            term_contrib <- term_contrib + b * x_full[, j]
          }
        }
        contribs[[term_labels[ti]]] <- term_contrib
      }
    }
    contribs
  }

  # ── Find living_area column from specials ────────────────────────
  find_living_area_ <- function() {
    specials <- data_out$col_specials()
    if (is.null(specials) || length(specials) == 0) return(NULL)
    la_idx <- which(specials == "living_area")
    if (length(la_idx) == 0) return(NULL)
    names(specials)[la_idx[1L]]
  }

  # --- 6. Download Output (Excel) ---
  observeEvent(input$export_data, {
    shiny::req(model_out$fitted(), data_out$data())
    if (!requireNamespace("writexl", quietly = TRUE)) {
      shiny::showNotification(
        "Package 'writexl' required. Install with: install.packages('writexl')",
        type = "error", duration = 10)
      return()
    }

    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    tryCatch({
      export_df <- data_out$data()
      model     <- model_out$model()
      lambda    <- model_out$lambda()
      gamma     <- model_out$gamma()
      response  <- data_out$response()

      em <- build_export_matrix_()
      x_full         <- em$x_full
      complete       <- em$complete
      beta           <- em$beta
      intercept      <- em$intercept
      term_labels    <- em$term_labels
      train_colnames <- em$train_colnames

      n <- nrow(export_df)
      est_col   <- rep(NA_real_, n)
      resid_col <- rep(NA_real_, n)

      if (!is.null(x_full) && any(complete)) {
        pred_args <- list(model, newx = x_full, s = lambda, type = "response")
        if (!is.null(gamma)) pred_args$gamma <- gamma
        pv <- as.numeric(do.call(stats::predict, pred_args))
        est_col[complete] <- pv
        resid_col[complete] <- export_df[[response]][complete] - pv

        # --- Per-term contributions ---
        contribs <- compute_contributions_(
          x_full, beta, train_colnames, term_labels,
          em$formula_str, em$x_df[complete, , drop = FALSE])

        export_df[["basis"]] <- NA_real_
        export_df[["basis"]][complete] <- round(intercept, 1)
        for (tl in names(contribs)) {
          col_name <- paste0(gsub(":", "_x_", tl), "_contribution")
          export_df[[col_name]] <- NA_real_
          export_df[[col_name]][complete] <- round(contribs[[tl]], 1)
        }

        # Verification column
        contrib_total <- intercept + Reduce(`+`, contribs)
        export_df[["calc_residual"]] <- NA_real_
        export_df[["calc_residual"]][complete] <-
          round(export_df[[response]][complete] - contrib_total, 1)
      }

      export_df[[paste0("est_", response)]] <- round(est_col, 1)
      export_df[["residual"]] <- round(resid_col, 1)

      # --- CQA scores ---
      la_col <- find_living_area_()
      comp_rows <- if (identical(input$purpose, "appraisal")) -1L else seq_len(n)
      comp_resid <- resid_col[comp_rows]
      comp_resid <- comp_resid[!is.na(comp_resid)]
      n_comps <- length(comp_resid)
      if (n_comps > 0) {
        cqa <- vapply(resid_col, function(r) {
          if (is.na(r)) return(NA_real_)
          sum(comp_resid < r, na.rm = TRUE) / n_comps * 10
        }, numeric(1))
        export_df[["cqa"]] <- round(cqa, 2)

        if (!is.null(la_col) && la_col %in% names(export_df)) {
          resid_sf <- resid_col / export_df[[la_col]]
          export_df[["residual_sf"]] <- round(resid_sf, 4)
          comp_resid_sf <- resid_sf[comp_rows]
          comp_resid_sf <- comp_resid_sf[!is.na(comp_resid_sf)]
          n_sf <- length(comp_resid_sf)
          if (n_sf > 0) {
            cqa_sf <- vapply(resid_sf, function(r) {
              if (is.na(r)) return(NA_real_)
              sum(comp_resid_sf < r, na.rm = TRUE) / n_sf * 10
            }, numeric(1))
            export_df[["cqa_sf"]] <- round(cqa_sf, 2)
          }
        }
      }

      # In appraisal mode, set subject row (row 1) actual/residual to NA
      if (identical(input$purpose, "appraisal")) {
        resid_col[1L] <- NA_real_
        export_df[["residual"]][1L] <- NA_real_
        if ("cqa" %in% names(export_df)) export_df[["cqa"]][1L] <- NA_real_
        if ("cqa_sf" %in% names(export_df)) export_df[["cqa_sf"]][1L] <- NA_real_
      }

      # Sort by residual_sf descending for appraisal/market
      if (input$purpose %in% c("appraisal", "market") &&
          "residual_sf" %in% names(export_df)) {
        ord <- order(export_df[["residual_sf"]], decreasing = TRUE,
                     na.last = TRUE)
        export_df <- export_df[ord, , drop = FALSE]
      }

      base <- tools::file_path_sans_ext(data_out$file_name() %||% "glmnetui")
      out_path <- file.path(folder, paste0(base, "_output_",
                            format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
      writexl::write_xlsx(export_df, out_path)
      shiny::showNotification(paste0("Output saved to: ", out_path),
                              type = "message", duration = 8)
      session$sendCustomMessage("btn_done", list(id = "export_data"))
    }, error = function(e) {
      shiny::showNotification(paste("Export error:", e$message),
                              type = "error", duration = 10)
    })
  })

  # --- 7. Calculate RCA Adjustments & Download ---
  observeEvent(input$rca_output_btn, {
    shiny::req(model_out$fitted(), data_out$data())

    la_col <- find_living_area_()
    cqa_choices <- c("CQA" = "cqa")
    if (!is.null(la_col)) {
      cqa_choices <- c(cqa_choices, "CQA per SF" = "cqa_sf")
    }

    shiny::showModal(shiny::modalDialog(
      title = "RCA Raw Output \u2014 Subject CQA Score",
      shiny::radioButtons("rca_cqa_type", "Score type:",
                          choices = cqa_choices, inline = TRUE),
      shiny::numericInput("rca_cqa_value", "Subject CQA Score:",
                          value = 5.00, min = 0, max = 9.99, step = 0.01),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("export_rca", "Generate",
                            class = "btn-success")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$export_rca, {
    shiny::removeModal()
    shiny::req(model_out$fitted(), data_out$data())
    if (!requireNamespace("writexl", quietly = TRUE)) {
      shiny::showNotification(
        "Package 'writexl' required. Install with: install.packages('writexl')",
        type = "error", duration = 10)
      return()
    }

    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    tryCatch({
      export_df <- data_out$data()
      model     <- model_out$model()
      lambda    <- model_out$lambda()
      gamma     <- model_out$gamma()
      response  <- data_out$response()
      user_cqa  <- input$rca_cqa_value
      n         <- nrow(export_df)

      if (n < 2L) {
        shiny::showNotification("Need at least 2 rows (subject + 1 comp).",
                                type = "error")
        return()
      }

      em <- build_export_matrix_()
      x_full         <- em$x_full
      complete       <- em$complete
      beta           <- em$beta
      intercept      <- em$intercept
      term_labels    <- em$term_labels
      train_colnames <- em$train_colnames
      shiny::req(x_full)

      # --- Predictions for all rows ---
      pred_args <- list(model, newx = x_full, s = lambda, type = "response")
      if (!is.null(gamma)) pred_args$gamma <- gamma
      pv_complete <- as.numeric(do.call(stats::predict, pred_args))

      predicted   <- rep(NA_real_, n)
      actual      <- export_df[[response]]
      predicted[complete] <- pv_complete
      residuals_val <- actual - predicted

      # Subject row (row 1): sale price treated as NA
      actual[1L] <- NA_real_
      residuals_val[1L] <- NA_real_

      export_df[[paste0("est_", response)]] <- round(predicted, 1)

      # --- CQA scores on comps (rows 2+) ---
      la_col <- find_living_area_()
      comp_resid <- residuals_val[-1L]
      comp_resid_valid <- comp_resid[!is.na(comp_resid)]
      n_comps <- length(comp_resid_valid)

      cqa_col <- rep(NA_real_, n)
      if (n_comps > 0) {
        cqa_col <- vapply(residuals_val, function(r) {
          if (is.na(r)) return(NA_real_)
          sum(comp_resid_valid < r, na.rm = TRUE) / n_comps * 10
        }, numeric(1))
      }
      export_df[["residual"]] <- round(residuals_val, 1)
      export_df[["cqa"]] <- round(cqa_col, 2)

      # CQA_SF if living_area designated
      resid_sf <- NULL
      cqa_sf_col <- NULL
      if (!is.null(la_col) && la_col %in% names(export_df)) {
        resid_sf <- residuals_val / export_df[[la_col]]
        export_df[["residual_sf"]] <- round(resid_sf, 4)
        comp_resid_sf <- resid_sf[-1L]
        comp_resid_sf_valid <- comp_resid_sf[!is.na(comp_resid_sf)]
        n_sf <- length(comp_resid_sf_valid)
        if (n_sf > 0) {
          cqa_sf_col <- vapply(resid_sf, function(r) {
            if (is.na(r)) return(NA_real_)
            sum(comp_resid_sf_valid < r, na.rm = TRUE) / n_sf * 10
          }, numeric(1))
          export_df[["cqa_sf"]] <- round(cqa_sf_col, 2)
        }
      }

      # --- Interpolate subject residual from CQA ---
      use_sf <- (input$rca_cqa_type == "cqa_sf" && !is.null(la_col))
      if (use_sf) {
        comp_cqa_vals  <- cqa_sf_col[-1L]
        comp_resid_for_interp <- resid_sf[-1L]
      } else {
        comp_cqa_vals  <- cqa_col[-1L]
        comp_resid_for_interp <- residuals_val[-1L]
      }

      valid <- !is.na(comp_cqa_vals) & !is.na(comp_resid_for_interp)
      cqa_sorted   <- comp_cqa_vals[valid]
      resid_sorted <- comp_resid_for_interp[valid]
      ord <- order(cqa_sorted)
      cqa_sorted   <- cqa_sorted[ord]
      resid_sorted <- resid_sorted[ord]

      subject_resid <- stats::approx(cqa_sorted, resid_sorted,
                                     xout = user_cqa, rule = 2)$y

      # Convert per-SF back to total if needed
      if (use_sf) {
        subject_la <- export_df[[la_col]][1L]
        subject_resid_total <- subject_resid * subject_la
      } else {
        subject_resid_total <- subject_resid
      }

      # Subject value = model estimate + interpolated residual
      subject_est <- predicted[1L] + subject_resid_total
      residuals_val[1L] <- subject_resid_total
      export_df[["residual"]][1L] <- round(subject_resid_total, 1)
      export_df[["subject_value"]] <- NA_real_
      export_df[["subject_value"]][1L] <- round(subject_est, 1)
      export_df[["subject_cqa"]] <- NA_real_
      export_df[["subject_cqa"]][1L] <- user_cqa

      # Weight-0 rows also get subject_value
      wt_col <- data_out$weight_col()
      zero_wt <- integer(0)
      if (!is.null(wt_col) && wt_col %in% names(export_df)) {
        wvals <- export_df[[wt_col]]
        zero_wt <- which(wvals == 0 & seq_len(n) != 1L)
        for (zw in zero_wt) {
          if (!is.na(predicted[zw])) {
            export_df[["subject_value"]][zw] <-
              round(predicted[zw] + subject_resid_total, 1)
          }
        }
      }

      # --- Per-term contributions ---
      contribs <- compute_contributions_(
        x_full, beta, train_colnames, term_labels,
        em$formula_str, em$x_df[complete, , drop = FALSE])

      # Expand contributions to full data (NA for incomplete rows)
      contribs_full <- list()
      for (tl in names(contribs)) {
        v <- rep(NA_real_, n)
        v[complete] <- contribs[[tl]]
        contribs_full[[tl]] <- v
      }

      export_df[["basis"]] <- NA_real_
      export_df[["basis"]][complete] <- round(intercept, 1)

      for (tl in names(contribs_full)) {
        col_name <- paste0(gsub(":", "_x_", tl), "_contribution")
        export_df[[col_name]] <- round(contribs_full[[tl]], 1)
      }

      # --- RCA Adjustments ---
      adj_sum   <- rep(0, n)
      gross_sum <- rep(0, n)

      for (tl in names(contribs_full)) {
        adj_col_name <- paste0(gsub(":", "_x_", tl), "_adjustment")
        subject_contrib <- contribs_full[[tl]][1L]
        adjustment <- subject_contrib - contribs_full[[tl]]
        export_df[[adj_col_name]] <- round(adjustment, 1)
        adj_sum   <- adj_sum + ifelse(is.na(adjustment), 0, adjustment)
        gross_sum <- gross_sum + ifelse(is.na(adjustment), 0, abs(adjustment))
      }

      # Residual adjustment
      resid_adj <- subject_resid_total - residuals_val
      export_df[["residual_adjustment"]] <- round(resid_adj, 1)
      adj_sum   <- adj_sum + ifelse(is.na(resid_adj), 0, resid_adj)
      gross_sum <- gross_sum + ifelse(is.na(resid_adj), 0, abs(resid_adj))

      export_df[["net_adjustments"]]     <- round(adj_sum, 1)
      export_df[["gross_adjustments"]]   <- round(gross_sum, 1)

      # Adjustment percentages (adjustment / comparable sale price)
      sale_price <- export_df[[response]]
      export_df[["residual_adj_pct"]] <- round(resid_adj / sale_price * 100, 2)
      export_df[["net_adj_pct"]]      <- round(adj_sum / sale_price * 100, 2)
      export_df[["gross_adj_pct"]]    <- round(gross_sum / sale_price * 100, 2)

      export_df[["adjusted_sale_price"]] <- round(actual + adj_sum, 1)

      # Subject row: adjustments are zero (subject vs self)
      adj_cols <- grep("_adjustment$|net_adjustments|gross_adjustments|adjusted_sale_price|_adj_pct$",
                       names(export_df), value = TRUE)
      for (ac in adj_cols) {
        export_df[[ac]][1L] <- NA_real_
      }
      export_df[["adjusted_sale_price"]][1L] <- round(subject_est, 1)

      # Store full RCA data frame for Sales Grid
      rv_rca$rca_df <- export_df

      # Store pct data for RCA Analysis plots (comps only, exclude subject)
      rv_rca$pct_data <- data.frame(
        residual_adj_pct = export_df[["residual_adj_pct"]][-1L],
        net_adj_pct      = export_df[["net_adj_pct"]][-1L],
        gross_adj_pct    = export_df[["gross_adj_pct"]][-1L],
        stringsAsFactors = FALSE
      )

      base <- tools::file_path_sans_ext(data_out$file_name() %||% "glmnetui")
      out_path <- file.path(folder, paste0(base, "_adjusted_",
                            format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
      writexl::write_xlsx(export_df, out_path)
      shiny::showNotification(paste0("RCA output saved to: ", out_path),
                              type = "message", duration = 8)
      session$sendCustomMessage("btn_done", list(id = "rca_output_btn"))
    }, error = function(e) {
      shiny::showNotification(paste("RCA error:", e$message),
                              type = "error", duration = 15)
    })
  })

  # --- RCA Analysis Plots ---
  rca_pct_histogram_ <- function(vals, title, xlab, fill_color) {
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return(NULL)

    avg_val    <- mean(vals)
    median_val <- stats::median(vals)
    sd_val     <- stats::sd(vals)
    font_fam   <- glmnet_font_family_()

    # Bins in 20% increments
    lo <- floor(min(vals) / 20) * 20
    hi <- ceiling(max(vals) / 20) * 20
    breaks <- seq(lo, hi, by = 20)
    if (length(breaks) < 2) breaks <- c(lo, lo + 20)

    df <- data.frame(x = vals)
    subtitle <- sprintf("Mean: %.2f%%    Median: %.2f%%    Std Dev: %.2f%%",
                        avg_val, median_val, sd_val)

    ggplot2::ggplot(df, ggplot2::aes(x = .data$x)) +
      ggplot2::geom_histogram(breaks = breaks, fill = fill_color,
                              color = "white", alpha = 0.85) +
      ggplot2::geom_vline(xintercept = avg_val, linetype = "dashed",
                          color = "#2e3440", linewidth = 0.8) +
      ggplot2::geom_vline(xintercept = median_val, linetype = "dotted",
                          color = "#5e81ac", linewidth = 0.8) +
      ggplot2::annotate("text", x = avg_val, y = Inf, label = "Mean",
                        vjust = 2, hjust = -0.15, size = 3.5,
                        color = "#2e3440", family = font_fam) +
      ggplot2::annotate("text", x = median_val, y = Inf, label = "Median",
                        vjust = 3.5, hjust = -0.15, size = 3.5,
                        color = "#5e81ac", family = font_fam) +
      ggplot2::scale_x_continuous(breaks = breaks,
                                  labels = paste0(breaks, "%")) +
      ggplot2::labs(title = title, subtitle = subtitle,
                    x = xlab, y = "Frequency") +
      ggplot2::theme_minimal(base_family = font_fam) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 11,
                                              color = "#4c566a"),
        axis.text.x = ggplot2::element_text(angle = 0)
      )
  }

  output$rca_resid_pct_plot <- shiny::renderPlot({
    shiny::req(rv_rca$pct_data)
    rca_pct_histogram_(rv_rca$pct_data$residual_adj_pct,
                       "Residual Adjustment %",
                       "Residual Adj. %", "#88c0d0")
  })

  output$rca_net_pct_plot <- shiny::renderPlot({
    shiny::req(rv_rca$pct_data)
    rca_pct_histogram_(rv_rca$pct_data$net_adj_pct,
                       "Net Adjustment %",
                       "Net Adj. %", "#5e81ac")
  })

  output$rca_gross_pct_plot <- shiny::renderPlot({
    shiny::req(rv_rca$pct_data)
    rca_pct_histogram_(rv_rca$pct_data$gross_adj_pct,
                       "Gross Adjustment %",
                       "Gross Adj. %", "#a3be8c")
  })

  # --- 8. Generate Sales Grid & Download (Appraisal only) ---
  observeEvent(input$sales_grid_btn, {
    shiny::req(rv_rca$rca_df)
    rca <- rv_rca$rca_df
    n_total <- nrow(rca)
    if (n_total < 2L) {
      shiny::showNotification("Need at least 2 rows (subject + 1 comp).",
                              type = "error", duration = 8)
      return()
    }

    # Build specials map from data module
    sg_specials <- data_out$col_specials()

    # Find sale_age column name
    sa_col_name <- "sale_age"
    if (!is.null(sg_specials)) {
      for (nm in names(sg_specials)) {
        if (sg_specials[[nm]] == "sale_age") { sa_col_name <- nm; break }
      }
    }

    # Weight column
    wt_col_name <- data_out$weight_col()
    wt_vals <- if (!is.null(wt_col_name) && wt_col_name %in% colnames(rca)) {
      rca[[wt_col_name]]
    } else {
      rep(1, n_total)
    }

    # Build comp info (rows 2..n)
    comp_info <- data.frame(
      row        = 2:n_total,
      address    = if ("street_address" %in% colnames(rca)) {
                     rca[["street_address"]][2:n_total]
                   } else rep("", n_total - 1L),
      sale_price = if ("sale_price" %in% colnames(rca)) {
                     rca[["sale_price"]][2:n_total]
                   } else rep(NA_real_, n_total - 1L),
      sale_age   = if (sa_col_name %in% colnames(rca)) {
                     rca[[sa_col_name]][2:n_total]
                   } else rep(NA_real_, n_total - 1L),
      weight     = wt_vals[2:n_total],
      gross_adj  = if ("gross_adjustments" %in% colnames(rca)) {
                     rca[["gross_adjustments"]][2:n_total]
                   } else rep(0, n_total - 1L),
      stringsAsFactors = FALSE
    )
    comp_info$gross_adj_pct <- ifelse(
      !is.na(comp_info$sale_price) & comp_info$sale_price != 0,
      abs(comp_info$gross_adj / comp_info$sale_price), NA_real_
    )

    eligible <- comp_info[!is.na(comp_info$weight) & comp_info$weight > 0, ]
    eligible <- eligible[order(eligible$gross_adj_pct, na.last = TRUE), ]

    recommended <- eligible[!is.na(eligible$gross_adj_pct) &
                            eligible$gross_adj_pct < 0.25, ]
    recommended <- recommended[order(recommended$sale_age, na.last = TRUE), ]
    if (nrow(recommended) > 30L) recommended <- recommended[1:30, ]

    others <- eligible[is.na(eligible$gross_adj_pct) |
                       eligible$gross_adj_pct >= 0.25, ]
    others <- others[order(others$gross_adj_pct, na.last = TRUE), ]

    rv_rca$sg_recommended <- recommended
    rv_rca$sg_others <- others

    rec_checks <- if (nrow(recommended) > 0L) {
      lapply(seq_len(nrow(recommended)), function(i) {
        r <- recommended[i, ]
        lbl <- sprintf("Row %d | %s | SP: $%s | Age: %s | Gross: %.1f%%",
                       r$row,
                       substr(as.character(r$address), 1, 30),
                       formatC(r$sale_price, format = "f", digits = 0,
                               big.mark = ","),
                       as.character(r$sale_age),
                       r$gross_adj_pct * 100)
        shiny::tags$div(
          shiny::checkboxInput(paste0("sg_rec_", r$row), lbl, value = TRUE),
          style = "margin-bottom: 0px;"
        )
      })
    } else {
      shiny::tags$p("No comps with gross adjustment < 25% found.",
                    style = "color: var(--bs-secondary-color);")
    }

    other_checks <- if (nrow(others) > 0L) {
      lapply(seq_len(min(nrow(others), 50L)), function(i) {
        r <- others[i, ]
        pct_str <- if (!is.na(r$gross_adj_pct)) {
          sprintf("%.1f%%", r$gross_adj_pct * 100)
        } else "N/A"
        lbl <- sprintf("Row %d | %s | SP: $%s | Age: %s | Gross: %s",
                       r$row,
                       substr(as.character(r$address), 1, 30),
                       formatC(r$sale_price, format = "f", digits = 0,
                               big.mark = ","),
                       as.character(r$sale_age),
                       pct_str)
        shiny::tags$div(
          shiny::checkboxInput(paste0("sg_rec_", r$row), lbl, value = FALSE),
          style = "margin-bottom: 0px;"
        )
      })
    } else NULL

    shiny::showModal(shiny::modalDialog(
      title = "Sales Grid \u2014 Select Comparables (max 30)",
      size = "l",
      shiny::tags$div(
        style = "max-height: 500px; overflow-y: auto;",
        shiny::tags$h5(paste0("Recommended Comps (gross adj < 25%, ",
                              "sorted by sale age) \u2014 ",
                              nrow(recommended), " found")),
        rec_checks,
        if (!is.null(other_checks)) {
          shiny::tagList(
            shiny::hr(),
            shiny::tags$h5("Additional Comps (gross adj >= 25%)"),
            other_checks
          )
        }
      ),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("sg_confirm", "Generate Sales Grid",
                            class = "btn-success")
      )
    ))
  })

  observeEvent(input$sg_confirm, {
    shiny::req(rv_rca$rca_df)
    shiny::removeModal()

    all_candidate_rows <- c(
      if (!is.null(rv_rca$sg_recommended) && nrow(rv_rca$sg_recommended) > 0L)
        rv_rca$sg_recommended$row else integer(0),
      if (!is.null(rv_rca$sg_others) && nrow(rv_rca$sg_others) > 0L)
        rv_rca$sg_others$row[seq_len(min(nrow(rv_rca$sg_others), 50L))]
      else integer(0)
    )
    comp_rows <- integer(0)
    for (r in all_candidate_rows) {
      cb_val <- input[[paste0("sg_rec_", r)]]
      if (!is.null(cb_val) && isTRUE(cb_val)) {
        comp_rows <- c(comp_rows, r)
      }
    }

    if (length(comp_rows) == 0L) {
      shiny::showNotification("No comps selected.",
                              type = "warning", duration = 8)
      return()
    }
    if (length(comp_rows) > 30L) {
      comp_rows <- comp_rows[1:30]
      shiny::showNotification("Capped at 30 comps.",
                              type = "warning", duration = 5)
    }

    # Sort by gross_adj_pct ascending
    rca <- rv_rca$rca_df
    sp <- if ("sale_price" %in% colnames(rca)) {
      rca[["sale_price"]][comp_rows]
    } else rep(NA_real_, length(comp_rows))
    gross <- if ("gross_adjustments" %in% colnames(rca)) {
      rca[["gross_adjustments"]][comp_rows]
    } else rep(0, length(comp_rows))
    gap <- ifelse(!is.na(sp) & sp != 0, abs(gross / sp), NA_real_)
    comp_rows <- comp_rows[order(gap, na.last = TRUE)]

    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    out_path <- file.path(folder, paste0("SalesGrid_",
                          format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))

    tryCatch({
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        shiny::showNotification(
          "Package 'openxlsx' required. Install with: install.packages('openxlsx')",
          type = "error", duration = 10)
        return()
      }
      if (!requireNamespace("writexl", quietly = TRUE)) {
        shiny::showNotification(
          "Package 'writexl' required. Install with: install.packages('writexl')",
          type = "error", duration = 10)
        return()
      }

      tmp_adj <- tempfile(fileext = ".xlsx")
      writexl::write_xlsx(rv_rca$rca_df, tmp_adj)

      grid_script <- system.file("app", "sales_grid.R", package = "glmnetUI")
      if (!nzchar(grid_script)) {
        shiny::showNotification("Sales grid script not found in package.",
                                type = "error", duration = 10)
        return()
      }
      source(grid_script, local = TRUE)

      # Build specials map from data module designations
      sg_specials_map <- list()
      sg_input <- data_out$col_specials()
      if (!is.null(sg_input)) {
        for (nm in names(sg_input)) {
          sp_type <- sg_input[[nm]]
          if (sp_type != "no") sg_specials_map[[sp_type]] <- nm
        }
      }

      n_comp <- length(comp_rows)
      shiny::withProgress(
        message = "Generating Sales Grid",
        detail = sprintf("0 of %d comps processed", n_comp),
        value = 0, {
        generate_sales_grid(
          adjusted_file = tmp_adj,
          comp_rows     = comp_rows,
          output_file   = out_path,
          specials      = sg_specials_map,
          progress_fn   = function(sheet, total_sheets, comps_done, total_comps) {
            shiny::setProgress(
              value = comps_done / total_comps,
              detail = sprintf("Sheet %d of %d \u2014 %d of %d comps processed",
                               sheet, total_sheets, comps_done, total_comps))
          }
        )
      })
      unlink(tmp_adj)

      shiny::showNotification(
        paste0("Sales grid saved to: ", out_path,
               " (", length(comp_rows), " comps, ",
               ceiling(length(comp_rows) / 3), " sheets)"),
        type = "message", duration = 10)
      session$sendCustomMessage("btn_done", list(id = "sales_grid_btn"))
    }, error = function(e) {
      shiny::showNotification(paste("Sales grid error:", e$message),
                              type = "error", duration = 10)
    })
  })

  # --- 9. Download Report (to output folder) ---
  observeEvent(input$export_report_btn, {
    shiny::req(model_out$fitted())

    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    fmt <- input$export_format
    ext <- if (fmt == "docx") ".docx" else ".pdf"
    base <- tools::file_path_sans_ext(data_out$file_name() %||% "glmnetui")
    out_path <- file.path(folder, paste0(base, "_report_",
                          format(Sys.time(), "%Y%m%d_%H%M%S"), ext))

    tryCatch({
      if (fmt == "docx") {
        # Reuse existing Word report builder from report module
        tmpdir <- tempdir()
        # Make plots
        model <- model_out$model()
        lambda <- model_out$lambda()
        gamma <- model_out$gamma()
        x_mat <- model_out$x_matrix()
        y_vec <- model_out$y_vector()
        coef_df <- coef_out$coef_df()

        pred_args <- list(model, newx = x_mat, s = lambda, type = "response")
        if (!is.null(gamma)) pred_args$gamma <- gamma
        preds <- as.numeric(do.call(stats::predict, pred_args))
        resids <- y_vec - preds

        fit_obj <- if (inherits(model, "cv.glmnet") ||
                         inherits(model, "cv.relaxed")) {
          model$glmnet.fit
        } else if (inherits(model, "relaxed")) {
          model$relaxed
        } else {
          model
        }

        font_fam <- glmnet_font_family_()

        # Coefficient path plot
        beta_mat <- as.matrix(fit_obj$beta)
        log_lambda <- log(fit_obj$lambda)
        df_long <- data.frame(
          log_lambda = rep(log_lambda, each = nrow(beta_mat)),
          coefficient = as.numeric(beta_mat),
          variable = rep(rownames(beta_mat), times = ncol(beta_mat)),
          stringsAsFactors = FALSE
        )
        p1 <- ggplot2::ggplot(df_long,
                   ggplot2::aes(x = .data$log_lambda,
                                y = .data$coefficient,
                                color = .data$variable)) +
          ggplot2::geom_line() +
          ggplot2::labs(x = "Log(Lambda)", y = "Coefficient",
                        title = "Coefficient Path") +
          ggplot2::theme_minimal(base_family = font_fam)
        ggplot2::ggsave(file.path(tmpdir, "coef_path.png"), p1,
                        width = 8, height = 5, dpi = 150)

        # CV plot
        if (inherits(model, "cv.glmnet") || inherits(model, "cv.relaxed")) {
          df_cv <- data.frame(
            log_lambda = log(model$lambda),
            cvm = model$cvm, cvup = model$cvup, cvlo = model$cvlo,
            stringsAsFactors = FALSE)
          p2 <- ggplot2::ggplot(df_cv, ggplot2::aes(x = .data$log_lambda,
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
          ggplot2::ggsave(file.path(tmpdir, "cv_error.png"), p2,
                          width = 8, height = 5, dpi = 150)
        }

        # Actual vs predicted
        df_avp <- data.frame(actual = y_vec, predicted = preds)
        p3 <- ggplot2::ggplot(df_avp, ggplot2::aes(x = .data$actual,
                                                     y = .data$predicted)) +
          ggplot2::geom_point(alpha = 0.5) +
          ggplot2::geom_abline(slope = 1, intercept = 0,
                               color = "#bf616a", linetype = "dashed") +
          ggplot2::labs(x = "Actual", y = "Predicted",
                        title = "Actual vs Predicted") +
          ggplot2::theme_minimal(base_family = font_fam)
        ggplot2::ggsave(file.path(tmpdir, "actual_vs_predicted.png"), p3,
                        width = 8, height = 5, dpi = 150)

        # Residuals vs fitted
        df_resid <- data.frame(fitted = preds, residuals = resids)
        p4 <- ggplot2::ggplot(df_resid, ggplot2::aes(x = .data$fitted,
                                                       y = .data$residuals)) +
          ggplot2::geom_point(alpha = 0.5) +
          ggplot2::geom_hline(yintercept = 0, color = "#bf616a",
                              linetype = "dashed") +
          ggplot2::labs(x = "Fitted Values", y = "Residuals",
                        title = "Residuals vs Fitted") +
          ggplot2::theme_minimal(base_family = font_fam)
        ggplot2::ggsave(file.path(tmpdir, "residuals_vs_fitted.png"), p4,
                        width = 8, height = 5, dpi = 150)

        # Build Word document
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "glmnetUI Model Report",
                                     style = "heading 1")
        doc <- officer::body_add_par(doc, paste("Date:",
                                     format(Sys.time(), "%Y-%m-%d %H:%M")))
        doc <- officer::body_add_par(doc, "")
        doc <- officer::body_add_par(doc, "Model Summary",
                                     style = "heading 2")
        alpha_val <- if (inherits(model, "cv.glmnet")) {
          model$glmnet.fit$call$alpha
        } else { model$call$alpha }
        if (!is.null(alpha_val))
          doc <- officer::body_add_par(doc, paste("Alpha:", alpha_val))
        doc <- officer::body_add_par(doc, paste("Lambda:", signif(lambda, 4)))
        if (inherits(model, "cv.glmnet") || inherits(model, "cv.relaxed")) {
          doc <- officer::body_add_par(doc,
            paste("lambda.min:", signif(model$lambda.min, 4)))
          doc <- officer::body_add_par(doc,
            paste("lambda.1se:", signif(model$lambda.1se, 4)))
        }
        doc <- officer::body_add_par(doc, "Coefficients",
                                     style = "heading 2")
        doc <- officer::body_add_table(doc, value = coef_df,
                                       style = "table_template")
        doc <- officer::body_add_par(doc, "Diagnostic Plots",
                                     style = "heading 2")
        for (pf in c("coef_path.png", "cv_error.png",
                      "actual_vs_predicted.png", "residuals_vs_fitted.png")) {
          fp <- file.path(tmpdir, pf)
          if (file.exists(fp)) {
            doc <- officer::body_add_img(doc, src = fp, width = 6, height = 3.75)
            doc <- officer::body_add_par(doc, "")
          }
        }
        print(doc, target = out_path)

      } else {
        # PDF via rmarkdown
        tmpdir <- tempdir()
        # Reuse the same plot generation as above
        model <- model_out$model()
        lambda <- model_out$lambda()
        coef_df <- coef_out$coef_df()

        rmd_template <- system.file("app", "report_template.Rmd",
                                     package = "glmnetUI")
        rmd_copy <- file.path(tmpdir, "report.Rmd")
        file.copy(rmd_template, rmd_copy, overwrite = TRUE)

        # Render to temp file first, then copy
        tmp_out <- file.path(tmpdir, paste0("report", ext))
        rmarkdown::render(
          rmd_copy,
          output_file = tmp_out,
          params = list(
            appraiser_name = "",
            property_address = "",
            report_date = as.character(Sys.Date()),
            file_number = "",
            lambda = lambda,
            lambda_min = if (inherits(model, "cv.glmnet")) model$lambda.min else NA,
            lambda_1se = if (inherits(model, "cv.glmnet")) model$lambda.1se else NA,
            coef_df = coef_df,
            plot_dir = tmpdir
          ),
          envir = new.env(parent = globalenv()),
          quiet = TRUE
        )
        file.copy(tmp_out, out_path, overwrite = TRUE)
      }

      shiny::showNotification(paste0("Report saved to: ", out_path),
                              type = "message", duration = 8)
      session$sendCustomMessage("btn_done", list(id = "export_report_btn"))
    }, error = function(e) {
      shiny::showNotification(paste("Report error:", e$message),
                              type = "error", duration = 10)
    })
  })

  # --- Settings defaults radio ---
  shiny::observeEvent(input$glmnet_defaults_action, {
    action <- input$glmnet_defaults_action
    shiny::req(data_out$file_name())

    if (action == "use_default") {
      session$sendCustomMessage("apply_saved_defaults", list(
        filename = data_out$file_name()
      ))
    } else if (action == "glmnet_defaults") {
      session$sendCustomMessage("apply_glmnet_defaults", list())
      shiny::showNotification("glmnet default parameters applied.",
                              type = "message", duration = 3)
      shiny::updateRadioButtons(session, "glmnet_defaults_action",
                                selected = "last")
    }
  }, ignoreInit = TRUE)

  # No saved defaults notification
  shiny::observeEvent(input$glmnet_no_defaults, {
    shiny::showNotification(
      "No default settings saved yet. Use 'Save current as default' first.",
      type = "warning", duration = 4
    )
    shiny::updateRadioButtons(session, "glmnet_defaults_action",
                              selected = "last")
  })

  # --- Save current settings as default ---
  shiny::observeEvent(input$glmnet_save_defaults, {
    shiny::req(data_out$file_name())
    session$sendCustomMessage("collect_and_save_defaults", list(
      filename = data_out$file_name()
    ))
    shiny::showNotification("Current settings saved as defaults.",
                            type = "message", duration = 3)
  })
}
