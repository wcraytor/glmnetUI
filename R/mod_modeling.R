#' Modeling Module UI
#'
#' UI component for the glmnet modeling module. Provides controls for
#' alpha, lambda, family, model fitting, sign constraints, relaxed lasso,
#' penalty factors, weights, alpha grid search, and interaction terms.
#'
#' @param id Module namespace ID.
#'
#' @return A Shiny \code{tagList} containing the module UI elements.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(modelingUI("model"))
#' }
modelingUI <- function(id) {

  ns <- shiny::NS(id)

  # Helper: inline "?" icon with Bootstrap popover
  help_icon <- function(text) {
    shiny::tags$span(
      class = "glmnet-help-icon",
      `data-bs-toggle` = "popover",
      `data-bs-trigger` = "hover focus",
      `data-bs-content` = text,
      `data-bs-placement` = "left",
      "?"
    )
  }

  # Helper: label with "?" icon
  label_with_help <- function(label, help_text) {
    shiny::tags$span(label, help_icon(help_text))
  }

  # Helper: wrap input with positioned "?" popover icon
  param_with_help <- function(input_el, help_text) {
    shiny::tags$div(
      style = "position: relative;",
      input_el,
      shiny::tags$span(
        class = "glmnet-param-help",
        `data-bs-toggle` = "popover",
        `data-bs-trigger` = "hover focus",
        `data-bs-content` = help_text,
        `data-bs-placement` = "left",
        "?"
      )
    )
  }

  shiny::tagList(
    shiny::uiOutput(ns("settings_js")),

    # --- Alpha ---
    shiny::radioButtons(
      ns("alpha_method"),
      label_with_help(
        "Alpha Selection",
        paste(
          "Alpha controls the mix between Ridge (alpha=0) and Lasso (alpha=1).",
          "Ridge shrinks coefficients but keeps all variables.",
          "Lasso can shrink coefficients to exactly zero, removing variables.",
          "Use 'Grid search' to automatically find the best alpha."
        )
      ),
      choices = c("Fixed" = "fixed",
                  "Grid search (find optimal)" = "grid"),
      selected = "fixed"
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("alpha_method"), "'] == 'fixed'"),
      shiny::sliderInput(ns("alpha"), "Alpha (Mixing Parameter)",
                         min = 0, max = 1, value = 1, step = 0.05),
      shiny::helpText("Ridge (0) \u2190\u2192 Lasso (1)")
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("alpha_method"), "'] == 'grid'"),
      shiny::sliderInput(ns("alpha_range"), "Alpha Range",
                         min = 0, max = 1, value = c(0, 1), step = 0.05),
      shiny::numericInput(ns("n_alphas"), "Number of Alpha Values",
                          value = 11, min = 3, max = 51, step = 1)
    ),

    # --- Lambda ---
    shiny::radioButtons(
      ns("lambda_method"),
      label_with_help(
        "Lambda Selection",
        paste(
          "Lambda controls the strength of regularization.",
          "Higher lambda = more shrinkage = fewer variables kept.",
          "Cross-validation automatically finds a good lambda.",
          "lambda.1se is more conservative (simpler model);",
          "lambda.min gives the lowest prediction error."
        )
      ),
      choices = c("Cross-validation (automatic)" = "cv",
                  "Manual (enter value)" = "manual"),
      selected = "cv"
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("lambda_method"), "'] == 'manual'"),
      shiny::numericInput(ns("lambda_manual"), "Lambda Value",
                          value = 0.01, min = 0, step = 0.001)
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("lambda_method"), "'] == 'cv'"),
      shiny::radioButtons(ns("lambda_choice"), "CV Lambda",
                          choices = c("lambda.1se (more regularized)" = "1se",
                                      "lambda.min (minimum error)" = "min"),
                          selected = "1se"),
      shiny::numericInput(
        ns("nfolds"),
        label_with_help(
          "Number of CV Folds",
          paste(
            "The data is split into this many groups for cross-validation.",
            "Each group takes a turn as the test set.",
            "More folds = more stable but slower. 10 is a good default."
          )
        ),
        value = 10, min = 3, max = 50, step = 1
      )
    ),

    # --- Family & Standardize ---
    shiny::selectInput(
      ns("family"),
      label_with_help(
        "Family",
        paste(
          "The type of response variable.",
          "gaussian: continuous numeric response (e.g., sale price).",
          "binomial: binary 0/1 response (e.g., sold yes/no).",
          "poisson: count response (e.g., number of sales)."
        )
      ),
      choices = c("gaussian", "binomial", "poisson"),
      selected = "gaussian"
    ),
    param_with_help(
      shiny::checkboxInput(ns("standardize"), "Standardize Predictors",
                           value = TRUE),
      paste(
        "Standardize scales all predictors to have mean 0 and SD 1",
        "before fitting, so the penalty treats them equally.",
        "Coefficients are returned on the original scale.",
        "Usually should be left on."
      )
    ),

    # --- Sign constraints ---
    param_with_help(
      shiny::checkboxInput(ns("enforce_signs"), "Enforce Sign Constraints",
                           value = FALSE),
      paste(
        "When enabled, constrains the fitted model coefficients to match",
        "the expected signs set in the variable table.",
        "A 'positive' sign forces the coefficient >= 0",
        "(monotonically increasing effect).",
        "A 'negative' sign forces the coefficient <= 0",
        "(monotonically decreasing effect).",
        "Variables set to 'either' are unconstrained.",
        "This does NOT constrain the input data values,",
        "only the model's estimated coefficients."
      )
    ),

    # --- Relaxed lasso ---
    param_with_help(
      shiny::checkboxInput(ns("relaxed"), "Relaxed Lasso", value = FALSE),
      paste(
        "Regular lasso uses the same penalty for variable selection AND",
        "coefficient estimation, which can over-shrink important",
        "coefficients toward zero.",
        "Relaxed lasso separates these steps: first select variables",
        "via lasso, then refit the surviving variables with less or no",
        "penalty for less biased estimates.",
        "All variables that survive regularization are refit,",
        "including forced-in variables.",
        "Gamma controls the degree of relaxation."
      )
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("relaxed"),
                         "'] && input['", ns("lambda_method"), "'] == 'manual'"),
      shiny::sliderInput(
        ns("gamma"),
        label_with_help(
          "Gamma (relaxation)",
          paste(
            "Controls how much the surviving coefficients are relaxed.",
            "gamma=0: fully relaxed (OLS refit, no shrinkage at all).",
            "gamma=1: no relaxation (same as regular glmnet).",
            "Values in between blend the two.",
            "With cross-validation, gamma is chosen automatically."
          )
        ),
        min = 0, max = 1, value = 0, step = 0.25
      )
    ),

    # --- Interaction Matrix ---
    shiny::hr(),
    shiny::tags$span(
      shiny::h5("Interaction Matrix", style = "display:inline;"),
      help_icon(paste(
        "Check pairs of variables to allow interaction terms.",
        "Unchecked pairs will not be combined.",
        "Click a variable name to toggle all its interactions.",
        "Only checked pairs are included in the model matrix.",
        "With many predictors, lasso will zero out unneeded interactions."
      ))
    ),
    shiny::tags$div(
      style = "margin-bottom:6px; margin-top:4px;",
      shiny::tags$label(
        style = "font-size:0.85em; margin-right:12px; cursor:pointer;",
        shiny::tags$input(
          type = "checkbox", id = ns("allow_all"),
          class = "glmnet-int-toggle", checked = "checked",
          style = "margin-right:4px;"
        ),
        "Allow All"
      ),
      shiny::tags$label(
        style = "font-size:0.85em; cursor:pointer;",
        shiny::tags$input(
          type = "checkbox", id = ns("clear_all"),
          class = "glmnet-int-toggle",
          style = "margin-right:4px;"
        ),
        "Clear All"
      )
    ),
    shiny::tags$script(shiny::HTML(sprintf(
      "
      $(document).on('change', '#%s', function() {
        if ($(this).is(':checked')) {
          $('#%s').prop('checked', false);
          $('.glmnet-interaction-cb').prop('checked', true).first().trigger('change');
        }
      });
      $(document).on('change', '#%s', function() {
        if ($(this).is(':checked')) {
          $('#%s').prop('checked', false);
          $('.glmnet-interaction-cb').prop('checked', false).first().trigger('change');
        }
      });
      $(document).on('change', '.glmnet-interaction-cb', function() {
        var all = $('.glmnet-interaction-cb').length;
        var checked = $('.glmnet-interaction-cb:checked').length;
        $('#%s').prop('checked', checked === all);
        $('#%s').prop('checked', checked === 0);
      });
      ",
      ns("allow_all"), ns("clear_all"),
      ns("clear_all"), ns("allow_all"),
      ns("allow_all"), ns("clear_all")
    ))),
    shiny::uiOutput(ns("interaction_matrix"))
  )
}

#' Fit Model Module UI
#'
#' UI component for the fit button and status display. Uses the same
#' module namespace as [modelingUI()] / [modelingServer()].
#'
#' @param id Module namespace ID (must match the ID used in
#'   \code{modelingServer}).
#'
#' @return A Shiny \code{tagList} containing the fit button and status.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(fitModelUI("model"))
#' }
fitModelUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::actionButton(ns("fit_btn"), "Fit Model",
                        class = "btn-primary btn-lg",
                        width = "100%"),
    shiny::hr(),
    shiny::uiOutput(ns("fit_status"))
  )
}

#' Modeling Module Server
#'
#' Server logic for the glmnet modeling module. Handles model fitting
#' with glmnet and cv.glmnet, including weights, penalty factors,
#' coefficient bounds, relaxed lasso, alpha grid search, and
#' interaction terms.
#'
#' @param id Module namespace ID.
#' @param data_module Reactive list returned by [dataImportServer()].
#' @param purpose Reactive returning the purpose mode string.
#' @param effective_date Reactive returning the effective date.
#'
#' @return A reactive list containing model results.
#'
#' @export
modelingServer <- function(id, data_module,
                           purpose = shiny::reactiveVal("general"),
                           effective_date = shiny::reactiveVal(Sys.Date())) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- shiny::reactiveValues(
      model = NULL,
      lambda = NULL,
      gamma_used = NULL,
      x_matrix = NULL,
      y_vector = NULL,
      weights_used = NULL,
      fitted = FALSE,
      fit_count = 0L,
      alpha_used = NULL,
      family_used = NULL,
      nfolds_used = NULL,
      n_obs = NULL,
      n_excluded = 0L
    )

    # --- Settings persistence JS ---
    output$settings_js <- shiny::renderUI({
      file_key <- data_module$file_name()
      storage_key <- if (is.null(file_key)) "default" else file_key

      shiny::tags$script(shiny::HTML(sprintf('
        (function() {
          var storageKey = "glmnetUI_settings_" + %s;

          var radioIds = ["model-alpha_method", "model-lambda_method",
                          "model-lambda_choice", "purpose"];
          var sliderIds = ["model-alpha", "model-gamma"];
          var rangeSliderIds = ["model-alpha_range"];
          var numericIds = ["model-n_alphas", "model-lambda_manual",
                            "model-nfolds"];
          var selectIds = ["model-family"];
          var checkboxIds = ["model-standardize", "model-enforce_signs",
                             "model-relaxed"];
          var textIds = ["output_folder"];
          var dateIds = ["effective_date"];
          var allIds = radioIds.concat(sliderIds).concat(rangeSliderIds)
            .concat(numericIds).concat(selectIds).concat(checkboxIds)
            .concat(textIds).concat(dateIds);

          function saveSettings() {
            var state = {};
            radioIds.forEach(function(id) {
              state[id] = $("input[name=\\"" + id + "\\"]:checked").val();
            });
            sliderIds.forEach(function(id) {
              var irs = $(document.getElementById(id)).data("ionRangeSlider");
              if (irs) state[id] = irs.result.from;
            });
            rangeSliderIds.forEach(function(id) {
              var irs = $(document.getElementById(id)).data("ionRangeSlider");
              if (irs) state[id] = [irs.result.from, irs.result.to];
            });
            numericIds.forEach(function(id) {
              state[id] = $(document.getElementById(id)).val();
            });
            selectIds.forEach(function(id) {
              var el = document.getElementById(id);
              if (el && el.selectize) state[id] = el.selectize.getValue();
              else if (el) state[id] = $(el).val();
            });
            checkboxIds.forEach(function(id) {
              state[id] = $(document.getElementById(id)).is(":checked");
            });
            textIds.forEach(function(id) {
              state[id] = $(document.getElementById(id)).val();
            });
            dateIds.forEach(function(id) {
              var $inp = $("#" + id + " input");
              state[id] = $inp.length ? $inp.val() :
                $(document.getElementById(id)).val();
            });
            try {
              localStorage.setItem(storageKey, JSON.stringify(state));
            } catch(e) {}
          }

          function restoreSettings() {
            var saved = null;
            try {
              saved = JSON.parse(localStorage.getItem(storageKey));
            } catch(e) {}
            if (!saved) return;
            if (typeof window.glmnetApplySettings === "function") {
              window.glmnetApplySettings(saved);
            }
          }

          var restoreComplete = false;
          var attempts = 0;
          function tryRestore() {
            var el = document.getElementById("model-family");
            if (el && el.selectize && el.selectize.isSetup) {
              restoreSettings();
              restoreComplete = true;
            } else if (attempts < 40) {
              attempts++;
              setTimeout(tryRestore, 250);
            } else {
              restoreComplete = true;
            }
          }
          tryRestore();

          $(document).off("shiny:inputchanged.glmnetsettings")
            .on("shiny:inputchanged.glmnetsettings", function(event) {
              if (restoreComplete && allIds.indexOf(event.name) >= 0) {
                saveSettings();
              }
            });
        })();
      ', jsonlite::toJSON(storage_key, auto_unbox = TRUE))))
    })

    # --- Interaction matrix UI ---
    output$interaction_matrix <- shiny::renderUI({
      preds <- data_module$predictors()
      if (is.null(preds) || length(preds) < 2) {
        return(shiny::helpText("Need at least 2 included predictors."))
      }
      n <- length(preds)
      file_key <- data_module$file_name()

      # Column headers
      header_cells <- list(shiny::tags$th(style = "padding:2px;", ""))
      for (j in seq_len(n)) {
        header_cells <- c(header_cells, list(
          shiny::tags$th(
            class = "glmnet-matrix-varlabel",
            `data-var-idx` = j,
            style = paste0(
              "padding:2px 4px; font-size:0.75em; text-align:center; ",
              "writing-mode:vertical-lr; transform:rotate(180deg); ",
              "max-height:100px; overflow:hidden; cursor:pointer;"
            ),
            title = preds[j], preds[j]
          )
        ))
      }
      header_row <- shiny::tags$tr(header_cells)

      # Build matrix rows (upper triangle only)
      body_rows <- list()
      for (i in seq_len(n)) {
        cells <- list(
          shiny::tags$td(
            class = "glmnet-matrix-varlabel",
            `data-var-idx` = i,
            style = paste0(
              "padding:2px 4px; font-size:0.75em; white-space:nowrap; ",
              "overflow:hidden; text-overflow:ellipsis; ",
              "max-width:100px; cursor:pointer;"
            ),
            title = preds[i], preds[i]
          )
        )
        for (j in seq_len(n)) {
          if (j > i) {
            cb_id <- ns(paste0("int_", i, "_", j))
            cells <- c(cells, list(
              shiny::tags$td(
                style = "text-align:center; padding:2px;",
                shiny::tags$input(
                  type = "checkbox", id = cb_id,
                  class = "glmnet-interaction-cb",
                  checked = "checked",
                  style = "margin:0;"
                )
              )
            ))
          } else {
            cells <- c(cells, list(
              shiny::tags$td(
                style = "padding:2px;",
                if (i == j) "\u00b7" else ""
              )
            ))
          }
        }
        body_rows <- c(body_rows, list(shiny::tags$tr(cells)))
      }

      storage_key <- if (is.null(file_key)) "default" else file_key
      int_js <- sprintf('
        (function() {
          var n = %d;
          var nsPrefix = "%s";
          var storageKey = "glmnetUI_interactions_" + %s;

          function saveState() {
            var state = {};
            for (var i = 1; i < n; i++) {
              for (var j = i + 1; j <= n; j++) {
                var id = nsPrefix + "int_" + i + "_" + j;
                state[i + "_" + j] = $("#" + CSS.escape(id)).is(":checked");
              }
            }
            try { localStorage.setItem(storageKey, JSON.stringify(state)); } catch(e) {}
          }

          function restoreState() {
            var saved = null;
            try { saved = JSON.parse(localStorage.getItem(storageKey)); } catch(e) {}
            if (!saved) return;
            for (var i = 1; i < n; i++) {
              for (var j = i + 1; j <= n; j++) {
                var key = i + "_" + j;
                if (saved[key] !== undefined) {
                  var id = nsPrefix + "int_" + i + "_" + j;
                  $("#" + CSS.escape(id)).prop("checked", saved[key]);
                }
              }
            }
          }

          function syncToShiny() {
            for (var i = 1; i < n; i++) {
              for (var j = i + 1; j <= n; j++) {
                var id = nsPrefix + "int_" + i + "_" + j;
                Shiny.setInputValue(id, $("#" + CSS.escape(id)).is(":checked"));
              }
            }
          }

          restoreState();
          setTimeout(function() {
            syncToShiny();
            var all = $(".glmnet-interaction-cb").length;
            var checked = $(".glmnet-interaction-cb:checked").length;
            $("#" + CSS.escape(nsPrefix + "allow_all")).prop("checked", checked === all);
            $("#" + CSS.escape(nsPrefix + "clear_all")).prop("checked", checked === 0);
          }, 200);

          $(document).off("change.glmnetmatrix").on("change.glmnetmatrix",
            ".glmnet-interaction-cb", function() {
              saveState();
              syncToShiny();
            });

          $(document).off("click.glmnetvarlabel").on("click.glmnetvarlabel",
            ".glmnet-matrix-varlabel", function() {
              var k = parseInt($(this).attr("data-var-idx"));
              if (isNaN(k)) return;
              var cbs = [];
              for (var i = 1; i <= n; i++) {
                if (i === k) continue;
                var lo = Math.min(i, k), hi = Math.max(i, k);
                var id = nsPrefix + "int_" + lo + "_" + hi;
                var $cb = $("#" + CSS.escape(id));
                if ($cb.length) cbs.push($cb);
              }
              var allChecked = cbs.every(function($cb) { return $cb.is(":checked"); });
              cbs.forEach(function($cb) { $cb.prop("checked", !allChecked); });
              if (cbs.length > 0) cbs[0].trigger("change");
            });
        })();
      ',
        n, ns(""),
        jsonlite::toJSON(storage_key, auto_unbox = TRUE)
      )
      js <- shiny::tags$script(shiny::HTML(int_js))

      shiny::tags$div(
        style = paste0(
          "max-height:300px; overflow:auto; border:1px solid #ddd; ",
          "padding:4px; border-radius:4px;"
        ),
        shiny::tags$table(
          style = "border-collapse:collapse;",
          shiny::tags$thead(header_row),
          shiny::tags$tbody(body_rows)
        ),
        js
      )
    })

    # --- Get allowed interaction pairs from matrix checkboxes ---
    get_interaction_pairs <- shiny::reactive({
      preds <- data_module$predictors()
      if (is.null(preds) || length(preds) < 2) return(list())
      n <- length(preds)
      pairs <- list()
      for (i in seq_len(n - 1)) {
        for (j in (i + 1):n) {
          cb_id <- paste0("int_", i, "_", j)
          val <- input[[cb_id]]
          if (isTRUE(val)) {
            pairs <- c(pairs, list(c(preds[i], preds[j])))
          }
        }
      }
      pairs
    })

    # --- Fit model ---
    shiny::observeEvent(input$fit_btn, {
      if (!data_module$valid()) {
        shiny::showNotification(
          "Select a response variable and at least one predictor.",
          type = "error"
        )
        return()
      }

      tryCatch({
        # --- Purpose-mode preprocessing ---
        if (purpose() %in% c("appraisal", "market")) {
          specials <- data_module$col_specials()
          rv_data <- data_module$rv

          # Round latitude/longitude to 3 decimal places
          for (nm in names(specials)) {
            if (specials[[nm]] %in% c("latitude", "longitude") &&
                  nm %in% names(rv_data$data) &&
                  is.numeric(rv_data$data[[nm]])) {
              rv_data$data[[nm]] <- round(rv_data$data[[nm]], 3L)
            }
          }

          # Add sale_age if contract_date is designated
          if (!("sale_age" %in% names(rv_data$data) &&
                  is.numeric(rv_data$data[["sale_age"]]))) {
            contract_col <- NULL
            for (nm in names(specials)) {
              if (specials[[nm]] == "contract_date") {
                contract_col <- nm
                break
              }
            }

            if (is.null(contract_col)) {
              shiny::showNotification(
                paste0("No 'contract_date' column designated",
                       " \u2014 skipping sale_age calculation."),
                type = "message", duration = 6
              )
            } else {
              eff_date <- as.POSIXct(as.character(effective_date()))
              contract_vals <- rv_data$data[[contract_col]]

              contract_posix <- if (inherits(contract_vals, "POSIXct")) {
                contract_vals
              } else if (inherits(contract_vals, "Date")) {
                as.POSIXct(contract_vals)
              } else if (is.character(contract_vals)) {
                suppressWarnings(as.POSIXct(contract_vals))
              } else if (is.numeric(contract_vals)) {
                as.POSIXct(as.Date(contract_vals, origin = "1899-12-30"))
              } else {
                NULL
              }

              if (is.null(contract_posix) ||
                    all(is.na(contract_posix[!is.na(contract_vals)]))) {
                shiny::showNotification(
                  paste0("Cannot parse '", contract_col,
                         "' as dates for sale_age."),
                  type = "error", duration = 10
                )
              } else {
                sale_age <- as.integer(
                  difftime(eff_date, contract_posix, units = "days")
                )
                rv_data$data[["sale_age"]] <- sale_age
                rv_data$col_types[["sale_age"]] <- "integer"

                session$sendCustomMessage("sale_age_added", list(
                  filename = data_module$file_name()
                ))
                shiny::showNotification(
                  paste0("Added 'sale_age' column (",
                         sum(!is.na(sale_age)),
                         " values). Click Fit again to include it."),
                  type = "message", duration = 8
                )
              }
            }
          }
        }

        df <- data_module$data()
        resp <- data_module$response()
        preds <- data_module$predictors()

        if (!resp %in% names(df)) {
          shiny::showNotification(
            paste0("Response variable '", resp, "' not found in data."),
            type = "error"
          )
          return()
        }
        missing_preds <- setdiff(preds, names(df))
        if (length(missing_preds) > 0) {
          shiny::showNotification(
            paste0("Predictor(s) not found: ",
                   paste(missing_preds, collapse = ", ")),
            type = "error"
          )
          return()
        }

        if (!is.numeric(df[[resp]])) {
          shiny::showNotification(
            paste0("Response '", resp, "' is not numeric."),
            type = "error"
          )
          return()
        }

        # --- Weight column handling ---
        wt_col <- data_module$weight_col()
        use_cols <- c(resp, preds)
        if (!is.null(wt_col) && wt_col %in% names(df)) {
          use_cols <- c(use_cols, wt_col)
        }

        complete <- stats::complete.cases(df[, use_cols, drop = FALSE])
        df_clean <- df[complete, , drop = FALSE]

        # Extract weights and filter weight = 0 rows
        wt_vec <- NULL
        n_excluded <- 0L
        if (!is.null(wt_col) && wt_col %in% names(df_clean)) {
          wt_vec <- as.numeric(df_clean[[wt_col]])
          keep <- wt_vec != 0
          n_excluded <- sum(!keep)
          df_clean <- df_clean[keep, , drop = FALSE]
          wt_vec <- wt_vec[keep]
        }

        if (nrow(df_clean) < 3) {
          shiny::showNotification(
            paste0("Too few observations for modeling (",
                   nrow(df_clean), " rows)."),
            type = "error"
          )
          return()
        }

        if (stats::var(df_clean[[resp]]) == 0) {
          shiny::showNotification(
            paste0("Response '", resp, "' is constant."),
            type = "error"
          )
          return()
        }

        # --- Apply type overrides ---
        x_df <- df_clean[, preds, drop = FALSE]
        col_types <- data_module$col_types()
        for (p in preds) {
          if (!is.null(col_types[p])) {
            if (col_types[p] %in% c("factor", "character")) {
              x_df[[p]] <- as.factor(x_df[[p]])
            } else if (col_types[p] == "numeric") {
              x_df[[p]] <- as.numeric(x_df[[p]])
            } else if (col_types[p] == "integer") {
              x_df[[p]] <- as.integer(x_df[[p]])
            }
          }
        }

        # --- Build model matrix with interactions ---
        ints <- get_interaction_pairs()
        formula_parts <- preds
        if (length(ints) > 0) {
          int_strs <- vapply(ints, function(pair) {
            paste(pair, collapse = ":")
          }, character(1))
          formula_parts <- c(formula_parts, int_strs)
        }
        formula_str <- paste("~", paste(formula_parts, collapse = " + "),
                             "- 1")
        x_mat <- stats::model.matrix(stats::as.formula(formula_str),
                                     data = x_df)
        y_vec <- df_clean[[resp]]

        # --- Penalty factors from force-in variables ---
        assign_attr <- attr(x_mat, "assign")
        n_main <- length(preds)
        penalty_fac <- rep(1, ncol(x_mat))
        force_vars <- data_module$force_in()
        for (j in seq_len(n_main)) {
          if (preds[j] %in% force_vars) {
            penalty_fac[assign_attr == j] <- 0
          }
        }

        # --- Coefficient bounds from expected signs ---
        lower_lim <- rep(-Inf, ncol(x_mat))
        upper_lim <- rep(Inf, ncol(x_mat))
        if (isTRUE(input$enforce_signs)) {
          expected <- data_module$expected_signs()
          for (j in seq_len(n_main)) {
            pred_name <- preds[j]
            if (pred_name %in% names(expected)) {
              sign_val <- expected[pred_name]
              cols_j <- which(assign_attr == j)
              if (sign_val == "positive") {
                lower_lim[cols_j] <- 0
              } else if (sign_val == "negative") {
                upper_lim[cols_j] <- 0
              }
            }
          }
        }

        # --- Common fit arguments ---
        family_val <- input$family
        standardize_val <- input$standardize
        relax_val <- isTRUE(input$relaxed)

        fit_args <- list(
          x = x_mat, y = y_vec,
          family = family_val,
          standardize = standardize_val,
          penalty.factor = penalty_fac,
          lower.limits = lower_lim,
          upper.limits = upper_lim
        )
        if (!is.null(wt_vec)) fit_args$weights <- wt_vec
        if (relax_val) fit_args$relax <- TRUE

        # --- Alpha: fixed or grid search ---
        if (input$alpha_method == "grid") {
          alpha_min <- input$alpha_range[1]
          alpha_max <- input$alpha_range[2]
          n_alphas <- input$n_alphas
          if (is.null(n_alphas) || n_alphas < 3) n_alphas <- 11
          alphas <- seq(alpha_min, alpha_max, length.out = n_alphas)
        } else {
          alphas <- input$alpha
        }

        shiny::withProgress(message = "Fitting model...", {
          if (input$lambda_method == "cv") {
            nfolds_val <- input$nfolds
            if (is.null(nfolds_val) || is.na(nfolds_val) ||
                  nfolds_val < 3) {
              nfolds_val <- 10L
            }
            fit_args$nfolds <- as.integer(nfolds_val)

            if (length(alphas) > 1) {
              # Alpha grid search: fit CV for each alpha, pick best
              best_cvm <- Inf
              best_fit <- NULL
              best_alpha <- alphas[1]
              for (a in alphas) {
                fit_args$alpha <- a
                shiny::incProgress(1 / length(alphas),
                                   detail = paste("alpha =", a))
                fit_a <- do.call(glmnet::cv.glmnet, fit_args)
                min_cvm <- min(fit_a$cvm)
                if (min_cvm < best_cvm) {
                  best_cvm <- min_cvm
                  best_fit <- fit_a
                  best_alpha <- a
                }
              }
              fit <- best_fit
              alpha_val <- best_alpha
            } else {
              fit_args$alpha <- alphas[1]
              fit <- do.call(glmnet::cv.glmnet, fit_args)
              alpha_val <- alphas[1]
            }

            # Select lambda
            lambda_val <- if (input$lambda_choice == "1se") {
              fit$lambda.1se
            } else {
              fit$lambda.min
            }

            # Select gamma for relaxed fits
            gamma_val <- NULL
            if (relax_val && inherits(fit, "cv.relaxed")) {
              choice <- input$lambda_choice
              gamma_field <- if (choice == "1se") "gamma.1se" else "gamma.min"
              gamma_val <- fit$relaxed[[gamma_field]]
              if (is.null(gamma_val)) gamma_val <- 0
              # Use the relaxed lambda which is jointly optimized
              lambda_field <- if (choice == "1se") "lambda.1se" else "lambda.min"
              relaxed_lambda <- fit$relaxed[[lambda_field]]
              if (!is.null(relaxed_lambda)) lambda_val <- relaxed_lambda
            }

          } else {
            # Manual lambda
            fit_args$alpha <- if (length(alphas) > 1) alphas[1] else alphas
            alpha_val <- fit_args$alpha
            lambda_val <- input$lambda_manual
            fit <- do.call(glmnet::glmnet, fit_args)

            gamma_val <- NULL
            if (relax_val) {
              gamma_val <- input$gamma
              if (is.null(gamma_val)) gamma_val <- 0
            }
          }
        })

        rv$model <- fit
        rv$lambda <- lambda_val
        rv$gamma_used <- gamma_val
        rv$x_matrix <- x_mat
        rv$y_vector <- y_vec
        rv$weights_used <- wt_vec
        rv$alpha_used <- alpha_val
        rv$family_used <- family_val
        rv$nfolds_used <- if (input$lambda_method == "cv") {
          as.integer(nfolds_val)
        } else {
          NA_integer_
        }
        rv$n_obs <- nrow(df_clean)
        rv$n_excluded <- n_excluded
        rv$fit_count <- rv$fit_count + 1L
        rv$fitted <- TRUE

        shiny::showNotification("Model fitted successfully!",
                                type = "message")

      }, error = function(e) {
        shiny::showNotification(paste("Fitting error:", e$message),
                                type = "error")
      })
    })

    # --- Fit status display ---
    output$fit_status <- shiny::renderUI({
      count <- rv$fit_count
      if (!rv$fitted) {
        return(shiny::helpText(
          "No model fitted yet. Configure parameters and click 'Fit Model'."
        ))
      }

      model <- rv$model
      lambda <- rv$lambda
      gamma <- rv$gamma_used
      x_mat <- rv$x_matrix
      y_vec <- rv$y_vector

      # Extract coefficients
      coef_args <- list(model, s = lambda)
      if (!is.null(gamma)) coef_args$gamma <- gamma
      coefs <- do.call(stats::coef, coef_args)
      n_nonzero <- sum(as.numeric(coefs) != 0) - 1L

      # Predictions
      pred_args <- list(model, newx = x_mat, s = lambda, type = "response")
      if (!is.null(gamma)) pred_args$gamma <- gamma
      predicted <- as.numeric(do.call(stats::predict, pred_args))

      ss_res <- sum((y_vec - predicted)^2)
      ss_tot <- sum((y_vec - mean(y_vec))^2)
      r_squared <- 1 - ss_res / ss_tot
      n <- length(y_vec)
      p <- n_nonzero
      adj_r_squared <- if (n - p - 1 > 0) {
        1 - (1 - r_squared) * (n - 1) / (n - p - 1)
      } else {
        NA_real_
      }
      rmse <- sqrt(ss_res / n)

      # CV R-squared
      cv_r2_line <- NULL
      is_cv <- inherits(model, "cv.glmnet") || inherits(model, "cv.relaxed")
      if (is_cv) {
        idx <- which(model$lambda == lambda)
        if (length(idx) == 1) {
          cv_mse <- model$cvm[idx]
        } else {
          cv_mse <- tryCatch(
            stats::approx(log(model$lambda), model$cvm,
                          xout = log(lambda))$y,
            error = function(e) NA
          )
        }
        if (!is.na(cv_mse)) {
          cv_r2 <- 1 - cv_mse / stats::var(y_vec)
          cv_r2_line <- paste0("CV R\u00b2: ", signif(cv_r2, 4))
        }
      }

      # Parameters
      alpha_str <- paste0("Alpha: ", rv$alpha_used)
      family_str <- paste0("Family: ", rv$family_used)
      nfolds_str <- if (!is.na(rv$nfolds_used)) {
        paste0("CV Folds: ", rv$nfolds_used)
      } else {
        "Lambda: manual"
      }

      extra_lines <- list()
      if (rv$n_excluded > 0) {
        extra_lines <- c(extra_lines, list(
          shiny::tags$br(),
          paste0("Weight-excluded rows: ", rv$n_excluded)
        ))
      }
      if (!is.null(gamma)) {
        extra_lines <- c(extra_lines, list(
          shiny::tags$br(),
          paste0("Relaxed gamma: ", signif(gamma, 3))
        ))
      }

      shiny::tagList(
        shiny::tags$div(
          class = "alert alert-success",
          shiny::tags$strong(
            paste0("Model fitted (fit #", count, ")")
          ),
          shiny::tags$br(),
          paste0(alpha_str, "  |  ", family_str, "  |  ", nfolds_str),
          shiny::tags$br(),
          paste0("Observations: ", rv$n_obs),
          extra_lines,
          shiny::tags$hr(style = "margin:4px 0;"),
          paste0("R\u00b2: ", signif(r_squared, 4),
                 "    Adj R\u00b2: ",
                 if (is.na(adj_r_squared)) "N/A"
                 else signif(adj_r_squared, 4)),
          if (!is.null(cv_r2_line)) {
            shiny::tagList(shiny::tags$br(), cv_r2_line)
          },
          shiny::tags$br(),
          paste0("RMSE: ", signif(rmse, 4)),
          shiny::tags$br(),
          paste0("Lambda: ", signif(lambda, 4)),
          shiny::tags$br(),
          paste0("Non-zero coefficients: ", n_nonzero,
                 " of ", ncol(x_mat)),
          if (is_cv) {
            shiny::tagList(
              shiny::tags$br(),
              paste0("lambda.min = ", signif(model$lambda.min, 4),
                     "    lambda.1se = ", signif(model$lambda.1se, 4))
            )
          }
        )
      )
    })

    return(list(
      model = shiny::reactive(rv$model),
      lambda = shiny::reactive(rv$lambda),
      gamma = shiny::reactive(rv$gamma_used),
      x_matrix = shiny::reactive(rv$x_matrix),
      y_vector = shiny::reactive(rv$y_vector),
      family = shiny::reactive(input$family),
      fitted = shiny::reactive(rv$fitted),
      fit_count = shiny::reactive(rv$fit_count)
    ))
  })
}
