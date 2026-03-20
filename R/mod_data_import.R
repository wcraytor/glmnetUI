#' Data Import Module UI
#'
#' UI component for the data import module. Provides file upload
#' and variable role assignment controls with per-variable
#' include checkboxes following the earthUI pattern.
#'
#' @param id Module namespace ID.
#'
#' @return A Shiny \code{tagList} containing the module UI elements.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(dataImportUI("data"))
#' }
dataImportUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$div(
      class = "glmnet-locale-row",
      style = "display:flex; align-items:center; justify-content:space-between; margin-bottom:4px;",
      shiny::tags$label(class = "control-label", "Choose CSV or Excel file"),
      shiny::tags$div(
        style = "display:flex; align-items:center; gap:6px;",
        shiny::tags$label(class = "control-label",
                          style = "margin-bottom:0;", "Locale"),
        shiny::tags$div(
          style = "width:150px; margin-bottom:0;",
          shiny::selectInput(ns("locale_import"), NULL,
                             choices = locale_country_choices_(),
                             selected = "us", width = "100%")
        )
      )
    ),
    shiny::fileInput(ns("file_input"), NULL,
                     accept = c(".csv", ".xls", ".xlsx")),
    shiny::conditionalPanel(
      condition = paste0("output['", ns("has_sheets"), "']"),
      shiny::selectInput(ns("sheet"), "Excel Sheet", choices = NULL)
    )
  )
}

#' Variable Configuration Module UI
#'
#' UI component for variable configuration: response, weight, and
#' predictor settings table. Uses the same module namespace as
#' [dataImportUI()] / [dataImportServer()].
#'
#' @param id Module namespace ID (must match the ID used in
#'   \code{dataImportServer}).
#'
#' @return A Shiny \code{tagList} containing the variable configuration UI.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(variableConfigUI("data"))
#' }
variableConfigUI <- function(id) {

  ns <- shiny::NS(id)

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

  label_with_help <- function(label, help_text) {
    shiny::tags$span(label, help_icon(help_text))
  }

  shiny::tagList(
    shiny::selectInput(
      ns("response"),
      label_with_help(
        "Response (Target) Variable",
        paste(
          "The variable you want to predict.",
          "Must be numeric for gaussian family.",
          "All other variables can be selected as predictors."
        )
      ),
      choices = NULL
    ),
    shiny::uiOutput(ns("response_type")),
    shiny::tags$h5("Predictor Settings"),
    shiny::helpText(
      shiny::tags$b("Inc"), " = include as predictor. ",
      shiny::tags$b("Force"), " = guarantee variable stays in the model",
      " (regularization cannot remove it). ",
      shiny::tags$b("Sign"), " = expected coefficient direction;",
      " used for warnings, and for hard constraints if",
      " 'Enforce Sign Constraints' is enabled."
    ),
    shiny::tags$div(
      class = "glmnet-var-table",
      style = paste0("max-height: 400px; overflow-y: auto; ",
                     "border: 1px solid #ddd; border-radius: 4px;"),
      shiny::uiOutput(ns("variable_table"))
    )
  )
}

#' Data Import Module - Data Preview UI
#'
#' UI component for the data preview table, intended for the main panel.
#'
#' @param id Module namespace ID (must match the ID used in
#'   \code{dataImportServer}).
#'
#' @return A Shiny \code{tagList} containing the data preview table.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(dataPreviewUI("data"))
#' }
dataPreviewUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::conditionalPanel(
      condition = "input.purpose === 'appraisal' || input.purpose === 'market'",
      shiny::h5("Subject Property"),
      DT::DTOutput(ns("preview_subjects")),
      shiny::h5("Comparable Sales"),
      DT::DTOutput(ns("preview_comps"))
    ),
    shiny::conditionalPanel(
      condition = "input.purpose === 'general'",
      DT::DTOutput(ns("preview_table"))
    )
  )
}

#' Data Import Module Server
#'
#' Server logic for the data import module. Handles file reading,
#' snake_case column name conversion, column type detection, variable
#' role management via per-variable include checkboxes, and settings
#' persistence via localStorage keyed by input filename.
#'
#' @param id Module namespace ID.
#' @param purpose Reactive returning the current purpose mode
#'   (\code{"general"}, \code{"appraisal"}, or \code{"market"}).
#'
#' @return A reactive list containing:
#' \describe{
#'   \item{data}{The imported data frame.}
#'   \item{response}{Selected response variable name.}
#'   \item{predictors}{Selected predictor variable names.}
#'   \item{expected_signs}{Named vector of expected signs per predictor.}
#'   \item{valid}{Logical, whether selections are valid for modeling.}
#'   \item{col_specials}{Named character vector of special column roles.}
#' }
#'
#' @export
#' @examples
#' if (interactive()) {
#'   server <- function(input, output, session) {
#'     data_out <- dataImportServer("data",
#'                                  shiny::reactiveVal("general"))
#'   }
#' }
dataImportServer <- function(id, purpose = shiny::reactiveVal("general")) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- shiny::reactiveValues(
      data = NULL,
      col_types = NULL,
      sheets = NULL,
      file_name = NULL
    )

    # Cache directory for persisting uploaded files across sessions
    cache_dir <- file.path(tools::R_user_dir("glmnetUI", "data"), "cache")
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

    # Load, cache, and populate helper
    load_and_cache_ <- function(path, name, sheet = 1L) {
      ext <- tolower(tools::file_ext(name))
      if (ext == "csv") {
        rv$data <- utils::read.csv(
          path, stringsAsFactors = FALSE,
          check.names = FALSE,
          sep = locale_csv_sep_(), dec = locale_csv_dec_()
        )
        rv$sheets <- NULL
      } else if (ext %in% c("xls", "xlsx")) {
        rv$sheets <- readxl::excel_sheets(path)
        rv$data <- as.data.frame(
          readxl::read_excel(path, sheet = sheet)
        )
        shiny::updateSelectInput(session, "sheet", choices = rv$sheets,
                                 selected = rv$sheets[sheet])
      } else {
        stop("Unsupported file type.")
      }

      rv$file_name <- name
      names(rv$data) <- to_snake_case(names(rv$data))
      rv$data <- auto_parse_dates_(rv$data)
      rv$col_types <- detect_column_types(rv$data)
      all_cols <- names(rv$data)

      default_response <- if (length(all_cols) > 0) all_cols[1] else NULL
      shiny::updateSelectInput(session, "response",
                               choices = all_cols,
                               selected = default_response)

      # Cache a copy for next session
      cached <- file.path(cache_dir, name)
      tryCatch(file.copy(path, cached, overwrite = TRUE),
               error = function(e) NULL)
      last_file <- file.path(cache_dir, ".last_data")
      tryCatch(writeLines(name, last_file), error = function(e) NULL)

      rv$data
    }

    # No auto-load: user must import data via the file input

    # --- File import ---
    shiny::observeEvent(input$file_input, {
      req_file <- input$file_input
      tryCatch({
        load_and_cache_(req_file$datapath, req_file$name)
      }, error = function(e) {
        shiny::showNotification(paste("Import error:", e$message),
                                type = "error")
      })
    })

    shiny::observeEvent(input$sheet, {
      if (is.null(rv$file_name)) return()
      # Only applies to Excel files
      ext <- tolower(tools::file_ext(rv$file_name))
      if (!ext %in% c("xls", "xlsx")) return()
      # Skip if sheets haven't been set yet (initial load)
      if (is.null(rv$sheets)) return()

      # Use cached file (Shiny temp files may be gone)
      cached_path <- file.path(cache_dir, rv$file_name)
      req_file <- input$file_input
      path <- if (!is.null(req_file) && file.exists(req_file$datapath)) {
        req_file$datapath
      } else if (file.exists(cached_path)) {
        cached_path
      } else {
        return()
      }
      tryCatch({
        rv$data <- as.data.frame(
          readxl::read_excel(path, sheet = input$sheet)
        )
        names(rv$data) <- to_snake_case(names(rv$data))
        rv$data <- auto_parse_dates_(rv$data)
        rv$col_types <- detect_column_types(rv$data)
        all_cols <- names(rv$data)

        default_response <- if (length(all_cols) > 0) all_cols[1] else NULL
        shiny::updateSelectInput(session, "response",
                                 choices = all_cols,
                                 selected = default_response)

      }, error = function(e) {
        shiny::showNotification(paste("Sheet error:", e$message),
                                type = "error")
      })
    })

    # --- Candidate columns: everything except response ---
    candidates <- shiny::reactive({
      shiny::req(rv$data, input$response)
      setdiff(names(rv$data), input$response)
    })

    # --- Response type indicator ---
    output$response_type <- shiny::renderUI({
      shiny::req(rv$data, input$response, rv$col_types)
      resp <- input$response
      rtype <- rv$col_types[resp]
      is_ok <- rtype %in% c("numeric", "integer")
      color <- if (is_ok) "var(--bs-success)" else "var(--bs-danger)"
      msg <- if (is_ok) {
        paste0("Type: ", rtype)
      } else {
        paste0("Type: ", rtype, " \u2014 must be numeric for modeling")
      }
      shiny::tags$div(
        style = paste0("font-size:12px; color:", color,
                       "; margin-top:-8px; margin-bottom:8px;"),
        msg
      )
    })

    # --- Variable table with Inc? checkboxes, type, expected sign, NAs ---
    output$variable_table <- shiny::renderUI({
      cols <- candidates()
      if (length(cols) == 0) return(shiny::helpText("No candidate predictors."))

      types <- rv$col_types
      df <- rv$data
      file_key <- rv$file_name

      # CSS for flexbox rows (colors handled via classes for dark mode)
      row_css <- "display:flex; align-items:center; padding:3px 6px;"
      hdr_css <- paste0(row_css, " font-weight:bold;")

      all_types <- c("numeric", "integer", "character", "factor",
                     "Date", "POSIXct")

      appraiser <- purpose() %in% c("appraisal", "market")
      special_options <- if (appraiser) {
        c("no", "weight", "actual_age", "area", "concessions",
          "contract_date", "display_only", "dom", "effective_age",
          "latitude", "listing_date", "living_area", "longitude",
          "lot_size", "sale_age", "site_dimensions")
      } else {
        c("no", "weight")
      }

      hdr_cols <- list(
        shiny::tags$div(style = "flex:1; min-width:100px;", "Variable"),
        shiny::tags$div(style = "width:90px; text-align:center;", "Type"),
        shiny::tags$div(style = "width:40px; text-align:center;", "Inc"),
        shiny::tags$div(style = "width:40px; text-align:center;", "Force"),
        shiny::tags$div(style = "width:120px; text-align:center;", "Special")
      )
      hdr_cols <- c(hdr_cols, list(
        shiny::tags$div(style = "width:80px; text-align:center;", "Sign"),
        shiny::tags$div(style = "width:45px; text-align:center;", "NAs")
      ))
      header <- shiny::tags$div(
        class = "glmnet-var-hdr",
        style = hdr_css,
        hdr_cols
      )

      rows <- lapply(seq_along(cols), function(i) {
        col_name <- cols[i]
        col_type <- types[col_name]
        na_count <- sum(is.na(df[[col_name]]))
        na_style <- if (na_count > nrow(df) * 0.3) "color:red;" else ""

        type_options <- lapply(all_types, function(tp) {
          if (tp == col_type) {
            shiny::tags$option(value = tp, selected = "selected", tp)
          } else {
            shiny::tags$option(value = tp, tp)
          }
        })
        type_el <- shiny::tags$select(
          id = ns(paste0("type_", col_name)),
          class = "form-control glmnet-type-sel",
          style = "padding:2px; height:auto; font-size:11px;",
          `data-col` = col_name,
          type_options
        )

        sign_el <- shiny::tags$select(
          id = ns(paste0("sign_", col_name)),
          class = "form-control glmnet-sign-sel",
          style = "padding:2px; height:auto; font-size:12px;",
          `data-col` = col_name,
          shiny::tags$option(value = "either", "either"),
          shiny::tags$option(value = "positive", "positive"),
          shiny::tags$option(value = "negative", "negative")
        )

        special_opts <- lapply(special_options, function(sp) {
          shiny::tags$option(value = sp, sp)
        })
        special_el <- shiny::tags$div(
          style = "width:120px; text-align:center;",
          shiny::tags$select(
            id = ns(paste0("special_", col_name)),
            class = "form-control glmnet-special-sel",
            style = "padding:2px; height:auto; font-size:11px;",
            `data-col` = col_name,
            special_opts
          )
        )

        row_cells <- list(
          shiny::tags$div(
            style = "flex:1; min-width:100px; font-size:13px;",
            col_name
          ),
          shiny::tags$div(
            style = "width:90px; text-align:center;",
            type_el
          ),
          shiny::tags$div(
            style = "width:40px; text-align:center;",
            shiny::tags$input(
              type = "checkbox",
              id = ns(paste0("inc_", col_name)),
              class = "glmnet-var-cb",
              `data-col` = col_name
            )
          ),
          shiny::tags$div(
            style = "width:40px; text-align:center;",
            shiny::tags$input(
              type = "checkbox",
              id = ns(paste0("force_", col_name)),
              class = "glmnet-force-cb",
              `data-col` = col_name
            )
          )
        )
        row_cells <- c(row_cells, list(special_el))
        row_cells <- c(row_cells, list(
          shiny::tags$div(
            style = "width:80px; text-align:center;",
            sign_el
          ),
          shiny::tags$div(
            style = paste0("width:45px; text-align:center; font-size:12px;",
                           na_style),
            as.character(na_count)
          )
        ))

        shiny::tags$div(
          class = "glmnet-var-row",
          style = row_css,
          row_cells
        )
      })

      # JavaScript: sync state, save/restore from localStorage by filename
      col_names_json <- jsonlite::toJSON(cols, auto_unbox = FALSE)
      js_code <- sprintf('
        (function() {
          var cols = %s;
          var nsPrefix = "%s";
          var fileKey = %s;
          var storageKey = "glmnetUI_vars_" + fileKey;

          function gatherState() {
            var preds = [];
            var types = {};
            var forceVars = [];
            var specials = {};
            for (var i = 0; i < cols.length; i++) {
              var cb = document.getElementById(nsPrefix + "inc_" + cols[i]);
              var tp = document.getElementById(nsPrefix + "type_" + cols[i]);
              var fcb = document.getElementById(nsPrefix + "force_" + cols[i]);
              var sp = document.getElementById(nsPrefix + "special_" + cols[i]);
              if (cb && cb.checked) {
                preds.push(cols[i]);
              }
              if (tp) {
                types[cols[i]] = tp.value;
              }
              if (fcb && fcb.checked) forceVars.push(cols[i]);
              if (sp) specials[cols[i]] = sp.value;
            }
            Shiny.setInputValue(nsPrefix + "predictors", preds,
                                {priority: "event"});
            Shiny.setInputValue(nsPrefix + "col_types_override", types,
                                {priority: "event"});
            Shiny.setInputValue(nsPrefix + "force_vars", forceVars,
                                {priority: "event"});
            Shiny.setInputValue(nsPrefix + "col_specials", specials,
                                {priority: "event"});
          }

          function saveState() {
            var state = {};
            for (var i = 0; i < cols.length; i++) {
              var cb = document.getElementById(nsPrefix + "inc_" + cols[i]);
              var sel = document.getElementById(nsPrefix + "sign_" + cols[i]);
              var tp = document.getElementById(nsPrefix + "type_" + cols[i]);
              var fcb = document.getElementById(nsPrefix + "force_" + cols[i]);
              var sp = document.getElementById(nsPrefix + "special_" + cols[i]);
              state[cols[i]] = {
                inc: cb ? cb.checked : false,
                sign: sel ? sel.value : "either",
                type: tp ? tp.value : "character",
                force: fcb ? fcb.checked : false,
                special: sp ? sp.value : "no"
              };
            }
            try {
              localStorage.setItem(storageKey, JSON.stringify(state));
            } catch(e) {}
          }

          function restoreState() {
            var saved = null;
            try {
              var raw = localStorage.getItem(storageKey);
              if (raw) saved = JSON.parse(raw);
            } catch(e) {}

            for (var i = 0; i < cols.length; i++) {
              var cb = document.getElementById(nsPrefix + "inc_" + cols[i]);
              var sel = document.getElementById(nsPrefix + "sign_" + cols[i]);
              var tp = document.getElementById(nsPrefix + "type_" + cols[i]);
              var fcb = document.getElementById(nsPrefix + "force_" + cols[i]);
              if (saved && saved[cols[i]]) {
                if (cb) cb.checked = saved[cols[i]].inc;
                if (sel) sel.value = saved[cols[i]].sign || "either";
                if (tp && saved[cols[i]].type) tp.value = saved[cols[i]].type;
                if (fcb && saved[cols[i]].force) fcb.checked = saved[cols[i]].force;
                var sp = document.getElementById(nsPrefix + "special_" + cols[i]);
                if (sp && saved[cols[i]].special) sp.value = saved[cols[i]].special;
              }
              // Sync sign selects to Shiny
              if (sel) {
                Shiny.setInputValue(nsPrefix + "sign_" + cols[i], sel.value);
              }
            }
          }

          // Event handlers
          $(document).off("change.glmnetvar").on("change.glmnetvar",
            ".glmnet-var-cb", function() {
              gatherState();
              saveState();
            });

          $(document).off("change.glmnetsign").on("change.glmnetsign",
            ".glmnet-sign-sel", function() {
              var col = $(this).data("col");
              Shiny.setInputValue(nsPrefix + "sign_" + col, this.value);
              saveState();
            });

          $(document).off("change.glmnettype").on("change.glmnettype",
            ".glmnet-type-sel", function() {
              gatherState();
              saveState();
            });

          $(document).off("change.glmnetforce").on("change.glmnetforce",
            ".glmnet-force-cb", function() {
              gatherState();
              saveState();
            });

          $(document).off("change.glmnetspecial").on("change.glmnetspecial",
            ".glmnet-special-sel", function() {
              gatherState();
              saveState();
            });

          // --- Response variable save/restore (selectize API) ---
          var respKey = "glmnetUI_response_" + fileKey;
          var respId = nsPrefix + "response";

          // Save response on change via selectize
          var respAttempts = 0;
          function getSelectize() {
            var $el = $("#" + CSS.escape(respId));
            if ($el.length && $el[0].selectize) return $el[0].selectize;
            return null;
          }

          function bindRespSave() {
            var sz = getSelectize();
            if (sz) {
              sz.on("change", function(value) {
                try {
                  localStorage.setItem(respKey, value);
                } catch(e) {}
              });
              return true;
            }
            return false;
          }

          // Restore response with polling (earthUI pattern)
          function tryRestoreResponse() {
            var sz = getSelectize();
            if (sz && sz.isSetup && Object.keys(sz.options).length > 0) {
              bindRespSave();
              var savedResp = null;
              try { savedResp = localStorage.getItem(respKey); } catch(e) {}
              if (savedResp && sz.options[savedResp] &&
                  sz.getValue() !== savedResp) {
                sz.setValue(savedResp, false);
              }
            } else if (respAttempts < 40) {
              respAttempts++;
              setTimeout(tryRestoreResponse, 250);
            }
          }
          tryRestoreResponse();

          // Restore saved predictor state, then sync
          restoreState();
          setTimeout(gatherState, 50);
        })();
      ', col_names_json, ns(""),
                         jsonlite::toJSON(file_key, auto_unbox = TRUE))

      shiny::tagList(
        header,
        rows,
        shiny::tags$script(shiny::HTML(js_code))
      )
    })

    # --- Reactive outputs for conditionalPanel ---
    output$data_loaded <- shiny::reactive({ !is.null(rv$data) })
    shiny::outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

    output$has_sheets <- shiny::reactive({ !is.null(rv$sheets) })
    shiny::outputOptions(output, "has_sheets", suspendWhenHidden = FALSE)

    # --- Data preview (rendered in main panel via dataPreviewUI) ---
    # General mode: single table
    output$preview_table <- DT::renderDT({
      shiny::req(rv$data, purpose() == "general")
      DT::datatable(rv$data,
                    options = list(scrollX = TRUE, pageLength = 15),
                    rownames = FALSE, class = "compact stripe")
    })

    # Appraisal/Market mode: Subject Property (row 1)
    output$preview_subjects <- DT::renderDT({
      shiny::req(rv$data, purpose() %in% c("appraisal", "market"),
                 nrow(rv$data) >= 1L)
      resp <- input$response

      subj <- rv$data[1L, , drop = FALSE]
      # Set response to NA for subject
      if (!is.null(resp) && resp %in% names(subj)) subj[[resp]] <- NA

      DT::datatable(subj,
                    options = list(scrollX = TRUE, dom = "t"),
                    rownames = FALSE, class = "compact stripe")
    })

    # Appraisal/Market mode: Comparable Sales (rows 2+)
    output$preview_comps <- DT::renderDT({
      shiny::req(rv$data, purpose() %in% c("appraisal", "market"),
                 nrow(rv$data) >= 2L)

      comps <- rv$data[2:nrow(rv$data), , drop = FALSE]

      DT::datatable(comps,
                    options = list(scrollX = TRUE, pageLength = 15),
                    rownames = FALSE, class = "compact stripe")
    })

    # --- Effective column types (auto-detected, then user overrides) ---
    effective_types <- shiny::reactive({
      base <- rv$col_types
      overrides <- input$col_types_override
      if (!is.null(overrides) && is.list(overrides)) {
        for (nm in names(overrides)) {
          if (nm %in% names(base)) {
            base[nm] <- overrides[[nm]]
          }
        }
      }
      base
    })

    # --- Derived reactives ---
    expected_signs <- shiny::reactive({
      preds <- input$predictors
      etypes <- effective_types()
      if (is.null(preds) || is.null(etypes)) {
        return(stats::setNames(character(0), character(0)))
      }
      numeric_preds <- preds[
        preds %in% names(etypes)[etypes %in% c("numeric", "integer")]
      ]
      if (length(numeric_preds) == 0) {
        return(stats::setNames(character(0), character(0)))
      }
      signs <- vapply(numeric_preds, function(var) {
        val <- input[[paste0("sign_", var)]]
        if (is.null(val)) "either" else val
      }, FUN.VALUE = character(1))
      signs
    })

    is_valid <- shiny::reactive({
      !is.null(rv$data) &&
        !is.null(input$response) && nzchar(input$response) &&
        !is.null(input$predictors) && length(input$predictors) > 0
    })

    force_in <- shiny::reactive({
      fv <- input$force_vars
      if (is.null(fv)) character(0) else fv
    })

    col_specials <- shiny::reactive({
      sp <- input$col_specials
      if (is.null(sp) || !is.list(sp)) {
        return(stats::setNames(character(0), character(0)))
      }
      unlist(sp)
    })

    return(list(
      data = shiny::reactive(rv$data),
      response = shiny::reactive(input$response),
      predictors = shiny::reactive(input$predictors),
      expected_signs = expected_signs,
      valid = is_valid,
      col_types = effective_types,
      weight_col = shiny::reactive({
        sp <- col_specials()
        if (is.null(sp) || length(sp) == 0) return(NULL)
        wt_idx <- which(sp == "weight")
        if (length(wt_idx) == 0) return(NULL)
        names(sp)[wt_idx[1L]]
      }),
      force_in = force_in,
      col_specials = col_specials,
      file_name = shiny::reactive(rv$file_name),
      rv = rv
    ))
  })
}

#' Convert Column Names to snake_case
#'
#' Converts column names to snake_case by replacing spaces, dots,
#' camelCase boundaries, and other non-alphanumeric characters with
#' underscores, then lowercasing.
#'
#' @param nms Character vector of column names.
#'
#' @return Character vector of snake_case names. Duplicate names are
#'   made unique with numeric suffixes.
#'
#' @export
#' @examples
#' to_snake_case(c("SalePrice", "Lot.Size", "Total Sq Ft", "AGE"))
to_snake_case <- function(nms) {
  out <- nms
  # Insert underscore before uppercase preceded by lowercase (camelCase)
  out <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", out)
  # Replace spaces, dots, dashes, and other separators with underscore
  out <- gsub("[^A-Za-z0-9]+", "_", out)
  # Lowercase
  out <- tolower(out)
  # Remove leading/trailing underscores
  out <- gsub("^_+|_+$", "", out)
  # Collapse multiple underscores
  out <- gsub("_+", "_", out)
  # Ensure unique
  make.unique(out, sep = "_")
}

#' Detect Column Types
#'
#' Classifies columns using R type names: \code{"numeric"},
#' \code{"integer"}, \code{"Date"}, \code{"POSIXct"},
#' \code{"factor"}, or \code{"character"}.
#'
#' @param df A data frame.
#'
#' @return A named character vector of R type names, one per column.
#'
#' @export
#' @examples
#' df <- data.frame(x = 1:10, y = letters[1:10],
#'                  d = Sys.Date() + 1:10, stringsAsFactors = FALSE)
#' detect_column_types(df)
detect_column_types <- function(df) {
  vapply(df, function(col) {
    if (inherits(col, "POSIXct") || inherits(col, "POSIXlt")) {
      "POSIXct"
    } else if (inherits(col, "Date")) {
      "Date"
    } else if (is.integer(col)) {
      "integer"
    } else if (is.numeric(col)) {
      "numeric"
    } else if (is.factor(col)) {
      "factor"
    } else {
      n_unique <- length(unique(stats::na.omit(col)))
      if (n_unique <= 10 && n_unique < length(col) / 2) {
        "factor"
      } else {
        "character"
      }
    }
  }, FUN.VALUE = character(1))
}

# Try to parse character columns as POSIXct dates.
# Modifies the data frame in place and returns it.
#' @noRd
auto_parse_dates_ <- function(df) {
  date_fmts <- locale_date_formats_()
  for (nm in names(df)) {
    col <- df[[nm]]
    if (!is.character(col)) next
    vals <- stats::na.omit(col)
    if (length(vals) == 0L) next
    # Sample up to 20 non-NA values for detection
    sample_vals <- if (length(vals) > 20L) vals[1:20] else vals

    # Detect if years are 2-digit: check if any date-separator-delimited
    # part is <= 2 digits where a year would be.
    # If sample contains only short tokens (no 4-digit year), prefer %y.
    has_4digit_year <- any(grepl("\\b\\d{4}\\b", sample_vals))

    # Build format list: if no 4-digit year found, try 2-digit first
    try_fmts <- if (has_4digit_year) {
      date_fmts
    } else {
      # Put 2-digit year formats first
      two_digit <- grep("%y(?!%)", date_fmts, perl = TRUE, value = TRUE)
      four_digit <- setdiff(date_fmts, two_digit)
      c(two_digit, four_digit)
    }

    # Try each format
    for (fmt in try_fmts) {
      parsed <- suppressWarnings(as.POSIXct(sample_vals, format = fmt))
      if (all(!is.na(parsed))) {
        # Format matches sample — parse the full column
        df[[nm]] <- suppressWarnings(as.POSIXct(col, format = fmt))
        break
      }
    }
  }
  df
}
