bslib::page_fluid(
  theme = nord_light,
  shiny::withMathJax(),

  # --- Adaptive CSS using Bootstrap CSS variables ---
  shiny::tags$style(shiny::HTML('
    /* Theme toggle button */
    #glmnet-theme-toggle {
      width: 38px; height: 38px; border-radius: 50%;
      border: 2px solid var(--bs-border-color);
      background: var(--bs-body-bg);
      color: var(--bs-body-color);
      font-size: 18px; cursor: pointer;
      display: flex; align-items: center; justify-content: center;
      box-shadow: 0 2px 6px rgba(0,0,0,0.15); transition: all 0.3s;
    }
    #glmnet-theme-toggle:hover {
      box-shadow: 0 2px 10px rgba(0,0,0,0.25);
    }

    /* Variable table adapts via BS CSS vars */
    .glmnet-var-table {
      border-color: var(--bs-border-color);
    }
    .glmnet-var-hdr {
      background: var(--bs-tertiary-bg);
      border-bottom: 1px solid var(--bs-border-color);
      color: var(--bs-body-color);
      position: sticky;
      top: 0;
      z-index: 2;
    }
    .glmnet-var-row {
      border-bottom: 1px solid var(--bs-border-color);
      color: var(--bs-body-color);
    }
    .glmnet-var-row select {
      background-color: var(--bs-body-bg);
      color: var(--bs-body-color);
      border-color: var(--bs-border-color);
    }
    .glmnet-var-row span {
      color: var(--bs-secondary-color);
    }

    /* DT DataTables: adapt to current theme */
    .dataTables_wrapper {
      color: var(--bs-body-color);
    }
    table.dataTable {
      color: var(--bs-body-color);
      border-color: var(--bs-border-color);
    }
    table.dataTable thead th,
    table.dataTable thead td {
      background-color: var(--bs-tertiary-bg) !important;
      color: var(--bs-body-color) !important;
      border-color: var(--bs-border-color) !important;
    }
    table.dataTable tbody td {
      background-color: var(--bs-body-bg);
      color: var(--bs-body-color);
      border-color: var(--bs-border-color);
    }
    table.dataTable tbody tr:hover td {
      background-color: var(--bs-tertiary-bg);
    }
    table.dataTable tbody tr.odd td {
      background-color: var(--bs-secondary-bg);
    }
    .dataTables_info,
    .dataTables_length,
    .dataTables_filter,
    .dataTables_paginate {
      color: var(--bs-body-color);
    }
    .dataTables_paginate .paginate_button {
      color: var(--bs-body-color) !important;
      background: var(--bs-body-bg);
      border-color: var(--bs-border-color);
    }
    .dataTables_paginate .paginate_button.current {
      color: var(--bs-body-color) !important;
      background: var(--bs-tertiary-bg) !important;
      border-color: var(--bs-border-color) !important;
    }
    .dataTables_paginate .paginate_button:hover {
      color: var(--bs-body-color) !important;
      background: var(--bs-secondary-bg) !important;
      border-color: var(--bs-border-color) !important;
    }
    .dataTables_filter input {
      background-color: var(--bs-body-bg);
      color: var(--bs-body-color);
      border-color: var(--bs-border-color);
    }
    .dataTables_length select {
      background-color: var(--bs-body-bg);
      color: var(--bs-body-color);
      border-color: var(--bs-border-color);
    }

    /* Alert success: readable in both modes */
    .alert-success {
      background-color: color-mix(in srgb, var(--bs-success) 20%, var(--bs-body-bg));
      color: color-mix(in srgb, var(--bs-success) 60%, var(--bs-body-color));
      border-color: var(--bs-success);
    }

    /* Wrap long file paths in notifications */
    .shiny-notification { word-wrap: break-word; overflow-wrap: anywhere; }

    /* Settings dropdown on title bar */
    .glmnet-navbar {
      display: flex; align-items: center; padding: 10px 15px; gap: 8px;
      flex-wrap: wrap;
    }
    .glmnet-navbar .dropdown { position: relative; }
    .glmnet-navbar .glmnet-menu-btn {
      background: none; border: 1px solid var(--bs-border-color);
      color: var(--bs-body-color); font-size: 0.9em;
      padding: 6px 12px; cursor: pointer; border-radius: 4px;
    }
    .glmnet-navbar .glmnet-menu-btn:hover {
      background: var(--bs-tertiary-bg);
    }
    .glmnet-navbar .glmnet-dropdown-menu {
      display: none; position: absolute; top: 100%; left: 0;
      background: var(--bs-body-bg);
      border: 1px solid var(--bs-border-color);
      border-radius: 6px; padding: 12px 16px;
      min-width: 280px; z-index: 10001;
      box-shadow: 0 4px 16px rgba(0,0,0,0.2);
    }
    .glmnet-navbar .dropdown.open .glmnet-dropdown-menu { display: block; }
    .glmnet-navbar .glmnet-spacer { flex: 1; }

    /* Locale row in import section */
    .glmnet-locale-row .form-group { margin-bottom: 0; }

    /* Help icon (?) with Bootstrap popover */
    .glmnet-param-help {
      position: absolute; top: 0; right: 0;
      width: 18px; height: 18px; border-radius: 50%;
      background: #88c0d0; color: #2e3440;
      font-size: 11px; font-weight: bold;
      text-align: center; line-height: 18px;
      cursor: pointer; z-index: 10;
    }
    .glmnet-param-help:hover { background: #5e81ac; color: #eceff4; }
    .glmnet-help-icon {
      display: inline-block; width: 18px; height: 18px;
      border-radius: 50%; background: #88c0d0; color: #2e3440;
      text-align: center; font-size: 11px; font-weight: bold;
      line-height: 18px; cursor: pointer;
      margin-left: 6px; vertical-align: middle;
    }
    .glmnet-help-icon:hover { background: #5e81ac; color: #eceff4; }

    /* --- shinyFiles browse: mimic native fileInput btn-file appearance --- */
    .glmnet-earth-browse .shinyFiles {
      width: auto !important;
      flex: 0 0 auto;
      background-color: transparent !important;
      border: 1px solid var(--bs-secondary) !important;
      color: var(--bs-secondary) !important;
      border-top-left-radius: 0.375rem !important;
      border-bottom-left-radius: 0.375rem !important;
      border-top-right-radius: 0 !important;
      border-bottom-right-radius: 0 !important;
      font-size: 1rem;
      padding: 0.375rem 0.75rem;
      line-height: 1.5;
      height: auto;
    }
    .glmnet-earth-browse .shinyFiles:hover {
      background-color: var(--bs-secondary) !important;
      color: var(--bs-body-bg) !important;
    }
    .glmnet-earth-browse .form-control {
      font-size: 1rem;
    }

    /* Collapsible sections */
    .glmnet-section > summary {
      cursor: pointer;
      list-style: none;
    }
    .glmnet-section > summary::-webkit-details-marker {
      display: none;
    }
    .glmnet-section > summary h4 {
      display: flex; align-items: center; margin: 0;
    }
    .glmnet-section > summary h4::before {
      content: "\\25B6";
      font-size: 0.7em;
      transition: transform 0.2s;
      display: inline-block;
      margin-right: 0.8em;
      flex-shrink: 0;
    }
    .glmnet-section-info {
      display: inline-flex; align-items: center; justify-content: center;
      width: 16px; height: 16px; flex-shrink: 0;
      border-radius: 50%; background: #88c0d0; color: #2e3440;
      font-size: 10px; font-weight: bold;
      cursor: pointer; margin-left: auto;
    }
    .glmnet-section-info:hover { background: #5e81ac; color: #eceff4; }
    .glmnet-section[open] > summary h4::before {
      transform: rotate(90deg);
    }

    /* Earth-sourced interaction cells in the interaction matrix */
    .glmnet-int-earth-cell {
      background-color: rgba(235, 203, 139, 0.35);
    }
    .glmnet-int-earth {
      cursor: not-allowed;
      opacity: 0.8;
    }
    [data-glmnet-theme="dark"] .glmnet-int-earth-cell {
      background-color: rgba(235, 203, 139, 0.2);
    }
    [data-glmnet-theme="dark"] .glmnet-var-hdr { background: #3b4252; }
    [data-glmnet-theme="dark"] .glmnet-var-row { border-color: #434c5e; }
    [data-glmnet-theme="dark"] .glmnet-var-row select {
      background: #3b4252 !important; color: #d8dee9 !important;
      border-color: #434c5e !important;
    }
    [data-glmnet-theme="dark"] details > summary { color: #d8dee9 !important; }
    [data-glmnet-theme="dark"] .nav-tabs .nav-link.active {
      color: #d8dee9 !important; background-color: #2e3440 !important;
      border-color: #434c5e #434c5e #2e3440 !important;
    }
    [data-glmnet-theme="dark"] .nav-tabs .nav-link { color: #81a1c1; }
    [data-glmnet-theme="dark"] .nav-tabs .nav-link:hover {
      color: #d8dee9; border-color: #434c5e;
    }
    [data-glmnet-theme="dark"] .modal .btn-outline-primary { color: #88c0d0; border-color: #88c0d0; }
    [data-glmnet-theme="dark"] .modal .btn-outline-primary:hover { background: #88c0d0; color: #2e3440; }
    [data-glmnet-theme="dark"] .modal .btn-outline-secondary { color: #81a1c1; border-color: #81a1c1; }
    [data-glmnet-theme="dark"] .modal .btn-outline-secondary:hover { background: #81a1c1; color: #2e3440; }
  ')),

  # --- Theme toggle button (repositioned into navbar below) ---

  # --- Bootstrap popover initializer for "?" help icons ---
  shiny::tags$script(shiny::HTML('
    $(document).on("shiny:connected", function() {
      function initPopovers() {
        var els = document.querySelectorAll("[data-bs-toggle=\\"popover\\"]");
        els.forEach(function(el) {
          if (!bootstrap.Popover.getInstance(el)) {
            new bootstrap.Popover(el, { html: false, container: "body" });
          }
        });
      }
      initPopovers();
      var obs = new MutationObserver(function() {
        setTimeout(initPopovers, 200);
      });
      obs.observe(document.body, { childList: true, subtree: true });
    });
  ')),

  # --- Settings dropdown + Theme toggle JS + settings message handlers ---
  shiny::tags$script(src = "glmnetui.js"),

  shiny::tags$head(
    shiny::tags$link(rel = "icon", type = "image/png", href = "favicon.png")
  ),

  shiny::tags$nav(class = "glmnet-navbar",
    shiny::tags$h2(
      shiny::tags$img(src = "logo.png", height = "32px",
                      style = "margin-right: 8px; vertical-align: middle;"),
      "glmnetUI",
      shiny::tags$small(" - Interactive Elastic Net Modeling",
                        style = "font-size: 0.6em; color: var(--bs-secondary-color);"),
      style = "margin: 0;"
    ),
    shiny::tags$div(class = "dropdown", id = "glmnet-settings-dropdown",
      shiny::tags$button(
        class = "glmnet-menu-btn",
        onclick = "glmnetToggleDropdown('glmnet-settings-dropdown')",
        shiny::HTML("&#9881; Settings")
      ),
      shiny::tags$div(class = "glmnet-dropdown-menu",
        shiny::selectInput("locale_country", "Country",
                           choices = glmnetUI:::locale_country_choices_(),
                           selected = "us", width = "100%"),
        shiny::selectInput("locale_paper", "Paper",
                           choices = c("Letter" = "letter", "A4" = "a4"),
                           selected = "letter", width = "100%"),
        shiny::actionLink("locale_save_default", "Save as my default",
                          style = "font-size: 0.85em; color: #5e81ac; display: block; margin-top: 4px;")
      )
    ),
    shiny::tags$div(class = "glmnet-spacer"),
    shiny::tags$button(id = "glmnet-theme-toggle", shiny::HTML("&#9790;"))
  ),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,

      # --- Purpose Mode ---
      shiny::tags$div(
        style = "font-weight:bold;",
        shiny::radioButtons("purpose", "Purpose:",
                            choices = c("General" = "general",
                                        "For Appraisal" = "appraisal",
                                        "Market Area Analysis" = "market"),
                            selected = "general", inline = TRUE)
      ),
      shiny::conditionalPanel(
        condition = "input.purpose !== 'appraisal'",
        shiny::checkboxInput("skip_subject_row", "Skip first row",
                             value = FALSE)
      ),
      shiny::hr(),

      # --- 1. Import Data ---
      shiny::tags$details(class = "glmnet-section",
        shiny::tags$summary(shiny::h4(
          "1. Import Data",
          shiny::tags$span(class = "glmnet-section-info",
            `data-bs-toggle` = "popover", `data-bs-trigger` = "hover",
            `data-bs-content` = "Load a CSV data file containing the response variable, predictors, and optional weight column.",
            "?"),
        )),
        dataImportUI("data")
      ),
      shiny::hr(),

      # --- 2. Import from earthUI (optional) ---
      shiny::tags$details(class = "glmnet-section",
        shiny::tags$summary(shiny::h4(
          "2. Import from earthUI (optional)",
          shiny::tags$span(class = "glmnet-section-info",
            `data-bs-toggle` = "popover", `data-bs-trigger` = "hover",
            `data-bs-content` = "Import an earthUI result (.rds) to use earth's hinge basis functions for nonlinear modeling. Earth defines the basis; glmnet applies regularization.",
            "?")
        )),
        shiny::tags$div(style = "padding-top: 6px;",
          earthImportUI("earth")
        )
      ),
      shiny::hr(),

      shiny::conditionalPanel(
        condition = "output['data-data_loaded']",

        # --- 3. Project Output Folder ---
        shiny::tags$details(class = "glmnet-section",
          shiny::tags$summary(shiny::h4(
            "3. Project Output Folder",
            shiny::tags$span(class = "glmnet-section-info",
              `data-bs-toggle` = "popover", `data-bs-trigger` = "hover",
              `data-bs-content` = "Set the directory where output files (Excel, PDF reports, sales grids) will be saved.",
              "?")
          )),
          shiny::textInput("output_folder", NULL,
                           value = path.expand("~/Downloads"))
        ),
        shiny::hr(),

        # --- 4. Variable Configuration ---
        shiny::tags$details(class = "glmnet-section",
          shiny::tags$summary(shiny::h4(
            "4. Variable Configuration",
            shiny::tags$span(class = "glmnet-section-info",
              `data-bs-toggle` = "popover", `data-bs-trigger` = "hover",
              `data-bs-content` = "Select the response variable, predictors, weight column, factor variables, expected signs, and force-in variables.",
              "?")
          )),
          shiny::conditionalPanel(
            condition = "input.purpose !== 'general'",
            shiny::dateInput("effective_date", "Effective Date",
                             value = Sys.Date())
          ),
          variableConfigUI("data")
        ),
        shiny::hr(),

        # --- 5. glmnet Call Parameters ---
        shiny::tags$details(class = "glmnet-section",
          shiny::tags$summary(shiny::h4(
            "5. glmnet Call Parameters",
            shiny::tags$span(class = "glmnet-section-info",
              `data-bs-toggle` = "popover", `data-bs-trigger` = "hover",
              `data-bs-content` = "Configure alpha (ridge/lasso mix), lambda selection method (CV or manual), cross-validation folds, and advanced parameters.",
              "?")
          )),
          shiny::tags$div(
            style = "margin-bottom: 4px; font-size: 0.85em;",
            shiny::radioButtons(
              "glmnet_defaults_action", NULL,
              choices = c("Use last settings for input file" = "last",
                          "Use default settings" = "use_default",
                          "glmnet defaults" = "glmnet_defaults"),
              selected = "last", inline = TRUE
            )
          ),
          shiny::actionButton(
            "glmnet_save_defaults", "Save current as default",
            class = "btn-dark btn-sm",
            style = paste0("padding: 2px 8px; font-size: 0.85em; ",
                           "margin-bottom: 8px;")
          ),
          modelingUI("model")
        ),
        shiny::hr(),

        # --- 6. Fit Glmnet Model ---
        shiny::tags$details(class = "glmnet-section",
          shiny::tags$summary(shiny::h4(
            "6. Fit Glmnet Model",
            shiny::tags$span(class = "glmnet-section-info",
              `data-bs-toggle` = "popover", `data-bs-trigger` = "hover",
              `data-bs-content` = "Fit the elastic net model with the configured parameters. Results appear in the tabs on the right.",
              "?")
          )),
          fitModelUI("model")
        ),

        # --- 7. Download Estimated Sale Prices & Residuals ---
        shiny::conditionalPanel(
          condition = "output.model_fitted",
          shiny::hr(),
          shiny::tags$details(class = "glmnet-section",
            shiny::tags$summary(shiny::uiOutput("download_heading",
                                                 inline = TRUE)),
            shiny::actionButton("export_data", "Download Output (Excel)",
                                class = "btn-primary",
                                style = "width: 100%;")
          )
        ),

        # --- 8. Calculate RCA Adjustments (Appraisal only) ---
        shiny::conditionalPanel(
          condition = "output.model_fitted && input.purpose === 'appraisal'",
          shiny::hr(),
          shiny::tags$details(class = "glmnet-section",
            shiny::tags$summary(shiny::h4(
              "8. Calculate RCA Adjustments & Download",
              shiny::tags$span(class = "glmnet-section-info",
                `data-bs-toggle` = "popover", `data-bs-trigger` = "hover",
                `data-bs-content` = "Compute per-variable RCA adjustments for each comparable, interpolate the subject residual via CQA, and download the results.",
                "?")
            )),
            shiny::actionButton("rca_output_btn",
                                "Calculate RCA Adjustments & Download",
                                class = "btn-primary",
                                style = "width: 100%;")
          )
        ),

        # --- 9. Generate Sales Grid (Appraisal only) ---
        shiny::conditionalPanel(
          condition = "output.rca_computed && input.purpose === 'appraisal'",
          shiny::hr(),
          shiny::tags$details(class = "glmnet-section",
            shiny::tags$summary(shiny::h4(
              "9. Generate Sales Grid & Download",
              shiny::tags$span(class = "glmnet-section-info",
                `data-bs-toggle` = "popover", `data-bs-trigger` = "hover",
                `data-bs-content` = "Generate the Intermediate Sales Comparable Grid for the appraisal report, ranking comparables by gross adjustment percentage.",
                "?")
            )),
            shiny::actionButton("sales_grid_btn",
                                "Generate Sales Grid & Download",
                                class = "btn-primary",
                                style = "width: 100%;")
          )
        ),

        # --- 10. Download Report ---
        shiny::conditionalPanel(
          condition = "output.model_fitted",
          shiny::hr(),
          shiny::tags$details(class = "glmnet-section",
            shiny::tags$summary(shiny::uiOutput("report_heading",
                                                 inline = TRUE)),
            shiny::selectInput("export_format", "Format",
                               choices = c("Word" = "docx",
                                           "PDF" = "pdf",
                                           "HTML" = "html")),
            shiny::actionButton("export_report_btn", "Download Report",
                                class = "btn-primary",
                                style = "width: 100%;")
          )
        )
      )
    ),
    shiny::mainPanel(
      width = 8,
      shiny::conditionalPanel(
        condition = "!output['data-data_loaded']",
        shiny::tags$div(
          style = "text-align:center; padding:60px 20px;",
          shiny::tags$h3("Welcome to glmnetUI"),
          shiny::tags$p(
            "Upload a CSV or Excel file to get started.",
            style = "font-size:1.2em; color: var(--bs-secondary-color);"
          ),
          shiny::tags$p(
            "This application provides an interactive interface for ",
            "elastic net regression modeling using glmnet. ",
            "Designed for real estate appraisers and general regression users."
          )
        )
      ),
      shiny::conditionalPanel(
        condition = "output['data-data_loaded']",
        shiny::tabsetPanel(
          id = "main_tabs",
          shiny::tabPanel("Data Preview", dataPreviewUI("data")),
          shiny::tabPanel("Equation",
            shiny::conditionalPanel(
              condition = "output.model_fitted",
              equationUI("eq")
            ),
            shiny::conditionalPanel(
              condition = "!output.model_fitted",
              shiny::tags$div(
                style = "text-align:center; padding:40px 20px;",
                shiny::tags$p("Fit a model to see the equation.",
                              style = "color: var(--bs-secondary-color);")
              )
            )
          ),
          shiny::tabPanel("Correlation", correlationUI("corr")),
          shiny::tabPanel("Summary",
            shiny::conditionalPanel(
              condition = "output.model_fitted",
              summaryUI("summ")
            ),
            shiny::conditionalPanel(
              condition = "!output.model_fitted",
              shiny::tags$div(
                style = "text-align:center; padding:40px 20px;",
                shiny::tags$p("Fit a model to see summary statistics.",
                              style = "color: var(--bs-secondary-color);")
              )
            )
          ),
          shiny::tabPanel("Coefficients", coefficientsUI("coefs")),
          shiny::tabPanel("Variable Importance",
            shiny::conditionalPanel(
              condition = "output.model_fitted",
              importanceUI("imp")
            ),
            shiny::conditionalPanel(
              condition = "!output.model_fitted",
              shiny::tags$div(
                style = "text-align:center; padding:40px 20px;",
                shiny::tags$p("Fit a model to see variable importance.",
                              style = "color: var(--bs-secondary-color);")
              )
            )
          ),
          shiny::tabPanel("Contributions",
            shiny::conditionalPanel(
              condition = "output.model_fitted",
              contributionsUI("contrib")
            ),
            shiny::conditionalPanel(
              condition = "!output.model_fitted",
              shiny::tags$div(
                style = "text-align:center; padding:40px 20px;",
                shiny::tags$p("Fit a model to see variable contributions.",
                              style = "color: var(--bs-secondary-color);")
              )
            )
          ),
          shiny::tabPanel("ANOVA",
            shiny::conditionalPanel(
              condition = "output.model_fitted",
              anovaUI("anova")
            ),
            shiny::conditionalPanel(
              condition = "!output.model_fitted",
              shiny::tags$div(
                style = "text-align:center; padding:40px 20px;",
                shiny::tags$p("Fit a model to see ANOVA decomposition.",
                              style = "color: var(--bs-secondary-color);")
              )
            )
          ),
          shiny::tabPanel("Diagnostics", diagnosticsUI("diag")),
          shiny::tabPanel("Glmnet Output",
            shiny::conditionalPanel(
              condition = "!output.model_fitted",
              shiny::tags$div(
                style = "text-align:center; padding:40px 20px;",
                shiny::tags$p("Fit a model to see glmnet output.",
                              style = "color: var(--bs-secondary-color);")
              )
            ),
            shiny::conditionalPanel(
              condition = "output.model_fitted",
              shiny::tags$div(
                style = "overflow-x: auto;",
                shiny::verbatimTextOutput("glmnet_output")
              )
            )
          ),
          shiny::tabPanel("Report", reportUI("report")),
          shiny::tabPanel("RCA Adjustments",
            shiny::conditionalPanel(
              condition = "!output.rca_computed",
              shiny::tags$div(
                style = "text-align:center; padding:40px 20px;",
                shiny::tags$p("Run RCA Adjustments (Step 7) to see analysis.",
                              style = "color: var(--bs-secondary-color);")
              )
            ),
            shiny::conditionalPanel(
              condition = "output.rca_computed",
              shiny::plotOutput("rca_resid_pct_plot", height = "350px"),
              shiny::hr(),
              shiny::plotOutput("rca_net_pct_plot", height = "350px"),
              shiny::hr(),
              shiny::plotOutput("rca_gross_pct_plot", height = "350px")
            )
          )
        )
      )
    )
  )
)
