bslib::page_fluid(
  theme = nord_light,

  # --- Adaptive CSS using Bootstrap CSS variables ---
  shiny::tags$style(shiny::HTML('
    /* Theme toggle button */
    #glmnet-theme-toggle {
      position: fixed; top: 12px; right: 20px; z-index: 10000;
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

    /* Help icon (?) with Bootstrap popover */
    .glmnet-param-help {
      position: absolute; top: 0; right: 0;
      width: 18px; height: 18px; border-radius: 50%;
      background: #5bc0de; color: #fff;
      font-size: 11px; font-weight: bold;
      text-align: center; line-height: 18px;
      cursor: pointer; z-index: 10;
    }
    .glmnet-param-help:hover { background: #31b0d5; }
    .glmnet-help-icon {
      display: inline-block; width: 18px; height: 18px;
      border-radius: 50%; background: #5bc0de; color: #fff;
      text-align: center; font-size: 11px; font-weight: bold;
      line-height: 18px; cursor: pointer;
      margin-left: 6px; vertical-align: middle;
    }
    .glmnet-help-icon:hover { background: #31b0d5; }

    /* Collapsible sections */
    .glmnet-section > summary {
      cursor: pointer;
      list-style: none;
    }
    .glmnet-section > summary::-webkit-details-marker {
      display: none;
    }
    .glmnet-section > summary h4::before {
      content: "\\25B6  ";
      font-size: 0.7em;
      transition: transform 0.2s;
      display: inline-block;
    }
    .glmnet-section[open] > summary h4::before {
      transform: rotate(90deg);
    }
  ')),

  # --- Theme toggle button ---
  shiny::tags$button(
    id = "glmnet-theme-toggle",
    shiny::HTML("&#9790;")
  ),

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

  # --- Theme toggle JS + settings message handlers ---
  shiny::tags$script(shiny::HTML('
    var glmnetCurrentMode = "light";

    function glmnetToggleTheme() {
      glmnetCurrentMode = (glmnetCurrentMode === "dark") ? "light" : "dark";
      Shiny.setInputValue("dark_mode", glmnetCurrentMode, {priority: "event"});
      glmnetUpdateIcon(glmnetCurrentMode);
      try { localStorage.setItem("glmnetUI_theme", glmnetCurrentMode); } catch(e) {}
    }

    function glmnetUpdateIcon(mode) {
      var btn = document.getElementById("glmnet-theme-toggle");
      if (btn) btn.innerHTML = (mode === "dark") ? "\\u2600" : "\\u263E";
    }

    document.getElementById("glmnet-theme-toggle").onclick = glmnetToggleTheme;

    $(document).on("shiny:connected", function() {
      var saved = null;
      try { saved = localStorage.getItem("glmnetUI_theme"); } catch(e) {}
      if (saved === "dark") {
        glmnetCurrentMode = "dark";
        glmnetUpdateIcon("dark");
        Shiny.setInputValue("dark_mode", "dark", {priority: "event"});
      } else {
        Shiny.setInputValue("dark_mode", "light", {priority: "event"});
      }
    });

    // Pre-seed sale_age as included in localStorage when computed
    Shiny.addCustomMessageHandler("sale_age_added", function(msg) {
      if (!msg.filename) return;
      var storageKey = "glmnetUI_vars_" + msg.filename;
      var saved = {};
      try { saved = JSON.parse(localStorage.getItem(storageKey)) || {}; } catch(e) {}
      saved["sale_age"] = { inc: true, fac: false, type: "integer", force: false, sign: "either", special: "no" };
      try { localStorage.setItem(storageKey, JSON.stringify(saved)); } catch(e) {}
    });

    // --- Global settings apply function ---
    window.glmnetApplySettings = function(s) {
      // Radio buttons (namespaced and global)
      ["model-alpha_method", "model-lambda_method", "model-lambda_choice",
       "purpose"].forEach(function(id) {
        if (s[id] !== undefined) {
          $("input[name=\\"" + id + "\\"][value=\\"" + s[id] + "\\"]")
            .prop("checked", true).trigger("change");
        }
      });
      // Sliders (ionRangeSlider)
      ["model-alpha", "model-gamma"].forEach(function(id) {
        if (s[id] !== undefined) {
          var irs = $(document.getElementById(id)).data("ionRangeSlider");
          if (irs) irs.update({from: s[id]});
        }
      });
      // Range slider
      if (s["model-alpha_range"] !== undefined) {
        var irs = $(document.getElementById("model-alpha_range"))
          .data("ionRangeSlider");
        if (irs) {
          var v = s["model-alpha_range"];
          irs.update({from: v[0], to: v[1]});
        }
      }
      // Numeric inputs
      ["model-n_alphas", "model-lambda_manual", "model-nfolds"]
        .forEach(function(id) {
          if (s[id] !== undefined) {
            $(document.getElementById(id)).val(s[id]).trigger("change");
          }
        });
      // Select (selectize)
      if (s["model-family"] !== undefined) {
        var el = document.getElementById("model-family");
        if (el && el.selectize) el.selectize.setValue(s["model-family"], true);
        else if (el) $(el).val(s["model-family"]).trigger("change");
      }
      // Checkboxes
      ["model-standardize", "model-enforce_signs", "model-relaxed"]
        .forEach(function(id) {
          if (s[id] !== undefined) {
            $(document.getElementById(id)).prop("checked", s[id])
              .trigger("change");
          }
        });
      // Text inputs
      if (s["output_folder"] !== undefined) {
        $(document.getElementById("output_folder"))
          .val(s["output_folder"]).trigger("change");
      }
      // Date inputs
      if (s["effective_date"] !== undefined && s["effective_date"] !== null) {
        var $inp = $("#effective_date input");
        if ($inp.length) $inp.val(s["effective_date"]).trigger("change");
      }
    };

    // Apply variables from saved state to DOM
    window.glmnetApplyVariables = function(saved) {
      for (var colName in saved) {
        var sv = saved[colName];
        var $inc = $(document.getElementById("data-inc_" + colName));
        var $force = $(document.getElementById("data-force_" + colName));
        var $sign = $(document.getElementById("data-sign_" + colName));
        var $type = $(document.getElementById("data-type_" + colName));
        var $special = $(document.getElementById("data-special_" + colName));

        if ($inc.length && sv.inc !== undefined) $inc.prop("checked", sv.inc);
        if ($force.length && sv.force !== undefined) $force.prop("checked", sv.force);
        if ($sign.length && sv.sign) $sign.val(sv.sign);
        if ($type.length && sv.type) $type.val(sv.type);
        if ($special.length && sv.special) $special.val(sv.special);
      }
      if ($(".glmnet-var-cb").length) $(".glmnet-var-cb").first().trigger("change");
    };

    // Apply interactions from saved state to DOM
    window.glmnetApplyInteractions = function(saved) {
      for (var key in saved) {
        var $cb = $(document.getElementById("model-int_" + key));
        if ($cb.length) $cb.prop("checked", saved[key]);
      }
      if ($(".glmnet-interaction-cb").length) {
        $(".glmnet-interaction-cb").first().trigger("change");
      }
    };

    // --- Apply saved defaults from localStorage ---
    Shiny.addCustomMessageHandler("apply_saved_defaults", function(msg) {
      var fn = msg.filename || "default";
      var defSettings = null;
      var defVars = null;
      var defInts = null;
      try { defSettings = JSON.parse(localStorage.getItem("glmnetUI_settings___defaults__")); } catch(e) {}
      try { defVars = JSON.parse(localStorage.getItem("glmnetUI_vars___defaults__")); } catch(e) {}
      try { defInts = JSON.parse(localStorage.getItem("glmnetUI_interactions___defaults__")); } catch(e) {}

      if (!defSettings) {
        Shiny.setInputValue("glmnet_no_defaults", Math.random(),
          {priority: "event"});
        return;
      }

      window.glmnetApplySettings(defSettings);
      // Save to current file localStorage too
      try { localStorage.setItem("glmnetUI_settings_" + fn, JSON.stringify(defSettings)); } catch(e) {}

      if (defVars) {
        try { localStorage.setItem("glmnetUI_vars_" + fn, JSON.stringify(defVars)); } catch(e) {}
        setTimeout(function() { window.glmnetApplyVariables(defVars); }, 200);
      }
      if (defInts) {
        try { localStorage.setItem("glmnetUI_interactions_" + fn, JSON.stringify(defInts)); } catch(e) {}
        setTimeout(function() { window.glmnetApplyInteractions(defInts); }, 200);
      }
    });

    // --- Apply glmnet factory defaults ---
    Shiny.addCustomMessageHandler("apply_glmnet_defaults", function(msg) {
      var defaults = {
        "model-alpha_method": "fixed",
        "model-alpha": 1,
        "model-alpha_range": [0, 1],
        "model-n_alphas": 11,
        "model-lambda_method": "cv",
        "model-lambda_manual": 0.01,
        "model-lambda_choice": "1se",
        "model-nfolds": 10,
        "model-family": "gaussian",
        "model-standardize": true,
        "model-enforce_signs": false,
        "model-relaxed": false,
        "model-gamma": 0,
        "purpose": "general"
      };
      window.glmnetApplySettings(defaults);
      // Reset variable table: check all inc, uncheck force, set sign=either
      $(".glmnet-var-cb").prop("checked", true);
      $(".glmnet-force-cb").prop("checked", false);
      $(".glmnet-sign-sel").val("either");
      $(".glmnet-special-sel").val("no");
      if ($(".glmnet-var-cb").length) $(".glmnet-var-cb").first().trigger("change");
      // Check all interactions
      $(".glmnet-interaction-cb").prop("checked", true);
      if ($(".glmnet-interaction-cb").length) {
        $(".glmnet-interaction-cb").first().trigger("change");
      }
    });

    // --- Collect and save current settings as defaults ---
    Shiny.addCustomMessageHandler("collect_and_save_defaults", function(msg) {
      var fn = msg.filename || "default";
      try {
        var s = localStorage.getItem("glmnetUI_settings_" + fn);
        if (s) localStorage.setItem("glmnetUI_settings___defaults__", s);
      } catch(e) {}
      try {
        var v = localStorage.getItem("glmnetUI_vars_" + fn);
        if (v) localStorage.setItem("glmnetUI_vars___defaults__", v);
      } catch(e) {}
      try {
        var i = localStorage.getItem("glmnetUI_interactions_" + fn);
        if (i) localStorage.setItem("glmnetUI_interactions___defaults__", i);
      } catch(e) {}
    });
  ')),

  shiny::tags$head(
    shiny::tags$link(rel = "icon", type = "image/png", href = "favicon.png")
  ),

  shiny::tags$div(
    style = "padding: 10px 15px;",
    shiny::tags$h2(
      shiny::tags$img(src = "logo.png", height = "32px",
                      style = "margin-right: 8px; vertical-align: middle;"),
      "glmnetUI",
      shiny::tags$small(" - Interactive Elastic Net Modeling",
                        style = "font-size: 0.6em; color: var(--bs-secondary-color);")
    )
  ),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 4,

      # --- Purpose Mode ---
      shiny::tags$div(
        style = "font-weight:bold;",
        shiny::radioButtons("purpose", "Purpose:",
                            choices = c("General" = "general",
                                        "For Appraisal" = "appraisal",
                                        "Market Area Analysis" = "market"),
                            selected = "general", inline = TRUE)
      ),
      shiny::hr(),

      # --- 1. Import Data ---
      shiny::tags$details(class = "glmnet-section",
        shiny::tags$summary(shiny::h4("1. Import Data",
                                      style = "display:inline;")),
        dataImportUI("data")
      ),
      shiny::hr(),

      shiny::conditionalPanel(
        condition = "output['data-data_loaded']",

        # --- 2. Project Output Folder ---
        shiny::tags$details(class = "glmnet-section",
          shiny::tags$summary(shiny::h4("2. Project Output Folder",
                                        style = "display:inline;")),
          shiny::textInput("output_folder", NULL,
                           value = path.expand("~/Downloads"))
        ),
        shiny::hr(),

        # --- 3. Variable Configuration ---
        shiny::tags$details(class = "glmnet-section",
          shiny::tags$summary(shiny::h4("3. Variable Configuration",
                                        style = "display:inline;")),
          shiny::conditionalPanel(
            condition = "input.purpose !== 'general'",
            shiny::dateInput("effective_date", "Effective Date",
                             value = Sys.Date())
          ),
          variableConfigUI("data")
        ),
        shiny::hr(),

        # --- 4. glmnet Call Parameters ---
        shiny::tags$details(class = "glmnet-section",
          shiny::tags$summary(shiny::h4("4. glmnet Call Parameters",
                                        style = "display:inline;")),
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

        # --- 5. Fit Glmnet Model ---
        shiny::tags$details(class = "glmnet-section",
          shiny::tags$summary(shiny::h4("5. Fit Glmnet Model",
                                        style = "display:inline;")),
          fitModelUI("model")
        ),

        # --- 6. Download Estimated Sale Prices & Residuals ---
        shiny::conditionalPanel(
          condition = "output.model_fitted",
          shiny::hr(),
          shiny::tags$details(class = "glmnet-section",
            shiny::tags$summary(shiny::uiOutput("download_heading",
                                                 inline = TRUE)),
            shiny::conditionalPanel(
              condition = "input.purpose !== 'general'",
              shiny::actionButton("export_data", "Download Output (Excel)",
                                  class = "btn-success",
                                  style = "width: 100%;")
            ),
            shiny::conditionalPanel(
              condition = "input.purpose === 'general'",
              shiny::tags$p(
                shiny::tags$em("Skip"),
                style = "color: var(--bs-secondary-color); margin: 4px 0;"
              )
            )
          )
        ),

        # --- 7. Calculate RCA Adjustments (Appraisal only) ---
        shiny::conditionalPanel(
          condition = "output.model_fitted",
          shiny::hr(),
          shiny::tags$details(class = "glmnet-section",
            shiny::tags$summary(shiny::h4("7. Calculate RCA Adjustments & Download",
                                          style = "display:inline;")),
            shiny::conditionalPanel(
              condition = "input.purpose === 'appraisal'",
              shiny::actionButton("rca_output_btn",
                                  "Calculate RCA Adjustments & Download",
                                  class = "btn-success",
                                  style = "width: 100%;")
            ),
            shiny::conditionalPanel(
              condition = "input.purpose !== 'appraisal'",
              shiny::tags$p(
                shiny::tags$em("Skip"),
                style = "color: var(--bs-secondary-color); margin: 4px 0;"
              )
            )
          )
        ),

        # --- 8. Download Report ---
        shiny::conditionalPanel(
          condition = "output.model_fitted",
          shiny::hr(),
          shiny::tags$details(class = "glmnet-section",
            shiny::tags$summary(shiny::uiOutput("report_heading",
                                                 inline = TRUE)),
            shiny::selectInput("export_format", "Format",
                               choices = c("Word" = "docx",
                                           "PDF" = "pdf")),
            shiny::actionButton("export_report_btn", "Download Report",
                                class = "btn-success",
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
          shiny::tabPanel("Coefficients", coefficientsUI("coefs")),
          shiny::tabPanel("Diagnostics", diagnosticsUI("diag")),
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
