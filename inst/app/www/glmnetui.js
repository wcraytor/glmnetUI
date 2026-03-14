// glmnetUI — Settings, theme, locale, and defaults management
// This file is loaded by ui.R via tags$script(src = "glmnetui.js")

// Settings dropdown toggle
function glmnetToggleDropdown(id) {
  var el = document.getElementById(id);
  if (el) el.classList.toggle("open");
}
document.addEventListener("click", function(e) {
  var dropdowns = document.querySelectorAll(".glmnet-navbar .dropdown");
  dropdowns.forEach(function(dd) {
    if (!dd.contains(e.target)) dd.classList.remove("open");
  });
});

var glmnetCurrentMode = "light";

function glmnetToggleTheme() {
  glmnetCurrentMode = (glmnetCurrentMode === "dark") ? "light" : "dark";
  Shiny.setInputValue("dark_mode", glmnetCurrentMode, {priority: "event"});
  glmnetUpdateIcon(glmnetCurrentMode);
  try { localStorage.setItem("glmnetUI_theme", glmnetCurrentMode); } catch(e) {}
}

function glmnetUpdateIcon(mode) {
  var btn = document.getElementById("glmnet-theme-toggle");
  if (btn) btn.innerHTML = (mode === "dark") ? "\u2600" : "\u263E";
}

$(document).on("shiny:connected", function() {
  var btn = document.getElementById("glmnet-theme-toggle");
  if (btn) btn.onclick = glmnetToggleTheme;

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

// Show white checkmark on button after successful action
Shiny.addCustomMessageHandler("btn_done", function(msg) {
  var btn = document.getElementById(msg.id);
  if (!btn) return;
  if (btn.querySelector(".glmnet-check")) return;
  var chk = document.createElement("span");
  chk.className = "glmnet-check";
  chk.style.cssText = "margin-left:8px; font-size:1.1em;";
  chk.textContent = "\u2713";
  btn.appendChild(chk);
});

// Clear checkmarks on all download buttons when a new model is fit
$(document).on("click", "#model-fit_btn", function() {
  document.querySelectorAll(".glmnet-check").forEach(function(el) {
    el.remove();
  });
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

// --- Locale persistence ---
// Restore saved locale defaults on connect
$(document).on("shiny:connected", function() {
  var ld = null;
  try { ld = JSON.parse(localStorage.getItem("glmnetUI_locale_defaults")); } catch(e) {}
  if (ld) {
    Shiny.setInputValue("glmnet_locale_defaults", ld, {priority: "event"});
  }
});

// Save locale defaults to localStorage
Shiny.addCustomMessageHandler("save_locale_defaults", function(msg) {
  try { localStorage.setItem("glmnetUI_locale_defaults", JSON.stringify(msg)); } catch(e) {}
});

// --- Global settings apply function ---
window.glmnetApplySettings = function(s) {
  // Radio buttons (namespaced and global)
  ["model-alpha_method", "model-lambda_method", "model-lambda_choice",
   "purpose"].forEach(function(id) {
    if (s[id] !== undefined) {
      $("input[name=\"" + id + "\"][value=\"" + s[id] + "\"]")
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
