# =============================================================================
# fct_modelhelp.R -- Collapsible "About this model" help for each analysis tab
# =============================================================================
#
# The Guide tab (mod_guide.R) holds a one-paragraph description of every model,
# but a new user on, say, the IRT tab has to leave for the Guide to find out
# what IRT is. This helper drops the same description into a collapsible panel
# at the top of each analysis tab, so the explanation sits where it is needed.
#
# It reuses the *exact* English strings already present in
# inst/i18n/translation.json (the model name, the one-line description and the
# data-type badge that mod_guide renders), so the only genuinely new i18n
# strings are the panel label and the Descriptives entry.
#
# Wiring (per module, UI only -- no server logic needed):
#   model_help_block("irt", i18n)   # placed just below uiOutput(ns("precheck"))
# =============================================================================

#' Registry of per-model help content
#'
#' Each entry references strings that already exist in translation.json (so
#' they are translated for free) unless marked NEW below.
#'
#' @return Named list keyed by module name.
#' @noRd
model_help_registry <- function() {
  list(
    descriptives = list(
      name = "Descriptives",
      # NEW string (Descriptives has no Guide card)
      desc = "Basic test-level and item-level summary statistics: score distribution, pass rates, and item-total correlations.",
      type = "Works with any response type"  # NEW string
    ),
    ctt = list(
      name = "Classical Test Theory",
      desc = "Reliability coefficients (Alpha, Omega) and item-level analysis.",
      type = "Binary data"
    ),
    irt = list(
      name = "Item Response Theory",
      desc = "2PL/3PL/4PL models for ability estimation and item characteristic curves.",
      type = "Binary data"
    ),
    grm = list(
      name = "Graded Response Model",
      desc = "IRT model for ordinal (polytomous) response data.",
      type = "Ordinal data"
    ),
    lca = list(
      name = "Latent Class Analysis",
      desc = "Classify examinees into latent classes based on response patterns.",
      type = "Binary data"
    ),
    lra = list(
      name = "Latent Rank Analysis",
      desc = "Rank examinees on an ordinal latent scale with item reference profiles.",
      type = "Binary data"
    ),
    biclustering = list(
      name = "Biclustering",
      desc = "Simultaneously cluster examinees and items into classes and fields.",
      type = "Binary data"
    ),
    irm = list(
      name = "Infinite Relational Model",
      desc = "Nonparametric Bayesian approach to automatically determine optimal cluster structure.",
      type = "Binary data"
    ),
    bnm = list(
      name = "Bayesian Network Model",
      desc = "Bayesian network analysis that models conditional dependencies between test items using a directed acyclic graph (DAG).",
      type = "Binary data"
    ),
    ldlra = list(
      name = "Locally Dependent Latent Rank Analysis",
      desc = "LDLRA extends LRA by modeling item dependencies within each latent rank using directed acyclic graphs (DAGs). Supports fixed DAG input and PBIL structure learning.",
      type = "Binary data"
    )
  )
}


#' Collapsible "About this model" panel for an analysis tab
#'
#' Uses a native `<details>` element, so no extra JavaScript is needed and it
#' degrades gracefully. Collapsed by default to keep the tab uncluttered.
#'
#' @param key Module key (e.g. "irt"); see `model_help_registry()`.
#' @param i18n shiny.i18n Translator object.
#'
#' @return A `tags$details` block, or NULL for an unknown key.
#' @noRd
model_help_block <- function(key, i18n) {
  info <- model_help_registry()[[key]]
  if (is.null(info)) {
    return(NULL)
  }

  tags$details(
    class = "mb-3",
    tags$summary(
      class = "text-primary",
      style = "cursor: pointer;",
      icon("circle-info", class = "me-1"),
      i18n$t("About this model")
    ),
    tags$div(
      class = "mt-2 p-3 bg-light rounded",
      tags$div(class = "fw-semibold mb-1", i18n$t(info$name)),
      tags$p(class = "mb-2 text-muted small", i18n$t(info$desc)),
      tags$span(class = "badge bg-secondary", i18n$t(info$type))
    )
  )
}
