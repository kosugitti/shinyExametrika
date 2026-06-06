# =============================================================================
# fct_precheck.R -- Shared data-readiness pre-check for analysis tabs
# =============================================================================
#
# Each analysis module needs the same guard before it can run:
#   1. Has the user loaded & formatted any data yet?
#   2. Is the loaded data the response type this analysis needs?
#
# Previously every tab relied on a silent `req(formatted_data())`, so an empty
# tab gave the user no hint about *why* nothing happened. These helpers render a
# consistent banner at the top of each analysis tab explaining the situation.
#
# Wiring (per module):
#   UI     : uiOutput(ns("precheck"))  -- placed above the result card
#   server : output$precheck <- renderUI(
#              precheck_banner(formatted_data(), required = "binary", i18n))
#
# `required` is a character vector of acceptable exametrika response.type values
# ("binary", "ordinal", "nominal", "rated"), or the literal "any".
# =============================================================================

#' Required response type for each gated analysis tab
#'
#' Maps a navbar tab `value` to the response type(s) it accepts. Used by
#' app_server to enable/disable tabs: a tab is disabled until data is loaded,
#' and only enables when the loaded data matches (e.g. GRM stays disabled for
#' binary data). Tabs not listed here (Guide, Data, the LDB/BINET placeholders)
#' are never gated.
#'
#' @return Named list: tab value -> acceptable `response.type` values (or "any").
#' @noRd
analysis_tab_requirements <- function() {
  list(
    tab_descriptives = "any",
    tab_ctt          = "binary",
    tab_irt          = "binary",
    tab_grm          = c("ordinal", "rated"),
    tab_lca          = "binary",
    tab_lra          = "binary",
    tab_biclustering = "binary",
    tab_irm          = "binary",
    tab_bnm          = "binary",
    tab_ldlra        = "binary"
  )
}


#' Check whether formatted data satisfies an analysis's type requirement
#'
#' @param fd Result of `dataFormat()` (the `formatted_data()` reactive value),
#'   or NULL when no data has been loaded yet.
#' @param required Character vector of acceptable `response.type` values, or the
#'   literal `"any"` to accept any loaded data.
#'
#' @return A list with:
#'   - `ok`: TRUE when the analysis can run
#'   - `status`: "ok", "no_data", or "wrong_type"
#'   - `current`: the detected response type (NA when no data)
#'
#' @noRd
check_data_requirement <- function(fd, required = "any") {
  if (is.null(fd)) {
    return(list(ok = FALSE, status = "no_data", current = NA_character_))
  }

  current <- fd$response.type %||% "unknown"

  if (identical(required, "any") || current %in% required) {
    return(list(ok = TRUE, status = "ok", current = current))
  }

  list(ok = FALSE, status = "wrong_type", current = current)
}


#' Render a data-readiness banner for an analysis tab
#'
#' Returns NULL (renders nothing) when the data satisfies the requirement, so it
#' is safe to call unconditionally inside `renderUI()`.
#'
#' @param fd Result of `dataFormat()` or NULL.
#' @param required Character vector of acceptable `response.type` values, or
#'   `"any"`.
#' @param i18n shiny.i18n Translator object.
#'
#' @return A Bootstrap alert `tags$div`, or NULL when no banner is needed.
#'
#' @noRd
precheck_banner <- function(fd, required, i18n) {
  chk <- check_data_requirement(fd, required)
  if (chk$ok) {
    return(NULL)
  }

  if (chk$status == "no_data") {
    msg <- i18n$t("No data loaded yet. Open the \"Data\" tab to upload a CSV (or pick a sample dataset) and format it before running this analysis.")
  } else {
    msg <- sprintf(
      i18n$t("This analysis needs %s data, but the loaded data is %s. Load matching data on the \"Data\" tab."),
      paste(required, collapse = " / "),
      chk$current
    )
  }

  tags$div(
    class = "alert alert-warning d-flex align-items-center mb-3",
    role = "alert",
    icon("triangle-exclamation", class = "me-2 fs-5"),
    tags$span(msg)
  )
}
