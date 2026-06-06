# =============================================================================
# fct_param_help.R -- Inline parameter guidance (hover tooltips on input labels)
# =============================================================================
#
# Several analysis parameters require statistical judgement to set well
# (IRT 2PL/3PL/4PL, LRA GTM/SOM, IRM concentration parameters, the GA/PBIL
# structure-learning knobs in BNM/LDLRA, ...). Previously the labels gave the
# name only. `param_label()` attaches a small "?" icon next to the label that
# reveals a one-line explanation plus a sensible default on hover.
#
# Usage inside a module UI:
#   radioButtons(
#     ns("model"),
#     label = param_label("IRT Model", "<help string>", i18n),
#     ...
#   )
#
# Both `label_key` and `help_key` are English strings that double as
# translation.json keys, so the tooltip is bilingual like the rest of the app.
# =============================================================================

#' Build an input label with an inline help tooltip
#'
#' @param label_key English label text (also an i18n key).
#' @param help_key English help text (also an i18n key); shown on hover.
#' @param i18n shiny.i18n Translator object.
#'
#' @return A `tagList` suitable for the `label =` argument of a Shiny input.
#' @noRd
param_label <- function(label_key, help_key, i18n) {
  tagList(
    i18n$t(label_key),
    bslib::tooltip(
      tags$span(
        style = "cursor: help;",
        icon("circle-question", class = "ms-1 text-muted")
      ),
      i18n$t(help_key),
      placement = "right"
    )
  )
}
