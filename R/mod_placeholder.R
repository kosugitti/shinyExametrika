#' Placeholder module UI (for unimplemented tabs)
#'
#' @param id Module namespace ID
#' @param i18n shiny.i18n Translator object
#'
#' @noRd
mod_placeholder_ui <- function(id, i18n) {
  ns <- NS(id)

  bslib::card(
    class = "mt-4 mx-auto",
    style = "max-width: 600px;",
    bslib::card_header(
      class = "text-center",
      tags$h4(i18n$t("Coming soon"))
    ),
    bslib::card_body(
      class = "text-center text-muted",
      tags$p(
        icon("wrench", class = "fa-2x mb-3 d-block"),
        i18n$t("This analysis will be available in a future release.")
      )
    )
  )
}
