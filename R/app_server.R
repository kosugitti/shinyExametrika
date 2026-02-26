#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # --- Translator object ---
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = app_sys("i18n/translation.json")
  )
  i18n$set_translation_language("en")

  # --- Language switch ---
  observeEvent(input$selected_language, {
    shiny.i18n::update_lang(input$selected_language, session)
    i18n$set_translation_language(input$selected_language)
  })

  # --- Data upload module ---
  formatted_data <- mod_data_upload_server("data_upload", i18n = i18n)

  # --- Analysis modules ---
  mod_descriptives_server("descriptives", formatted_data = formatted_data, i18n = i18n)
  mod_ctt_server("ctt", formatted_data = formatted_data, i18n = i18n)
  mod_irt_server("irt", formatted_data = formatted_data, i18n = i18n)
  mod_grm_server("grm", formatted_data = formatted_data, i18n = i18n)
  mod_lca_server("lca", formatted_data = formatted_data, i18n = i18n)
  mod_lra_server("lra", formatted_data = formatted_data, i18n = i18n)
  mod_biclustering_server("biclustering", formatted_data = formatted_data, i18n = i18n)
  mod_irm_server("irm", formatted_data = formatted_data, i18n = i18n)
}
