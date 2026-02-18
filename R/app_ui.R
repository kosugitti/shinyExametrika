#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  # --- 翻訳オブジェクトの初期化 ---
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = app_sys("i18n/translation.json")
  )
  i18n$set_translation_language("en")

  tagList(
    golem_add_external_resources(),
    shinyjs::useShinyjs(),

    bslib::page_navbar(
      id = "main_navbar",
      title = "shinyExametrika",
      theme = bslib::bs_theme(
        version = 5,
        bootswatch = "flatly",
        primary = "#2c3e50"
      ),

      # --- 言語切替 ---
      header = tags$div(
        class = "d-flex justify-content-end pe-3 pt-1",
        shinyWidgets::radioGroupButtons(
          inputId = "selected_language",
          label = NULL,
          choices = c("EN" = "en", "JA" = "ja"),
          selected = "en",
          size = "xs"
        )
      ),

      # --- データ読み込みタブ ---
      bslib::nav_panel(
        title = i18n$t("Data"),
        value = "tab_data",
        mod_data_upload_ui("data_upload", i18n)
      ),

      # --- 記述統計タブ ---
      bslib::nav_panel(
        title = i18n$t("Descriptives"),
        value = "tab_descriptives",
        mod_descriptives_ui("descriptives", i18n)
      ),

      # --- 分析タブ（Phase 1 以降で実装） ---
      bslib::nav_panel(
        title = i18n$t("CTT"),
        value = "tab_ctt",
        mod_placeholder_ui("ctt", i18n)
      ),
      bslib::nav_panel(
        title = i18n$t("IRT"),
        value = "tab_irt",
        mod_placeholder_ui("irt", i18n)
      ),
      bslib::nav_panel(
        title = i18n$t("LCA"),
        value = "tab_lca",
        mod_placeholder_ui("lca", i18n)
      ),
      bslib::nav_panel(
        title = i18n$t("LRA"),
        value = "tab_lra",
        mod_placeholder_ui("lra", i18n)
      ),
      bslib::nav_panel(
        title = i18n$t("Biclustering"),
        value = "tab_biclustering",
        mod_placeholder_ui("biclustering", i18n)
      ),
      bslib::nav_panel(
        title = i18n$t("IRM"),
        value = "tab_irm",
        mod_placeholder_ui("irm", i18n)
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "shinyExametrika"
    ),
    # --- shiny.i18n: 言語切替用 JS ---
    shiny.i18n::usei18n(
      shiny.i18n::Translator$new(
        translation_json_path = app_sys("i18n/translation.json")
      )
    )
  )
}
