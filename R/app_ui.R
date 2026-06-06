#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  # --- Initialize translator object ---
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = app_sys("i18n/translation.json")
  )
  i18n$set_translation_language("en")
  # Enable automatic client-side translation BEFORE any i18n$t() is evaluated
  # while building the UI below. In this mode i18n$t("X") emits a
  # <span class="i18n" data-key="X"> that shiny.i18n's JS swaps live on language
  # change. usei18n() (added further down) injects the dictionary + JS and also
  # calls use_js(); calling it here first is what makes every static label --
  # tab titles, sidebar headings, input labels -- actually switch language.
  i18n$use_js()

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

      # --- Header: loaded-dataset indicator (left) + language switch (right) ---
      header = tags$div(
        class = "d-flex justify-content-between align-items-center px-3 pt-1",
        # Persistent indicator of which dataset is currently loaded
        uiOutput("current_dataset", inline = TRUE),
        shinyWidgets::radioGroupButtons(
          inputId = "selected_language",
          label = NULL,
          choices = c("EN" = "en", "JA" = "ja"),
          selected = "en",
          size = "xs"
        )
      ),

      # --- Guide tab (landing page) ---
      bslib::nav_panel(
        title = i18n$t("Guide"),
        value = "tab_guide",
        mod_guide_ui("guide", i18n)
      ),

      # --- Data upload tab ---
      bslib::nav_panel(
        title = i18n$t("Data"),
        value = "tab_data",
        mod_data_upload_ui("data_upload", i18n)
      ),

      # --- Descriptives tab ---
      bslib::nav_panel(
        title = i18n$t("Descriptives"),
        value = "tab_descriptives",
        mod_descriptives_ui("descriptives", i18n)
      ),

      # --- Analysis tabs ---
      bslib::nav_panel(
        title = i18n$t("CTT"),
        value = "tab_ctt",
        mod_ctt_ui("ctt", i18n)
      ),
      bslib::nav_panel(
        title = i18n$t("IRT"),
        value = "tab_irt",
        mod_irt_ui("irt", i18n)
      ),
      bslib::nav_panel(
        title = i18n$t("GRM"),
        value = "tab_grm",
        mod_grm_ui("grm", i18n)
      ),
      bslib::nav_panel(
        title = i18n$t("LCA"),
        value = "tab_lca",
        mod_lca_ui("lca", i18n)
      ),
      bslib::nav_panel(
        title = i18n$t("LRA"),
        value = "tab_lra",
        mod_lra_ui("lra", i18n)
      ),
      bslib::nav_panel(
        title = i18n$t("Biclustering"),
        value = "tab_biclustering",
        mod_biclustering_ui("biclustering", i18n)
      ),
      bslib::nav_panel(
        title = i18n$t("IRM"),
        value = "tab_irm",
        mod_irm_ui("irm", i18n)
      ),
      bslib::nav_panel(
        title = i18n$t("BNM"),
        value = "tab_bnm",
        mod_bnm_ui("bnm", i18n)
      ),

      # --- Phase 3: Network / locally dependent models ---
      bslib::nav_panel(
        title = i18n$t("LDLRA"),
        value = "tab_ldlra",
        mod_ldlra_ui("ldlra", i18n)
      ),
      bslib::nav_panel(
        title = i18n$t("LDB"),
        value = "tab_ldb",
        mod_placeholder_ui("ldb_placeholder", i18n)
      ),
      bslib::nav_panel(
        title = i18n$t("BINET"),
        value = "tab_binet",
        mod_placeholder_ui("binet_placeholder", i18n)
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
    # --- shiny.i18n: Language switch JS ---
    shiny.i18n::usei18n(
      shiny.i18n::Translator$new(
        translation_json_path = app_sys("i18n/translation.json")
      )
    ),

    # --- Tab gating: disable analysis tabs until data is formatted ---
    # (the class itself is toggled from the server with shinyjs)
    tags$style(htmltools::HTML(
      ".nav-disabled { pointer-events: none; opacity: 0.4; cursor: not-allowed; }"
    ))
  )
}
