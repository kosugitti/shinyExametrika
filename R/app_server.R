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
  # We do the DOM swap ourselves with shinyjs instead of shiny.i18n::update_lang.
  # update_lang() round-trips through shiny.i18n's #i18n-state input binding,
  # which is incompatible with shiny >= 1.x ("Unexpected input value mode"
  # console errors). We still rely on usei18n() to inject the `i18n_translations`
  # dictionary and to render labels as `.i18n` spans; here we just rewrite those
  # spans' text from that dictionary. ignoreInit = TRUE because the UI is already
  # rendered in the initial language.
  observeEvent(input$selected_language, {
    lang <- input$selected_language
    i18n$set_translation_language(lang)
    shinyjs::runjs(sprintf(
      "document.querySelectorAll('.i18n').forEach(function(w){
         var k = w.getAttribute('data-key');
         var r = (window.i18n_translations || []).filter(function(c){ return c._row == k; })[0];
         if (r && r['%s'] !== undefined && r['%s'] !== null) { w.textContent = r['%s']; }
       });",
      lang, lang, lang
    ))
  }, ignoreInit = TRUE)

  # --- Data upload module ---
  data_mod <- mod_data_upload_server("data_upload", i18n = i18n)
  formatted_data <- data_mod$data

  # --- Analysis modules ---
  mod_descriptives_server("descriptives", formatted_data = formatted_data, i18n = i18n)
  mod_ctt_server("ctt", formatted_data = formatted_data, i18n = i18n)
  mod_irt_server("irt", formatted_data = formatted_data, i18n = i18n)
  mod_grm_server("grm", formatted_data = formatted_data, i18n = i18n)
  mod_lca_server("lca", formatted_data = formatted_data, i18n = i18n)
  mod_lra_server("lra", formatted_data = formatted_data, i18n = i18n)
  mod_biclustering_server("biclustering", formatted_data = formatted_data, i18n = i18n)
  mod_irm_server("irm", formatted_data = formatted_data, i18n = i18n)
  mod_bnm_server("bnm", formatted_data = formatted_data, i18n = i18n)
  mod_ldlra_server("ldlra", formatted_data = formatted_data, i18n = i18n)

  # --- Tab gating: analysis tabs stay disabled until data is formatted, and a
  #     tab only enables when the loaded data matches its required type
  #     (e.g. GRM stays disabled for binary data). ---
  observe({
    fd <- formatted_data()
    current <- if (is.null(fd)) NA_character_ else (fd$response.type %||% "unknown")

    reqs <- analysis_tab_requirements()
    disabled <- character(0)
    for (tab in names(reqs)) {
      ok <- !is.null(fd) &&
        (identical(reqs[[tab]], "any") || current %in% reqs[[tab]])
      # Toggle the .nav-disabled class on the tab's nav link. shinyjs is used
      # (rather than a hand-rolled custom message handler) because it is
      # initialised reliably, avoiding the "is Shiny ready yet" timing trap.
      sel <- sprintf("a[data-value='%s']", tab)
      if (ok) {
        shinyjs::removeClass(class = "nav-disabled", selector = sel)
      } else {
        shinyjs::addClass(class = "nav-disabled", selector = sel)
        disabled <- c(disabled, tab)
      }
    }

    # If the user is sitting on a tab that just became disabled, send them back
    # to the Data tab so they are not stuck on an inert screen. Read the active
    # tab with isolate() so this observer only re-runs on data changes, not on
    # every tab switch.
    active <- isolate(input$main_navbar)
    if (!is.null(active) && active %in% disabled) {
      bslib::nav_select("main_navbar", "tab_data", session = session)
    }
  })

  # --- Persistent "currently loaded dataset" indicator (navbar header) ---
  output$current_dataset <- renderUI({
    input$selected_language  # re-render the label on language switch
    fd <- formatted_data()
    nm <- data_mod$name()

    if (is.null(fd)) {
      txt <- i18n$t("No dataset loaded")
    } else {
      mat <- if (!is.null(fd$U)) fd$U else fd$Q
      txt <- sprintf(
        "%s  [%s, %d × %d]",
        if (is.null(nm)) i18n$t("dataset") else nm,
        fd$response.type %||% "unknown",
        nrow(mat), ncol(mat)
      )
    }

    tags$span(
      class = "fw-semibold",
      style = "color: #c0392b;",
      tags$span(style = "opacity: 0.7;", "● "),
      txt
    )
  })
}
