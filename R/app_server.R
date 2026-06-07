#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @param tabs Optional character vector of tab values to wire (must match the
#'     subset passed to `app_ui`). When `NULL` (default) all analysis modules are
#'     wired -- the full shinyapps.io app. The shinylive per-function builds pass
#'     a subset so only those modules' servers run.
#' @import shiny
#' @noRd
app_server <- function(input, output, session, tabs = NULL) {

  all_tabs <- c(
    "tab_guide", "tab_data", "tab_descriptives", "tab_ctt", "tab_irt",
    "tab_grm", "tab_lca", "tab_lra", "tab_biclustering", "tab_irm",
    "tab_bnm", "tab_ldlra", "tab_ldb", "tab_binet"
  )
  if (is.null(tabs)) tabs <- all_tabs
  tabs <- union(c("tab_guide", "tab_data"), tabs)
  has <- function(x) x %in% tabs

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

  # --- Session R-script log (reproducible code of everything done).
  #     The download button itself lives in each module's sidebar (wired by
  #     mod_downloads_server); this just holds the shared accumulating log. ---
  script_log <- reactiveVal(list())

  # --- Data upload module (always present) ---
  data_mod <- mod_data_upload_server("data_upload", i18n = i18n, script_log = script_log)
  formatted_data <- data_mod$data

  # --- Analysis modules (each gets the shared script_log for the R-script button).
  #     Only the modules whose tab is included are wired. ---
  if (has("tab_descriptives")) mod_descriptives_server("descriptives", formatted_data = formatted_data, i18n = i18n, script_log = script_log)
  if (has("tab_ctt")) mod_ctt_server("ctt", formatted_data = formatted_data, i18n = i18n, script_log = script_log)
  if (has("tab_irt")) mod_irt_server("irt", formatted_data = formatted_data, i18n = i18n, script_log = script_log)
  if (has("tab_grm")) mod_grm_server("grm", formatted_data = formatted_data, i18n = i18n, script_log = script_log)
  if (has("tab_lca")) mod_lca_server("lca", formatted_data = formatted_data, i18n = i18n, script_log = script_log)
  if (has("tab_lra")) mod_lra_server("lra", formatted_data = formatted_data, i18n = i18n, script_log = script_log)
  if (has("tab_biclustering")) mod_biclustering_server("biclustering", formatted_data = formatted_data, i18n = i18n, script_log = script_log)
  if (has("tab_irm")) mod_irm_server("irm", formatted_data = formatted_data, i18n = i18n, script_log = script_log)
  if (has("tab_bnm")) mod_bnm_server("bnm", formatted_data = formatted_data, i18n = i18n, script_log = script_log)
  if (has("tab_ldlra")) mod_ldlra_server("ldlra", formatted_data = formatted_data, i18n = i18n, script_log = script_log)

  # --- Tab gating: analysis tabs stay disabled until data is formatted, and a
  #     tab only enables when the loaded data matches its required type
  #     (e.g. GRM stays disabled for binary data). Only tabs present in this
  #     build are toggled. ---
  observe({
    fd <- formatted_data()
    current <- if (is.null(fd)) NA_character_ else (fd$response.type %||% "unknown")

    reqs <- analysis_tab_requirements()
    disabled <- character(0)
    for (tab in intersect(names(reqs), tabs)) {
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
        "%s  [%s, %d \u00d7 %d]",
        if (is.null(nm)) i18n$t("dataset") else nm,
        fd$response.type %||% "unknown",
        nrow(mat), ncol(mat)
      )
    }

    tags$span(
      class = "fw-semibold",
      style = "color: #c0392b;",
      tags$span(style = "opacity: 0.7;", "\u25cf "),
      txt
    )
  })
}
