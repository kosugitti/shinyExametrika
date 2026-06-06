#' CTT Analysis Module UI
#'
#' @param id Module namespace ID
#' @param i18n shiny.i18n Translator object
#'
#' @noRd
mod_ctt_ui <- function(id, i18n) {
  ns <- NS(id)

  bslib::layout_sidebar(
    # --- Sidebar ---
    sidebar = bslib::sidebar(
      width = 280,
      title = i18n$t("CTT"),

      tags$p(
        i18n$t("CTT analyzes test reliability using Classical Test Theory."),
        class = "text-muted small"
      ),

      tags$hr(),

      actionButton(
        ns("btn_run"),
        label = i18n$t("Run Analysis"),
        class = "btn-primary w-100",
        icon = icon("play")
      ),

      # Unified download section (outputs appear after a successful run;
      # the R-script button is always available)
      download_sidebar_ui(ns, i18n)
    ),

    # --- Main panel ---
    uiOutput(ns("precheck")),
    model_help_block("ctt", i18n),

    bslib::navset_card_tab(
      id = ns("result_tabs"),

      # Reliability coefficients tab
      bslib::nav_panel(
        title = i18n$t("Reliability"),
        bslib::card_body(
          uiOutput(ns("reliability_summary")),
          DT::DTOutput(ns("reliability_table"))
        )
      ),

      # Reliability if item deleted tab
      bslib::nav_panel(
        title = i18n$t("Reliability if Item Deleted"),
        bslib::card_body(
          DT::DTOutput(ns("item_deleted_table"))
        )
      )
    )
  )
}


#' CTT Analysis Module Server
#'
#' @param id Module namespace ID
#' @param formatted_data reactive: result of exametrika dataFormat()
#' @param i18n shiny.i18n Translator object
#'
#' @noRd
mod_ctt_server <- function(id, formatted_data, i18n, script_log = NULL) {
  moduleServer(id, function(input, output, session) {

    # --- Data-readiness banner ---
    output$precheck <- renderUI({
      precheck_banner(formatted_data(), required = "binary", i18n)
    })

    # --- CTT analysis result ---
    result <- eventReactive(input$btn_run, {
      req(formatted_data())

      fd <- formatted_data()

      # CTT supports binary data only
      if (!is.null(fd$response.type) && fd$response.type != "binary") {
        shiny::showNotification(
          i18n$t("CTT requires binary response data."),
          type = "warning"
        )
        return(NULL)
      }

      withProgress(message = i18n$t("Running CTT analysis..."), value = 0.5, {
        result <- tryCatch(
          exametrika::CTT(fd),
          error = function(e) {
            shiny::showNotification(
              paste(i18n$t("Analysis failed"), ":", e$message),
              type = "error"
            )
            NULL
          }
        )

        if (!is.null(result)) {
          log_append(script_log, c("fit_ctt <- CTT(dat)", "print(fit_ctt)"), label = "CTT")
        }
        result
      })
    })

    # --- Reliability coefficients: summary value_box ---
    output$reliability_summary <- renderUI({
      req(result())
      rel <- result()$Reliability

      alpha_val <- rel$value[rel$name == "Alpha(Covariance)"]
      omega_val <- rel$value[rel$name == "Omega(Covariance)"]

      tags$div(
        class = "d-flex flex-wrap gap-3 mb-3",
        bslib::value_box(
          title = i18n$t("Alpha (Covariance)"),
          value = tags$span(round(alpha_val, 3),
                            style = "font-size: 2rem; line-height: 1.2;"),
          showcase = icon("calculator"),
          showcase_layout = bslib::showcase_left_center(),
          theme = "primary",
          height = "100px",
          style = "flex: 1; min-width: 150px;"
        ),
        bslib::value_box(
          title = i18n$t("Omega (Covariance)"),
          value = tags$span(round(omega_val, 3),
                            style = "font-size: 2rem; line-height: 1.2;"),
          showcase = icon("chart-line"),
          showcase_layout = bslib::showcase_left_center(),
          theme = "info",
          height = "100px",
          style = "flex: 1; min-width: 150px;"
        )
      )
    })

    # --- Reliability coefficients table ---
    output$reliability_table <- DT::renderDT({
      req(result())
      df <- result()$Reliability
      colnames(df) <- c(i18n$t("Index"), i18n$t("Value"))
      DT::datatable(
        df,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          dom = "t",
          scrollX = TRUE
        )
      ) |>
        DT::formatRound(columns = i18n$t("Value"), digits = 4)
    })

    # --- Reliability if item deleted table ---
    output$item_deleted_table <- DT::renderDT({
      req(result())
      df <- result()$ReliabilityExcludingItem
      # Detect and round numeric columns
      num_cols <- names(df)[sapply(df, is.numeric)]
      dt <- DT::datatable(
        df,
        rownames = FALSE,
        options = list(
          pageLength = 20,
          scrollX = TRUE
        )
      )
      if (length(num_cols) > 0) dt <- DT::formatRound(dt, columns = num_cols, digits = 4)
      dt
    })

    # --- Downloads ---

    # Result tables exposed for download, named as Excel sheets (one report per
    # sheet, Shojima "Test Data Engineering" layout).
    report_sheets <- reactive({
      req(result())
      list(
        Reliability  = list(data = result()$Reliability, rowNames = FALSE),
        ItemAnalysis = list(data = result()$ReliabilityExcludingItem, rowNames = FALSE)
      )
    })

    mod_downloads_server(
      output, session, i18n,
      prefix = "CTT",
      result = result,
      sheets = report_sheets,
      csv_items = list(
        list(id = "dl_reliability", label = "Reliability",  sheet = "Reliability"),
        list(id = "dl_item",        label = "Item analysis", sheet = "ItemAnalysis")
      ),
      script_log = script_log
    )
  })
}
