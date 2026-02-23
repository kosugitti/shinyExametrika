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

      tags$hr(),

      # Download Reliability and ReliabilityExcludingItem separately
      downloadButton(
        ns("dl_reliability"),
        label = i18n$t("Download CSV (Reliability)"),
        class = "btn-outline-secondary w-100 mb-2"
      ),
      downloadButton(
        ns("dl_item_deleted"),
        label = i18n$t("Download CSV (Item Deleted)"),
        class = "btn-outline-secondary w-100"
      )
    ),

    # --- Main panel ---
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
mod_ctt_server <- function(id, formatted_data, i18n) {
  moduleServer(id, function(input, output, session) {

    # --- CTT analysis result ---
    ctt_result <- reactiveVal(NULL)

    # --- Run analysis ---
    observeEvent(input$btn_run, {
      req(formatted_data())

      fd <- formatted_data()

      # CTT supports binary data only
      if (!is.null(fd$response.type) && fd$response.type != "binary") {
        showNotification(
          i18n$t("CTT requires binary response data."),
          type = "warning"
        )
        return()
      }

      withProgress(message = i18n$t("Running CTT analysis..."), value = 0.5, {
        tryCatch({
          result <- exametrika::CTT(fd)
          ctt_result(result)
          showNotification(i18n$t("Analysis completed!"), type = "message")
        }, error = function(e) {
          showNotification(
            paste(i18n$t("Analysis failed"), ":", e$message),
            type = "error"
          )
        })
      })
    })

    # --- Reliability coefficients: summary value_box ---
    output$reliability_summary <- renderUI({
      req(ctt_result())
      rel <- ctt_result()$Reliability

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
      req(ctt_result())
      df <- ctt_result()$Reliability
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
      req(ctt_result())
      df <- ctt_result()$ReliabilityExcludingItem
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

    # --- CSV download: Reliability ---
    output$dl_reliability <- downloadHandler(
      filename = function() paste0("CTT_Reliability_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(ctt_result())
        utils::write.csv(ctt_result()$Reliability, file, row.names = FALSE)
      }
    )

    # --- CSV download: ReliabilityExcludingItem ---
    output$dl_item_deleted <- downloadHandler(
      filename = function() paste0("CTT_ReliabilityExcludingItem_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(ctt_result())
        utils::write.csv(ctt_result()$ReliabilityExcludingItem, file, row.names = FALSE)
      }
    )
  })
}
