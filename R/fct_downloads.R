#' Unified result-download helpers
#'
#' Provides a consistent download section (placed in each analysis module's
#' sidebar, below the Run button) across all models:
#'   - one CSV button per result table (fit indices, item parameters, examinee
#'     parameters, ...)
#'   - one "all results" button that writes a single multi-sheet Excel workbook
#'     in the Shojima "Test Data Engineering" layout (one report per sheet,
#'     English CamelCase sheet names: TestFit / ItemReport / ScoreReport ...),
#'     matching exametrika/develop's reference *.xlsx output.
#'
#' A module supplies:
#'   - `sheets`  : a reactive returning a named list `name -> list(data, rowNames)`
#'                 where `name` is the Excel sheet name (and report key).
#'   - `csv_items`: a static list of `list(id, label, sheet)` describing which
#'                 sheets get an individual CSV button, in display order.
#'
#' @noRd
NULL


#' Write a multi-sheet Excel report (openxlsx) in the reference layout
#'
#' @param path output .xlsx path
#' @param sheets named list: sheet_name -> list(data = <data.frame/matrix>, rowNames = <lgl>)
#' @noRd
write_report_xlsx <- function(path, sheets) {
  wb <- openxlsx::createWorkbook()
  for (nm in names(sheets)) {
    spec <- sheets[[nm]]
    if (is.null(spec) || is.null(spec$data)) next
    df <- as.data.frame(spec$data, check.names = FALSE, stringsAsFactors = FALSE)
    sn <- substr(nm, 1, 31)            # Excel sheet-name length limit
    openxlsx::addWorksheet(wb, sn)
    openxlsx::writeData(wb, sn, df, rowNames = isTRUE(spec$rowNames))
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}


#' Render the sidebar download section UI (call from server -> output$dl_panel)
#'
#' Shown only once a result is available, so there are no dead buttons.
#'
#' @noRd
downloads_panel <- function(ns, i18n, csv_items) {
  buttons <- lapply(csv_items, function(it) {
    downloadButton(
      ns(it$id),
      i18n$t(it$label),
      icon = icon("file-csv"),
      class = "btn-sm btn-outline-secondary w-100 mb-2"
    )
  })
  tagList(
    tags$hr(),
    tags$h6(i18n$t("Outputs"), class = "mt-1 mb-2"),
    buttons,
    downloadButton(
      ns("dl_xlsx"),
      i18n$t("All results (Excel)"),
      icon = icon("file-excel"),
      class = "btn-sm btn-primary w-100 mb-1"
    )
  )
}


#' Sidebar download block for a module (call from the module UI, in the sidebar)
#'
#' Combines the result-gated Outputs section (rendered server-side into
#' `dl_panel`) with the always-available session R-script button, so every
#' download lives together in the left sidebar.
#'
#' @noRd
download_sidebar_ui <- function(ns, i18n) {
  tagList(
    uiOutput(ns("dl_panel")),
    tags$div(
      class = "mt-2",
      downloadButton(
        ns("dl_script"),
        i18n$t("R script"),
        icon = icon("file-code"),
        class = "btn-sm btn-outline-secondary w-100"
      )
    )
  )
}


#' Wire the unified download section for a module
#'
#' Renders `output$dl_panel` (the sidebar section) and registers every CSV
#' handler, the combined Excel handler, and the session R-script handler.
#'
#' @param output,session module server's output/session
#' @param i18n translator
#' @param prefix file-name prefix, e.g. "IRT"
#' @param result reactive returning the fitted object (gates visibility)
#' @param sheets reactive returning the named sheet list (see write_report_xlsx)
#' @param csv_items list of list(id, label, sheet)
#' @param script_log reactiveVal of the session script log (for the R-script button)
#' @noRd
mod_downloads_server <- function(output, session, i18n, prefix, result, sheets, csv_items,
                                 script_log = NULL) {
  ns <- session$ns

  # Sidebar section appears only after a successful run
  output$dl_panel <- renderUI({
    req(result())
    downloads_panel(ns, i18n, csv_items)
  })

  # Session-wide reproduction script (always available)
  output$dl_script <- downloadHandler(
    filename = function() paste0("shinyExametrika_session_", Sys.Date(), ".R"),
    content = function(file) {
      lines <- if (is.null(script_log)) character(0) else assemble_script(script_log())
      writeLines(lines, file)
    }
  )

  # Individual CSV buttons
  for (it in csv_items) {
    local({
      item <- it
      output[[item$id]] <- downloadHandler(
        filename = function() paste0(prefix, "_", item$sheet, "_", Sys.Date(), ".csv"),
        content = function(file) {
          spec <- sheets()[[item$sheet]]
          utils::write.csv(spec$data, file, row.names = isTRUE(spec$rowNames))
        }
      )
    })
  }

  # Combined multi-sheet Excel
  output$dl_xlsx <- downloadHandler(
    filename = function() paste0(prefix, "_results_", Sys.Date(), ".xlsx"),
    content = function(file) {
      write_report_xlsx(file, sheets())
    }
  )
}
