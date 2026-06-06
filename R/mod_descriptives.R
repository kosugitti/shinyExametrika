#' Descriptives Module UI
#'
#' @param id Module namespace ID
#' @param i18n shiny.i18n Translator object
#'
#' @noRd
mod_descriptives_ui <- function(id, i18n) {
  ns <- NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 250,
      title = i18n$t("Descriptives"),

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

    uiOutput(ns("precheck")),
    model_help_block("descriptives", i18n),

    bslib::navset_card_tab(
      bslib::nav_panel(
        title = i18n$t("Test Statistics"),
        bslib::card_body(
          DT::DTOutput(ns("test_stats_table"))
        )
      ),
      bslib::nav_panel(
        title = i18n$t("Item Statistics"),
        bslib::card_body(
          DT::DTOutput(ns("item_stats_table"))
        )
      )
    )
  )
}


#' Descriptives Module Server
#'
#' @param id Module namespace ID
#' @param formatted_data reactive: result of exametrika dataFormat()
#' @param i18n shiny.i18n Translator object
#'
#' @noRd
mod_descriptives_server <- function(id, formatted_data, i18n, script_log = NULL) {
  moduleServer(id, function(input, output, session) {

    # ========== Data-readiness banner ==========
    output$precheck <- renderUI({
      precheck_banner(formatted_data(), required = "any", i18n)
    })

    desc_result <- reactiveVal(NULL)

    observeEvent(input$btn_run, {
      req(formatted_data())
      fd <- formatted_data()

      withProgress(message = i18n$t("Running Descriptives analysis..."), value = 0.5, {
        tryCatch({
          result <- list(
            test  = exametrika::TestStatistics(fd),
            item  = exametrika::ItemStatistics(fd)
          )
          desc_result(result)
          log_append(script_log, c(
            "print(TestStatistics(dat))",
            "print(ItemStatistics(dat))"
          ), label = "Descriptive statistics")
          showNotification(i18n$t("Analysis completed!"), type = "message")
        }, error = function(e) {
          showNotification(
            paste(i18n$t("Analysis failed"), ":", e$message),
            type = "error"
          )
        })
      })
    })

    # Helper to convert TestStatistics to data.frame
    # Fields differ between binary/ordinal, so dynamically scan names(ts)
    test_stats_df <- reactive({
      req(desc_result())
      ts <- desc_result()$test

      rows <- lapply(names(ts), function(nm) {
        val <- ts[[nm]]
        if (is.null(val)) return(NULL)

        if (length(val) == 1) {
          data.frame(Statistic = nm, Value = as.numeric(val))
        } else {
          # Named vector (e.g. Stanine) -- expand to multiple rows
          labels <- if (!is.null(names(val))) {
            paste0(nm, " (", names(val), ")")
          } else {
            paste0(nm, "[", seq_along(val), "]")
          }
          data.frame(Statistic = labels, Value = as.numeric(val))
        }
      })

      do.call(rbind, Filter(Negate(is.null), rows))
    })

    # Helper to convert ItemStatistics to data.frame
    # Fields differ between binary/ordinal, so build dynamically with existence checks
    item_stats_df <- reactive({
      req(desc_result())
      is_r <- desc_result()$item

      df <- data.frame(
        Item = is_r$ItemLabel,
        NR   = as.integer(is_r$NR),
        stringsAsFactors = FALSE
      )

      # CRR, ODDs are binary only
      if (!is.null(is_r$CRR))  df$CRR  <- round(drop(is_r$CRR),  3)
      if (!is.null(is_r$ODDs)) df$ODDs <- round(drop(is_r$ODDs), 3)

      # Threshold: binary is n x 1 matrix (1 column), ordinal is n x (cat-1) matrix (multiple columns)
      if (!is.null(is_r$Threshold)) {
        thr <- is_r$Threshold
        if (ncol(thr) == 1) {
          df$Threshold <- round(drop(thr), 3)
        } else {
          for (j in seq_len(ncol(thr))) {
            df[[paste0("Threshold.", j)]] <- round(thr[, j], 3)
          }
        }
      }

      if (!is.null(is_r$Entropy)) df$Entropy <- round(drop(is_r$Entropy), 3)
      if (!is.null(is_r$ITCrr))   df$ITCrr   <- round(drop(is_r$ITCrr),  3)

      df
    })

    # --- Test Statistics table ---
    output$test_stats_table <- DT::renderDT({
      req(test_stats_df())
      DT::datatable(
        test_stats_df(),
        rownames = FALSE,
        options = list(
          pageLength = 30,
          dom = "t",
          scrollX = TRUE
        )
      ) |>
        DT::formatRound("Value", digits = 4)
    })

    # --- Item Statistics table ---
    output$item_stats_table <- DT::renderDT({
      req(item_stats_df())
      # Round numeric columns only
      df <- item_stats_df()
      num_cols <- names(df)[sapply(df, is.numeric)]
      dt <- DT::datatable(
        df,
        rownames = FALSE,
        options = list(
          pageLength = 30,
          scrollX = TRUE
        )
      )
      if (length(num_cols) > 0) dt <- DT::formatRound(dt, columns = num_cols, digits = 3)
      dt
    })

    # ========== Downloads ==========

    # Result tables exposed for download, named as Excel sheets (one report per
    # sheet, Shojima "Test Data Engineering" layout).
    report_sheets <- reactive({
      req(desc_result())
      list(
        TestStatistics = list(data = test_stats_df(), rowNames = FALSE),
        ItemStatistics = list(data = item_stats_df(), rowNames = FALSE)
      )
    })

    mod_downloads_server(
      output, session, i18n,
      prefix = "Descriptives",
      result = desc_result,
      sheets = report_sheets,
      csv_items = list(
        list(id = "dl_test", label = "Test statistics", sheet = "TestStatistics"),
        list(id = "dl_item", label = "Item statistics", sheet = "ItemStatistics")
      ),
      script_log = script_log
    )
  })
}
