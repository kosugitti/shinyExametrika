#' Descriptives モジュール UI
#'
#' @param id モジュールの名前空間 ID
#' @param i18n shiny.i18n Translator オブジェクト
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

      tags$hr(),

      downloadButton(
        ns("dl_test_csv"),
        label = paste(i18n$t("Download CSV"), "(Test)"),
        class = "btn-outline-secondary w-100 mb-2"
      ),
      downloadButton(
        ns("dl_item_csv"),
        label = paste(i18n$t("Download CSV"), "(Item)"),
        class = "btn-outline-secondary w-100"
      )
    ),

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


#' Descriptives モジュール Server
#'
#' @param id モジュールの名前空間 ID
#' @param formatted_data reactive: exametrika の dataFormat() 結果
#' @param i18n shiny.i18n Translator オブジェクト
#'
#' @noRd
mod_descriptives_server <- function(id, formatted_data, i18n) {
  moduleServer(id, function(input, output, session) {

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
          showNotification(i18n$t("Analysis completed!"), type = "message")
        }, error = function(e) {
          showNotification(
            paste(i18n$t("Analysis failed"), ":", e$message),
            type = "error"
          )
        })
      })
    })

    # TestStatistics を data.frame に変換するヘルパー
    test_stats_df <- reactive({
      req(desc_result())
      ts <- desc_result()$test

      scalars <- c("TestLength", "SampleSize", "Mean", "SEofMean",
                   "Variance", "SD", "Skewness", "Kurtosis",
                   "Min", "Max", "Range")
      rows <- lapply(scalars, function(nm) {
        data.frame(Statistic = nm, Value = as.numeric(ts[[nm]]))
      })

      # Q1, Median, Q3, IQR（named numeric length-1）
      for (nm in c("Q1", "Median", "Q3", "IQR")) {
        rows <- c(rows, list(data.frame(Statistic = nm, Value = as.numeric(ts[[nm]]))))
      }

      # Stanine（named numeric length-8）
      stanine_vals <- ts$Stanine
      stanine_rows <- lapply(seq_along(stanine_vals), function(i) {
        data.frame(
          Statistic = paste0("Stanine (", names(stanine_vals)[i], ")"),
          Value     = as.numeric(stanine_vals[i])
        )
      })
      rows <- c(rows, stanine_rows)

      do.call(rbind, rows)
    })

    # ItemStatistics を data.frame に変換するヘルパー
    item_stats_df <- reactive({
      req(desc_result())
      is_r <- desc_result()$item
      data.frame(
        Item      = is_r$ItemLabel,
        NR        = as.integer(is_r$NR),
        CRR       = round(as.numeric(is_r$CRR),   3),
        ODDs      = round(as.numeric(is_r$ODDs),  3),
        Threshold = round(as.numeric(is_r$Threshold), 3),
        Entropy   = round(as.numeric(is_r$Entropy),   3),
        ITCrr     = round(as.numeric(is_r$ITCrr),     3),
        stringsAsFactors = FALSE
      )
    })

    # --- Test Statistics テーブル ---
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

    # --- Item Statistics テーブル ---
    output$item_stats_table <- DT::renderDT({
      req(item_stats_df())
      DT::datatable(
        item_stats_df(),
        rownames = FALSE,
        options = list(
          pageLength = 30,
          scrollX = TRUE
        )
      )
    })

    # --- CSV ダウンロード ---
    output$dl_test_csv <- downloadHandler(
      filename = function() "test_statistics.csv",
      content  = function(file) utils::write.csv(test_stats_df(), file, row.names = FALSE)
    )

    output$dl_item_csv <- downloadHandler(
      filename = function() "item_statistics.csv",
      content  = function(file) utils::write.csv(item_stats_df(), file, row.names = FALSE)
    )
  })
}
