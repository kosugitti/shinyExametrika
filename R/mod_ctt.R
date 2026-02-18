#' CTT 分析モジュール UI
#'
#' @param id モジュールの名前空間 ID
#' @param i18n shiny.i18n Translator オブジェクト
#'
#' @noRd
mod_ctt_ui <- function(id, i18n) {
  ns <- NS(id)

  bslib::layout_sidebar(
    # --- サイドバー ---
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

      # Reliability と ReliabilityExcludingItem を別々にダウンロード
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

    # --- メインパネル ---
    bslib::navset_card_tab(
      id = ns("result_tabs"),

      # 信頼性係数タブ
      bslib::nav_panel(
        title = i18n$t("Reliability"),
        bslib::card_body(
          uiOutput(ns("reliability_summary")),
          DT::DTOutput(ns("reliability_table"))
        )
      ),

      # 項目削除時の信頼性タブ
      bslib::nav_panel(
        title = i18n$t("Reliability if Item Deleted"),
        bslib::card_body(
          DT::DTOutput(ns("item_deleted_table"))
        )
      )
    )
  )
}


#' CTT 分析モジュール Server
#'
#' @param id モジュールの名前空間 ID
#' @param formatted_data reactive: exametrika の dataFormat() 結果
#' @param i18n shiny.i18n Translator オブジェクト
#'
#' @noRd
mod_ctt_server <- function(id, formatted_data, i18n) {
  moduleServer(id, function(input, output, session) {

    # --- CTT 分析結果 ---
    ctt_result <- reactiveVal(NULL)

    # --- 分析実行 ---
    observeEvent(input$btn_run, {
      req(formatted_data())

      fd <- formatted_data()

      # binary データのみ CTT 対応
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

    # --- 信頼性係数: サマリー value_box ---
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

    # --- 信頼性係数テーブル ---
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

    # --- 項目削除時の信頼性テーブル ---
    output$item_deleted_table <- DT::renderDT({
      req(ctt_result())
      df <- ctt_result()$ReliabilityExcludingItem
      # 数値列を自動検出して丸める
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

    # --- CSV ダウンロード: Reliability ---
    output$dl_reliability <- downloadHandler(
      filename = function() paste0("CTT_Reliability_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(ctt_result())
        utils::write.csv(ctt_result()$Reliability, file, row.names = FALSE)
      }
    )

    # --- CSV ダウンロード: ReliabilityExcludingItem ---
    output$dl_item_deleted <- downloadHandler(
      filename = function() paste0("CTT_ReliabilityExcludingItem_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(ctt_result())
        utils::write.csv(ctt_result()$ReliabilityExcludingItem, file, row.names = FALSE)
      }
    )
  })
}
