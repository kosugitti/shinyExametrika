#' IRT モジュール UI
#'
#' @param id モジュールの名前空間 ID
#' @param i18n shiny.i18n Translator オブジェクト
#'
#' @noRd
mod_irt_ui <- function(id, i18n) {
  ns <- NS(id)

  bslib::layout_sidebar(
    # ========== サイドバー ==========
    sidebar = bslib::sidebar(
      width = 300,
      title = i18n$t("Settings"),

      # モデル選択
      radioButtons(
        ns("model"),
        label = i18n$t("IRT Model"),
        choices = c(
          "2PL (2-Parameter Logistic)" = "2",
          "3PL (3-Parameter Logistic)" = "3",
          "4PL (4-Parameter Logistic)" = "4"
        ),
        selected = "2"
      ),

      tags$hr(),

      # 実行ボタン
      actionButton(
        ns("btn_run"),
        label = i18n$t("Run Analysis"),
        class = "btn-primary",
        width = "100%"
      )
    ),

    # ========== メインパネル ==========
    tabsetPanel(
      id = ns("main_tabs"),

      # --- Results タブ ---
      tabPanel(
        title = i18n$t("Results"),
        value = "tab_results",

        # 適合度指標セクション
        tags$div(
          class = "mb-5",
          tags$h5(i18n$t("Fit Indices"), class = "mt-3 mb-3"),
          DT::DTOutput(ns("table_fit"))
        ),

        # 項目パラメータセクション
        tags$div(
          class = "mb-5",
          tags$h5(i18n$t("Item Parameters"), class = "mt-3 mb-3"),
          DT::DTOutput(ns("table_params")),
          downloadButton(ns("download_params"), i18n$t("Download CSV"), class = "mt-3 mb-2")
        ),

        # 能力推定値セクション
        tags$div(
          class = "mb-5",
          tags$h5(i18n$t("Ability Estimates"), class = "mt-3 mb-3"),
          DT::DTOutput(ns("table_ability")),
          downloadButton(ns("download_ability"), i18n$t("Download CSV"), class = "mt-3 mb-2")
        )
      ),

      # --- Plots タブ ---
      tabPanel(
        title = i18n$t("Plots"),
        value = "tab_plots",

        selectInput(
          ns("plot_type"),
          label = i18n$t("Plot Type"),
          choices = c(
            "ICC (Item Characteristic Curve)" = "ICC",
            "TRF (Test Response Function)" = "TRF",
            "IIC (Item Information Curve)" = "IIC",
            "TIC (Test Information Curve)" = "TIC"
          )
        ),

        plotOutput(ns("plot"), height = "600px"),
        downloadButton(ns("download_plot"), i18n$t("Download Plot"), class = "mt-2")
      )
    )
  )
}


#' IRT モジュール サーバ
#'
#' @param id モジュールの名前空間 ID
#' @param formatted_data リアクティブな dataFormat() 結果
#' @param i18n shiny.i18n Translator オブジェクト
#'
#' @noRd
mod_irt_server <- function(id, formatted_data, i18n) {
  moduleServer(id, function(input, output, session) {

    # ========== リアクティブ値 ==========

    # IRT分析結果
    result <- eventReactive(input$btn_run, {
      req(formatted_data())

      fd <- formatted_data()
      model_num <- as.numeric(input$model)

      # 二値データチェック
      maxscore <- fd$maxscore
      if (!is.null(maxscore) && length(maxscore) > 0 && any(maxscore > 1)) {
        shiny::showNotification(
          "IRT requires binary data (0/1). Please use GRM for ordinal data.",
          type = "warning",
          duration = 5
        )
        return(NULL)
      }

      # プログレスバー表示
      withProgress(message = 'Running IRT analysis...', value = 0, {
        incProgress(0.3, detail = "Estimating parameters...")

        # IRT実行（exametrikaオブジェクト全体を渡す）
        result <- tryCatch(
          exametrika::IRT(fd, model = model_num),
          error = function(e) {
            shiny::showNotification(
              paste("Error:", e$message),
              type = "error",
              duration = 10
            )
            NULL
          }
        )

        incProgress(1)
        result
      })
    })

    # ========== テーブル出力 ==========

    # 適合度指標
    output$table_fit <- DT::renderDT({
      req(result())

      fit <- result()$TestFitIndices
      # リストをデータフレームに変換
      fit_df <- data.frame(
        Index = names(fit),
        Value = unlist(fit),
        stringsAsFactors = FALSE
      )

      dt <- DT::datatable(
        fit_df,
        options = list(dom = 't', pageLength = 20),
        rownames = FALSE
      )
      DT::formatRound(dt, columns = "Value", digits = 4)
    })

    # 項目パラメータ
    output$table_params <- DT::renderDT({
      req(result())

      params <- result()$params
      dt <- DT::datatable(
        params,
        options = list(dom = 'tip', pageLength = 20),
        rownames = TRUE
      )
      DT::formatRound(dt, columns = 1:ncol(params), digits = 3)
    })

    # 能力推定値
    output$table_ability <- DT::renderDT({
      req(result())

      ability <- result()$ability

      dt <- DT::datatable(
        ability,
        options = list(dom = 'tip', pageLength = 20),
        rownames = FALSE
      )
      DT::formatRound(dt, columns = c("EAP", "PSD"), digits = 3)
    })

    # ========== プロット ==========

    output$plot <- renderPlot({
      req(result())

      plot_type <- input$plot_type

      if (plot_type == "ICC") {
        ggExametrika::plotICC_gg(result())
      } else if (plot_type == "TRF") {
        ggExametrika::plotTRF_gg(result())
      } else if (plot_type == "IIC") {
        ggExametrika::plotIIC_gg(result())
      } else if (plot_type == "TIC") {
        ggExametrika::plotTIC_gg(result())
      }
    })

    # ========== ダウンロード ==========

    # パラメータCSV
    output$download_params <- downloadHandler(
      filename = function() {
        paste0("IRT_parameters_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(result()$params, file, row.names = TRUE)
      }
    )

    # 能力推定値CSV
    output$download_ability <- downloadHandler(
      filename = function() {
        paste0("IRT_ability_", Sys.Date(), ".csv")
      },
      content = function(file) {
        ability <- result()$ability
        write.csv(ability, file, row.names = FALSE)
      }
    )

    # プロットPNG
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("IRT_", input$plot_type, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        plot_type <- input$plot_type

        if (plot_type == "ICC") {
          p <- ggExametrika::plotICC_gg(result())
        } else if (plot_type == "TRF") {
          p <- ggExametrika::plotTRF_gg(result())
        } else if (plot_type == "IIC") {
          p <- ggExametrika::plotIIC_gg(result())
        } else if (plot_type == "TIC") {
          p <- ggExametrika::plotTIC_gg(result())
        }

        ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
      }
    )
  })
}
