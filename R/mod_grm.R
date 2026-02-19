#' GRM モジュール UI
#'
#' @param id モジュールの名前空間 ID
#' @param i18n shiny.i18n Translator オブジェクト
#'
#' @noRd
mod_grm_ui <- function(id, i18n) {
  ns <- NS(id)

  bslib::layout_sidebar(
    # ========== サイドバー ==========
    sidebar = bslib::sidebar(
      width = 300,
      title = i18n$t("GRM"),

      # モデル説明
      tags$p(
        class = "text-muted small",
        i18n$t("GRM analyzes ordinal response data using Graded Response Model.")
      ),

      tags$hr(),

      # 実行ボタン
      actionButton(
        ns("btn_run"),
        label = i18n$t("Run Analysis"),
        class = "btn-primary w-100",
        icon = icon("play")
      )
    ),

    # ========== メインパネル ==========
    bslib::navset_card_tab(
      id = ns("main_tabs"),

      # --- Results タブ ---
      bslib::nav_panel(
        title = i18n$t("Results"),
        bslib::card_body(
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
        )
      ),

      # --- Plots タブ ---
      bslib::nav_panel(
        title = i18n$t("Plots"),
        bslib::card_body(
          selectInput(
            ns("plot_type"),
            label = i18n$t("Plot Type"),
            choices = c(
              # TODO: ggExametrika に plotICRF_gg 実装後に有効化する
              # "ICRF (Item Category Response Function)" = "ICRF",
              "IIC (Item Information Curve)" = "IIC",
              "TIC (Test Information Curve)" = "TIC"
            )
          ),

          # アイテム選択UI（ICRF/IICの場合のみ表示）
          uiOutput(ns("item_selection_ui")),

          plotOutput(ns("plot"), height = "600px"),
          downloadButton(ns("download_plot"), i18n$t("Download Plot"), class = "mt-2")
        )
      )
    )
  )
}


#' GRM モジュール サーバ
#'
#' @param id モジュールの名前空間 ID
#' @param formatted_data リアクティブな dataFormat() 結果
#' @param i18n shiny.i18n Translator オブジェクト
#'
#' @noRd
mod_grm_server <- function(id, formatted_data, i18n) {
  moduleServer(id, function(input, output, session) {

    # ========== リアクティブ値 ==========

    # GRM分析結果
    result <- eventReactive(input$btn_run, {
      req(formatted_data())

      fd <- formatted_data()

      # 順序データチェック（response.typeを確認）
      if (!is.null(fd$response.type) && fd$response.type == "binary") {
        shiny::showNotification(
          i18n$t("GRM requires ordinal data (more than 2 categories). Please use IRT for binary data."),
          type = "warning",
          duration = 5
        )
        return(NULL)
      }

      # categoriesチェック（データ整合性）
      if (!is.null(fd$categories) && length(fd$categories) > 0) {
        if (all(fd$categories == 2)) {
          shiny::showNotification(
            i18n$t("All items have only 2 categories (binary data). Please use IRT for binary data."),
            type = "warning",
            duration = 5
          )
          return(NULL)
        }
      }

      # プログレスバー表示
      withProgress(message = i18n$t("Running GRM analysis..."), value = 0, {
        incProgress(0.3, detail = i18n$t("Estimating parameters..."))

        # GRM実行（exametrikaオブジェクト全体を渡す）
        result <- tryCatch(
          exametrika::GRM(fd),
          error = function(e) {
            error_msg <- e$message
            # 特定のエラーに対する追加情報
            if (grepl("subscript|添え字", error_msg)) {
              error_msg <- paste(
                error_msg,
                "\n\nNote: Some sample datasets (e.g., J15S3810) may have data format issues.",
                "Try using J5S1000 or other GRM-compatible datasets."
              )
            }
            shiny::showNotification(
              paste(i18n$t("Analysis failed"), ":\n", error_msg),
              type = "error",
              duration = 15
            )
            NULL
          }
        )

        incProgress(1)
        result
      })
    })

    # ========== アイテム選択UI（動的） ==========

    output$item_selection_ui <- renderUI({
      req(result())

      # IICの場合のみアイテム選択UIを表示
      if (input$plot_type == "IIC") {
        item_names <- rownames(result()$params)

        tags$div(
          class = "mb-3",
          checkboxGroupInput(
            session$ns("selected_items"),
            label = i18n$t("Select Items to Plot"),
            choices = setNames(seq_along(item_names), item_names),
            selected = seq_along(item_names),  # デフォルトは全選択
            inline = TRUE
          )
        )
      }
    })

    # ========== プロット生成ヘルパー ==========

    current_plot <- reactive({
      req(result())

      if (!requireNamespace("ggExametrika", quietly = TRUE)) {
        shiny::showNotification(
          i18n$t("ggExametrika package is required for plots."),
          type = "warning"
        )
        return(NULL)
      }

      tryCatch({
        switch(input$plot_type,
          "IIC" = {
            items <- if (!is.null(input$selected_items) && length(input$selected_items) > 0) {
              as.numeric(input$selected_items)
            } else {
              NULL
            }
            ggExametrika::plotIIC_overlay_gg(result(), items = items, show_legend = TRUE)
          },
          "TIC" = ggExametrika::plotTIC_gg(result())
        )
      }, error = function(e) {
        shiny::showNotification(
          paste(i18n$t("Plot generation failed"), ":", e$message),
          type = "error",
          duration = 10
        )
        NULL
      })
    })

    # ========== テーブル出力 ==========

    # 適合度指標（共通ヘルパー関数を使用）
    output$table_fit <- DT::renderDT({
      req(result())

      fit_df <- extract_fit_indices(result())

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

    # 能力推定値（共通ヘルパー関数を使用）
    output$table_ability <- DT::renderDT({
      req(result())

      ability <- extract_ability(result())

      # 数値列のみ丸め対象にする
      numeric_cols <- names(ability)[vapply(ability, is.numeric, logical(1))]

      dt <- DT::datatable(
        ability,
        options = list(dom = 'tip', pageLength = 20),
        rownames = FALSE
      )
      if (length(numeric_cols) > 0) {
        dt <- DT::formatRound(dt, columns = numeric_cols, digits = 3)
      }
      dt
    })

    # ========== プロット ==========

    output$plot <- renderPlot({
      p <- current_plot()
      shiny::validate(
        shiny::need(!is.null(p), i18n$t("Plot generation failed"))
      )
      p
    })

    # ========== ダウンロード ==========

    # パラメータCSV
    output$download_params <- downloadHandler(
      filename = function() {
        paste0("GRM_parameters_", Sys.Date(), ".csv")
      },
      content = function(file) {
        utils::write.csv(result()$params, file, row.names = TRUE)
      }
    )

    # 能力推定値CSV（共通ヘルパー関数を使用）
    output$download_ability <- downloadHandler(
      filename = function() {
        paste0("GRM_ability_", Sys.Date(), ".csv")
      },
      content = function(file) {
        ability <- extract_ability(result())
        utils::write.csv(ability, file, row.names = FALSE)
      }
    )

    # プロットPNG
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("GRM_", input$plot_type, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        p <- current_plot()
        req(p)
        ggplot2::ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
      }
    )
  })
}
