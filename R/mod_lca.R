#' LCA モジュール UI
#'
#' @param id モジュールの名前空間 ID
#' @param i18n shiny.i18n Translator オブジェクト
#'
#' @noRd
mod_lca_ui <- function(id, i18n) {
  ns <- NS(id)

  bslib::layout_sidebar(
    # ========== サイドバー ==========
    sidebar = bslib::sidebar(
      width = 280,
      title = i18n$t("LCA"),

      sliderInput(
        ns("ncls"),
        label = i18n$t("Number of Classes"),
        min = 2, max = 10, value = 3, step = 1
      ),

      tags$hr(),

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

          # 適合度指標
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Fit Indices"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_fit"))
          ),

          # クラスプロファイル（IRP）
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Class Profiles (IRP)"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_irp")),
            downloadButton(ns("dl_irp"), i18n$t("Download CSV"), class = "mt-3 mb-2")
          ),

          # クラスサマリー（TRP / LCD）
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Class Summary"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_class_summary"))
          ),

          # 受検者クラス帰属
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Student Membership"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_students")),
            downloadButton(ns("dl_students"), i18n$t("Download CSV"), class = "mt-3 mb-2")
          )
        )
      ),

      # --- Item Fit タブ ---
      bslib::nav_panel(
        title = i18n$t("Item Fit"),
        bslib::card_body(
          DT::DTOutput(ns("table_item_fit"))
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
              "IRP (Item Reference Profile)" = "IRP",
              "TRP (Test Reference Profile)" = "TRP",
              "LCD (Latent Class Distribution)" = "LCD",
              "CMP (Class Membership Profile)" = "CMP"
            )
          ),
          plotOutput(ns("plot"), height = "500px"),
          downloadButton(ns("dl_plot"), i18n$t("Download Plot"), class = "mt-2")
        )
      )
    )
  )
}


#' LCA モジュール サーバ
#'
#' @param id モジュールの名前空間 ID
#' @param formatted_data リアクティブな dataFormat() 結果
#' @param i18n shiny.i18n Translator オブジェクト
#'
#' @noRd
mod_lca_server <- function(id, formatted_data, i18n) {
  moduleServer(id, function(input, output, session) {

    # ========== 分析実行 ==========
    result <- eventReactive(input$btn_run, {
      req(formatted_data())
      fd <- formatted_data()

      withProgress(message = i18n$t("Running LCA analysis..."), value = 0.5, {
        tryCatch(
          exametrika::LCA(fd, ncls = input$ncls),
          error = function(e) {
            shiny::showNotification(
              paste(i18n$t("Analysis failed"), ":", e$message),
              type = "error", duration = 10
            )
            NULL
          }
        )
      })
    })

    # ========== テーブル出力 ==========

    # 適合度指標（TestFitIndices を縦長に変換）
    output$table_fit <- DT::renderDT({
      req(result())
      fit <- result()$TestFitIndices
      fit_df <- data.frame(
        Index = names(fit),
        Value = unlist(fit),
        stringsAsFactors = FALSE
      )
      DT::datatable(fit_df, rownames = FALSE,
                    options = list(dom = "t", pageLength = 20)) |>
        DT::formatRound("Value", digits = 4)
    })

    # IRP テーブル（行: 項目、列: クラス）
    output$table_irp <- DT::renderDT({
      req(result())
      irp <- as.data.frame(result()$IRP)
      DT::datatable(irp, rownames = TRUE,
                    options = list(dom = "tip", pageLength = 20, scrollX = TRUE)) |>
        DT::formatRound(columns = seq_len(ncol(irp)), digits = 3)
    })

    # クラスサマリー（TRP: クラス平均得点, LCD: クラス人数）
    output$table_class_summary <- DT::renderDT({
      req(result())
      r <- result()
      ncls <- r$n_class
      df <- data.frame(
        Class     = paste0("Class ", seq_len(ncls)),
        TRP       = round(r$TRP, 3),
        LCD       = as.integer(r$LCD),
        LCD_pct   = round(as.integer(r$LCD) / sum(r$LCD) * 100, 1)
      )
      colnames(df) <- c("Class", "TRP", "N", "N (%)")
      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t", pageLength = 15))
    })

    # 受検者クラス帰属テーブル
    output$table_students <- DT::renderDT({
      req(result())
      df <- result()$Students
      num_cols <- names(df)[sapply(df, is.numeric) & names(df) != "Estimate"]
      dt <- DT::datatable(df, rownames = TRUE,
                          options = list(dom = "tip", pageLength = 20, scrollX = TRUE))
      if (length(num_cols) > 0) dt <- DT::formatRound(dt, columns = num_cols, digits = 3)
      dt
    })

    # 項目適合度テーブル
    output$table_item_fit <- DT::renderDT({
      req(result())
      df <- result()$ItemFitIndices
      num_cols <- names(df)[sapply(df, is.numeric)]
      dt <- DT::datatable(df, rownames = TRUE,
                          options = list(dom = "tip", pageLength = 20, scrollX = TRUE))
      if (length(num_cols) > 0) dt <- DT::formatRound(dt, columns = num_cols, digits = 4)
      dt
    })

    # ========== プロット ==========

    current_plot <- reactive({
      req(result())

      r <- result()

      if (requireNamespace("ggExametrika", quietly = TRUE)) {
        tryCatch(
          switch(input$plot_type,
            "IRP" = ggExametrika::plotIRP_gg(r),
            "TRP" = ggExametrika::plotTRP_gg(r),
            "LCD" = ggExametrika::plotLCD_gg(r),
            "CMP" = ggExametrika::plotCMP_gg(r)
          ),
          error = function(e) {
            # ggExametrika に未実装の場合は base plot で代替
            plot(r, type = input$plot_type)
            NULL
          }
        )
      } else {
        plot(r, type = input$plot_type)
        NULL
      }
    })

    output$plot <- renderPlot({
      req(result())
      p <- current_plot()
      if (!is.null(p)) print(p)
    })

    # ========== ダウンロード ==========

    output$dl_irp <- downloadHandler(
      filename = function() paste0("LCA_IRP_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(result()$IRP, file, row.names = TRUE)
    )

    output$dl_students <- downloadHandler(
      filename = function() paste0("LCA_Students_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(result()$Students, file, row.names = TRUE)
    )

    output$dl_plot <- downloadHandler(
      filename = function() paste0("LCA_", input$plot_type, "_", Sys.Date(), ".png"),
      content  = function(file) {
        p <- current_plot()
        if (!is.null(p)) {
          ggplot2::ggsave(file, plot = p, width = 8, height = 5, dpi = 300)
        } else {
          png(file, width = 800, height = 500)
          plot(result(), type = input$plot_type)
          dev.off()
        }
      }
    )
  })
}
