#' LRA モジュール UI
#'
#' @param id モジュールの名前空間 ID
#' @param i18n shiny.i18n Translator オブジェクト
#'
#' @noRd
mod_lra_ui <- function(id, i18n) {
  ns <- NS(id)

  bslib::layout_sidebar(
    # ========== サイドバー ==========
    sidebar = bslib::sidebar(
      width = 300,
      title = i18n$t("LRA"),

      sliderInput(
        ns("nrank"),
        label = i18n$t("Number of Ranks"),
        min = 2, max = 10, value = 3, step = 1
      ),

      radioButtons(
        ns("method"),
        label = i18n$t("Method"),
        choices = c("GTM", "SOM"),
        selected = "GTM",
        inline = TRUE
      ),

      checkboxInput(
        ns("mic"),
        label = i18n$t("Monotone Increasing Constraint"),
        value = FALSE
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

          # IRP テーブル
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("IRP (Item Reference Profile)"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_irp")),
            downloadButton(ns("dl_irp"), i18n$t("Download CSV"), class = "mt-3 mb-2")
          ),

          # IRP Index テーブル
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("IRP Index"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_irp_index"))
          ),

          # ランクサマリー（TRP / LRD）
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Rank Summary"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_rank_summary"))
          ),

          # 受検者ランク帰属
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
              "LRD (Latent Rank Distribution)" = "LRD",
              "RMP (Rank Membership Profile)" = "RMP"
            )
          ),
          uiOutput(ns("item_selector_ui")),
          uiOutput(ns("student_selector_ui")),
          plotOutput(ns("plot"), height = "600px"),
          downloadButton(ns("dl_plot"), i18n$t("Download Plot"), class = "mt-2")
        )
      )
    )
  )
}


#' LRA モジュール サーバ
#'
#' @param id モジュールの名前空間 ID
#' @param formatted_data リアクティブな dataFormat() 結果
#' @param i18n shiny.i18n Translator オブジェクト
#'
#' @noRd
mod_lra_server <- function(id, formatted_data, i18n) {
  moduleServer(id, function(input, output, session) {

    # ========== 分析実行 ==========
    result <- eventReactive(input$btn_run, {
      req(formatted_data())
      fd <- formatted_data()

      # 二値データチェック（IRT と同様に maxscore を使用）
      maxscore <- fd$maxscore
      if (!is.null(maxscore) && length(maxscore) > 0 && any(maxscore > 1)) {
        shiny::showNotification(
          i18n$t("LRA requires binary response data."),
          type = "warning", duration = 5
        )
        return(NULL)
      }

      withProgress(message = i18n$t("Running LRA analysis..."), value = 0, {
        incProgress(0.3, detail = i18n$t("Estimating parameters..."))
        r <- tryCatch(
          exametrika::LRA(fd, nrank = input$nrank, method = input$method, mic = input$mic),
          error = function(e) {
            shiny::showNotification(
              paste(i18n$t("Analysis failed"), ":", e$message),
              type = "error", duration = 10
            )
            NULL
          }
        )
        incProgress(1)
        if (!is.null(r)) {
          shiny::showNotification(i18n$t("Analysis completed!"), type = "message", duration = 3)
        }
        r
      })
    })

    # ========== テーブル出力 ==========

    # 適合度指標（共通ヘルパー関数を使用）
    output$table_fit <- DT::renderDT({
      req(result())
      fit_df <- extract_fit_indices(result())
      dt <- DT::datatable(fit_df, rownames = FALSE,
                          options = list(dom = "t", pageLength = 20))
      DT::formatRound(dt, columns = "Value", digits = 4)
    })

    # IRP テーブル（行: 項目、列: ランク）
    output$table_irp <- DT::renderDT({
      req(result())
      irp <- as.data.frame(result()$IRP)
      DT::datatable(irp, rownames = TRUE,
                    options = list(dom = "tip", pageLength = 20, scrollX = TRUE)) |>
        DT::formatRound(columns = seq_len(ncol(irp)), digits = 3)
    })

    # IRP Index テーブル（Alpha, A, Beta, B, Gamma, C）
    output$table_irp_index <- DT::renderDT({
      req(result())
      df <- result()$IRPIndex
      DT::datatable(df, rownames = TRUE,
                    options = list(dom = "tip", pageLength = 20, scrollX = TRUE)) |>
        DT::formatRound(columns = seq_len(ncol(df)), digits = 3)
    })

    # ランクサマリー（TRP + LRD）
    output$table_rank_summary <- DT::renderDT({
      req(result())
      r <- result()
      nrank <- r$n_rank
      df <- data.frame(
        Rank    = paste0("Rank ", seq_len(nrank)),
        TRP     = round(r$TRP, 3),
        LRD     = as.integer(r$LRD),
        LRD_pct = round(as.integer(r$LRD) / sum(r$LRD) * 100, 1)
      )
      colnames(df) <- c("Rank", "TRP", "N", "N (%)")
      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t", pageLength = 15))
    })

    # 受検者ランク帰属テーブル（Students は matrix → data.frame 変換）
    output$table_students <- DT::renderDT({
      req(result())
      df <- as.data.frame(result()$Students)
      membership_cols <- grep("^Membership", names(df), value = TRUE)
      dt <- DT::datatable(df, rownames = TRUE,
                          options = list(dom = "tip", pageLength = 20, scrollX = TRUE))
      if (length(membership_cols) > 0) dt <- DT::formatRound(dt, columns = membership_cols, digits = 3)
      dt
    })

    # 項目適合度テーブル（ItemFitIndices: 各要素が nItems 長ベクトルのリスト）
    output$table_item_fit <- DT::renderDT({
      req(result())
      fit <- result()$ItemFitIndices
      df <- tryCatch(
        {
          d <- as.data.frame(lapply(fit, as.numeric))
          rownames(d) <- names(fit[[1]])
          d
        },
        error = function(e) data.frame()
      )
      if (nrow(df) == 0) return(DT::datatable(data.frame()))
      dt <- DT::datatable(df, rownames = TRUE,
                          options = list(dom = "tip", pageLength = 20, scrollX = TRUE))
      DT::formatRound(dt, columns = seq_len(ncol(df)), digits = 4)
    })

    # ========== プロット ==========

    # IRP 選択時のみ項目セレクタを表示
    output$item_selector_ui <- renderUI({
      req(result(), input$plot_type == "IRP")
      item_names <- rownames(result()$IRP)
      selectInput(
        session$ns("selected_item"),
        label = i18n$t("Select Item"),
        choices = setNames(seq_along(item_names), item_names),
        selected = 1
      )
    })

    # RMP 選択時のみ受検者セレクタを表示
    output$student_selector_ui <- renderUI({
      req(result(), input$plot_type == "RMP")
      student_names <- rownames(result()$Students)
      selectInput(
        session$ns("selected_student"),
        label = i18n$t("Select Student"),
        choices = setNames(seq_along(student_names), student_names),
        selected = 1
      )
    })

    # ggplot オブジェクトを返す（base plot が必要な場合は NULL を返す）
    current_plot <- reactive({
      req(result())
      r <- result()

      # RMP: exametrika / ggExametrika 両方にバグがあるため ggplot2 で手動描画
      # - plot(r, type="RMP", students=1) → 単一受検者で matrix→vector になり params[i,] が失敗
      # - plotRMP_gg は内部で $Nclass を参照するため LRA オブジェクトで動作しない
      if (input$plot_type == "RMP") {
        req(input$selected_student)
        idx <- as.integer(input$selected_student)
        if (is.na(idx)) idx <- 1L
        nrank <- r$n_rank
        membership <- as.numeric(r$Students[idx, seq_len(nrank)])
        student_name <- rownames(r$Students)[idx]
        df_rmp <- data.frame(
          Rank       = seq_len(nrank),
          Membership = membership
        )
        return(
          ggplot2::ggplot(df_rmp, ggplot2::aes(x = Rank, y = Membership)) +
            ggplot2::geom_point(size = 3) +
            ggplot2::geom_line(linetype = "dashed") +
            ggplot2::scale_x_continuous(breaks = seq_len(nrank)) +
            ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
            ggplot2::labs(
              title = paste("Rank Membership Profile,", student_name),
              x = "Latent Rank",
              y = "Membership"
            ) +
            ggplot2::theme_bw()
        )
      }

      if (!requireNamespace("ggExametrika", quietly = TRUE)) return(NULL)

      tryCatch(
        switch(input$plot_type,
          "IRP" = {
            plots <- ggExametrika::plotIRP_gg(r)
            req(input$selected_item)
            idx <- as.integer(input$selected_item)
            plots[[idx]]
          },
          "TRP" = ggExametrika::plotTRP_gg(r),
          "LRD" = ggExametrika::plotLRD_gg(r)
        ),
        error = function(e) NULL
      )
    })

    output$plot <- renderPlot({
      req(result())
      p <- current_plot()
      if (!is.null(p)) {
        print(p)
      } else {
        # ggExametrika 未使用 / エラー時は base plot にフォールバック
        plot(result(), type = input$plot_type)
      }
    })

    # ========== ダウンロード ==========

    output$dl_irp <- downloadHandler(
      filename = function() paste0("LRA_IRP_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(result()$IRP, file, row.names = TRUE)
    )

    output$dl_students <- downloadHandler(
      filename = function() paste0("LRA_Students_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(result()$Students, file, row.names = TRUE)
    )

    output$dl_plot <- downloadHandler(
      filename = function() {
        if (input$plot_type == "RMP") {
          paste0("LRA_RMP_student", input$selected_student, "_", Sys.Date(), ".png")
        } else if (input$plot_type == "IRP") {
          paste0("LRA_IRP_item", input$selected_item, "_", Sys.Date(), ".png")
        } else {
          paste0("LRA_", input$plot_type, "_", Sys.Date(), ".png")
        }
      },
      content = function(file) {
        p <- current_plot()
        if (!is.null(p)) {
          ggplot2::ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
        } else {
          png(file, width = 800, height = 500)
          plot(result(), type = input$plot_type)
          dev.off()
        }
      }
    )
  })
}
