#' LRA Module UI
#'
#' @param id Module namespace ID
#' @param i18n shiny.i18n Translator object
#'
#' @noRd
mod_lra_ui <- function(id, i18n) {
  ns <- NS(id)

  bslib::layout_sidebar(
    # ========== Sidebar ==========
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

    # ========== Main Panel ==========
    bslib::navset_card_tab(
      id = ns("main_tabs"),

      # --- Results Tab ---
      bslib::nav_panel(
        title = i18n$t("Results"),
        bslib::card_body(

          # Fit Indices
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Fit Indices"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_fit"))
          ),

          # IRP Table
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("IRP (Item Reference Profile)"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_irp")),
            downloadButton(ns("dl_irp"), i18n$t("Download CSV"), class = "mt-3 mb-2")
          ),

          # IRP Index Table
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("IRP Index"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_irp_index"))
          ),

          # Rank Summary (TRP / LRD)
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Rank Summary"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_rank_summary"))
          ),

          # Student Rank Membership
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Student Membership"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_students")),
            downloadButton(ns("dl_students"), i18n$t("Download CSV"), class = "mt-3 mb-2")
          )
        )
      ),

      # --- Item Fit Tab ---
      bslib::nav_panel(
        title = i18n$t("Item Fit"),
        bslib::card_body(
          DT::DTOutput(ns("table_item_fit"))
        )
      ),

      # --- Plots Tab ---
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


#' LRA Module Server
#'
#' @param id Module namespace ID
#' @param formatted_data Reactive dataFormat() result
#' @param i18n shiny.i18n Translator object
#'
#' @noRd
mod_lra_server <- function(id, formatted_data, i18n) {
  moduleServer(id, function(input, output, session) {

    # ========== Run Analysis ==========
    result <- eventReactive(input$btn_run, {
      req(formatted_data())
      fd <- formatted_data()

      # Binary data validation (using maxscore, same as IRT)
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

    # ========== Table Output ==========

    # Fit Indices (using shared helper function)
    output$table_fit <- DT::renderDT({
      req(result())
      fit_df <- extract_fit_indices(result())
      dt <- DT::datatable(fit_df, rownames = FALSE,
                          options = list(dom = "t", pageLength = 20))
      DT::formatRound(dt, columns = "Value", digits = 4)
    })

    # IRP Table (rows: items, columns: ranks)
    output$table_irp <- DT::renderDT({
      req(result())
      irp <- as.data.frame(result()$IRP)
      DT::datatable(irp, rownames = TRUE,
                    options = list(dom = "tip", pageLength = 20, scrollX = TRUE)) |>
        DT::formatRound(columns = seq_len(ncol(irp)), digits = 3)
    })

    # IRP Index Table (Alpha, A, Beta, B, Gamma, C)
    output$table_irp_index <- DT::renderDT({
      req(result())
      df <- result()$IRPIndex
      DT::datatable(df, rownames = TRUE,
                    options = list(dom = "tip", pageLength = 20, scrollX = TRUE)) |>
        DT::formatRound(columns = seq_len(ncol(df)), digits = 3)
    })

    # Rank Summary (TRP + LRD)
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

    # Student Rank Membership Table (Students: matrix to data.frame conversion)
    output$table_students <- DT::renderDT({
      req(result())
      df <- as.data.frame(result()$Students)
      membership_cols <- grep("^Membership", names(df), value = TRUE)
      dt <- DT::datatable(df, rownames = TRUE,
                          options = list(dom = "tip", pageLength = 20, scrollX = TRUE))
      if (length(membership_cols) > 0) dt <- DT::formatRound(dt, columns = membership_cols, digits = 3)
      dt
    })

    # Item Fit Table (ItemFitIndices: list of nItems-length vectors)
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

    # ========== Plots ==========

    # Show item selector only when IRP is selected
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

    # Show student selector only when RMP is selected
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

    # Returns a ggplot object (returns NULL if base plot is needed)
    current_plot <- reactive({
      req(result())
      r <- result()

      # Place req() outside tryCatch (inside it would be captured by the error handler)
      if (input$plot_type == "IRP") req(input$selected_item)
      if (input$plot_type == "RMP") req(input$selected_student)

      if (!requireNamespace("ggExametrika", quietly = TRUE)) return(NULL)

      tryCatch(
        switch(input$plot_type,
          "IRP" = {
            plots <- ggExametrika::plotIRP_gg(r)
            idx <- as.integer(input$selected_item)
            plots[[idx]]
          },
          "TRP" = ggExametrika::plotTRP_gg(r),
          "LRD" = ggExametrika::plotLRD_gg(r),
          "RMP" = {
            # plotRMP_gg supports LRA since ggExametrika v0.0.29
            all_plots <- ggExametrika::plotRMP_gg(r)
            idx <- as.integer(input$selected_student)
            if (is.na(idx)) idx <- 1L
            all_plots[[idx]]
          }
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
        # Fallback to base plot when ggExametrika is unused or on error
        if (input$plot_type == "RMP") {
          idx <- as.integer(input$selected_student)
          if (is.null(idx) || length(idx) == 0 || is.na(idx)) idx <- 1L
          plot(result(), type = "RMP", students = idx)
        } else {
          plot(result(), type = input$plot_type)
        }
      }
    })

    # ========== Downloads ==========

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
