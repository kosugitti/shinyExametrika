#' IRM Module UI
#'
#' @param id Module namespace ID
#' @param i18n shiny.i18n Translator object
#'
#' @noRd
mod_irm_ui <- function(id, i18n) {
  ns <- NS(id)

  bslib::layout_sidebar(
    # ========== Sidebar ==========
    sidebar = bslib::sidebar(
      width = 300,
      title = i18n$t("IRM"),

      numericInput(
        ns("gamma_c"),
        label = i18n$t("Concentration Parameter (Classes)"),
        value = 1.0, min = 0.1, max = 10, step = 0.1
      ),

      numericInput(
        ns("gamma_f"),
        label = i18n$t("Concentration Parameter (Fields)"),
        value = 1.0, min = 0.1, max = 10, step = 0.1
      ),

      tags$small(
        class = "text-muted d-block mb-3",
        i18n$t("IRM automatically determines the optimal number of classes and fields.")
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

      # --- Results tab ---
      bslib::nav_panel(
        title = i18n$t("Results"),
        bslib::card_body(

          # Discovered structure summary
          uiOutput(ns("discovered_structure_ui")),

          # Fit indices
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Fit Indices"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_fit"))
          ),

          # FRP table
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("FRP (Field Reference Profile)"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_frp")),
            downloadButton(ns("dl_frp"), i18n$t("Download CSV"), class = "mt-3 mb-2")
          ),

          # FRP Index table
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("FRP Index"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_frp_index"))
          ),

          # Class summary (TRP / LCD)
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Class Summary"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_class_summary"))
          ),

          # Field summary (LFD)
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Field Summary"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_field_summary"))
          ),

          # Student class membership
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Student Membership"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_students")),
            downloadButton(ns("dl_students"), i18n$t("Download CSV"), class = "mt-3 mb-2")
          ),

          # Item field analysis
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Field Analysis"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_field_analysis"))
          )
        )
      ),

      # --- Plots tab ---
      bslib::nav_panel(
        title = i18n$t("Plots"),
        bslib::card_body(
          uiOutput(ns("plot_type_ui")),
          uiOutput(ns("field_selector_ui")),
          plotOutput(ns("plot"), height = "600px"),
          downloadButton(ns("dl_plot"), i18n$t("Download Plot"), class = "mt-2")
        )
      )
    )
  )
}


#' IRM Module Server
#'
#' @param id Module namespace ID
#' @param formatted_data Reactive dataFormat() result
#' @param i18n shiny.i18n Translator object
#'
#' @noRd
mod_irm_server <- function(id, formatted_data, i18n) {
  moduleServer(id, function(input, output, session) {

    # ========== Run Analysis ==========
    result <- eventReactive(input$btn_run, {
      req(formatted_data())
      fd <- formatted_data()

      # Binary data validation
      maxscore <- fd$maxscore
      if (!is.null(maxscore) && length(maxscore) > 0 && any(maxscore > 1)) {
        shiny::showNotification(
          i18n$t("IRM requires binary response data."),
          type = "warning", duration = 5
        )
        return(NULL)
      }

      # Validate concentration parameters
      gamma_c <- input$gamma_c
      gamma_f <- input$gamma_f
      if (is.na(gamma_c) || gamma_c <= 0) {
        shiny::showNotification(
          i18n$t("Concentration Parameter (Classes) must be a positive number."),
          type = "warning", duration = 5
        )
        return(NULL)
      }
      if (is.na(gamma_f) || gamma_f <= 0) {
        shiny::showNotification(
          i18n$t("Concentration Parameter (Fields) must be a positive number."),
          type = "warning", duration = 5
        )
        return(NULL)
      }

      withProgress(message = i18n$t("Running IRM analysis..."), value = 0, {
        incProgress(0.3, detail = i18n$t("Estimating parameters..."))
        r <- tryCatch(
          exametrika::Biclustering_IRM(
            fd,
            gamma_c = gamma_c,
            gamma_f = gamma_f,
            verbose = FALSE
          ),
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

    # ========== Discovered structure UI ==========
    output$discovered_structure_ui <- renderUI({
      req(result())
      r <- result()
      ncls <- r$n_class
      nfld <- r$n_field
      tags$div(
        class = "alert alert-success mb-4",
        tags$strong(i18n$t("Discovered Classes"), ": "), ncls,
        tags$span(" / "),
        tags$strong(i18n$t("Discovered Fields"), ": "), nfld
      )
    })

    # ========== Table Output ==========

    # Fit indices
    output$table_fit <- DT::renderDT({
      req(result())
      fit_df <- extract_fit_indices(result())
      dt <- DT::datatable(fit_df, rownames = FALSE,
                          options = list(dom = "t", pageLength = 20))
      DT::formatRound(dt, columns = "Value", digits = 4)
    })

    # FRP table
    output$table_frp <- DT::renderDT({
      req(result())
      frp <- as.data.frame(result()$FRP)
      DT::datatable(frp, rownames = TRUE,
                    options = list(dom = "tip", pageLength = 20, scrollX = TRUE)) |>
        DT::formatRound(columns = seq_len(ncol(frp)), digits = 3)
    })

    # FRP Index table
    output$table_frp_index <- DT::renderDT({
      req(result())
      df <- result()$FRPIndex
      DT::datatable(df, rownames = TRUE,
                    options = list(dom = "tip", pageLength = 20, scrollX = TRUE)) |>
        DT::formatRound(columns = seq_len(ncol(df)), digits = 3)
    })

    # Class summary (TRP + LCD)
    output$table_class_summary <- DT::renderDT({
      req(result())
      r <- result()
      ncls <- r$n_class
      df <- data.frame(
        Class   = paste0("Class ", seq_len(ncls)),
        TRP     = round(r$TRP, 3),
        LCD     = as.integer(r$LCD),
        LCD_pct = round(as.integer(r$LCD) / sum(r$LCD) * 100, 1)
      )
      colnames(df) <- c("Class", "TRP", "N", "N (%)")
      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t", pageLength = 15))
    })

    # Field summary (LFD)
    output$table_field_summary <- DT::renderDT({
      req(result())
      r <- result()
      nfld <- r$n_field
      df <- data.frame(
        Field   = paste0("Field ", seq_len(nfld)),
        LFD     = as.integer(r$LFD),
        LFD_pct = round(as.integer(r$LFD) / sum(r$LFD) * 100, 1)
      )
      colnames(df) <- c("Field", "N", "N (%)")
      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t", pageLength = 15))
    })

    # Student class membership table (IRM: hard assignment via ClassEstimated)
    output$table_students <- DT::renderDT({
      req(result())
      r <- result()
      df <- data.frame(
        Student = seq_len(r$nobs),
        Class   = paste0("Class ", r$ClassEstimated)
      )
      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "tip", pageLength = 20, scrollX = TRUE))
    })

    # Item field analysis table (IRM: hard assignment via FieldEstimated)
    output$table_field_analysis <- DT::renderDT({
      req(result())
      r <- result()
      df <- data.frame(
        Item  = seq_len(r$testlength),
        Field = paste0("Field ", r$FieldEstimated)
      )
      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "tip", pageLength = 20, scrollX = TRUE))
    })

    # ========== Plots ==========

    # IRM supports FRP, TRP, Array only (no CMP/RMP)
    output$plot_type_ui <- renderUI({
      req(result())
      selectInput(
        session$ns("plot_type"),
        label = i18n$t("Plot Type"),
        choices = c(
          "FRP (Field Reference Profile)" = "FRP",
          "TRP (Test Reference Profile)"  = "TRP",
          "Array"                         = "Array"
        )
      )
    })

    # Show field selector only when FRP is selected
    output$field_selector_ui <- renderUI({
      req(result(), input$plot_type == "FRP")
      nfld <- result()$n_field
      field_names <- paste0("Field ", seq_len(nfld))
      selectInput(
        session$ns("selected_field"),
        label = i18n$t("Select Field"),
        choices = setNames(seq_len(nfld), field_names),
        selected = 1
      )
    })

    # Return ggplot object
    current_plot <- reactive({
      req(result(), input$plot_type)
      r <- result()

      if (input$plot_type == "FRP") req(input$selected_field)

      if (!requireNamespace("ggExametrika", quietly = TRUE)) return(NULL)

      tryCatch(
        switch(input$plot_type,
          "FRP" = {
            idx <- as.integer(input$selected_field)
            ggExametrika::plotFRP_gg(r, fields = idx)
          },
          "TRP"   = ggExametrika::plotTRP_gg(r),
          "Array" = ggExametrika::plotArray_gg(r)
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
        # Base plot fallback
        if (input$plot_type == "FRP") {
          idx <- as.integer(input$selected_field)
          if (is.null(idx) || length(idx) == 0 || is.na(idx)) idx <- 1L
          plot(result(), type = "FRP", fields = idx)
        } else {
          plot(result(), type = input$plot_type)
        }
      }
    })

    # ========== Downloads ==========

    output$dl_frp <- downloadHandler(
      filename = function() paste0("IRM_FRP_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(result()$FRP, file, row.names = TRUE)
    )

    output$dl_students <- downloadHandler(
      filename = function() paste0("IRM_Students_", Sys.Date(), ".csv"),
      content  = function(file) {
        r <- result()
        df <- data.frame(Student = seq_len(r$nobs), Class = paste0("Class ", r$ClassEstimated))
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    output$dl_plot <- downloadHandler(
      filename = function() {
        plot_type <- input$plot_type
        if (plot_type == "FRP") {
          paste0("IRM_FRP_field", input$selected_field, "_", Sys.Date(), ".png")
        } else {
          paste0("IRM_", plot_type, "_", Sys.Date(), ".png")
        }
      },
      content = function(file) {
        p <- current_plot()
        if (!is.null(p)) {
          ggplot2::ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
        } else {
          png(file, width = 800, height = 500)
          if (input$plot_type == "FRP") {
            idx <- as.integer(input$selected_field)
            if (is.null(idx) || length(idx) == 0 || is.na(idx)) idx <- 1L
            plot(result(), type = "FRP", fields = idx)
          } else {
            plot(result(), type = input$plot_type)
          }
          dev.off()
        }
      }
    )
  })
}
