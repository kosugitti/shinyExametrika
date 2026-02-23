#' Biclustering Module UI
#'
#' @param id Module namespace ID
#' @param i18n shiny.i18n Translator object
#'
#' @noRd
mod_biclustering_ui <- function(id, i18n) {
  ns <- NS(id)

  bslib::layout_sidebar(
    # ========== Sidebar ==========
    sidebar = bslib::sidebar(
      width = 300,
      title = i18n$t("Biclustering"),

      sliderInput(
        ns("ncls"),
        label = i18n$t("Number of Classes"),
        min = 2, max = 10, value = 3, step = 1
      ),

      sliderInput(
        ns("nfld"),
        label = i18n$t("Number of Fields"),
        min = 2, max = 10, value = 3, step = 1
      ),

      radioButtons(
        ns("method"),
        label = i18n$t("Method"),
        choices = c(
          "Biclustering" = "B",
          "Ranklustering" = "R"
        ),
        selected = "B",
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

      # --- Results tab ---
      bslib::nav_panel(
        title = i18n$t("Results"),
        bslib::card_body(

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
          uiOutput(ns("student_selector_ui")),
          plotOutput(ns("plot"), height = "600px"),
          downloadButton(ns("dl_plot"), i18n$t("Download Plot"), class = "mt-2")
        )
      ),

      # --- GridSearch tab ---
      bslib::nav_panel(
        title = i18n$t("GridSearch"),
        bslib::card_body(

          tags$div(
            class = "row mb-4",
            tags$div(
              class = "col-md-4",
              sliderInput(
                ns("gs_max_ncls"),
                label = i18n$t("Max Classes"),
                min = 2, max = 10, value = 6, step = 1
              )
            ),
            tags$div(
              class = "col-md-4",
              sliderInput(
                ns("gs_max_nfld"),
                label = i18n$t("Max Fields"),
                min = 2, max = 10, value = 6, step = 1
              )
            ),
            tags$div(
              class = "col-md-4",
              radioButtons(
                ns("gs_index"),
                label = i18n$t("Fit Index"),
                choices = c("BIC", "AIC", "loglik"),
                selected = "BIC",
                inline = FALSE
              )
            )
          ),

          actionButton(
            ns("btn_gridsearch"),
            label = i18n$t("Run GridSearch"),
            class = "btn-warning mb-4",
            icon = icon("search")
          ),

          uiOutput(ns("gs_result_ui")),
          DT::DTOutput(ns("table_gs_matrix"))
        )
      )
    )
  )
}


#' Biclustering Module Server
#'
#' @param id Module namespace ID
#' @param formatted_data Reactive dataFormat() result
#' @param i18n shiny.i18n Translator object
#'
#' @noRd
mod_biclustering_server <- function(id, formatted_data, i18n) {
  moduleServer(id, function(input, output, session) {

    # ========== Run Analysis ==========
    result <- eventReactive(input$btn_run, {
      req(formatted_data())
      fd <- formatted_data()

      # Binary data validation (using maxscore)
      maxscore <- fd$maxscore
      if (!is.null(maxscore) && length(maxscore) > 0 && any(maxscore > 1)) {
        shiny::showNotification(
          i18n$t("Biclustering requires binary response data."),
          type = "warning", duration = 5
        )
        return(NULL)
      }

      withProgress(message = i18n$t("Running Biclustering analysis..."), value = 0, {
        incProgress(0.3, detail = i18n$t("Estimating parameters..."))
        r <- tryCatch(
          exametrika::Biclustering(
            fd,
            ncls = input$ncls,
            nfld = input$nfld,
            method = input$method,
            mic = input$mic
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

    # ========== Run GridSearch ==========
    gs_result <- eventReactive(input$btn_gridsearch, {
      req(formatted_data())
      fd <- formatted_data()

      maxscore <- fd$maxscore
      if (!is.null(maxscore) && length(maxscore) > 0 && any(maxscore > 1)) {
        shiny::showNotification(
          i18n$t("Biclustering requires binary response data."),
          type = "warning", duration = 5
        )
        return(NULL)
      }

      withProgress(message = i18n$t("Running GridSearch..."), value = 0.2, {
        gs <- tryCatch(
          exametrika::GridSearch(
            fd,
            method = input$method,
            max_ncls = input$gs_max_ncls,
            max_nfld = input$gs_max_nfld,
            index = input$gs_index
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
        if (!is.null(gs)) {
          shiny::showNotification(i18n$t("Analysis completed!"), type = "message", duration = 3)
        }
        gs
      })
    })

    # ========== Table Output ==========

    # Fit indices (using shared helper function)
    output$table_fit <- DT::renderDT({
      req(result())
      fit_df <- extract_fit_indices(result())
      dt <- DT::datatable(fit_df, rownames = FALSE,
                          options = list(dom = "t", pageLength = 20))
      DT::formatRound(dt, columns = "Value", digits = 4)
    })

    # FRP table (rows: fields, columns: classes)
    output$table_frp <- DT::renderDT({
      req(result())
      frp <- as.data.frame(result()$FRP)
      DT::datatable(frp, rownames = TRUE,
                    options = list(dom = "tip", pageLength = 20, scrollX = TRUE)) |>
        DT::formatRound(columns = seq_len(ncol(frp)), digits = 3)
    })

    # FRP Index table (Alpha, A, Beta, B, Gamma, C)
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

    # Student class membership table (Students: matrix -> data.frame)
    output$table_students <- DT::renderDT({
      req(result())
      df <- as.data.frame(result()$Students)
      membership_cols <- grep("^Membership", names(df), value = TRUE)
      dt <- DT::datatable(df, rownames = TRUE,
                          options = list(dom = "tip", pageLength = 20, scrollX = TRUE))
      if (length(membership_cols) > 0) dt <- DT::formatRound(dt, columns = membership_cols, digits = 3)
      dt
    })

    # Item field analysis table (FieldAnalysis)
    output$table_field_analysis <- DT::renderDT({
      req(result())
      df <- tryCatch(
        as.data.frame(result()$FieldAnalysis),
        error = function(e) data.frame()
      )
      if (nrow(df) == 0) return(DT::datatable(data.frame()))
      dt <- DT::datatable(df, rownames = TRUE,
                          options = list(dom = "tip", pageLength = 20, scrollX = TRUE))
      DT::formatRound(dt, columns = seq_len(ncol(df)), digits = 3)
    })

    # ========== Plots ==========

    # Switch plot types based on the analysis method used
    output$plot_type_ui <- renderUI({
      req(result())
      method_used <- result()$model
      if (is.null(method_used)) method_used <- input$method

      choices <- if (identical(method_used, "R")) {
        c(
          "FRP (Field Reference Profile)" = "FRP",
          "TRP (Test Reference Profile)"  = "TRP",
          "Array"                          = "Array",
          "RMP (Rank Membership Profile)"  = "RMP"
        )
      } else {
        c(
          "FRP (Field Reference Profile)"   = "FRP",
          "TRP (Test Reference Profile)"    = "TRP",
          "Array"                           = "Array",
          "CMP (Class Membership Profile)"  = "CMP"
        )
      }

      selectInput(
        session$ns("plot_type"),
        label = i18n$t("Plot Type"),
        choices = choices
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

    # Show student selector only when CMP/RMP is selected
    output$student_selector_ui <- renderUI({
      req(result(), input$plot_type %in% c("CMP", "RMP"))
      student_names <- rownames(result()$Students)
      selectInput(
        session$ns("selected_student"),
        label = i18n$t("Select Student"),
        choices = setNames(seq_along(student_names), student_names),
        selected = 1
      )
    })

    # Return ggplot object (returns NULL when base plot fallback is needed)
    current_plot <- reactive({
      req(result(), input$plot_type)
      r <- result()

      # Place req() outside tryCatch (inside it would be caught by the error handler)
      if (input$plot_type == "FRP")  req(input$selected_field)
      if (input$plot_type %in% c("CMP", "RMP"))  req(input$selected_student)

      if (!requireNamespace("ggExametrika", quietly = TRUE)) return(NULL)

      tryCatch(
        switch(input$plot_type,
          "FRP" = {
            idx <- as.integer(input$selected_field)
            ggExametrika::plotFRP_gg(r, fields = idx)
          },
          "TRP" = ggExametrika::plotTRP_gg(r),
          "Array" = ggExametrika::plotArray_gg(r),
          "CMP" = {
            all_plots <- ggExametrika::plotCMP_gg(r)
            idx <- as.integer(input$selected_student)
            if (is.na(idx)) idx <- 1L
            all_plots[[idx]]
          },
          "RMP" = {
            # plotRMP_gg supports Biclustering since ggExametrika v0.0.29
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
        # Base plot fallback
        if (input$plot_type %in% c("CMP", "RMP")) {
          idx <- as.integer(input$selected_student)
          if (is.null(idx) || length(idx) == 0 || is.na(idx)) idx <- 1L
          plot(result(), type = input$plot_type, students = idx)
        } else if (input$plot_type == "FRP") {
          idx <- as.integer(input$selected_field)
          if (is.null(idx) || length(idx) == 0 || is.na(idx)) idx <- 1L
          plot(result(), type = "FRP", fields = idx)
        } else {
          plot(result(), type = input$plot_type)
        }
      }
    })

    # ========== GridSearch Output ==========

    # Display optimal parameters
    output$gs_result_ui <- renderUI({
      req(gs_result())
      gs <- gs_result()
      tags$div(
        class = "alert alert-info mb-3",
        tags$strong(i18n$t("Optimal Classes"), ": "), gs$optimal_ncls,
        tags$span(" / "),
        tags$strong(i18n$t("Optimal Fields"), ": "), gs$optimal_nfld
      )
    })

    # GridSearch index matrix table
    output$table_gs_matrix <- DT::renderDT({
      req(gs_result())
      df <- as.data.frame(gs_result()$index_matrix)
      dt <- DT::datatable(df, rownames = TRUE,
                          options = list(dom = "t", pageLength = 20, scrollX = TRUE))
      DT::formatRound(dt, columns = seq_len(ncol(df)), digits = 1)
    })

    # ========== Downloads ==========

    output$dl_frp <- downloadHandler(
      filename = function() paste0("Biclustering_FRP_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(result()$FRP, file, row.names = TRUE)
    )

    output$dl_students <- downloadHandler(
      filename = function() paste0("Biclustering_Students_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(result()$Students, file, row.names = TRUE)
    )

    output$dl_plot <- downloadHandler(
      filename = function() {
        plot_type <- input$plot_type
        if (plot_type %in% c("CMP", "RMP")) {
          paste0("Biclustering_", plot_type, "_student", input$selected_student,
                 "_", Sys.Date(), ".png")
        } else if (plot_type == "FRP") {
          paste0("Biclustering_FRP_field", input$selected_field, "_", Sys.Date(), ".png")
        } else {
          paste0("Biclustering_", plot_type, "_", Sys.Date(), ".png")
        }
      },
      content = function(file) {
        p <- current_plot()
        if (!is.null(p)) {
          ggplot2::ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
        } else {
          png(file, width = 800, height = 500)
          if (input$plot_type == "CMP") {
            idx <- as.integer(input$selected_student)
            if (is.null(idx) || length(idx) == 0 || is.na(idx)) idx <- 1L
            plot(result(), type = "CMP", students = idx)
          } else {
            plot(result(), type = input$plot_type)
          }
          dev.off()
        }
      }
    )
  })
}
