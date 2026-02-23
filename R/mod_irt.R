#' IRT Module UI
#'
#' @param id Module namespace ID
#' @param i18n shiny.i18n Translator object
#'
#' @noRd
mod_irt_ui <- function(id, i18n) {
  ns <- NS(id)

  bslib::layout_sidebar(
    # ========== Sidebar ==========
    sidebar = bslib::sidebar(
      width = 300,
      title = i18n$t("IRT"),

      # Model selection
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

      # Run button
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
          # Fit indices section
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Fit Indices"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_fit"))
          ),

          # Item parameters section
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Item Parameters"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_params")),
            downloadButton(ns("download_params"), i18n$t("Download CSV"), class = "mt-3 mb-2")
          ),

          # Ability estimates section
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Ability Estimates"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_ability")),
            downloadButton(ns("download_ability"), i18n$t("Download CSV"), class = "mt-3 mb-2")
          )
        )
      ),

      # --- Plots tab ---
      bslib::nav_panel(
        title = i18n$t("Plots"),
        bslib::card_body(
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

          # Item selection UI (shown only for ICC/IIC)
          uiOutput(ns("item_selection_ui")),

          plotOutput(ns("plot"), height = "600px"),
          downloadButton(ns("download_plot"), i18n$t("Download Plot"), class = "mt-2")
        )
      )
    )
  )
}


#' IRT Module Server
#'
#' @param id Module namespace ID
#' @param formatted_data Reactive dataFormat() result
#' @param i18n shiny.i18n Translator object
#'
#' @noRd
mod_irt_server <- function(id, formatted_data, i18n) {
  moduleServer(id, function(input, output, session) {

    # ========== Reactive values ==========

    # IRT analysis result
    result <- eventReactive(input$btn_run, {
      req(formatted_data())

      fd <- formatted_data()
      model_num <- as.numeric(input$model)

      # Binary data validation
      maxscore <- fd$maxscore
      if (!is.null(maxscore) && length(maxscore) > 0 && any(maxscore > 1)) {
        shiny::showNotification(
          i18n$t("IRT requires binary data. Please use GRM for ordinal data."),
          type = "warning",
          duration = 5
        )
        return(NULL)
      }

      # Show progress bar
      withProgress(message = i18n$t("Running IRT analysis..."), value = 0, {
        incProgress(0.3, detail = i18n$t("Estimating parameters..."))

        # Run IRT (pass entire exametrika object)
        result <- tryCatch(
          exametrika::IRT(fd, model = model_num),
          error = function(e) {
            shiny::showNotification(
              paste(i18n$t("Analysis failed"), ":", e$message),
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

    # ========== Item selection UI (dynamic) ==========

    output$item_selection_ui <- renderUI({
      req(result())

      # Show item selection UI only for ICC/IIC
      if (input$plot_type %in% c("ICC", "IIC")) {
        item_names <- rownames(result()$params)

        tags$div(
          class = "mb-3",
          checkboxGroupInput(
            session$ns("selected_items"),
            label = i18n$t("Select Items to Plot"),
            choices = setNames(seq_along(item_names), item_names),
            selected = seq_along(item_names),  # all selected by default
            inline = TRUE
          )
        )
      }
    })

    # ========== Plot generation helper ==========

    current_plot <- reactive({
      req(result())

      if (!requireNamespace("ggExametrika", quietly = TRUE)) {
        shiny::showNotification(
          i18n$t("ggExametrika package is required for plots."),
          type = "warning"
        )
        return(NULL)
      }

      switch(input$plot_type,
        "ICC" = {
          items <- if (!is.null(input$selected_items) && length(input$selected_items) > 0) {
            as.numeric(input$selected_items)
          } else {
            NULL
          }
          ggExametrika::plotICC_overlay_gg(result(), items = items, show_legend = TRUE)
        },
        "TRF" = ggExametrika::plotTRF_gg(result()),
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
    })

    # ========== Table output ==========

    # Fit indices (using common helper function)
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

    # Item parameters
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

    # Ability estimates (using common helper function)
    output$table_ability <- DT::renderDT({
      req(result())

      ability <- extract_ability(result())

      # Round numeric columns only
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

    # ========== Plots ==========

    output$plot <- renderPlot({
      current_plot()
    })

    # ========== Downloads ==========

    # Parameters CSV
    output$download_params <- downloadHandler(
      filename = function() {
        paste0("IRT_parameters_", Sys.Date(), ".csv")
      },
      content = function(file) {
        utils::write.csv(result()$params, file, row.names = TRUE)
      }
    )

    # Ability estimates CSV (using common helper function)
    output$download_ability <- downloadHandler(
      filename = function() {
        paste0("IRT_ability_", Sys.Date(), ".csv")
      },
      content = function(file) {
        ability <- extract_ability(result())
        utils::write.csv(ability, file, row.names = FALSE)
      }
    )

    # Plot PNG
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("IRT_", input$plot_type, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        p <- current_plot()
        req(p)
        ggplot2::ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
      }
    )
  })
}
