#' BNM Module UI
#'
#' Bayesian Network Model module. Supports three analysis modes:
#' - BNM (fixed DAG): user uploads a DAG via CSV
#' - BNM_GA: structure learning via Genetic Algorithm
#' - BNM_PBIL: structure learning via Population-Based Incremental Learning
#'
#' @param id Module namespace ID
#' @param i18n shiny.i18n Translator object
#'
#' @noRd
mod_bnm_ui <- function(id, i18n) {
  ns <- NS(id)

  bslib::layout_sidebar(
    # ========== Sidebar ==========
    sidebar = bslib::sidebar(
      width = 340,
      title = i18n$t("BNM"),

      # --- Analysis mode selection ---
      radioButtons(
        ns("analysis_mode"),
        label = param_label(
          "Analysis Mode",
          "Fixed DAG analyses a network you upload as CSV. GA and PBIL learn the network structure automatically from the data.",
          i18n
        ),
        choices = c(
          "BNM (Fixed DAG)"    = "BNM",
          "BNM_GA (Genetic Algorithm)" = "BNM_GA",
          "BNM_PBIL (PBIL)"   = "BNM_PBIL"
        ),
        selected = "BNM"
      ),

      tags$small(
        class = "text-muted d-block mb-3",
        i18n$t("BNM analyzes item dependencies using a Bayesian Network.")
      ),

      tags$hr(),

      # --- DAG input (shown only for fixed BNM mode) ---
      conditionalPanel(
        condition = sprintf("input['%s'] == 'BNM'", ns("analysis_mode")),
        dag_input_ui(ns, i18n, show_rank_col = FALSE)
      ),

      # --- Structure learning parameters (GA / PBIL) ---
      conditionalPanel(
        condition = sprintf(
          "input['%s'] == 'BNM_GA' || input['%s'] == 'BNM_PBIL'",
          ns("analysis_mode"), ns("analysis_mode")
        ),

        tags$small(
          class = "text-muted d-block mb-2",
          i18n$t("Structure learning finds the optimal DAG automatically.")
        ),

        numericInput(
          ns("seed"),
          label = i18n$t("Random Seed"),
          value = 123, min = 1, max = 99999, step = 1
        ),

        numericInput(
          ns("population"),
          label = param_label(
            "Population Size",
            "Number of candidate networks evaluated per generation. Larger explores more of the search space but is slower; 20 is a reasonable default.",
            i18n
          ),
          value = 20, min = 5, max = 100, step = 5
        ),

        sliderInput(
          ns("max_parents"),
          label = param_label(
            "Max Parents per Item",
            "Maximum number of incoming edges (parents) per item in the learned network. Smaller values give simpler, more stable structures; 2 is a common choice.",
            i18n
          ),
          min = 1, max = 5, value = 2, step = 1
        ),

        numericInput(
          ns("max_generation"),
          label = i18n$t("Max Generations"),
          value = 100, min = 10, max = 1000, step = 10
        ),

        numericInput(
          ns("successive_limit"),
          label = i18n$t("Early Stopping (generations)"),
          value = 5, min = 1, max = 50, step = 1
        ),

        numericInput(
          ns("survival_rate"),
          label = i18n$t("Survival Rate"),
          value = 0.5, min = 0.1, max = 0.9, step = 0.1
        ),

        numericInput(
          ns("mutation_rate"),
          label = param_label(
            "Mutation Rate",
            "Probability of randomly flipping an edge each generation. Small values (around 0.005) keep the search stable.",
            i18n
          ),
          value = 0.005, min = 0.001, max = 0.1, step = 0.001
        )
      ),

      # --- GA-specific parameters ---
      conditionalPanel(
        condition = sprintf("input['%s'] == 'BNM_GA'", ns("analysis_mode")),

        selectInput(
          ns("crossover"),
          label = i18n$t("Crossover Type"),
          choices = c(
            "Uniform"  = "0",
            "1-point"  = "1",
            "2-point"  = "2"
          ),
          selected = "0"
        ),

        numericInput(
          ns("elitism"),
          label = i18n$t("Elitism Count"),
          value = 0, min = 0, max = 10, step = 1
        )
      ),

      # --- PBIL-specific parameters ---
      conditionalPanel(
        condition = sprintf("input['%s'] == 'BNM_PBIL'", ns("analysis_mode")),

        numericInput(
          ns("pbil_alpha"),
          label = param_label(
            "Learning Rate (alpha)",
            "PBIL learning-rate (update step size). Smaller values learn more slowly but more stably; 0.05 is typical.",
            i18n
          ),
          value = 0.05, min = 0.01, max = 0.5, step = 0.01
        ),

        selectInput(
          ns("pbil_estimate"),
          label = i18n$t("Estimation Method"),
          choices = c(
            "Best individual"     = "1",
            "All population mean" = "2",
            "Survivor mean"       = "3",
            "Gene of genes"       = "4"
          ),
          selected = "1"
        )
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
    uiOutput(ns("precheck")),
    model_help_block("bnm", i18n),

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

          # Adjacency matrix
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Adjacency Matrix"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_adj")),
            downloadButton(ns("dl_adj"), i18n$t("Download CSV"), class = "mt-3 mb-2")
          ),

          # PIRP (conditional response probabilities)
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Parameter Estimates (PIRP)"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_param"))
          ),

          # CCRR table
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Conditional Correct Response Rates"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_ccrr")),
            downloadButton(ns("dl_ccrr"), i18n$t("Download CSV"), class = "mt-3 mb-2")
          )
        )
      ),

      # --- Plots tab ---
      bslib::nav_panel(
        title = i18n$t("Plots"),
        bslib::card_body(
          uiOutput(ns("plot_type_ui")),
          uiOutput(ns("plot_options_ui")),
          downloadButton(ns("dl_plot"), i18n$t("Download Plot"), class = "mb-3"),
          plotOutput(ns("plot"), height = "600px")
        )
      )
    )
  )
}


#' BNM Module Server
#'
#' @param id Module namespace ID
#' @param formatted_data Reactive dataFormat() result
#' @param i18n shiny.i18n Translator object
#'
#' @noRd
mod_bnm_server <- function(id, formatted_data, i18n) {
  moduleServer(id, function(input, output, session) {

    # ========== Data-readiness banner ==========
    output$precheck <- renderUI({
      precheck_banner(formatted_data(), required = "binary", i18n)
    })

    # ========== DAG Parsing (for fixed BNM mode) ==========

    # Reactive: parsed DAG from uploaded CSV
    parsed_dag <- reactive({
      req(input$dag_file)
      file_info <- input$dag_file

      # Get item labels from formatted data (if available)
      item_labels <- NULL
      if (!is.null(formatted_data())) {
        fd <- formatted_data()
        item_labels <- fd$ItemLabel
      }

      parse_dag_csv(
        file_path   = file_info$datapath,
        item_labels = item_labels,
        i18n        = i18n
      )
    })

    # DAG status display in sidebar
    output$dag_status_ui <- renderUI({
      if (input$analysis_mode != "BNM") return(NULL)
      dag_status_display(parsed_dag(), i18n)
    })

    # Sample DAG CSV download
    output$dl_dag_sample <- downloadHandler(
      filename = function() {
        paste0("sample_dag_", Sys.Date(), ".csv")
      },
      content = function(file) {
        item_labels <- NULL
        if (!is.null(formatted_data())) {
          fd <- formatted_data()
          item_labels <- fd$ItemLabel
        }
        csv_content <- generate_sample_dag_csv(
          item_labels  = item_labels,
          include_rank = FALSE
        )
        writeLines(csv_content, file)
      }
    )

    # ========== Run Analysis ==========
    result <- eventReactive(input$btn_run, {
      req(formatted_data())
      fd <- formatted_data()

      # Binary data validation
      maxscore <- fd$maxscore
      if (!is.null(maxscore) && length(maxscore) > 0 && any(maxscore > 1)) {
        shiny::showNotification(
          i18n$t("BNM requires binary response data."),
          type = "warning", duration = 5
        )
        return(NULL)
      }

      mode <- input$analysis_mode

      # --- Fixed BNM: requires DAG ---
      if (mode == "BNM") {
        dag <- parsed_dag()
        if (is.null(dag) || !is.null(dag$error)) {
          shiny::showNotification(
            i18n$t("Please upload a valid DAG CSV file first."),
            type = "warning", duration = 5
          )
          return(NULL)
        }

        withProgress(message = i18n$t("Running BNM analysis..."), value = 0, {
          incProgress(0.3, detail = i18n$t("Estimating parameters..."))
          r <- tryCatch(
            exametrika::BNM(fd, adj_matrix = dag$adj_matrix),
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
            shiny::showNotification(
              i18n$t("Analysis completed!"),
              type = "message", duration = 3
            )
          }
          r
        })

      } else if (mode == "BNM_GA") {
        # --- BNM_GA: structure learning ---
        seed_val <- input$seed
        if (is.null(seed_val) || is.na(seed_val)) seed_val <- 123L

        withProgress(
          message = i18n$t("Running BNM_GA structure learning..."),
          value = 0,
          {
            incProgress(0.1, detail = i18n$t("This may take several minutes..."))
            r <- tryCatch(
              exametrika::BNM_GA(
                fd,
                seed            = as.integer(seed_val),
                population      = as.integer(input$population),
                Rs              = input$survival_rate,
                Rm              = input$mutation_rate,
                maxParents      = as.integer(input$max_parents),
                maxGeneration   = as.integer(input$max_generation),
                successiveLimit = as.integer(input$successive_limit),
                crossover       = as.integer(input$crossover),
                elitism         = as.integer(input$elitism),
                verbose         = FALSE
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
              shiny::showNotification(
                i18n$t("Analysis completed!"),
                type = "message", duration = 3
              )
            }
            r
          }
        )

      } else if (mode == "BNM_PBIL") {
        # --- BNM_PBIL: structure learning ---
        seed_val <- input$seed
        if (is.null(seed_val) || is.na(seed_val)) seed_val <- 123L

        withProgress(
          message = i18n$t("Running BNM_PBIL structure learning..."),
          value = 0,
          {
            incProgress(0.1, detail = i18n$t("This may take several minutes..."))
            r <- tryCatch(
              exametrika::BNM_PBIL(
                fd,
                seed            = as.integer(seed_val),
                population      = as.integer(input$population),
                Rs              = input$survival_rate,
                Rm              = input$mutation_rate,
                maxParents      = as.integer(input$max_parents),
                maxGeneration   = as.integer(input$max_generation),
                successiveLimit = as.integer(input$successive_limit),
                alpha           = input$pbil_alpha,
                estimate        = as.integer(input$pbil_estimate),
                verbose         = FALSE
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
              shiny::showNotification(
                i18n$t("Analysis completed!"),
                type = "message", duration = 3
              )
            }
            r
          }
        )
      }
    })

    # ========== Table Outputs ==========

    # Fit indices
    output$table_fit <- DT::renderDT({
      req(result())
      fit_df <- extract_fit_indices(result())
      dt <- DT::datatable(fit_df, rownames = FALSE,
                          options = list(dom = "t", pageLength = 20))
      DT::formatRound(dt, columns = "Value", digits = 4)
    })

    # Adjacency matrix
    output$table_adj <- DT::renderDT({
      req(result())
      adj <- as.data.frame(result()$adj)
      DT::datatable(adj, rownames = TRUE,
                    options = list(dom = "t", pageLength = 50, scrollX = TRUE))
    })

    # PIRP parameter estimates
    output$table_param <- DT::renderDT({
      req(result())
      param <- result()$param
      if (is.null(param)) return(NULL)
      df <- as.data.frame(param)
      dt <- DT::datatable(df, rownames = TRUE,
                          options = list(dom = "tip", pageLength = 20, scrollX = TRUE))
      # Format numeric columns
      num_cols <- seq_len(ncol(df))
      DT::formatRound(dt, columns = num_cols, digits = 4)
    })

    # CCRR table (Conditional Correct Response Rates)
    output$table_ccrr <- DT::renderDT({
      req(result())
      ccrr <- result()$CCRR_table
      if (is.null(ccrr)) return(NULL)
      df <- as.data.frame(ccrr)
      dt <- DT::datatable(df, rownames = FALSE,
                          options = list(dom = "tip", pageLength = 20, scrollX = TRUE))
      # Format the Conditional CRR column
      crr_col <- grep("Conditional|CRR|CCRR", colnames(df), value = TRUE)
      if (length(crr_col) > 0) {
        dt <- DT::formatRound(dt, columns = crr_col, digits = 4)
      }
      dt
    })

    # ========== Plots ==========

    # Plot type selector
    output$plot_type_ui <- renderUI({
      req(result())
      selectInput(
        session$ns("plot_type"),
        label = i18n$t("Plot Type"),
        choices = c(
          "DAG (Network Graph)" = "DAG"
        )
      )
    })

    # Plot options (layout, direction) for DAG
    output$plot_options_ui <- renderUI({
      req(result(), input$plot_type == "DAG")
      tagList(
        selectInput(
          session$ns("dag_layout"),
          label = i18n$t("Layout Algorithm"),
          choices = c(
            "Sugiyama (hierarchical)" = "sugiyama",
            "Fruchterman-Reingold"    = "fr",
            "Kamada-Kawai"            = "kk",
            "Tree"                    = "tree",
            "Stress"                  = "stress"
          ),
          selected = "sugiyama"
        ),
        selectInput(
          session$ns("dag_direction"),
          label = i18n$t("Direction"),
          choices = c(
            "Bottom to Top" = "BT",
            "Top to Bottom" = "TB",
            "Left to Right" = "LR",
            "Right to Left" = "RL"
          ),
          selected = "BT"
        ),
        sliderInput(
          session$ns("plot_height"),
          label = i18n$t("Plot Height (px)"),
          min = 400, max = 1200, value = 600, step = 50
        )
      )
    })

    # Render plot
    current_plot <- reactive({
      req(result(), input$plot_type)
      r <- result()

      if (input$plot_type == "DAG") {
        if (!requireNamespace("ggExametrika", quietly = TRUE)) return(NULL)

        layout_val    <- input$dag_layout
        direction_val <- input$dag_direction
        if (is.null(layout_val))    layout_val    <- "sugiyama"
        if (is.null(direction_val)) direction_val <- "BT"

        tryCatch({
          plots <- ggExametrika::plotGraph_gg(
            r,
            layout    = layout_val,
            direction = direction_val
          )
          # plotGraph_gg returns a list; BNM returns list of length 1
          if (is.list(plots) && length(plots) >= 1) {
            plots[[1]]
          } else {
            plots
          }
        }, error = function(e) NULL)
      } else {
        NULL
      }
    })

    output$plot <- renderPlot({
      req(result())
      p <- current_plot()
      if (!is.null(p)) {
        print(p)
      } else {
        # Base plot fallback: use igraph plot via exametrika's print method
        r <- result()
        if (!is.null(r$g)) {
          igraph::plot.igraph(
            r$g,
            layout      = igraph::layout_with_sugiyama(r$g)$layout,
            vertex.size = 20,
            vertex.label.cex = 0.8,
            edge.arrow.size  = 0.5,
            main = "Bayesian Network DAG"
          )
        } else {
          plot.new()
          text(0.5, 0.5, i18n$t("No DAG to display."), cex = 1.2)
        }
      }
    }, height = function() {
      if (!is.null(input$plot_type) && input$plot_type == "DAG" &&
          !is.null(input$plot_height)) {
        input$plot_height
      } else {
        600
      }
    })

    # ========== Downloads ==========

    # Download adjacency matrix
    output$dl_adj <- downloadHandler(
      filename = function() paste0("BNM_AdjMatrix_", Sys.Date(), ".csv"),
      content  = function(file) {
        utils::write.csv(result()$adj, file, row.names = TRUE)
      }
    )

    # Download CCRR table
    output$dl_ccrr <- downloadHandler(
      filename = function() paste0("BNM_CCRR_", Sys.Date(), ".csv"),
      content  = function(file) {
        utils::write.csv(result()$CCRR_table, file, row.names = FALSE)
      }
    )

    # Download plot
    output$dl_plot <- downloadHandler(
      filename = function() {
        paste0("BNM_DAG_", Sys.Date(), ".png")
      },
      content = function(file) {
        p <- current_plot()
        if (!is.null(p)) {
          ggplot2::ggsave(file, plot = p, width = 10, height = 8, dpi = 300)
        } else {
          png(file, width = 800, height = 600)
          r <- result()
          if (!is.null(r$g)) {
            igraph::plot.igraph(
              r$g,
              layout      = igraph::layout_with_sugiyama(r$g)$layout,
              vertex.size = 20,
              vertex.label.cex = 0.8,
              edge.arrow.size  = 0.5,
              main = "Bayesian Network DAG"
            )
          } else {
            plot.new()
            text(0.5, 0.5, "No DAG to display.", cex = 1.2)
          }
          dev.off()
        }
      }
    )
  })
}
