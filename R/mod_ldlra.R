#' LDLRA Module UI
#'
#' Locally Dependent Latent Rank Analysis module. Supports two analysis modes:
#' - LDLRA (fixed DAG): user uploads a rank-specific DAG via CSV
#' - LDLRA_PBIL: structure learning via Population-Based Incremental Learning
#'
#' @param id Module namespace ID
#' @param i18n shiny.i18n Translator object
#'
#' @noRd
mod_ldlra_ui <- function(id, i18n) {
  ns <- NS(id)

  bslib::layout_sidebar(
    # ========== Sidebar ==========
    sidebar = bslib::sidebar(
      width = 340,
      title = i18n$t("LDLRA"),

      # --- Analysis mode selection ---
      radioButtons(
        ns("analysis_mode"),
        label = i18n$t("Analysis Mode"),
        choices = c(
          "LDLRA (Fixed DAG)"       = "LDLRA",
          "LDLRA_PBIL (Structure Learning)" = "LDLRA_PBIL"
        ),
        selected = "LDLRA"
      ),

      tags$small(
        class = "text-muted d-block mb-3",
        i18n$t("LDLRA analyzes locally dependent item structures across latent ranks.")
      ),

      tags$hr(),

      # --- Common parameters ---
      sliderInput(
        ns("ncls"),
        label = i18n$t("Number of Ranks"),
        min = 2, max = 20, value = 5, step = 1
      ),

      radioButtons(
        ns("method"),
        label = i18n$t("Method"),
        choices = c("Rank" = "R", "Class" = "C"),
        selected = "R",
        inline = TRUE
      ),

      tags$hr(),

      # --- DAG input (shown only for fixed LDLRA mode) ---
      conditionalPanel(
        condition = sprintf("input['%s'] == 'LDLRA'", ns("analysis_mode")),
        dag_input_ui(ns, i18n, show_rank_col = TRUE)
      ),

      # --- Structure learning parameters (PBIL only) ---
      conditionalPanel(
        condition = sprintf("input['%s'] == 'LDLRA_PBIL'", ns("analysis_mode")),

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
          label = i18n$t("Population Size"),
          value = 20, min = 5, max = 100, step = 5
        ),

        sliderInput(
          ns("max_parents"),
          label = i18n$t("Max Parents per Item"),
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
          label = i18n$t("Mutation Rate"),
          value = 0.002, min = 0.001, max = 0.1, step = 0.001
        ),

        numericInput(
          ns("pbil_alpha"),
          label = i18n$t("Learning Rate (alpha)"),
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

      # --- Advanced parameters ---
      tags$details(
        tags$summary(
          class = "text-muted small mb-2",
          i18n$t("Advanced Parameters")
        ),
        numericInput(
          ns("beta1"),
          label = i18n$t("Beta1 (prior parameter)"),
          value = 2, min = 0.1, max = 10, step = 0.1
        ),
        numericInput(
          ns("beta2"),
          label = i18n$t("Beta2 (prior parameter)"),
          value = 2, min = 0.1, max = 10, step = 0.1
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

          # Student Membership
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Student Membership"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_students")),
            downloadButton(ns("dl_students"), i18n$t("Download CSV"), class = "mt-3 mb-2")
          ),

          # CCRR Table
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Conditional Correct Response Rates"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_ccrr")),
            downloadButton(ns("dl_ccrr"), i18n$t("Download CSV"), class = "mt-3 mb-2")
          ),

          # Estimation Table
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Estimation Table"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_estimation")),
            downloadButton(ns("dl_estimation"), i18n$t("Download CSV"), class = "mt-3 mb-2")
          )
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
              "IRP (Item Reference Profile)"     = "IRP",
              "TRP (Test Reference Profile)"     = "TRP",
              "LRD (Latent Rank Distribution)"   = "LRD",
              "RMP (Rank Membership Profile)"    = "RMP",
              "DAG (Network Graph)"              = "DAG"
            )
          ),
          uiOutput(ns("item_selector_ui")),
          uiOutput(ns("student_selector_ui")),
          uiOutput(ns("rank_selector_ui")),
          uiOutput(ns("dag_options_ui")),
          uiOutput(ns("plot_height_ui")),
          plotOutput(ns("plot")),
          downloadButton(ns("dl_plot"), i18n$t("Download Plot"), class = "mt-2")
        )
      )
    )
  )
}


#' LDLRA Module Server
#'
#' @param id Module namespace ID
#' @param formatted_data Reactive dataFormat() result
#' @param i18n shiny.i18n Translator object
#'
#' @noRd
mod_ldlra_server <- function(id, formatted_data, i18n) {
  moduleServer(id, function(input, output, session) {

    # ========== DAG Parsing (for fixed LDLRA mode) ==========

    parsed_dag <- reactive({
      req(input$dag_file)
      file_info <- input$dag_file

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

    output$dag_status_ui <- renderUI({
      if (input$analysis_mode != "LDLRA") return(NULL)
      dag_status_display(parsed_dag(), i18n)
    })

    output$dl_dag_sample <- downloadHandler(
      filename = function() {
        paste0("sample_dag_ldlra_", Sys.Date(), ".csv")
      },
      content = function(file) {
        item_labels <- NULL
        if (!is.null(formatted_data())) {
          fd <- formatted_data()
          item_labels <- fd$ItemLabel
        }
        csv_content <- generate_sample_dag_csv(
          item_labels  = item_labels,
          include_rank = TRUE
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
          i18n$t("LDLRA requires binary response data."),
          type = "warning", duration = 5
        )
        return(NULL)
      }

      mode <- input$analysis_mode
      ncls_val <- as.integer(input$ncls)
      method_val <- input$method
      beta1_val <- input$beta1
      beta2_val <- input$beta2
      if (is.null(beta1_val) || is.na(beta1_val)) beta1_val <- 2
      if (is.null(beta2_val) || is.na(beta2_val)) beta2_val <- 2

      if (mode == "LDLRA") {
        # --- Fixed DAG mode ---
        dag <- parsed_dag()
        if (is.null(dag) || !is.null(dag$error)) {
          shiny::showNotification(
            i18n$t("Please upload a valid DAG CSV file first."),
            type = "warning", duration = 5
          )
          return(NULL)
        }

        # Validate rank values in DAG
        if ("Rank" %in% colnames(dag$edges)) {
          rank_vals <- as.integer(dag$edges$Rank)
          if (any(is.na(rank_vals) | rank_vals < 1 | rank_vals > ncls_val)) {
            shiny::showNotification(
              i18n$t("Rank values in DAG must be between 1 and the number of ranks."),
              type = "warning", duration = 5
            )
            return(NULL)
          }
        }

        # Build rank-specific adjacency list
        adj_list <- build_adj_list_from_edges(dag$edges, fd$ItemLabel, ncls_val)

        withProgress(message = i18n$t("Running LDLRA analysis..."), value = 0, {
          incProgress(0.3, detail = i18n$t("Estimating parameters..."))
          r <- tryCatch(
            exametrika::LDLRA(
              fd,
              ncls     = ncls_val,
              method   = method_val,
              adj_list = adj_list,
              beta1    = beta1_val,
              beta2    = beta2_val,
              verbose  = FALSE
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
        })

      } else if (mode == "LDLRA_PBIL") {
        # --- Structure Learning mode ---
        seed_val <- input$seed
        if (is.null(seed_val) || is.na(seed_val)) seed_val <- 123L

        withProgress(
          message = i18n$t("Running LDLRA_PBIL structure learning..."),
          value = 0,
          {
            incProgress(0.1, detail = i18n$t("This may take several minutes..."))
            r <- tryCatch(
              exametrika::LDLRA_PBIL(
                fd,
                seed            = as.integer(seed_val),
                ncls            = ncls_val,
                method          = method_val,
                population      = as.integer(input$population),
                Rs              = input$survival_rate,
                Rm              = input$mutation_rate,
                maxParents      = as.integer(input$max_parents),
                maxGeneration   = as.integer(input$max_generation),
                successiveLimit = as.integer(input$successive_limit),
                alpha           = input$pbil_alpha,
                estimate        = as.integer(input$pbil_estimate),
                beta1           = beta1_val,
                beta2           = beta2_val,
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

    # Fit Indices
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

    # IRP Index Table
    output$table_irp_index <- DT::renderDT({
      req(result())
      df <- result()$IRPIndex
      if (is.null(df)) return(NULL)
      DT::datatable(df, rownames = TRUE,
                    options = list(dom = "tip", pageLength = 20, scrollX = TRUE)) |>
        DT::formatRound(columns = seq_len(ncol(df)), digits = 3)
    })

    # Rank Summary (TRP + LRD)
    output$table_rank_summary <- DT::renderDT({
      req(result())
      r <- result()
      ncls <- safe_field(r, "n_class", "Nclass", safe_field(r, "n_rank", "Nrank", NULL))
      if (is.null(ncls)) ncls <- length(r$TRP)
      df <- data.frame(
        Rank    = paste0("Rank ", seq_len(ncls)),
        TRP     = round(r$TRP, 3),
        LRD     = as.integer(r$LRD),
        LRD_pct = round(as.integer(r$LRD) / sum(r$LRD) * 100, 1)
      )
      colnames(df) <- c("Rank", "TRP", "N", "N (%)")
      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t", pageLength = 25))
    })

    # Student Membership
    output$table_students <- DT::renderDT({
      req(result())
      df <- as.data.frame(result()$Students)
      membership_cols <- grep("^Membership", names(df), value = TRUE)
      dt <- DT::datatable(df, rownames = TRUE,
                          options = list(dom = "tip", pageLength = 20, scrollX = TRUE))
      if (length(membership_cols) > 0) {
        dt <- DT::formatRound(dt, columns = membership_cols, digits = 3)
      }
      dt
    })

    # CCRR Table
    output$table_ccrr <- DT::renderDT({
      req(result())
      ccrr <- result()$CCRR_table
      if (is.null(ccrr)) return(NULL)
      df <- as.data.frame(ccrr)
      dt <- DT::datatable(df, rownames = FALSE,
                          options = list(dom = "tip", pageLength = 20, scrollX = TRUE))
      crr_col <- grep("Conditional|CRR|CCRR|PIRP", colnames(df), value = TRUE)
      if (length(crr_col) > 0) {
        dt <- DT::formatRound(dt, columns = crr_col, digits = 4)
      }
      dt
    })

    # Estimation Table
    output$table_estimation <- DT::renderDT({
      req(result())
      est <- result()$Estimation_table
      if (is.null(est)) return(NULL)
      df <- as.data.frame(est)
      num_cols <- names(df)[sapply(df, is.numeric)]
      dt <- DT::datatable(df, rownames = FALSE,
                          options = list(dom = "tip", pageLength = 20, scrollX = TRUE))
      if (length(num_cols) > 0) {
        dt <- DT::formatRound(dt, columns = num_cols, digits = 4)
      }
      dt
    })

    # ========== Plots ==========

    # Item selector (IRP)
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

    # Student selector (RMP)
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

    # Rank selector (DAG -- one graph per rank)
    output$rank_selector_ui <- renderUI({
      req(result(), input$plot_type == "DAG")
      r <- result()
      ncls <- safe_field(r, "n_class", "Nclass", safe_field(r, "n_rank", "Nrank", NULL))
      if (is.null(ncls)) ncls <- length(r$TRP)
      rank_names <- paste0("Rank ", seq_len(ncls))
      selectInput(
        session$ns("selected_rank"),
        label = i18n$t("Select Rank"),
        choices = setNames(seq_len(ncls), rank_names),
        selected = 1
      )
    })

    # DAG options (layout + direction)
    output$dag_options_ui <- renderUI({
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
        )
      )
    })

    # Current plot (ggplot object or NULL for base plot fallback)
    current_plot <- reactive({
      req(result(), input$plot_type)
      r <- result()

      if (input$plot_type == "IRP") req(input$selected_item)
      if (input$plot_type == "RMP") req(input$selected_student)
      if (input$plot_type == "DAG") req(input$selected_rank, input$dag_layout, input$dag_direction)

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
            all_plots <- ggExametrika::plotRMP_gg(r)
            idx <- as.integer(input$selected_student)
            if (is.na(idx)) idx <- 1L
            all_plots[[idx]]
          },
          "DAG" = {
            layout_val    <- input$dag_layout
            direction_val <- input$dag_direction
            if (is.null(layout_val))    layout_val    <- "sugiyama"
            if (is.null(direction_val)) direction_val <- "BT"

            plots <- ggExametrika::plotGraph_gg(
              r,
              layout    = layout_val,
              direction = direction_val
            )
            # plotGraph_gg returns a list of plots (one per rank for LDLRA)
            rank_idx <- as.integer(input$selected_rank)
            p <- NULL
            if (is.list(plots) && length(plots) >= rank_idx) {
              p <- plots[[rank_idx]]
            } else if (is.list(plots) && length(plots) >= 1) {
              p <- plots[[1]]
            } else {
              p <- plots
            }
            # NULL means no edges for this rank (skipped by plotGraph_gg)
            if (is.null(p)) return("NO_EDGES")
            p
          }
        ),
        error = function(e) {
          message("[LDLRA] Plot error: ", conditionMessage(e))
          NULL
        }
      )
    })

    # Dynamic plot output with adjustable height
    # Plot height slider (DAG only)
    output$plot_height_ui <- renderUI({
      req(result(), input$plot_type == "DAG")
      sliderInput(
        session$ns("plot_height"), label = i18n$t("Plot Height (px)"),
        min = 400, max = 1200, value = 600, step = 50
      )
    })

    output$plot <- renderPlot({
      req(result())
      p <- current_plot()
      if (identical(p, "NO_EDGES")) {
        rank_idx <- as.integer(input$selected_rank)
        plot.new()
        text(0.5, 0.5, paste(i18n$t("No DAG to display."),
             "\n(Rank", rank_idx, "- no edges)"), cex = 1.2)
      } else if (!is.null(p)) {
        print(p)
      } else {
        # Fallback to base plot
        r <- result()
        if (input$plot_type == "RMP") {
          idx <- as.integer(input$selected_student)
          if (is.null(idx) || length(idx) == 0 || is.na(idx)) idx <- 1L
          plot(r, type = "RMP", students = idx)
        } else if (input$plot_type == "DAG") {
          # igraph fallback for DAG
          rank_idx <- as.integer(input$selected_rank)
          if (is.null(rank_idx) || is.na(rank_idx)) rank_idx <- 1L
          g_list <- r$g_list
          if (!is.null(g_list) && length(g_list) >= rank_idx) {
            g <- g_list[[rank_idx]]
            igraph::plot.igraph(
              g,
              layout      = igraph::layout_with_sugiyama(g)$layout,
              vertex.size = 20,
              vertex.label.cex = 0.8,
              edge.arrow.size  = 0.5,
              main = paste("DAG - Rank", rank_idx)
            )
          } else {
            plot.new()
            text(0.5, 0.5, i18n$t("No DAG to display."), cex = 1.2)
          }
        } else {
          plot(r, type = input$plot_type)
        }
      }
    }, height = function() {
      if (!is.null(input$plot_type) && input$plot_type == "DAG" && !is.null(input$plot_height)) {
        input$plot_height
      } else {
        600
      }
    })

    # ========== Downloads ==========

    output$dl_irp <- downloadHandler(
      filename = function() paste0("LDLRA_IRP_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(result()$IRP, file, row.names = TRUE)
    )

    output$dl_students <- downloadHandler(
      filename = function() paste0("LDLRA_Students_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(result()$Students, file, row.names = TRUE)
    )

    output$dl_ccrr <- downloadHandler(
      filename = function() paste0("LDLRA_CCRR_", Sys.Date(), ".csv"),
      content  = function(file) {
        ccrr <- result()$CCRR_table
        if (!is.null(ccrr)) utils::write.csv(ccrr, file, row.names = FALSE)
      }
    )

    output$dl_estimation <- downloadHandler(
      filename = function() paste0("LDLRA_Estimation_", Sys.Date(), ".csv"),
      content  = function(file) {
        est <- result()$Estimation_table
        if (!is.null(est)) utils::write.csv(est, file, row.names = FALSE)
      }
    )

    output$dl_plot <- downloadHandler(
      filename = function() {
        prefix <- "LDLRA"
        suffix <- switch(input$plot_type,
          "IRP" = paste0("IRP_item", input$selected_item),
          "RMP" = paste0("RMP_student", input$selected_student),
          "DAG" = paste0("DAG_rank", input$selected_rank),
          input$plot_type
        )
        paste0(prefix, "_", suffix, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        p <- current_plot()
        if (!is.null(p)) {
          ggplot2::ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
        } else {
          png(file, width = 800, height = 500)
          r <- result()
          if (input$plot_type == "DAG") {
            rank_idx <- as.integer(input$selected_rank)
            if (is.null(rank_idx) || is.na(rank_idx)) rank_idx <- 1L
            g_list <- r$g_list
            if (!is.null(g_list) && length(g_list) >= rank_idx) {
              g <- g_list[[rank_idx]]
              igraph::plot.igraph(
                g,
                layout      = igraph::layout_with_sugiyama(g)$layout,
                vertex.size = 20,
                vertex.label.cex = 0.8,
                edge.arrow.size  = 0.5,
                main = paste("DAG - Rank", rank_idx)
              )
            }
          } else {
            plot(r, type = input$plot_type)
          }
          dev.off()
        }
      }
    )
  })
}
