#' LDLRA Module UI
#'
#' Locally Dependent Latent Rank Analysis module. Supports two analysis modes:
#' - LDLRA (fixed DAG): user uploads rank-specific DAGs via CSV with Rank column
#' - LDLRA_PBIL: structure learning via Population-Based Incremental Learning
#'
#' @param id Module namespace ID
#' @param i18n shiny.i18n Translator object
#'
#' @importFrom stats setNames
#' @importFrom grDevices png dev.off
#' @importFrom graphics plot.new text
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
          "LDLRA (Fixed DAG)"              = "LDLRA",
          "LDLRA_PBIL (Structure Learning)" = "LDLRA_PBIL"
        ),
        selected = "LDLRA"
      ),

      tags$small(
        class = "text-muted d-block mb-3",
        i18n$t("LDLRA extends LRA by modeling local item dependencies within each rank using rank-specific DAGs.")
      ),

      tags$hr(),

      # --- Common parameters ---
      sliderInput(
        ns("ncls"),
        label = i18n$t("Number of Ranks"),
        min = 2, max = 10, value = 5, step = 1
      ),

      radioButtons(
        ns("method"),
        label = i18n$t("Method"),
        choices = c(
          "Rank"  = "R",
          "Class" = "C"
        ),
        selected = "R",
        inline = TRUE
      ),

      tags$hr(),

      # --- DAG input (shown only for fixed LDLRA mode) ---
      conditionalPanel(
        condition = sprintf("input['%s'] == 'LDLRA'", ns("analysis_mode")),
        dag_input_ui(ns, i18n, show_rank_col = TRUE)
      ),

      # --- PBIL parameters (shown only for LDLRA_PBIL mode) ---
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

          # Ordinal Alignment Conditions
          tags$div(
            class = "mb-5",
            tags$h5(
              i18n$t("Ordinal Alignment Conditions"),
              class = "mt-3 mb-3"
            ),
            uiOutput(ns("oac_display"))
          ),

          # IRP Table
          tags$div(
            class = "mb-5",
            tags$h5(
              i18n$t("IRP (Item Reference Profile)"),
              class = "mt-3 mb-3"
            ),
            DT::DTOutput(ns("table_irp")),
            downloadButton(
              ns("dl_irp"),
              i18n$t("Download CSV"),
              class = "mt-3 mb-2"
            )
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

          # CCRR table
          tags$div(
            class = "mb-5",
            tags$h5(
              i18n$t("Conditional Correct Response Rates"),
              class = "mt-3 mb-3"
            ),
            DT::DTOutput(ns("table_ccrr")),
            downloadButton(
              ns("dl_ccrr"),
              i18n$t("Download CSV"),
              class = "mt-3 mb-2"
            )
          ),

          # Estimation table (PIRP per rank)
          tags$div(
            class = "mb-5",
            tags$h5(
              i18n$t("Estimation Table (PIRP)"),
              class = "mt-3 mb-3"
            ),
            DT::DTOutput(ns("table_estimation"))
          ),

          # Student Membership
          tags$div(
            class = "mb-5",
            tags$h5(i18n$t("Student Membership"), class = "mt-3 mb-3"),
            DT::DTOutput(ns("table_students")),
            downloadButton(
              ns("dl_students"),
              i18n$t("Download CSV"),
              class = "mt-3 mb-2"
            )
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
              "IRP (Item Reference Profile)"  = "IRP",
              "TRP (Test Reference Profile)"  = "TRP",
              "LRD (Latent Rank Distribution)" = "LRD",
              "RMP (Rank Membership Profile)" = "RMP",
              "DAG (Network Graph)"           = "DAG"
            )
          ),
          uiOutput(ns("plot_options_ui")),
          downloadButton(
            ns("dl_plot"),
            i18n$t("Download Plot"),
            class = "mb-3"
          ),
          plotOutput(ns("plot"), height = "600px")
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

    # Reactive: parsed ranked DAG from uploaded CSV
    parsed_dag <- reactive({
      req(input$dag_file)
      file_info <- input$dag_file

      # Get item labels from formatted data (if available)
      item_labels <- NULL
      if (!is.null(formatted_data())) {
        fd <- formatted_data()
        item_labels <- fd$ItemLabel
      }

      parse_ranked_dag_csv(
        file_path   = file_info$datapath,
        item_labels = item_labels,
        n_ranks     = input$ncls,
        i18n        = i18n
      )
    })

    # DAG status display in sidebar
    output$dag_status_ui <- renderUI({
      if (input$analysis_mode != "LDLRA") return(NULL)
      dag_status_display_ranked(parsed_dag(), i18n)
    })

    # Sample DAG CSV download (with Rank column)
    output$dl_dag_sample <- downloadHandler(
      filename = function() {
        paste0("sample_dag_ranked_", Sys.Date(), ".csv")
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

      # --- Fixed LDLRA: requires ranked DAG ---
      if (mode == "LDLRA") {
        dag <- parsed_dag()
        if (is.null(dag) || !is.null(dag$error)) {
          shiny::showNotification(
            i18n$t("Please upload a valid DAG CSV file first."),
            type = "warning", duration = 5
          )
          return(NULL)
        }

        withProgress(
          message = i18n$t("Running LDLRA analysis..."),
          value = 0,
          {
            incProgress(0.3, detail = i18n$t("Estimating parameters..."))
            r <- tryCatch(
              exametrika::LDLRA(
                fd,
                ncls     = input$ncls,
                method   = input$method,
                adj_list = dag$adj_list
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

      } else if (mode == "LDLRA_PBIL") {
        # --- LDLRA_PBIL: structure learning ---
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
                ncls            = input$ncls,
                method          = input$method,
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

    # Ordinal Alignment Conditions display
    output$oac_display <- renderUI({
      req(result())
      r <- result()
      soac <- r$SOACflg
      woac <- r$WOACflg
      if (is.null(soac) && is.null(woac)) return(NULL)

      soac_text <- if (isTRUE(soac)) {
        i18n$t("Satisfied")
      } else {
        i18n$t("Not Satisfied")
      }
      woac_text <- if (isTRUE(woac)) {
        i18n$t("Satisfied")
      } else {
        i18n$t("Not Satisfied")
      }
      soac_class <- if (isTRUE(soac)) "text-success" else "text-warning"
      woac_class <- if (isTRUE(woac)) "text-success" else "text-warning"

      tags$div(
        class = "card p-3",
        tags$div(
          tags$strong(
            i18n$t("SOAC (Strongly Ordinal Alignment Condition)")
          ),
          ": ",
          tags$span(class = soac_class, soac_text)
        ),
        tags$div(
          class = "mt-2",
          tags$strong(
            i18n$t("WOAC (Weakly Ordinal Alignment Condition)")
          ),
          ": ",
          tags$span(class = woac_class, woac_text)
        )
      )
    })

    # IRP Table (rows: items, columns: ranks)
    output$table_irp <- DT::renderDT({
      req(result())
      irp <- as.data.frame(result()$IRP)
      DT::datatable(irp, rownames = TRUE,
                    options = list(dom = "tip", pageLength = 20,
                                  scrollX = TRUE)) |>
        DT::formatRound(columns = seq_len(ncol(irp)), digits = 3)
    })

    # IRP Index Table (Alpha, Beta, Gamma)
    output$table_irp_index <- DT::renderDT({
      req(result())
      df <- result()$IRPIndex
      if (is.null(df)) return(NULL)
      DT::datatable(df, rownames = TRUE,
                    options = list(dom = "tip", pageLength = 20,
                                  scrollX = TRUE)) |>
        DT::formatRound(columns = seq_len(ncol(df)), digits = 3)
    })

    # Rank Summary (TRP / LRD / RMD)
    output$table_rank_summary <- DT::renderDT({
      req(result())
      r <- result()
      ncls <- safe_field(r, "n_class", "Nclass", 2)
      msg_label <- if (!is.null(r$msg) && r$msg == "Class") "Class" else "Rank"
      df <- data.frame(
        Label   = paste0(msg_label, " ", seq_len(ncls)),
        TRP     = round(r$TRP, 3),
        LRD     = as.integer(r$LRD),
        LRD_pct = round(as.integer(r$LRD) / sum(r$LRD) * 100, 1)
      )
      colnames(df) <- c(msg_label, "TRP", "N", "N (%)")
      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t", pageLength = 15))
    })

    # CCRR table (Conditional Correct Response Rates)
    output$table_ccrr <- DT::renderDT({
      req(result())
      ccrr <- result()$CCRR_table
      if (is.null(ccrr)) return(NULL)
      df <- as.data.frame(ccrr)
      dt <- DT::datatable(df, rownames = FALSE,
                          options = list(dom = "tip", pageLength = 20,
                                        scrollX = TRUE))
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      if (length(num_cols) > 0) {
        dt <- DT::formatRound(dt, columns = num_cols, digits = 4)
      }
      dt
    })

    # Estimation table (PIRP per rank)
    output$table_estimation <- DT::renderDT({
      req(result())
      est <- result()$Estimation_table
      if (is.null(est)) return(NULL)
      df <- as.data.frame(est)
      dt <- DT::datatable(df, rownames = FALSE,
                          options = list(dom = "tip", pageLength = 30,
                                        scrollX = TRUE))
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      if (length(num_cols) > 0) {
        dt <- DT::formatRound(dt, columns = num_cols, digits = 4)
      }
      dt
    })

    # Student Membership Table
    output$table_students <- DT::renderDT({
      req(result())
      df <- as.data.frame(result()$Students)
      membership_cols <- grep("^Membership", names(df), value = TRUE)
      dt <- DT::datatable(df, rownames = TRUE,
                          options = list(dom = "tip", pageLength = 20,
                                        scrollX = TRUE))
      if (length(membership_cols) > 0) {
        dt <- DT::formatRound(dt, columns = membership_cols, digits = 3)
      }
      dt
    })

    # ========== Plots ==========

    # Plot options (item selector, student selector, DAG rank/layout/direction)
    output$plot_options_ui <- renderUI({
      req(result())
      r <- result()
      pt <- input$plot_type

      if (pt == "IRP") {
        item_names <- rownames(r$IRP)
        selectInput(
          session$ns("selected_item"),
          label = i18n$t("Select Item"),
          choices = setNames(seq_along(item_names), item_names),
          selected = 1
        )
      } else if (pt == "RMP") {
        student_names <- rownames(r$Students)
        selectInput(
          session$ns("selected_student"),
          label = i18n$t("Select Student"),
          choices = setNames(seq_along(student_names), student_names),
          selected = 1
        )
      } else if (pt == "DAG") {
        ncls <- safe_field(r, "n_class", "Nclass", 2)
        msg_label <- if (!is.null(r$msg) && r$msg == "Class") {
          "Class"
        } else {
          "Rank"
        }
        rank_choices <- setNames(
          seq_len(ncls),
          paste(msg_label, seq_len(ncls))
        )
        tagList(
          selectInput(
            session$ns("selected_rank"),
            label = i18n$t("Select Rank"),
            choices = rank_choices,
            selected = 1
          ),
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
      } else {
        NULL
      }
    })

    # Returns a ggplot object (returns NULL if base plot is needed)
    current_plot <- reactive({
      req(result(), input$plot_type)
      r <- result()
      pt <- input$plot_type

      # Place req() outside tryCatch
      if (pt == "IRP") req(input$selected_item)
      if (pt == "RMP") req(input$selected_student)
      if (pt == "DAG") req(input$selected_rank)

      if (!requireNamespace("ggExametrika", quietly = TRUE)) return(NULL)

      tryCatch(
        switch(pt,
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
            rank_idx <- as.integer(input$selected_rank)
            if (is.na(rank_idx) || rank_idx < 1) rank_idx <- 1L
            if (is.list(plots) && length(plots) >= rank_idx) {
              plots[[rank_idx]]
            } else if (!is.list(plots)) {
              plots
            } else {
              NULL
            }
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
        r <- result()
        pt <- input$plot_type
        if (pt == "RMP") {
          idx <- as.integer(input$selected_student)
          if (is.null(idx) || length(idx) == 0 || is.na(idx)) idx <- 1L
          plot(r, type = "RMP", students = idx)
        } else if (pt == "DAG") {
          # Fallback: plot the selected rank's DAG using igraph
          rank_idx <- as.integer(input$selected_rank)
          if (is.null(rank_idx) || is.na(rank_idx)) rank_idx <- 1L
          g_list <- r$g_list
          if (!is.null(g_list) && length(g_list) >= rank_idx) {
            g <- g_list[[rank_idx]]
            msg_label <- if (!is.null(r$msg) && r$msg == "Class") {
              "Class"
            } else {
              "Rank"
            }
            igraph::plot.igraph(
              g,
              layout           = igraph::layout_with_sugiyama(g)$layout,
              vertex.size      = 20,
              vertex.label.cex = 0.8,
              edge.arrow.size  = 0.5,
              main             = paste("LDLRA -", msg_label, rank_idx)
            )
          } else {
            plot.new()
            text(0.5, 0.5, i18n$t("No DAG to display."), cex = 1.2)
          }
        } else {
          plot(r, type = pt)
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

    # Download IRP table
    output$dl_irp <- downloadHandler(
      filename = function() paste0("LDLRA_IRP_", Sys.Date(), ".csv"),
      content  = function(file) {
        utils::write.csv(result()$IRP, file, row.names = TRUE)
      }
    )

    # Download CCRR table
    output$dl_ccrr <- downloadHandler(
      filename = function() paste0("LDLRA_CCRR_", Sys.Date(), ".csv"),
      content  = function(file) {
        utils::write.csv(result()$CCRR_table, file, row.names = FALSE)
      }
    )

    # Download student membership
    output$dl_students <- downloadHandler(
      filename = function() paste0("LDLRA_Students_", Sys.Date(), ".csv"),
      content  = function(file) {
        utils::write.csv(result()$Students, file, row.names = TRUE)
      }
    )

    # Download plot
    output$dl_plot <- downloadHandler(
      filename = function() {
        pt <- input$plot_type
        if (pt == "IRP") {
          paste0("LDLRA_IRP_item", input$selected_item, "_",
                 Sys.Date(), ".png")
        } else if (pt == "RMP") {
          paste0("LDLRA_RMP_student", input$selected_student, "_",
                 Sys.Date(), ".png")
        } else if (pt == "DAG") {
          paste0("LDLRA_DAG_rank", input$selected_rank, "_",
                 Sys.Date(), ".png")
        } else {
          paste0("LDLRA_", pt, "_", Sys.Date(), ".png")
        }
      },
      content = function(file) {
        p <- current_plot()
        if (!is.null(p)) {
          ggplot2::ggsave(file, plot = p, width = 10, height = 8, dpi = 300)
        } else {
          png(file, width = 800, height = 600)
          r <- result()
          pt <- input$plot_type
          if (pt == "DAG") {
            rank_idx <- as.integer(input$selected_rank)
            if (is.null(rank_idx) || is.na(rank_idx)) rank_idx <- 1L
            g_list <- r$g_list
            if (!is.null(g_list) && length(g_list) >= rank_idx) {
              g <- g_list[[rank_idx]]
              msg_label <- if (!is.null(r$msg) && r$msg == "Class") {
                "Class"
              } else {
                "Rank"
              }
              igraph::plot.igraph(
                g,
                layout           = igraph::layout_with_sugiyama(g)$layout,
                vertex.size      = 20,
                vertex.label.cex = 0.8,
                edge.arrow.size  = 0.5,
                main             = paste("LDLRA -", msg_label, rank_idx)
              )
            } else {
              plot.new()
              text(0.5, 0.5, "No DAG to display.", cex = 1.2)
            }
          } else if (pt == "RMP") {
            idx <- as.integer(input$selected_student)
            if (is.null(idx) || is.na(idx)) idx <- 1L
            plot(r, type = "RMP", students = idx)
          } else {
            plot(r, type = pt)
          }
          dev.off()
        }
      }
    )
  })
}
