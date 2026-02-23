#' Guide Page Module UI
#'
#' Displays an introductory guide page for new users.
#' This module is UI-only (no server logic needed).
#'
#' @param id Module namespace ID
#' @param i18n shiny.i18n Translator object
#'
#' @noRd
mod_guide_ui <- function(id, i18n) {
  ns <- NS(id)

  bslib::layout_column_wrap(
    width = 1,
    heights_equal = "row",

    # --- Hero Section ---
    bslib::card(
      class = "border-0 bg-light",
      bslib::card_body(
        class = "text-center py-4",
        tags$h2(
          i18n$t("Welcome to shinyExametrika"),
          class = "fw-bold mb-3"
        ),
        tags$p(
          i18n$t("A web-based GUI for test data analysis powered by the exametrika R package. No coding required -- just upload your data and run analyses interactively."),
          class = "lead text-muted mb-0",
          style = "max-width: 700px; margin: 0 auto;"
        )
      )
    ),

    # --- Getting Started ---
    bslib::card(
      bslib::card_header(
        tags$h4(
          icon("rocket"),
          i18n$t("Getting Started"),
          class = "mb-0"
        )
      ),
      bslib::card_body(
        tags$p(
          i18n$t("Follow these 4 steps to run your first analysis:"),
          class = "text-muted mb-4"
        ),

        # Step 1: Load Data
        tags$div(
          class = "d-flex align-items-start mb-4",
          tags$span(
            class = "badge bg-primary rounded-circle me-3 fs-5",
            style = "min-width: 36px; height: 36px; line-height: 36px; text-align: center;",
            "1"
          ),
          tags$div(
            tags$h5(i18n$t("Step 1: Load Your Data"), class = "fw-bold"),
            tags$p(
              i18n$t("Click the \"Data\" tab in the navigation bar at the top. In the left sidebar, either upload a CSV file or select one of the built-in sample datasets."),
              class = "mb-1"
            ),
            tags$ul(
              class = "text-muted small",
              tags$li(i18n$t("Upload your own CSV/TSV file using the file upload area")),
              tags$li(i18n$t("Or choose a built-in sample dataset from the dropdown (e.g., \"J15S500\" for 15 items, 500 examinees)")),
              tags$li(i18n$t("The data preview will appear in the main area on the right"))
            )
          )
        ),

        # Step 2: Format Data
        tags$div(
          class = "d-flex align-items-start mb-4",
          tags$span(
            class = "badge bg-primary rounded-circle me-3 fs-5",
            style = "min-width: 36px; height: 36px; line-height: 36px; text-align: center;",
            "2"
          ),
          tags$div(
            tags$h5(i18n$t("Step 2: Format the Data"), class = "fw-bold"),
            tags$p(
              i18n$t("Still on the Data tab, configure the formatting options in the left sidebar, then click the \"Format Data\" button to prepare your data for analysis."),
              class = "mb-1"
            ),
            tags$ul(
              class = "text-muted small",
              tags$li(i18n$t("Set the Response Type (Auto-detect, Binary, Ordinal, etc.)")),
              tags$li(i18n$t("Specify whether the first column contains IDs")),
              tags$li(i18n$t("Enter a missing value code if applicable (e.g., 99), then click \"Format Data\""))
            )
          )
        ),

        # Step 3: Choose Analysis & Run
        tags$div(
          class = "d-flex align-items-start mb-4",
          tags$span(
            class = "badge bg-primary rounded-circle me-3 fs-5",
            style = "min-width: 36px; height: 36px; line-height: 36px; text-align: center;",
            "3"
          ),
          tags$div(
            tags$h5(i18n$t("Step 3: Choose an Analysis Tab and Run"), class = "fw-bold"),
            tags$p(
              i18n$t("Click an analysis tab (e.g., CTT, IRT, LCA) in the navigation bar. Each analysis tab has a left sidebar for parameter settings and a \"Run Analysis\" button. Set the parameters, then click the blue \"Run Analysis\" button at the top of the sidebar to start the analysis."),
              class = "mb-1"
            ),
            tags$ul(
              class = "text-muted small",
              tags$li(i18n$t("Each analysis tab shows a left sidebar with parameter settings (e.g., model type, number of classes)")),
              tags$li(i18n$t("After setting parameters, click the blue \"Run Analysis\" button (with a play icon) at the top of the sidebar")),
              tags$li(i18n$t("A progress bar will appear while the analysis runs -- please wait for it to complete"))
            )
          )
        ),

        # Step 4: View Results & Download
        tags$div(
          class = "d-flex align-items-start mb-2",
          tags$span(
            class = "badge bg-primary rounded-circle me-3 fs-5",
            style = "min-width: 36px; height: 36px; line-height: 36px; text-align: center;",
            "4"
          ),
          tags$div(
            tags$h5(i18n$t("Step 4: View Results and Download"), class = "fw-bold"),
            tags$p(
              i18n$t("After the analysis completes, results appear in the main area on the right. Use the tabs within the results panel to switch between tables, plots, and other outputs."),
              class = "mb-1"
            ),
            tags$ul(
              class = "text-muted small",
              tags$li(i18n$t("Results are organized into sub-tabs: Results, Plots, Item Fit, etc.")),
              tags$li(i18n$t("Download tables as CSV or plots as PNG using the download buttons in the sidebar")),
              tags$li(i18n$t("Switch between items or students using the selection controls in the plot tab"))
            )
          )
        )
      )
    ),

    # --- Screen Layout Explanation ---
    bslib::card(
      bslib::card_header(
        tags$h4(
          icon("desktop"),
          i18n$t("Screen Layout"),
          class = "mb-0"
        )
      ),
      bslib::card_body(
        tags$p(
          i18n$t("All analysis tabs follow a consistent two-panel layout:"),
          class = "text-muted mb-3"
        ),

        # Data Tab Layout
        tags$div(
          class = "mb-4",
          tags$h5(
            icon("database"),
            i18n$t("Data Tab"),
            class = "fw-bold"
          ),
          tags$p(
            i18n$t("Left sidebar: File upload, sample data selection, formatting options (Response Type, ID Column, Missing Value Code), and the \"Format Data\" button. Right main area: Data preview table showing raw and formatted data with summary statistics."),
            class = "mb-0"
          )
        ),

        # Analysis Tab Layout
        tags$div(
          class = "mb-4",
          tags$h5(
            icon("chart-bar"),
            i18n$t("Analysis Tabs (CTT, IRT, GRM, LCA, LRA, Biclustering)"),
            class = "fw-bold"
          ),
          tags$p(
            i18n$t("Left sidebar: Analysis description, parameter controls (model selection, sliders for class/rank/field counts), the blue \"Run Analysis\" button, and download buttons. Right main area: Result sub-tabs containing fit indices tables, parameter estimates, plots with item/student selectors, and downloadable outputs."),
            class = "mb-1"
          ),
          tags$div(
            class = "bg-light rounded p-3 mb-2",
            style = "font-family: monospace; font-size: 0.85em;",
            tags$pre(
              class = "mb-0",
              style = "white-space: pre; background: none; border: none; padding: 0;",
              i18n$t(paste0(
                "+-------------------------------+\n",
                "|  [Sidebar]  |  [Main Area]    |\n",
                "|             |                 |\n",
                "|  Parameters |  Results Tab    |\n",
                "|  - Model    |  - Fit Indices  |\n",
                "|  - Classes  |  - Parameters   |\n",
                "|  - ...      |  - Plots        |\n",
                "|             |                 |\n",
                "| [Run Analy] |  Plots Tab      |\n",
                "| [Download]  |  - Item select  |\n",
                "|             |  - Plot output  |\n",
                "+-------------------------------+"
              ))
            )
          )
        ),

        # Run Button Emphasis
        tags$div(
          class = "alert alert-info mb-0",
          tags$div(
            class = "d-flex align-items-center",
            icon("circle-info", class = "me-2 fs-5"),
            tags$div(
              tags$strong(i18n$t("About the Run Button")),
              tags$br(),
              tags$span(
                i18n$t("Analysis does NOT start automatically. You must click the \"Run Analysis\" button in the left sidebar after setting your parameters. The button is blue with a play icon and spans the full width of the sidebar.")
              )
            )
          )
        )
      )
    ),

    # --- Available Analysis Methods ---
    bslib::card(
      bslib::card_header(
        tags$h4(
          icon("list-check"),
          i18n$t("Available Analysis Methods"),
          class = "mb-0"
        )
      ),
      bslib::card_body(
        tags$p(
          i18n$t("shinyExametrika currently supports the following test data analysis methods:"),
          class = "text-muted mb-3"
        ),

        # Phase 1: Basic Analysis
        tags$h5(i18n$t("Basic Analysis"), class = "fw-bold mt-2"),
        tags$div(
          class = "row g-3 mb-4",

          tags$div(
            class = "col-md-4",
            tags$div(
              class = "border rounded p-3 h-100",
              tags$h6(
                tags$span(class = "badge bg-success me-1", "CTT"),
                i18n$t("Classical Test Theory")
              ),
              tags$p(
                i18n$t("Reliability coefficients (Alpha, Omega) and item-level analysis."),
                class = "text-muted small mb-1"
              ),
              tags$span(
                class = "badge bg-light text-dark",
                i18n$t("Binary data")
              )
            )
          ),

          tags$div(
            class = "col-md-4",
            tags$div(
              class = "border rounded p-3 h-100",
              tags$h6(
                tags$span(class = "badge bg-success me-1", "IRT"),
                i18n$t("Item Response Theory")
              ),
              tags$p(
                i18n$t("2PL/3PL/4PL models for ability estimation and item characteristic curves."),
                class = "text-muted small mb-1"
              ),
              tags$span(
                class = "badge bg-light text-dark",
                i18n$t("Binary data")
              )
            )
          ),

          tags$div(
            class = "col-md-4",
            tags$div(
              class = "border rounded p-3 h-100",
              tags$h6(
                tags$span(class = "badge bg-success me-1", "GRM"),
                i18n$t("Graded Response Model")
              ),
              tags$p(
                i18n$t("IRT model for ordinal (polytomous) response data."),
                class = "text-muted small mb-1"
              ),
              tags$span(
                class = "badge bg-light text-dark",
                i18n$t("Ordinal data")
              )
            )
          )
        ),

        # Phase 2: Latent Structure Analysis
        tags$h5(i18n$t("Latent Structure Analysis"), class = "fw-bold"),
        tags$div(
          class = "row g-3 mb-4",

          tags$div(
            class = "col-md-3",
            tags$div(
              class = "border rounded p-3 h-100",
              tags$h6(
                tags$span(class = "badge bg-info me-1", "LCA"),
                i18n$t("Latent Class Analysis")
              ),
              tags$p(
                i18n$t("Classify examinees into latent classes based on response patterns."),
                class = "text-muted small mb-1"
              ),
              tags$span(
                class = "badge bg-light text-dark",
                i18n$t("Binary data")
              )
            )
          ),

          tags$div(
            class = "col-md-3",
            tags$div(
              class = "border rounded p-3 h-100",
              tags$h6(
                tags$span(class = "badge bg-info me-1", "LRA"),
                i18n$t("Latent Rank Analysis")
              ),
              tags$p(
                i18n$t("Rank examinees on an ordinal latent scale with item reference profiles."),
                class = "text-muted small mb-1"
              ),
              tags$span(
                class = "badge bg-light text-dark",
                i18n$t("Binary data")
              )
            )
          ),

          tags$div(
            class = "col-md-3",
            tags$div(
              class = "border rounded p-3 h-100",
              tags$h6(
                tags$span(class = "badge bg-info me-1", "Biclustering"),
                i18n$t("Biclustering")
              ),
              tags$p(
                i18n$t("Simultaneously cluster examinees and items into classes and fields."),
                class = "text-muted small mb-1"
              ),
              tags$span(
                class = "badge bg-light text-dark",
                i18n$t("Binary data")
              )
            )
          ),

          tags$div(
            class = "col-md-3",
            tags$div(
              class = "border rounded p-3 h-100",
              tags$h6(
                tags$span(class = "badge bg-secondary me-1", "IRM"),
                i18n$t("Infinite Relational Model")
              ),
              tags$p(
                i18n$t("Nonparametric Bayesian approach to automatically determine optimal cluster structure."),
                class = "text-muted small mb-2"
              ),
              tags$span(
                class = "badge bg-warning text-dark",
                i18n$t("Coming Soon")
              )
            )
          )
        ),

        # Reference
        tags$hr(),
        tags$p(
          class = "text-muted small mb-0",
          i18n$t("Reference: Shojima, K. (2022). Test Data Engineering: Latent Rank Analysis, Biclustering, and Bayesian Network. Springer.")
        )
      )
    ),

    # --- Tips ---
    bslib::card(
      bslib::card_header(
        tags$h4(
          icon("lightbulb"),
          i18n$t("Tips"),
          class = "mb-0"
        )
      ),
      bslib::card_body(
        tags$ul(
          class = "mb-0",
          tags$li(
            class = "mb-2",
            tags$strong(i18n$t("Sample data: ")),
            i18n$t("Try the built-in sample datasets first to explore each analysis method before using your own data.")
          ),
          tags$li(
            class = "mb-2",
            tags$strong(i18n$t("Data requirements: ")),
            i18n$t("CTT, IRT, LCA, LRA, and Biclustering require binary (0/1) data. GRM accepts ordinal data with 3 or more categories.")
          ),
          tags$li(
            class = "mb-2",
            tags$strong(i18n$t("Language switch: ")),
            i18n$t("Use the EN/JA toggle in the top-right corner to switch the interface language at any time.")
          ),
          tags$li(
            class = "mb-0",
            tags$strong(i18n$t("Large datasets: ")),
            i18n$t("For large datasets, analysis may take longer. A progress bar will show the computation status.")
          )
        )
      )
    )
  )
}
