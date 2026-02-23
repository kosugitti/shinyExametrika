#' Data Upload Module UI
#'
#' @param id Module namespace ID
#' @param i18n shiny.i18n Translator object
#'
#' @noRd
mod_data_upload_ui <- function(id, i18n) {
  ns <- NS(id)

  bslib::layout_sidebar(
    # --- Sidebar: input settings ---
    sidebar = bslib::sidebar(
      width = 350,
      title = i18n$t("Data Upload"),

      # File upload
      fileInput(
        ns("file_upload"),
        label = i18n$t("Upload CSV File"),
        accept = c(".csv", ".tsv", ".txt"),
        placeholder = "CSV / TSV"
      ),

      tags$hr(),

      # Sample data selection
      tags$p(i18n$t("Or use sample data"), class = "text-muted"),
      selectInput(
        ns("sample_data"),
        label = i18n$t("Select sample data"),
        choices = c(
          "---" = "",
          "J5S10 (5 items, 10 examinees, binary)" = "J5S10",
          "J15S500 (15 items, 500 examinees, binary)" = "J15S500",
          "J35S515 (35 items, 515 examinees, binary)" = "J35S515",
          "J20S400 (20 items, 400 examinees, binary)" = "J20S400",
          "J12S5000 (12 items, 5000 examinees, binary)" = "J12S5000",
          "J35S5000 (35 items, 5000 examinees, binary)" = "J35S5000",
          "J50S100 (50 items, 100 examinees, binary)" = "J50S100",
          "J5S1000 (5 items, 1000 examinees, ordinal)" = "J5S1000",
          "J15S3810 (15 items, 3810 examinees, ordinal)" = "J15S3810"
        )
      ),

      tags$hr(),

      # Data formatting options
      tags$h6(i18n$t("Settings"), class = "fw-bold"),

      selectInput(
        ns("response_type"),
        label = i18n$t("Response Type"),
        choices = c(
          "Auto-detect" = "auto",
          "Binary" = "binary",
          "Ordinal" = "ordinal",
          "Nominal" = "nominal",
          "Rated" = "rated"
        )
      ),

      selectInput(
        ns("id_column"),
        label = i18n$t("ID Column"),
        choices = c(
          "First column" = "first",
          "No ID column" = "none"
        )
      ),

      textInput(
        ns("na_code"),
        label = i18n$t("Missing Value Code"),
        placeholder = i18n$t("e.g., -9, 99, NA")
      ),

      # Format button
      actionButton(
        ns("btn_format"),
        label = i18n$t("Format Data"),
        class = "btn-primary w-100 mt-3",
        icon = icon("check")
      )
    ),

    # --- Main panel: preview ---
    bslib::navset_card_tab(
      id = ns("data_tabs"),

      # Raw data tab
      bslib::nav_panel(
        title = i18n$t("Raw Data"),
        bslib::card_body(
          uiOutput(ns("raw_summary")),
          DT::DTOutput(ns("raw_table"))
        )
      ),

      # Formatted data tab
      bslib::nav_panel(
        title = i18n$t("Formatted Data"),
        bslib::card_body(
          uiOutput(ns("formatted_summary")),
          DT::DTOutput(ns("formatted_table"))
        )
      )
    )
  )
}


#' Data Upload Module Server
#'
#' @param id Module namespace ID
#' @param i18n shiny.i18n Translator object
#'
#' @return reactive: result of exametrika dataFormat()
#' @noRd
mod_data_upload_server <- function(id, i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Reactive values ---
    raw_data <- reactiveVal(NULL)
    formatted_data <- reactiveVal(NULL)

    # --- CSV file upload ---
    observeEvent(input$file_upload, {
      req(input$file_upload)
      tryCatch({
        df <- utils::read.csv(
          input$file_upload$datapath,
          header = TRUE,
          stringsAsFactors = FALSE
        )
        raw_data(df)
        formatted_data(NULL)
        showNotification(i18n$t("Data loaded successfully!"), type = "message")
      }, error = function(e) {
        showNotification(
          paste(i18n$t("Error loading data"), ":", e$message),
          type = "error"
        )
      })
    })

    # --- Sample data selection ---
    observeEvent(input$sample_data, {
      req(input$sample_data != "")
      tryCatch({
        env <- new.env(parent = emptyenv())
        utils::data(list = input$sample_data, package = "exametrika", envir = env)
        df <- get(input$sample_data, envir = env)

        # Sample data is already in exametrikaData format
        # Raw Data tab: display original values (ordinal uses Q, binary uses U)
        raw_df <- as.data.frame(if (!is.null(df$Q)) df$Q else df$U)
        if (!is.null(df$ID)) raw_df <- cbind(ID = df$ID, raw_df)
        if (!is.null(df$ItemLabel)) colnames(raw_df)[seq_along(df$ItemLabel) + (!is.null(df$ID))] <- df$ItemLabel
        raw_data(raw_df)

        # Set Formatted Data directly (no Format Data button needed)
        formatted_data(df)
        showNotification(i18n$t("Data loaded successfully!"), type = "message")
      }, error = function(err) {
        showNotification(
          paste(i18n$t("Error loading data"), ":", err$message),
          type = "error"
        )
      })
    })

    # --- Data formatting ---
    observeEvent(input$btn_format, {
      req(raw_data())

      tryCatch({
        df <- raw_data()

        # Missing value code
        na_arg <- NULL
        if (nchar(trimws(input$na_code)) > 0) {
          na_arg <- as.numeric(trimws(input$na_code))
        }

        # Response type
        resp_type <- if (input$response_type == "auto") NULL else input$response_type

        # Execute dataFormat()
        # Passing id=NULL causes an error, so build arguments dynamically with do.call()
        fmt_args <- list(
          df,
          na = na_arg,
          response.type = resp_type
        )
        if (input$id_column == "first") fmt_args$id <- 1
        result <- do.call(exametrika::dataFormat, fmt_args)

        formatted_data(result)
        showNotification(i18n$t("Data formatted successfully!"), type = "message")
      }, error = function(e) {
        showNotification(
          paste(i18n$t("Error loading data"), ":", e$message),
          type = "error"
        )
      })
    })

    # --- Raw data: summary ---
    output$raw_summary <- renderUI({
      req(raw_data())
      df <- raw_data()
      tags$div(
        class = "d-flex flex-wrap gap-3 mb-3",
        bslib::value_box(
          title = i18n$t("Rows"),
          value = nrow(df),
          showcase = icon("users"),
          showcase_layout = bslib::showcase_left_center(),
          theme = "primary",
          height = "100px",
          style = "flex: 1; min-width: 150px;"
        ),
        bslib::value_box(
          title = i18n$t("Columns"),
          value = ncol(df),
          showcase = icon("table-columns"),
          showcase_layout = bslib::showcase_left_center(),
          theme = "info",
          height = "100px",
          style = "flex: 1; min-width: 150px;"
        )
      )
    })

    # --- Raw data: table ---
    output$raw_table <- DT::renderDT({
      req(raw_data())
      DT::datatable(
        raw_data(),
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          language = list(url = "")
        )
      )
    })

    # --- Formatted data: summary ---
    output$formatted_summary <- renderUI({
      req(formatted_data())
      fd <- formatted_data()

      resp_type <- if (!is.null(fd$response.type)) fd$response.type else "unknown"
      mat <- if (!is.null(fd$U)) fd$U else fd$Q
      n_items <- ncol(mat)
      n_examinees <- nrow(mat)

      tags$div(
        class = "d-flex flex-wrap gap-3 mb-3",
        bslib::value_box(
          title = i18n$t("Examinees"),
          value = n_examinees,
          showcase = icon("users"),
          showcase_layout = bslib::showcase_left_center(),
          theme = "primary",
          height = "100px",
          style = "flex: 1; min-width: 150px;"
        ),
        bslib::value_box(
          title = i18n$t("Items"),
          value = n_items,
          showcase = icon("list-check"),
          showcase_layout = bslib::showcase_left_center(),
          theme = "info",
          height = "100px",
          style = "flex: 1; min-width: 150px;"
        ),
        bslib::value_box(
          title = i18n$t("Detected type"),
          value = tags$span(resp_type, style = "font-size: 2rem; line-height: 1.2;"),
          showcase = icon("tag"),
          showcase_layout = bslib::showcase_left_center(),
          theme = "success",
          height = "100px",
          style = "flex: 1; min-width: 150px;"
        )
      )
    })

    # --- Formatted data: table ---
    output$formatted_table <- DT::renderDT({
      req(formatted_data())
      fd <- formatted_data()

      # Get matrix for display (binary: U, ordinal/nominal/rated: Q)
      display_df <- as.data.frame(if (!is.null(fd$U)) fd$U else fd$Q)
      if (!is.null(fd$ID)) {
        display_df <- cbind(ID = fd$ID, display_df)
      }
      if (!is.null(fd$ItemLabel)) {
        item_cols <- if (!is.null(fd$ID)) 2:ncol(display_df) else 1:ncol(display_df)
        colnames(display_df)[item_cols] <- fd$ItemLabel
      }

      DT::datatable(
        display_df,
        options = list(
          pageLength = 15,
          scrollX = TRUE
        )
      )
    })

    # --- Return formatted data ---
    return(reactive({ formatted_data() }))
  })
}
