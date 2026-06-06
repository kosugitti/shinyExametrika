# =============================================================================
# fct_dag.R -- Common DAG input components for Phase 3 modules
# =============================================================================
#
# Shared DAG (Directed Acyclic Graph) input UI and logic used by:
# - BNM (Bayesian Network Model)
# - LDLRA (Locally Dependent Latent Rank Analysis)
# - LDB (Locally Dependent Biclustering)
# - BINET (Bayesian Network Item Embedding and Testing)
#
# CSV format: Two columns (From, To) defining directed edges.
#   From,To
#   Item01,Item02
#   Item02,Item03
#
# For LDLRA/LDB/BINET, an optional "Rank" column specifies
# rank-specific adjacency (edges that apply at specific ranks).
#   From,To,Rank
#   Item01,Item02,1
#   Item02,Item03,2
#
# =============================================================================


#' Create DAG input UI components for sidebar
#'
#' Generates UI elements for DAG CSV upload and sample download.
#' Designed to be embedded in a module's sidebar panel.
#'
#' @param ns The namespace function from the parent module (created by NS(id))
#' @param i18n shiny.i18n Translator object
#' @param show_rank_col Logical; if TRUE, include rank-column documentation
#'   and rank-aware CSV sample download (for LDLRA/LDB/BINET). Default FALSE.
#'
#' @return A tagList of Shiny UI elements for DAG input
#'
#' @noRd
dag_input_ui <- function(ns, i18n, show_rank_col = FALSE) {
  tagList(
    tags$h6(i18n$t("DAG Input (CSV)"), class = "mt-2 mb-2"),

    tags$small(
      class = "text-muted d-block mb-2",
      i18n$t("Upload a CSV file with From/To columns defining directed edges.")
    ),

    fileInput(
      ns("dag_file"),
      label = NULL,
      accept = c(".csv", ".tsv", "text/csv"),
      buttonLabel = t_plain(i18n, "Browse..."),
      placeholder = t_plain(i18n, "No file selected")
    ),

    downloadButton(
      ns("dl_dag_sample"),
      label = i18n$t("Download Sample DAG CSV"),
      class = "btn-outline-secondary btn-sm w-100 mb-2"
    ),

    # Show parsed edge summary (reactive)
    uiOutput(ns("dag_status_ui"))
  )
}


#' Parse uploaded DAG CSV file
#'
#' Reads a CSV file containing edge definitions (From/To columns),
#' validates the structure, and returns parsed edge data.
#'
#' Supports two header formats:
#' - Simple: "From", "To" (case-insensitive)
#' - Extended: "From Item (Parent) >>>", ">>> To Item (Child)"
#'   (detected automatically and normalized)
#'
#' @param file_path Path to the uploaded CSV file
#' @param item_labels Optional character vector of item labels from the
#'   data. If provided, validates that all nodes in the DAG match.
#' @param i18n shiny.i18n Translator object for error messages
#'
#' @return A list with:
#'   \item{edges}{data.frame with From, To (and optionally Rank) columns}
#'   \item{adj_matrix}{Adjacency matrix (items x items) with dimnames}
#'   \item{n_edges}{Number of directed edges}
#'   \item{nodes}{Character vector of unique node names}
#'   \item{is_acyclic}{Logical; TRUE if graph is a DAG}
#'   \item{error}{NULL on success, or error message string}
#'
#' @noRd
parse_dag_csv <- function(file_path, item_labels = NULL, i18n = NULL) {
  # Helper for error messages (fallback to English if i18n not available)
  msg <- function(key) {
    if (!is.null(i18n)) i18n$t(key) else key
  }

  result <- tryCatch({
    # Read CSV
    raw <- utils::read.csv(file_path, stringsAsFactors = FALSE, header = TRUE)

    if (ncol(raw) < 2) {
      return(list(error = msg("DAG CSV must have at least 2 columns (From, To).")))
    }

    # Normalize column names (handle extended header format)
    cnames <- tolower(trimws(colnames(raw)))
    # Detect extended format: first col contains "from", second contains "to"
    if (grepl("from", cnames[1]) && grepl("to", cnames[2])) {
      colnames(raw)[1] <- "From"
      colnames(raw)[2] <- "To"
      if (ncol(raw) >= 3 && grepl("rank", cnames[3])) {
        colnames(raw)[3] <- "Rank"
      }
    } else {
      # Try exact match (case-insensitive)
      from_idx <- which(cnames == "from")
      to_idx <- which(cnames == "to")
      if (length(from_idx) == 0 || length(to_idx) == 0) {
        return(list(error = msg("DAG CSV must have 'From' and 'To' columns.")))
      }
      colnames(raw)[from_idx[1]] <- "From"
      colnames(raw)[to_idx[1]] <- "To"
      rank_idx <- which(cnames == "rank")
      if (length(rank_idx) > 0) colnames(raw)[rank_idx[1]] <- "Rank"
    }

    edges <- raw[, intersect(c("From", "To", "Rank"), colnames(raw)), drop = FALSE]

    # Trim whitespace from node names
    edges$From <- trimws(as.character(edges$From))
    edges$To   <- trimws(as.character(edges$To))

    # Remove empty rows
    edges <- edges[edges$From != "" & edges$To != "", , drop = FALSE]

    if (nrow(edges) == 0) {
      return(list(error = msg("DAG CSV contains no valid edges.")))
    }

    # Extract unique nodes
    nodes <- sort(unique(c(edges$From, edges$To)))

    # Check for self-loops
    self_loops <- edges$From == edges$To
    if (any(self_loops)) {
      return(list(error = msg("DAG contains self-loops. Each edge must connect different items.")))
    }

    # Check for duplicate edges
    edge_keys <- paste(edges$From, edges$To, sep = "->")
    if (any(duplicated(edge_keys))) {
      return(list(error = msg("DAG contains duplicate edges.")))
    }

    # Build adjacency matrix
    n <- length(nodes)
    adj <- matrix(0L, nrow = n, ncol = n,
                  dimnames = list(nodes, nodes))
    for (i in seq_len(nrow(edges))) {
      adj[edges$From[i], edges$To[i]] <- 1L
    }

    # Acyclicity check via topological sort (Kahn's algorithm)
    is_acyclic <- check_dag_acyclic(adj)

    if (!is_acyclic) {
      return(list(error = msg("DAG contains cycles. The graph must be acyclic.")))
    }

    # Validate node names against item labels (if provided)
    if (!is.null(item_labels)) {
      missing_in_data <- setdiff(nodes, item_labels)
      if (length(missing_in_data) > 0) {
        err_msg <- paste0(
          msg("DAG contains nodes not found in data"),
          ": ", paste(missing_in_data, collapse = ", ")
        )
        return(list(error = err_msg))
      }
    }

    list(
      edges      = edges,
      adj_matrix = adj,
      n_edges    = nrow(edges),
      nodes      = nodes,
      is_acyclic = is_acyclic,
      error      = NULL
    )

  }, error = function(e) {
    list(error = paste(msg("Failed to parse DAG CSV"), ":", e$message))
  })

  result
}


#' Check if an adjacency matrix represents a DAG (no cycles)
#'
#' Uses Kahn's algorithm for topological sorting. If all nodes can be
#' processed, the graph is acyclic.
#'
#' @param adj Square adjacency matrix (0/1). adj[i,j] = 1 means i -> j.
#'
#' @return Logical; TRUE if graph is acyclic
#'
#' @noRd
check_dag_acyclic <- function(adj) {
  n <- nrow(adj)
  if (n == 0) return(TRUE)

  # Calculate in-degree for each node
  in_degree <- colSums(adj)

  # Start with nodes having no incoming edges
  queue <- which(in_degree == 0)
  processed <- 0L


  while (length(queue) > 0) {
    node <- queue[1]
    queue <- queue[-1]
    processed <- processed + 1L

    # Find children of this node
    children <- which(adj[node, ] == 1)
    for (child in children) {
      in_degree[child] <- in_degree[child] - 1L
      if (in_degree[child] == 0) {
        queue <- c(queue, child)
      }
    }
  }

  # If all nodes processed, graph is acyclic
  processed == n
}


#' Generate sample DAG CSV content
#'
#' Creates a sample CSV string for download. If item_labels are provided,
#' generates a realistic DAG using those labels. Otherwise, uses generic
#' Item01-Item05 labels.
#'
#' @param item_labels Optional character vector of item labels
#' @param include_rank Logical; if TRUE, include a Rank column (for LDLRA/LDB/BINET)
#'
#' @return Character string containing CSV content
#'
#' @noRd
generate_sample_dag_csv <- function(item_labels = NULL, include_rank = FALSE) {
  if (is.null(item_labels) || length(item_labels) < 3) {
    # Default sample: simple 5-item chain DAG
    if (include_rank) {
      lines <- c(
        "From,To,Rank",
        "Item01,Item02,1",
        "Item02,Item03,1",
        "Item02,Item04,2",
        "Item03,Item05,2",
        "Item04,Item05,3"
      )
    } else {
      lines <- c(
        "From,To",
        "Item01,Item02",
        "Item02,Item03",
        "Item02,Item04",
        "Item03,Item05",
        "Item04,Item05"
      )
    }
  } else {
    # Generate a simple chain DAG from actual item labels
    n_items <- length(item_labels)
    # Create edges: item[i] -> item[i+1] for a simple chain
    n_edges <- min(n_items - 1, 10)  # cap at 10 edges for readability
    from_items <- item_labels[seq_len(n_edges)]
    to_items   <- item_labels[seq_len(n_edges) + 1]

    if (include_rank) {
      # Distribute edges across ranks
      n_ranks <- min(3, n_edges)
      ranks <- rep(seq_len(n_ranks), length.out = n_edges)
      lines <- c("From,To,Rank",
                  paste(from_items, to_items, ranks, sep = ","))
    } else {
      lines <- c("From,To",
                  paste(from_items, to_items, sep = ","))
    }
  }

  paste(lines, collapse = "\n")
}


#' Parse uploaded DAG CSV file with rank-specific edges
#'
#' Reads a CSV file containing rank-specific edge definitions (From/To/Rank
#' columns), validates the structure, and returns per-rank adjacency matrices.
#' Used by LDLRA/LDB/BINET modules where each rank can have a different DAG.
#'
#' @param file_path Path to the uploaded CSV file
#' @param item_labels Optional character vector of item labels from the data.
#'   If provided, validates that all nodes in the DAG match.
#' @param n_ranks Number of ranks expected (must match ncls parameter)
#' @param i18n shiny.i18n Translator object for error messages
#'
#' @return A list with:
#'   \item{adj_list}{List of adjacency matrices (one per rank)}
#'   \item{edges}{data.frame with From, To, Rank columns}
#'   \item{rank_edges}{Integer vector of edge counts per rank}
#'   \item{nodes}{Character vector of unique node names}
#'   \item{n_ranks}{Number of ranks}
#'   \item{error}{NULL on success, or error message string}
#'
#' @noRd
parse_ranked_dag_csv <- function(file_path, item_labels = NULL, n_ranks, i18n = NULL) {
  msg <- function(key) {
    if (!is.null(i18n)) i18n$t(key) else key
  }

  result <- tryCatch({
    # Read CSV
    raw <- utils::read.csv(file_path, stringsAsFactors = FALSE, header = TRUE)

    if (ncol(raw) < 3) {
      return(list(error = msg("DAG CSV for LDLRA must have 3 columns (From, To, Rank).")))
    }

    # Normalize column names (handle extended header format)
    cnames <- tolower(trimws(colnames(raw)))
    if (grepl("from", cnames[1]) && grepl("to", cnames[2])) {
      colnames(raw)[1] <- "From"
      colnames(raw)[2] <- "To"
      if (ncol(raw) >= 3 && grepl("rank|class", cnames[3])) {
        colnames(raw)[3] <- "Rank"
      }
    } else {
      from_idx <- which(cnames == "from")
      to_idx <- which(cnames == "to")
      rank_idx <- which(cnames %in% c("rank", "class"))
      if (length(from_idx) == 0 || length(to_idx) == 0) {
        return(list(error = msg("DAG CSV must have 'From' and 'To' columns.")))
      }
      colnames(raw)[from_idx[1]] <- "From"
      colnames(raw)[to_idx[1]] <- "To"
      if (length(rank_idx) > 0) {
        colnames(raw)[rank_idx[1]] <- "Rank"
      }
    }

    if (!"Rank" %in% colnames(raw)) {
      return(list(error = msg("DAG CSV for LDLRA must include a Rank column.")))
    }

    edges <- raw[, c("From", "To", "Rank"), drop = FALSE]

    # Trim whitespace from node names
    edges$From <- trimws(as.character(edges$From))
    edges$To   <- trimws(as.character(edges$To))
    edges$Rank <- as.integer(edges$Rank)

    # Remove empty rows
    edges <- edges[edges$From != "" & edges$To != "" & !is.na(edges$Rank),
                   , drop = FALSE]

    if (nrow(edges) == 0) {
      return(list(error = msg("DAG CSV contains no valid edges.")))
    }

    # Check for self-loops
    if (any(edges$From == edges$To)) {
      return(list(error = msg(
        "DAG contains self-loops. Each edge must connect different items."
      )))
    }

    # Check for duplicate edges within the same rank
    edge_keys <- paste(edges$From, edges$To, edges$Rank, sep = "->")
    if (any(duplicated(edge_keys))) {
      return(list(error = msg("DAG contains duplicate edges.")))
    }

    # Validate rank values
    unique_ranks <- sort(unique(edges$Rank))
    if (any(unique_ranks < 1) || any(unique_ranks > n_ranks)) {
      return(list(error = paste0(
        msg("Rank values in DAG must be between 1 and"), " ", n_ranks, "."
      )))
    }

    # Extract unique nodes
    nodes <- sort(unique(c(edges$From, edges$To)))

    # Validate node names against item labels (if provided)
    if (!is.null(item_labels)) {
      missing_in_data <- setdiff(nodes, item_labels)
      if (length(missing_in_data) > 0) {
        return(list(error = paste0(
          msg("DAG contains nodes not found in data"),
          ": ", paste(missing_in_data, collapse = ", ")
        )))
      }
    }

    # Determine the full set of labels for matrix construction
    all_labels <- if (!is.null(item_labels)) item_labels else nodes
    n_items <- length(all_labels)

    # Build per-rank adjacency matrices
    adj_list <- vector("list", n_ranks)
    rank_edges <- integer(n_ranks)

    for (r in seq_len(n_ranks)) {
      adj_r <- matrix(0L, nrow = n_items, ncol = n_items,
                      dimnames = list(all_labels, all_labels))
      r_edges <- edges[edges$Rank == r, , drop = FALSE]
      for (i in seq_len(nrow(r_edges))) {
        adj_r[r_edges$From[i], r_edges$To[i]] <- 1L
      }

      # Check acyclicity for this rank
      if (!check_dag_acyclic(adj_r)) {
        return(list(error = paste0(
          msg("DAG contains cycles at rank"), " ", r, "."
        )))
      }

      adj_list[[r]] <- adj_r
      rank_edges[r] <- nrow(r_edges)
    }

    list(
      adj_list   = adj_list,
      edges      = edges,
      rank_edges = rank_edges,
      nodes      = nodes,
      n_ranks    = n_ranks,
      error      = NULL
    )

  }, error = function(e) {
    list(error = paste(msg("Failed to parse DAG CSV"), ":", e$message))
  })

  result
}


#' Create DAG status display for ranked DAGs
#'
#' Shows per-rank edge counts in a success/error alert box.
#' Used by LDLRA/LDB/BINET modules.
#'
#' @param dag_result The result from parse_ranked_dag_csv()
#' @param i18n shiny.i18n Translator object
#'
#' @return A Shiny tags object (alert box)
#'
#' @noRd
dag_status_display_ranked <- function(dag_result, i18n) {
  if (is.null(dag_result)) {
    return(NULL)
  }

  if (!is.null(dag_result$error)) {
    tags$div(
      class = "alert alert-danger py-2 px-3 mb-2",
      style = "font-size: 0.85em;",
      shiny::icon("exclamation-triangle"),
      " ", dag_result$error
    )
  } else {
    rank_info <- paste0(
      "Rank ", seq_along(dag_result$rank_edges),
      ": ", dag_result$rank_edges, " ",
      i18n$t("edges")
    )
    tags$div(
      class = "alert alert-success py-2 px-3 mb-2",
      style = "font-size: 0.85em;",
      shiny::icon("check-circle"),
      " ",
      paste0(
        i18n$t("DAG loaded"), " (",
        length(dag_result$nodes), " ", i18n$t("nodes"), ")"
      ),
      tags$br(),
      tags$small(paste(rank_info, collapse = " | "))
    )
  }
}


#' Create DAG status display (parsed edge summary)
#'
#' Renders a small info box showing the number of edges and nodes
#' parsed from the uploaded DAG CSV, or an error message.
#'
#' @param dag_result The result from parse_dag_csv()
#' @param i18n shiny.i18n Translator object
#'
#' @return A Shiny tags object (alert box)
#'
#' @noRd
dag_status_display <- function(dag_result, i18n) {
  if (is.null(dag_result)) {
    return(NULL)
  }

  if (!is.null(dag_result$error)) {
    tags$div(
      class = "alert alert-danger py-2 px-3 mb-2",
      style = "font-size: 0.85em;",
      shiny::icon("exclamation-triangle"),
      " ", dag_result$error
    )
  } else {
    tags$div(
      class = "alert alert-success py-2 px-3 mb-2",
      style = "font-size: 0.85em;",
      shiny::icon("check-circle"),
      " ",
      paste0(
        i18n$t("DAG loaded"), ": ",
        dag_result$n_edges, " ",
        i18n$t("edges"), ", ",
        length(dag_result$nodes), " ",
        i18n$t("nodes")
      )
    )
  }
}
