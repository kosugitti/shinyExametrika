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
      buttonLabel = i18n$t("Browse..."),
      placeholder = i18n$t("No file selected")
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

    # Check for duplicate edges (include Rank if present)
    if ("Rank" %in% colnames(edges) && !all(is.na(edges$Rank))) {
      edge_keys <- paste(edges$From, edges$To, edges$Rank, sep = "->")
    } else {
      edge_keys <- paste(edges$From, edges$To, sep = "->")
    }
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


#' Build rank-specific adjacency list from parsed DAG edges
#'
#' Converts a data.frame of edges (From, To, Rank) into a list of adjacency
#' matrices, one per rank. Used by LDLRA, LDB, BINET modules.
#'
#' @param edges data.frame with columns From, To, and optionally Rank
#' @param item_labels Character vector of all item labels (defines matrix dimensions)
#' @param ncls Number of ranks/classes (determines list length)
#'
#' @return A list of length ncls, where each element is an adjacency matrix
#'   (items x items) with 0/1 entries. If Rank column is missing, all ranks
#'   share the same adjacency structure.
#'
#' @noRd
build_adj_list_from_edges <- function(edges, item_labels, ncls) {
  n_items <- length(item_labels)

  # Initialize empty adjacency matrices for each rank
  adj_list <- vector("list", ncls)
  for (k in seq_len(ncls)) {
    adj_list[[k]] <- matrix(
      0L, nrow = n_items, ncol = n_items,
      dimnames = list(item_labels, item_labels)
    )
  }

  if ("Rank" %in% colnames(edges) && !all(is.na(edges$Rank))) {
    # Rank-specific edges
    for (i in seq_len(nrow(edges))) {
      rank_val <- as.integer(edges$Rank[i])
      from_item <- as.character(edges$From[i])
      to_item <- as.character(edges$To[i])
      if (!is.na(rank_val) && rank_val >= 1 && rank_val <= ncls &&
          from_item %in% item_labels && to_item %in% item_labels) {
        adj_list[[rank_val]][from_item, to_item] <- 1L
      }
    }
  } else {
    # No Rank column: same DAG for all ranks
    for (i in seq_len(nrow(edges))) {
      from_item <- as.character(edges$From[i])
      to_item <- as.character(edges$To[i])
      if (from_item %in% item_labels && to_item %in% item_labels) {
        for (k in seq_len(ncls)) {
          adj_list[[k]][from_item, to_item] <- 1L
        }
      }
    }
  }

  adj_list
}
