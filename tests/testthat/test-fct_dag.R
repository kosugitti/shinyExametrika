# =============================================================================
# test-fct_dag.R -- Unit tests for DAG input helper functions
# =============================================================================

test_that("check_dag_acyclic returns TRUE for acyclic graph", {
  # Simple chain: 1 -> 2 -> 3
  adj <- matrix(0L, nrow = 3, ncol = 3,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  adj["A", "B"] <- 1L
  adj["B", "C"] <- 1L
  expect_true(check_dag_acyclic(adj))
})

test_that("check_dag_acyclic returns FALSE for cyclic graph", {
  # Cycle: 1 -> 2 -> 3 -> 1
  adj <- matrix(0L, nrow = 3, ncol = 3,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  adj["A", "B"] <- 1L
  adj["B", "C"] <- 1L
  adj["C", "A"] <- 1L
  expect_false(check_dag_acyclic(adj))
})

test_that("check_dag_acyclic handles empty graph", {
  adj <- matrix(0L, nrow = 0, ncol = 0)
  expect_true(check_dag_acyclic(adj))
})

test_that("check_dag_acyclic handles disconnected DAG", {
  # Two disconnected chains: A -> B, C -> D
  adj <- matrix(0L, nrow = 4, ncol = 4,
                dimnames = list(c("A", "B", "C", "D"), c("A", "B", "C", "D")))
  adj["A", "B"] <- 1L
  adj["C", "D"] <- 1L
  expect_true(check_dag_acyclic(adj))
})

test_that("parse_dag_csv parses valid CSV correctly", {
  # Create temporary CSV
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("From,To", "Item01,Item02", "Item02,Item03"), tmp)
  on.exit(unlink(tmp))

  result <- parse_dag_csv(tmp)
  expect_null(result$error)
  expect_equal(result$n_edges, 2)
  expect_equal(sort(result$nodes), c("Item01", "Item02", "Item03"))
  expect_true(result$is_acyclic)
  expect_equal(nrow(result$adj_matrix), 3)
  expect_equal(ncol(result$adj_matrix), 3)
  expect_equal(result$adj_matrix["Item01", "Item02"], 1L)
  expect_equal(result$adj_matrix["Item02", "Item03"], 1L)
  expect_equal(result$adj_matrix["Item01", "Item03"], 0L)
})

test_that("parse_dag_csv detects cycles", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("From,To", "A,B", "B,C", "C,A"), tmp)
  on.exit(unlink(tmp))

  result <- parse_dag_csv(tmp)
  expect_false(is.null(result$error))
})

test_that("parse_dag_csv detects self-loops", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("From,To", "A,A", "B,C"), tmp)
  on.exit(unlink(tmp))

  result <- parse_dag_csv(tmp)
  expect_false(is.null(result$error))
})

test_that("parse_dag_csv detects duplicate edges", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("From,To", "A,B", "A,B"), tmp)
  on.exit(unlink(tmp))

  result <- parse_dag_csv(tmp)
  expect_false(is.null(result$error))
})

test_that("parse_dag_csv validates node names against item labels", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("From,To", "Item01,Item02", "Item02,Item03"), tmp)
  on.exit(unlink(tmp))

  # Item03 not in labels
  result <- parse_dag_csv(tmp, item_labels = c("Item01", "Item02"))
  expect_false(is.null(result$error))

  # All items match
  result2 <- parse_dag_csv(tmp, item_labels = c("Item01", "Item02", "Item03"))
  expect_null(result2$error)
})

test_that("parse_dag_csv handles extended header format", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c(
    "From Item (Parent) >>>,>>> To Item (Child)",
    "Item01,Item02",
    "Item02,Item03"
  ), tmp)
  on.exit(unlink(tmp))

  result <- parse_dag_csv(tmp)
  expect_null(result$error)
  expect_equal(result$n_edges, 2)
})

test_that("parse_dag_csv rejects CSV with insufficient columns", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("OnlyColumn", "A", "B"), tmp)
  on.exit(unlink(tmp))

  result <- parse_dag_csv(tmp)
  expect_false(is.null(result$error))
})

test_that("generate_sample_dag_csv returns valid CSV content", {
  csv <- generate_sample_dag_csv()
  lines <- strsplit(csv, "\n")[[1]]
  expect_equal(lines[1], "From,To")
  expect_true(length(lines) > 1)
})

test_that("generate_sample_dag_csv with item labels uses actual labels", {
  labels <- c("Q1", "Q2", "Q3", "Q4")
  csv <- generate_sample_dag_csv(item_labels = labels)
  lines <- strsplit(csv, "\n")[[1]]
  expect_equal(lines[1], "From,To")
  # Check that at least one item label appears

  expect_true(any(grepl("Q1", lines)))
})

test_that("generate_sample_dag_csv with rank column", {
  csv <- generate_sample_dag_csv(include_rank = TRUE)
  lines <- strsplit(csv, "\n")[[1]]
  expect_equal(lines[1], "From,To,Rank")
})

# --- build_adj_list_from_edges tests ---

test_that("build_adj_list_from_edges creates correct structure with Rank column", {
  edges <- data.frame(
    From = c("Item01", "Item02", "Item02"),
    To   = c("Item02", "Item03", "Item03"),
    Rank = c(1, 1, 2),
    stringsAsFactors = FALSE
  )
  item_labels <- c("Item01", "Item02", "Item03")
  adj_list <- build_adj_list_from_edges(edges, item_labels, ncls = 3)

  expect_length(adj_list, 3)
  # Rank 1: Item01 -> Item02, Item02 -> Item03
  expect_equal(adj_list[[1]]["Item01", "Item02"], 1L)
  expect_equal(adj_list[[1]]["Item02", "Item03"], 1L)
  # Rank 2: Item02 -> Item03 only
  expect_equal(adj_list[[2]]["Item01", "Item02"], 0L)
  expect_equal(adj_list[[2]]["Item02", "Item03"], 1L)
  # Rank 3: no edges
  expect_equal(adj_list[[3]]["Item01", "Item02"], 0L)
  expect_equal(adj_list[[3]]["Item02", "Item03"], 0L)
})

test_that("build_adj_list_from_edges without Rank column copies to all ranks", {
  edges <- data.frame(
    From = c("Item01", "Item02"),
    To   = c("Item02", "Item03"),
    stringsAsFactors = FALSE
  )
  item_labels <- c("Item01", "Item02", "Item03")
  adj_list <- build_adj_list_from_edges(edges, item_labels, ncls = 3)

  expect_length(adj_list, 3)
  for (k in 1:3) {
    expect_equal(adj_list[[k]]["Item01", "Item02"], 1L)
    expect_equal(adj_list[[k]]["Item02", "Item03"], 1L)
    expect_equal(adj_list[[k]]["Item01", "Item03"], 0L)
  }
})

test_that("build_adj_list_from_edges ignores out-of-range ranks", {
  edges <- data.frame(
    From = c("A", "A"),
    To   = c("B", "B"),
    Rank = c(1, 5),
    stringsAsFactors = FALSE
  )
  item_labels <- c("A", "B")
  adj_list <- build_adj_list_from_edges(edges, item_labels, ncls = 2)

  expect_length(adj_list, 2)
  expect_equal(adj_list[[1]]["A", "B"], 1L)
  expect_equal(adj_list[[2]]["A", "B"], 0L)  # Rank 5 out of range, ignored
})
