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


# =============================================================================
# Tests for parse_ranked_dag_csv (LDLRA/LDB/BINET support)
# =============================================================================

test_that("parse_ranked_dag_csv parses valid ranked CSV correctly", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c(
    "From,To,Rank",
    "Item01,Item02,1",
    "Item02,Item03,1",
    "Item02,Item04,2",
    "Item03,Item05,2"
  ), tmp)
  on.exit(unlink(tmp))

  result <- parse_ranked_dag_csv(tmp, n_ranks = 2)
  expect_null(result$error)
  expect_equal(result$n_ranks, 2)
  expect_equal(result$rank_edges, c(2L, 2L))
  expect_equal(length(result$adj_list), 2)
  expect_true(is.matrix(result$adj_list[[1]]))
  expect_true(is.matrix(result$adj_list[[2]]))
  # Rank 1 edges: Item01->Item02, Item02->Item03
  expect_equal(result$adj_list[[1]]["Item01", "Item02"], 1L)
  expect_equal(result$adj_list[[1]]["Item02", "Item03"], 1L)
  # Rank 2 edges: Item02->Item04, Item03->Item05
  expect_equal(result$adj_list[[2]]["Item02", "Item04"], 1L)
  expect_equal(result$adj_list[[2]]["Item03", "Item05"], 1L)
  # Rank 1 should NOT have rank 2 edges
  expect_equal(result$adj_list[[1]]["Item02", "Item04"], 0L)
})

test_that("parse_ranked_dag_csv rejects CSV without Rank column", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("From,To", "A,B", "B,C"), tmp)
  on.exit(unlink(tmp))

  result <- parse_ranked_dag_csv(tmp, n_ranks = 2)
  expect_false(is.null(result$error))
})

test_that("parse_ranked_dag_csv rejects CSV with fewer than 3 columns", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("From,To", "A,B"), tmp)
  on.exit(unlink(tmp))

  result <- parse_ranked_dag_csv(tmp, n_ranks = 2)
  expect_false(is.null(result$error))
})

test_that("parse_ranked_dag_csv rejects out-of-range rank values", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("From,To,Rank", "A,B,1", "B,C,5"), tmp)
  on.exit(unlink(tmp))

  result <- parse_ranked_dag_csv(tmp, n_ranks = 3)
  expect_false(is.null(result$error))
})

test_that("parse_ranked_dag_csv detects per-rank cycles", {
  tmp <- tempfile(fileext = ".csv")
  # Rank 1 has a cycle: A -> B -> C -> A
  writeLines(c("From,To,Rank", "A,B,1", "B,C,1", "C,A,1"), tmp)
  on.exit(unlink(tmp))

  result <- parse_ranked_dag_csv(tmp, n_ranks = 2)
  expect_false(is.null(result$error))
  expect_true(grepl("cycle", result$error, ignore.case = TRUE))
})

test_that("parse_ranked_dag_csv allows cross-rank anti-parallel edges", {
  tmp <- tempfile(fileext = ".csv")
  # Rank 1: A -> B; Rank 2: B -> A (not a cycle within any single rank)
  writeLines(c("From,To,Rank", "A,B,1", "B,A,2"), tmp)
  on.exit(unlink(tmp))

  result <- parse_ranked_dag_csv(tmp, n_ranks = 2)
  expect_null(result$error)
  expect_equal(result$adj_list[[1]]["A", "B"], 1L)
  expect_equal(result$adj_list[[2]]["B", "A"], 1L)
})

test_that("parse_ranked_dag_csv handles empty ranks", {
  tmp <- tempfile(fileext = ".csv")
  # Only rank 1 has edges; rank 2 and 3 are empty
  writeLines(c("From,To,Rank", "A,B,1", "B,C,1"), tmp)
  on.exit(unlink(tmp))

  result <- parse_ranked_dag_csv(tmp, n_ranks = 3)
  expect_null(result$error)
  expect_equal(result$rank_edges, c(2L, 0L, 0L))
  expect_equal(sum(result$adj_list[[2]]), 0)
  expect_equal(sum(result$adj_list[[3]]), 0)
})

test_that("parse_ranked_dag_csv validates nodes against item labels", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("From,To,Rank", "A,B,1", "B,C,2"), tmp)
  on.exit(unlink(tmp))

  # C not in labels
  result <- parse_ranked_dag_csv(tmp, item_labels = c("A", "B"), n_ranks = 2)
  expect_false(is.null(result$error))

  # All nodes match
  result2 <- parse_ranked_dag_csv(
    tmp, item_labels = c("A", "B", "C"), n_ranks = 2
  )
  expect_null(result2$error)
})

test_that("parse_ranked_dag_csv uses item_labels for matrix dimensions", {
  tmp <- tempfile(fileext = ".csv")
  # Only A->B edge, but D and E are also items
  writeLines(c("From,To,Rank", "A,B,1"), tmp)
  on.exit(unlink(tmp))

  labels <- c("A", "B", "C", "D", "E")
  result <- parse_ranked_dag_csv(tmp, item_labels = labels, n_ranks = 2)
  expect_null(result$error)
  expect_equal(nrow(result$adj_list[[1]]), 5)
  expect_equal(ncol(result$adj_list[[1]]), 5)
  expect_equal(rownames(result$adj_list[[1]]), labels)
})

test_that("parse_ranked_dag_csv detects self-loops", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("From,To,Rank", "A,A,1"), tmp)
  on.exit(unlink(tmp))

  result <- parse_ranked_dag_csv(tmp, n_ranks = 1)
  expect_false(is.null(result$error))
})

test_that("parse_ranked_dag_csv detects duplicate edges within same rank", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("From,To,Rank", "A,B,1", "A,B,1"), tmp)
  on.exit(unlink(tmp))

  result <- parse_ranked_dag_csv(tmp, n_ranks = 1)
  expect_false(is.null(result$error))
})
