# Unit tests for fct_analysis.R
# Tests for common helper functions
#
# Note: safe_field / extract_fit_indices are non-exported functions, accessed via :::

test_that("safe_field returns new name when available", {
  obj <- list(n_class = 5, Nclass = 3)
  result <- shinyExametrika:::safe_field(obj, "n_class", "Nclass")
  expect_equal(result, 5)
})

test_that("safe_field falls back to old name", {
  obj <- list(Nclass = 3)
  result <- shinyExametrika:::safe_field(obj, "n_class", "Nclass")
  expect_equal(result, 3)
})

test_that("safe_field returns NULL when neither exists", {
  obj <- list(other = 1)
  result <- shinyExametrika:::safe_field(obj, "n_class", "Nclass")
  expect_null(result)
})

test_that("extract_fit_indices handles ModelFit object", {
  fit <- list(
    model_log_like = -100,
    bench_log_like = -50,
    null_log_like = -200,
    model_Chi_sq = 10,
    model_df = 5,
    bench_Chi_sq = 20,
    null_Chi_sq = 50,
    NFI = 0.9,
    RFI = 0.85,
    IFI = 0.95,
    TLI = 0.92,
    CFI = 0.94,
    RMSEA = 0.05,
    AIC = 210,
    CAIC = 220,
    BIC = 215
  )
  class(fit) <- "ModelFit"
  result <- shinyExametrika:::extract_fit_indices(fit)
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
})

test_that("extract_fit_indices handles data.frame input", {
  fit_df <- data.frame(
    Index = c("AIC", "BIC"),
    Value = c(100, 110)
  )
  result <- shinyExametrika:::extract_fit_indices(fit_df)
  expect_true(is.data.frame(result))
})
