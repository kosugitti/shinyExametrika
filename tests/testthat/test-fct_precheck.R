# Tests for the data-readiness pre-check helpers (fct_precheck.R)

test_that("check_data_requirement flags missing data", {
  res <- check_data_requirement(NULL, required = "binary")
  expect_false(res$ok)
  expect_equal(res$status, "no_data")
  expect_true(is.na(res$current))
})

test_that("check_data_requirement accepts a matching type", {
  res <- check_data_requirement(list(response.type = "binary"), required = "binary")
  expect_true(res$ok)
  expect_equal(res$status, "ok")
  expect_equal(res$current, "binary")
})

test_that("check_data_requirement rejects a mismatching type", {
  res <- check_data_requirement(list(response.type = "ordinal"), required = "binary")
  expect_false(res$ok)
  expect_equal(res$status, "wrong_type")
  expect_equal(res$current, "ordinal")
})

test_that("check_data_requirement supports multiple acceptable types", {
  expect_true(check_data_requirement(list(response.type = "rated"),
                                     required = c("ordinal", "rated"))$ok)
  expect_true(check_data_requirement(list(response.type = "ordinal"),
                                     required = c("ordinal", "rated"))$ok)
  expect_false(check_data_requirement(list(response.type = "binary"),
                                      required = c("ordinal", "rated"))$ok)
})

test_that("required = 'any' accepts any loaded data but still needs data", {
  expect_true(check_data_requirement(list(response.type = "nominal"), required = "any")$ok)
  expect_false(check_data_requirement(NULL, required = "any")$ok)
})

test_that("analysis_tab_requirements gates the implemented analysis tabs", {
  reqs <- analysis_tab_requirements()
  expect_true("tab_grm" %in% names(reqs))
  expect_identical(reqs[["tab_descriptives"]], "any")
  expect_identical(reqs[["tab_irt"]], "binary")
  expect_identical(reqs[["tab_grm"]], "ordinal")  # GRM is not applicable to rated data
  # Guide / Data / placeholders are never gated
  expect_false(any(c("tab_guide", "tab_data", "tab_ldb", "tab_binet") %in% names(reqs)))
})

test_that("tab requirements + check_data_requirement agree on enabling", {
  reqs <- analysis_tab_requirements()
  enabled_for <- function(type) {
    fd <- list(response.type = type)
    vapply(names(reqs), function(t) check_data_requirement(fd, reqs[[t]])$ok,
           logical(1))
  }
  bin <- enabled_for("binary")
  expect_true(bin[["tab_irt"]])
  expect_false(bin[["tab_grm"]])
  ord <- enabled_for("ordinal")
  expect_true(ord[["tab_grm"]])
  expect_true(ord[["tab_descriptives"]])
  expect_false(ord[["tab_irt"]])
})

test_that("precheck_banner returns NULL when the requirement is satisfied", {
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = app_sys("i18n/translation.json")
  )
  i18n$set_translation_language("en")
  banner <- precheck_banner(list(response.type = "binary"), "binary", i18n)
  expect_null(banner)
})

test_that("precheck_banner renders an alert for missing / wrong-type data", {
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = app_sys("i18n/translation.json")
  )
  i18n$set_translation_language("en")

  no_data <- precheck_banner(NULL, "binary", i18n)
  expect_s3_class(no_data, "shiny.tag")
  expect_match(as.character(no_data), "alert-warning")

  wrong <- precheck_banner(list(response.type = "ordinal"), "binary", i18n)
  expect_match(as.character(wrong), "binary")
  expect_match(as.character(wrong), "ordinal")
})
