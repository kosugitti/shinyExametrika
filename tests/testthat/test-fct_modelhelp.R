# Tests for the inline model-help and parameter-guidance helpers

test_that("model_help_registry covers every analysis module", {
  reg <- model_help_registry()
  expected <- c("descriptives", "ctt", "irt", "grm", "lca", "lra",
                "biclustering", "irm", "bnm", "ldlra")
  expect_setequal(names(reg), expected)
  for (info in reg) {
    expect_true(all(c("name", "desc", "type") %in% names(info)))
    expect_true(nzchar(info$name) && nzchar(info$desc) && nzchar(info$type))
  }
})

test_that("model_help_block renders a collapsible details panel", {
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = app_sys("i18n/translation.json")
  )
  i18n$set_translation_language("en")

  blk <- model_help_block("irt", i18n)
  html <- as.character(blk)
  expect_match(html, "<details")
  expect_match(html, "<summary")
  expect_match(html, "Item Response Theory")
})

test_that("model_help_block returns NULL for an unknown key", {
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = app_sys("i18n/translation.json")
  )
  i18n$set_translation_language("en")
  expect_null(model_help_block("does_not_exist", i18n))
})

test_that("model_help_block honours the active language", {
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = app_sys("i18n/translation.json")
  )
  i18n$set_translation_language("ja")
  html <- as.character(model_help_block("irt", i18n))
  expect_match(html, "項目反応理論")
  expect_match(html, "このモデルについて")
})

test_that("param_label attaches a help tooltip to the label", {
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = app_sys("i18n/translation.json")
  )
  i18n$set_translation_language("en")

  lbl <- param_label(
    "IRT Model",
    "2PL estimates item discrimination and difficulty. 3PL adds a guessing (lower asymptote) parameter; 4PL adds a careless-slip (upper asymptote) parameter. Start with 2PL unless you have a reason to model guessing or slipping.",
    i18n
  )
  html <- as.character(lbl)
  expect_match(html, "IRT Model")
  expect_match(html, "tooltip")
  expect_match(html, "2PL estimates")
})
