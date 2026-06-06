# Tests for i18n live-switching wiring and the t_plain() attribute helper

make_i18n <- function(lang = "en", js = FALSE) {
  tr <- shiny.i18n::Translator$new(
    translation_json_path = app_sys("i18n/translation.json")
  )
  tr$set_translation_language(lang)
  if (js) tr$use_js()
  tr
}

test_that("t_plain returns the key in the source language", {
  i18n <- make_i18n("en", js = TRUE)
  expect_identical(t_plain(i18n, "Settings"), "Settings")
})

test_that("t_plain returns a plain translated string (no span) in JA", {
  i18n <- make_i18n("ja", js = TRUE)
  val <- t_plain(i18n, "Settings")
  expect_type(val, "character")
  expect_identical(val, "設定")
  expect_false(grepl("<span", val))
})

test_that("t_plain falls back to the key for unknown strings", {
  i18n <- make_i18n("ja", js = TRUE)
  expect_identical(t_plain(i18n, "no such key at all"), "no such key at all")
})

test_that("after use_js, i18n$t emits a swappable .i18n span", {
  # This is the core of the live-switch fix: usei18n/use_js must run BEFORE the
  # UI is built so every static label becomes a data-key span the JS can swap.
  i18n <- make_i18n("en", js = TRUE)
  html <- as.character(i18n$t("Settings"))
  expect_match(html, "class=\"i18n\"")
  expect_match(html, "data-key=\"Settings\"")
})

test_that("app_ui builds swappable spans and injects the JA dictionary", {
  req <- list(QUERY_STRING = "")
  rt <- htmltools::renderTags(app_ui(req))
  body <- rt$html
  head <- paste(as.character(rt$head), collapse = "")

  # Static sidebar labels are now .i18n spans (would have been plain text before)
  expect_match(body, "data-key=\"Settings\"")
  expect_match(body, "data-key=\"Response Type\"")
  # The translation dictionary (with Japanese) is injected for the JS to use
  expect_match(head, "i18n_translations")
  expect_match(head, "設定")  # 設定 = Settings (ja)
})
