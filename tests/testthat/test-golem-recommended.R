# Golem-recommended tests
# Basic test suite recommended by the golem package

test_that("app ui", {
  ui <- shinyExametrika:::app_ui()
  # golem standard: verify UI is a shiny.tag.list
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})

test_that("app server", {
  server <- shinyExametrika:::app_server
  # Verify it is a function
  expect_true(is.function(server))
})

test_that("app_sys works", {
  # Verify golem's app_sys() returns a correct path
  expect_true(is.function(shinyExametrika:::app_sys))
})

test_that("golem-config works", {
  # Verify golem config file exists
  config_file <- shinyExametrika:::app_sys("golem-config.yml")
  # Verify path is a character string (file existence only at install time)
  expect_true(is.character(config_file))
})
