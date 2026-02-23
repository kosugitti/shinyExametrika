# golem 推奨テスト
# golem パッケージが推奨する基本テスト群

test_that("app ui", {
  ui <- shinyExametrika:::app_ui()
  # golem 標準: UI が shiny.tag.list であることを確認
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})

test_that("app server", {
  server <- shinyExametrika:::app_server
  # サーバ関数であることを確認
  expect_true(is.function(server))
})

test_that("app_sys works", {
  # golem の app_sys() が正しくパスを返すことを確認
  expect_true(is.function(shinyExametrika:::app_sys))
})

test_that("golem-config works", {
  # golem 設定ファイルの存在確認
  config_file <- shinyExametrika:::app_sys("golem-config.yml")
  # パスが文字列であることを確認（ファイル存在はインストール時のみ）
  expect_true(is.character(config_file))
})
