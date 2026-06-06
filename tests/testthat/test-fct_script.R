test_that("script_block_sample builds a runnable sample-data block", {
  blk <- script_block_sample("J15S500")
  expect_equal(blk, c('data("J15S500", package = "exametrika")', "dat <- J15S500"))
})

test_that("script_block_upload wraps long column vectors and emits valid args", {
  cols <- c("ID", paste0("Item", sprintf("%02d", 1:15)))
  blk <- script_block_upload("my.csv", cols, has_id = TRUE, na_code = 99, resp_type = "ordinal")
  txt <- paste(blk, collapse = "\n")
  # read.csv path, id/na/response.type all present, parseable
  expect_match(txt, 'read\\.csv\\("my\\.csv"\\)')
  expect_match(txt, "id = 1")
  expect_match(txt, "na = 99")
  expect_match(txt, 'response.type = "ordinal"')
  expect_silent(parse(text = txt))
  # the column vector is wrapped across multiple lines (no single very long line)
  expect_true(max(nchar(blk)) < 90)
})

test_that("script_block_upload omits absent optional args", {
  blk <- script_block_upload("x.csv", c("Q1", "Q2"), has_id = FALSE, na_code = NULL, resp_type = NULL)
  txt <- paste(blk, collapse = "\n")
  expect_false(grepl("id =", txt))
  expect_false(grepl("na =", txt))
  expect_false(grepl("response.type", txt))
  expect_silent(parse(text = txt))
})

test_that("wrap_quoted_vec keeps lines under width and quotes items", {
  lines <- wrap_quoted_vec(paste0("Item", 1:20), indent = "    ", width = 40)
  expect_true(all(nchar(lines) <= 42))
  expect_true(all(grepl('^    "', lines)))
  # every line except the last ends with a comma
  expect_true(all(grepl(",$", lines[-length(lines)])))
  expect_false(grepl(",$", lines[length(lines)]))
})

test_that("assemble_script renders a timestamped, chronological journal", {
  log <- list(
    list(ts = "2026-06-06 14:20:01", label = "Load sample dataset: J15S500",
         code = script_block_sample("J15S500")),
    list(ts = "2026-06-06 14:21:33", label = "IRT (2PL)",
         code = c("fit_irt <- IRT(dat, model = 2)", "print(fit_irt)"))
  )
  txt <- assemble_script(log, date_str = "2026-06-06")
  expect_match(txt, "library\\(exametrika\\)")
  expect_match(txt, "\\[2026-06-06 14:20:01\\]")
  expect_match(txt, "\\[2026-06-06 14:21:33\\]  IRT \\(2PL\\)")
  # chronological order preserved (data section before IRT section)
  expect_lt(regexpr("14:20:01", txt), regexpr("14:21:33", txt))
  expect_silent(parse(text = txt))
})

test_that("assemble_script handles an empty log", {
  txt <- assemble_script(list(), date_str = "2026-06-06")
  expect_match(txt, "no steps recorded yet")
})

test_that("log_append is a no-op when script_log is NULL", {
  expect_silent(log_append(NULL, c("x <- 1"), label = "x"))
})
