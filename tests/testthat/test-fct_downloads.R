test_that("write_report_xlsx writes one sheet per report in the given order", {
  skip_if_not_installed("openxlsx")
  sheets <- list(
    TestFit     = list(data = data.frame(Index = c("AIC", "BIC"), Value = c(1.2, 3.4)), rowNames = FALSE),
    ItemReport  = list(data = data.frame(slope = c(1, 2), location = c(0.1, 0.2),
                                         row.names = c("Item01", "Item02")), rowNames = TRUE),
    ScoreReport = list(data = data.frame(id = c("S1", "S2"), EAP = c(0.5, -0.5)), rowNames = FALSE)
  )
  f <- tempfile(fileext = ".xlsx")
  write_report_xlsx(f, sheets)
  expect_true(file.exists(f))
  expect_equal(openxlsx::getSheetNames(f), c("TestFit", "ItemReport", "ScoreReport"))

  # row names are written when requested (ItemReport keeps the item IDs)
  ir <- openxlsx::read.xlsx(f, sheet = "ItemReport", rowNames = TRUE)
  expect_equal(rownames(ir), c("Item01", "Item02"))
  sr <- openxlsx::read.xlsx(f, sheet = "ScoreReport")
  expect_equal(nrow(sr), 2L)
})

test_that("write_report_xlsx skips NULL sheet data and truncates long sheet names", {
  skip_if_not_installed("openxlsx")
  sheets <- list(
    Present = list(data = data.frame(a = 1), rowNames = FALSE),
    Missing = list(data = NULL, rowNames = FALSE),
    ThisSheetNameIsWayTooLongForExcelLimit = list(data = data.frame(b = 1), rowNames = FALSE)
  )
  f <- tempfile(fileext = ".xlsx")
  write_report_xlsx(f, sheets)
  nms <- openxlsx::getSheetNames(f)
  expect_true("Present" %in% nms)
  expect_false("Missing" %in% nms)
  expect_true(all(nchar(nms) <= 31))
})
