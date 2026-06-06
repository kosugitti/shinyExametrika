# Tests for the column-selection (ID + analysis variables) wiring in the
# data-upload module.

du_i18n <- function() {
  tr <- shiny.i18n::Translator$new(
    translation_json_path = app_sys("i18n/translation.json")
  )
  tr$set_translation_language("en")
  tr
}

make_two_id_df <- function() {
  set.seed(42)
  df <- data.frame(
    ID = sprintf("s%02d", 1:20),
    GID = sample(c("A", "B", "C"), 20, replace = TRUE),
    stringsAsFactors = FALSE
  )
  for (j in 1:6) df[[paste0("Q", j)]] <- sample(0:1, 20, replace = TRUE)
  df
}

test_that("excluding the extra GID column yields binary (not nominal) data", {
  shiny::testServer(mod_data_upload_server, args = list(i18n = du_i18n()), {
    raw_data(make_two_id_df())
    session$setInputs(
      id_column = "ID",
      item_columns = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6"),
      na_code = "",
      response_type = "auto",
      btn_format = 1
    )
    fd <- formatted_data()
    expect_false(is.null(fd))
    expect_equal(fd$response.type, "binary")
    mat <- if (!is.null(fd$U)) fd$U else fd$Q
    expect_equal(ncol(mat), 6)
  })
})

test_that("including GID as an analysis variable degrades to nominal (the original bug)", {
  shiny::testServer(mod_data_upload_server, args = list(i18n = du_i18n()), {
    raw_data(make_two_id_df())
    # exametrika warns when it meets the character GID column; that warning is
    # exactly the symptom we are documenting, so suppress it to keep test output
    # clean.
    suppressWarnings(
      session$setInputs(
        id_column = "ID",
        item_columns = c("GID", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6"),
        na_code = "",
        response_type = "auto",
        btn_format = 1
      )
    )
    fd <- formatted_data()
    expect_equal(fd$response.type, "nominal")
  })
})

test_that("selecting a sample dataset loads it (env[[]], not a maskable get())", {
  shiny::testServer(mod_data_upload_server, args = list(i18n = du_i18n()), {
    session$setInputs(data_source = "sample", sample_data = "J15S500")
    fd <- formatted_data()
    expect_false(is.null(fd))
    expect_equal(fd$response.type, "binary")
    mat <- if (!is.null(fd$U)) fd$U else fd$Q
    expect_equal(dim(mat), c(500L, 15L))
    expect_equal(session$returned$name(), "J15S500")
  })
})

test_that("the module returns reactives for data and dataset name", {
  shiny::testServer(mod_data_upload_server, args = list(i18n = du_i18n()), {
    expect_true(is.function(session$returned$data))
    expect_true(is.function(session$returned$name))
    # Nothing loaded yet
    expect_null(session$returned$data())
    expect_null(session$returned$name())
    # The name reactive tracks the loaded dataset id
    dataset_name("J15S500")
    expect_equal(session$returned$name(), "J15S500")
  })
})

test_that("selecting no analysis variable does not produce a formatted object", {
  shiny::testServer(mod_data_upload_server, args = list(i18n = du_i18n()), {
    raw_data(make_two_id_df())
    session$setInputs(
      id_column = "ID",
      item_columns = character(0),
      na_code = "",
      response_type = "auto",
      btn_format = 1
    )
    expect_null(formatted_data())
  })
})
