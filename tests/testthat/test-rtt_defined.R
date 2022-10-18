for (i in seq_along(lq_models)) {
  test_that("Table is defined", {

    is_defined <- function(region) {
      out <- !is.na(sum(lq_models[[i]]))
      if (isTRUE(out)) out else i
    }

    expect_true(is_defined(i))
  })
}
