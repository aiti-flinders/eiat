for (i in seq_along(lq_models)) {
  test_that("Exports are non-zero", {

    is_positive_exports <- function(region) {
      out <- all(round(lq_models[[i]][, "Exports of Goods and Services"], 6) >= 0)

      if (isTRUE(out)) out else i
    }

    expect_true(is_positive_exports(i))

  })

}

