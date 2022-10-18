for (i in seq_along(lq_models)) {
  test_that("Sector-sector transactions are positive", {

    is_positive <- function(region) {
      out <- all(lq_models[[i]][1:19, 1:19] >= 0)

      if (isTRUE(out)) out else i
    }

    capture_output_lines(expect_true(is_positive(i)))

  })

}

