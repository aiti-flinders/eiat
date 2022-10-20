years <- 2022:2031
impacts <- matrix(1, nrow = 19, ncol = length(years), byrow = T, dimnames = list(eiat:::anzsic_swap$letter, years))

for (i in seq_along(lq_models)) {
  test_that("impact analysis works (10 years year)", {

    is_impact <- function(region) {
      out <- impact_analysis(region, impacts)

      x <- !is.na(sum(out$output$value))
      y <- !is.na(sum(out$emp$value))
      z <- !is.na(sum(out$grp$value))

      t <- (x & y) & z

      if (isTRUE(t)) t else i
    }


    expect_true(is_impact(i))
  })
}
