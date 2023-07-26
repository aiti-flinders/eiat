years <- 2023
impacts <- matrix(1, nrow = 19, ncol = length(years), byrow = T, dimnames = list(eiat:::anzsic_swap$letter, years))

for (i in seq_along(lq_models)) {
  test_that("impact analysis works (one year)", {

    is_impact <- function(region) {
      out <- impact_analysis(region, impacts)

      # Check that there's nothing missing. We can also guess roughly how big the impact should be for a given $1 million to all industries input.
      # For output, the direct output should equal the input and the total output should be greater than the input
      x <- !is.na(sum(out$output$value)) & sum(out$output[out$output$type == "Direct Output", "value"]) == 19 & sum(out$output[out$output$type == "Total Output", "value"]) > 19
      y <- !is.na(sum(out$emp$value)) & sum(out$emp[out$emp$type == "Direct Employment", "value"]) > 0 & sum(out$emp[out$emp$type == "Total Employment", "value"]) > 0
      z <- !is.na(sum(out$grp$value)) & sum(out$grp[out$grp$type == "Direct GRP","value"]) > 0 & sum(out$grp[out$grp$type == "Total GRP","value"]) > 0

      t <- (x & y) & z

      if (isTRUE(t)) t else i
    }


    expect_true(is_impact(i))
  })
}
