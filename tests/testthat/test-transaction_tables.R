for (i in seq_along(lq_basic)) {
  test_that("Total Supply = Total Production", {

    inputs_equal_outputs <- function(region) {
      int_use <- lq_basic[[region]]["Australian Production", 1:24]
      int_demand <- lq_basic[[region]][1:23, "Total Supply"]

      # Sum of production == sum of supply

      t1 <- all.equal(sum(int_use), sum(int_demand))

      # Sum of production == Total supply

      t2 <-  all.equal(sum(int_use), lq_basic[[region]]["Australian Production", "Total Supply"])

      # Internal sum == Total supply

      t3 <- all.equal(sum(lq_basic[[region]][1:23, 1:24]), lq_basic[[region]]["Australian Production", "Total Supply"])

      # Total Production == Column sums

      t4 <- all.equal(colSums(lq_basic[[region]][-24, -25]), lq_basic[[region]]["Australian Production", -25])

      x <- t1 & t2 & t3

      if (isTRUE(x)) x else i
    }
    expect_true(inputs_equal_outputs(i))
  })
}





# test_that("Total intermediate inputs = total intermediate demand", {
#   model_to_test <- function(region) {
#     int_use <- lq_models[[region]]["Intermediate Inputs", 1:19]
#     int_demand <- lq_models[[region]][1:19, "intermediate_demand"]
#
#     x <- sum(int_use) == sum(int_demand) & sum(int_use) == lq_models[[region]]["Intermediate Inputs", "intermediate_demand"]
#
#     if (isTRUE(x)) x else region
#   }
#   for (i in sample(get_available_regions(2021)[["lga"]], 100)) expect_true(model_to_test(i))
# })
#
# test_that("Industry-industry transactions are positive", {
#   test_fn <- function(region) {
#
#     x <- all(lq_models[[region]][1:19, 1:19] > 0)
#
#     if (isTRUE(x)) x else region
#     }
#
#   for (i in get_available_regions(2021)[["lga"]]) expect_true(test_fn(i))
#
# })
#
# test_that("Exports are non-zero", {
#   test_fn <- function(region) {
#
#     x <- all(lq_models[[region]][1:19, "Exports of Goods and Services"] > 0)
#     if (isTRUE(x)) x else region
#   }
#
#   for (i in get_available_regions(2021)[["lga"]]) expect_true(test_fn(i))
# })
