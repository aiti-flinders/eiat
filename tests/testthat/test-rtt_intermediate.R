for (i in seq_along(lq_models)) {
  test_that("Intermediate inputs = Intermediate demand", {

    int_use <- lq_models[[i]]["Intermediate Inputs", c(1:19, 21:26)]
    int_demand <- lq_models[[i]][c(1:19, 21:25), "intermediate_demand"]

    expect_true(all.equal(sum(int_use), sum(int_demand)))
    expect_true(all.equal(sum(int_use)[1:19], sum(int_demand)[1:19]))

  })

}


# inputs_equal_outputs <- function(region) {
#
#
#   # Sum of production == sum of supply
#
#   t1 <- if (isTRUE(all.equal(sum(int_use), sum(int_demand)))) TRUE else FALSE
#
#   # Sum of production == Total supply
#
#   t2 <-  if (isTRUE(all.equal(sum(int_use), lq_models[[region]]["Australian Production", "Total Supply"]))) TRUE else FALSE
#
#   # Internal sum == Total supply
#
#   t3 <- if (isTRUE(all.equal(sum(lq_models[[region]][c(1:19, 21:25), c(1:19,21:26)]), lq_models[[region]]["Australian Production", "Total Supply"]))) TRUE else FALSE
#
#   # Total Production == Column sums
#
#   t4 <- if (isTRUE(all.equal(colSums(lq_models[[region]][-c(20,26:29), -c(20,27)]), lq_models[[region]]["Australian Production", -27]))) TRUE else FALSE
#
#   x <- t1 & t2 & t3 & t4
#
#   if (isTRUE(x)) x else i
# }
# expect_true(inputs_equal_outputs(i))
# })
#   }
