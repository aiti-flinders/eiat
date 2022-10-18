for (i in seq_along(lq_models)) {
  test_that("Total production = total supply", {

    total_production <- lq_models[[i]]["Australian Production", 1:19]
    total_supply <- lq_models[[i]][1:19, "Total Supply"]

    expect_true(all.equal(total_production, total_supply))
  })

}
