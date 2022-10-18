test_that("impact analysis works", {
  region <- "Adelaide"
  years <- 2022:2023
  impacts <- matrix(c(1,2), nrow = 19, ncol = length(years), byrow = T, dimnames = list(eiat:::anzsic_swap$letter, years))
  expect_type(impact_analysis(region, years, impacts), "list")
})
