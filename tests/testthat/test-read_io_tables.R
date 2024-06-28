test_that("Industry-industry-flow table is read correctly", {

  x <- read_industry_flow_table()

  expect_s3_class(x, class = "data.frame")
  expect_equal(nrow(x), 132)
  expect_equal(ncol(x), 125)
  expect_true(all(colnames(x) == io_cols))
})
