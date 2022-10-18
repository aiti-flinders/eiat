test_that("114 sector io table works", {
  x <- create_114_sector()
  expect_s3_class(x$flows, "data.frame")
  expect_equal(nrow(x$flows), 120)
  expect_equal(ncol(x$flows), 122)
  expect_equal(sum(x$flows[120, 2:122]), sum(x$flows[,122]))
})

test_that("19 sector io table works", {
  x <- create_19_sector()
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 27)
  expect_equal(ncol(x), 27)
  expect_equal(sum(x[25, 2:27]),sum(x[1:25, 27]))
})
