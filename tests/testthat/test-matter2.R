require(testthat)
require(matter)

context("matter-class")

test_that("matter array indexing", {

	x <- matter2_arr(0, dim=10)

	x[1:5]

})
 