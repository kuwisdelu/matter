require(testthat)
require(matter)

context("matter-class")

test_that("matter array indexing", {

	x <- matter2_arr(1:10, readonly=FALSE)

	set_matter_arr_elts(x, 1:10)

})
