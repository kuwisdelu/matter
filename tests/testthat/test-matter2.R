require(testthat)
require(matter)

context("matter-class")

test_that("matter array indexing", {

	x <- matter2_arr(1:10, readonly=FALSE)

	.Call("C_setMatterArray", x, NULL, 1:10)

	.Call("C_getMatterArray", x, NULL)

})
