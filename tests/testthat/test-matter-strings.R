require(testthat)
require(matter)

context("matter-strings")

test_that("matter strings indexing", {

	x <- c("neon", "genesis", "evangelion")
	y <- matter2_chr(x)

	expect_equal(x, y[])
	expect_equal(x[1], y[1])

})

