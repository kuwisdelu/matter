require(testthat)
require(matter)

context("matter-factor")

test_that("matter factor indexing", {

	x <- factor(c("A", "A", "B", "B", "A", "C", "A"))
	y <- matter_fct(x)

	expect_equal(x, y[])
	expect_equal(x, as.factor(y))
	expect_equal(x[1], y[1])
	expect_equal(x[1:7], y[1:7])
	expect_equal(x[7:1], y[7:1])

	x[c(1,7)] <- "C"
	y[c(1,7)] <- "C"

	expect_equal(x, y[])
	expect_equal(x, as.factor(y))
	expect_equal(x[1], y[1])
	expect_equal(x[1:7], y[1:7])
	expect_equal(x[7:1], y[7:1])

})

