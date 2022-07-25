require(testthat)
require(matter)

context("sparse-classes")

test_that("sparse vector subsetting", {

	set.seed(1)

	x <- rbinom(100, 1, 0.2)

	y <- sparse_vec(x)

	expect_equal(x, y[])

	expect_equal(x[1], y[1])

	expect_equal(x[1:10], y[1:10])

	expect_equal(x[10:1], y[10:1])

	# expect_equivalent(as.matter(x), y)

})
