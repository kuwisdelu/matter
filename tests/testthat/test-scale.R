require(testthat)
require(matter)

context("scale")

test_that("scale", {

	x <- matrix(1:100, nrow=10, ncol=10)

	y <- matter_mat(x, nrow=10, ncol=10)

	expect_equivalent(scale(x), scale(y)[])

	x <- t(x)

	y <- t(y)

	expect_equivalent(scale(x), scale(y)[])

	x <- matrix(1:91, nrow=7, ncol=13)

	y <- matter_mat(x, nrow=7, ncol=13)

	expect_equivalent(scale(x), scale(y)[])

	x <- t(x)

	y <- t(y)

	expect_equivalent(scale(x), scale(y)[])

})

