require(testthat)
require(matter)

context("signal-processing-2")

test_that("approx2", {

	x <- matrix(1:25, nrow=5, ncol=5)
	x1 <- approx2(x, xout=3, yout=3, tol=1, interp="none")
	x2 <- approx2(x, xout=3, yout=3, tol=1, interp="mean")
	x3 <- approx2(x, xout=3, yout=3, tol=1, interp="sum")
	x4 <- approx2(x, xout=3, yout=3, tol=1, interp="max")
	x5 <- approx2(x, xout=3, yout=3, tol=1, interp="min")

	expect_equivalent(x[3,3], x1)
	expect_equivalent(mean(x[2:4,2:4]), x2)
	expect_equivalent(sum(x[2:4,2:4]), x3)
	expect_equivalent(max(x[2:4,2:4]), x4)
	expect_equivalent(min(x[2:4,2:4]), x5)
	expect_equal(x, approx2(x, interp="none"))
	expect_equal(x, approx2(x, tol=1, interp="linear"))
	expect_equal(x, approx2(x, tol=2, interp="cubic"))

	set.seed(1)
	y <- matrix(rnorm(25), nrow=5, ncol=5)
	y1 <- approx2(y, xout=3, yout=3, tol=1, interp="none")
	y2 <- approx2(y, xout=3, yout=3, tol=1, interp="mean")
	y3 <- approx2(y, xout=3, yout=3, tol=1, interp="sum")
	y4 <- approx2(y, xout=3, yout=3, tol=1, interp="max")
	y5 <- approx2(y, xout=3, yout=3, tol=1, interp="min")

	expect_equivalent(y[3,3], y1)
	expect_equivalent(mean(y[2:4,2:4]), y2)
	expect_equivalent(sum(y[2:4,2:4]), y3)
	expect_equivalent(max(y[2:4,2:4]), y4)
	expect_equivalent(min(y[2:4,2:4]), y5)
	expect_equal(y, approx2(y, interp="none"))
	expect_equal(y, approx2(y, tol=1, interp="linear"))
	expect_equal(y, approx2(y, tol=2, interp="cubic"))

})
