require(testthat)
require(matter)

context("signal-processing-2")

test_that("filter2", {

	x <- matrix(1, nrow=7, ncol=11)
	x[3:7,1:5] <- x[3:7,1:5] + 2 * diag(5)
	x[2:7,1:6] <- x[2:7,1:6] + diag(6)
	x[1:7,1:7] <- x[1:7,1:7] + 2 * diag(7)
	x[1:7,2:8] <- x[1:7,2:8] + 3 * diag(7)
	x[1:7,3:9] <- x[1:7,3:9] + diag(7)
	x[1:7,5:11] <- x[1:7,5:11] + 3 * diag(7)
	x[1:6,6:11] <- x[1:6,6:11] + 2 * diag(6)

	w <- dnorm((-2):2) %o% dnorm((-2):2)
	w <- w / sum(w)
	
	x1 <- filt2_ma(x, width=5)
	x2 <- filt2_gauss(x, width=5)

	expect_equal(mean(x[1:5,1:5]), x1[3,3])
	expect_equal(mean(x[1:5,7:11]), x1[3,9])
	expect_equal(mean(x[3:7,1:5]), x1[5,3])
	expect_equal(mean(x[3:7,7:11]), x1[5,9])

	expect_equal(sum(w * x[1:5,1:5]), x2[3,3])
	expect_equal(sum(w * x[1:5,7:11]), x2[3,9])
	expect_equal(sum(w * x[3:7,1:5]), x2[5,3])
	expect_equal(sum(w * x[3:7,7:11]), x2[5,9])

	set.seed(1)
	y <- diag(10)
	i <- upper.tri(y, TRUE)
	y[i] <- y[i] + 10
	z <- y + runif(length(y))
	
	x3 <- filt2_bi(z, width=5)
	x4 <- filt2_adapt(z, width=5, spar=20)

	expect_lt(sum((x3 - y)^2), sum((z - y)^2))
	expect_lt(sum((x4 - y)^2), sum((z - y)^2))

})

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
