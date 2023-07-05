require(testthat)
require(matter)

context("signal-processing-2")

test_that("filter2", {

	set.seed(1)
	i <- seq(-4, 4, length.out=12)
	j <- seq(1, 3, length.out=9)
	co <- expand.grid(i=i, j=j)
	x <- matrix(atan(co$i / co$j), nrow=12, ncol=9)
	x <- 10 * (x - min(x)) / diff(range(x))
	y <- x + 2.5 * runif(length(x))

	w <- dnorm((-2):2) %o% dnorm((-2):2)
	w <- w / sum(w)
	
	x1 <- filt2_ma(x, width=5)
	x2 <- filt2_gauss(x, width=5)

	expect_equal(mean(x[1:5,1:5]), x1[3,3])
	expect_equal(mean(x[1:5,5:9]), x1[3,7])
	expect_equal(mean(x[8:12,1:5]), x1[10,3])
	expect_equal(mean(x[8:12,5:9]), x1[10,7])

	expect_equal(sum(w * x[1:5,1:5]), x2[3,3])
	expect_equal(sum(w * x[1:5,5:9]), x2[3,7])
	expect_equal(sum(w * x[8:12,1:5]), x2[10,3])
	expect_equal(sum(w * x[8:12,5:9]), x2[10,7])

	x3 <- filt2_bi(y, width=5)
	x4 <- filt2_adapt(y, width=5)
	x5 <- filt2_diff(y, niter=5)
	x6 <- filt2_guide(y, width=5)

	expect_lt(sum((x3 - x)^2), sum((y - x)^2))
	expect_lt(sum((x4 - x)^2), sum((y - x)^2))
	expect_lt(sum((x5 - x)^2), sum((y - x)^2))
	expect_lt(sum((x6 - x)^2), sum((y - x)^2))

	set.seed(1)
	u <- diag(10)
	i <- upper.tri(u, TRUE)
	u[i] <- u[i] + 10
	v <- u + rnorm(length(u))
	
	v1 <- filt2_bi(v, width=5)
	v2 <- filt2_adapt(v, width=5)
	v3 <- filt2_guide(v, width=5, sdreg=1)

	expect_lt(sum((v1 - u)^2), sum((v - u)^2))
	expect_lt(sum((v2 - u)^2), sum((v - u)^2))
	expect_lt(sum((v3 - u)^2), sum((v - u)^2))

})

test_that("warp2 + align", {

	set.seed(1)
	x <- matrix(0, nrow=32, ncol=32)
	x[9:24,9:24] <- 1
	x <- x + 0.5 * runif(length(x))

	y <- trans2d(x, rotate=15, translate=c(-5, -5))

	z1 <- warp2_trans(y, x, metric="mi")
	z2 <- warp2_trans(y, x, metric="mse")
	z3 <- warp2_trans(y, x, metric="cor")

	expect_gt(mi(z1, x), mi(y, x))
	expect_gt(mi(z2, x), mi(y, x))
	expect_gt(mi(z3, x), mi(y, x))

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
