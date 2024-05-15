require(testthat)
require(matter)

context("signal-processing-2")

test_that("filter 2d", {

	set.seed(1, kind="default")
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

	x2c <- filt2_conv(x, w)

	expect_equal(x2, x2c)

	x3 <- filt2_bi(y, width=5)
	x4 <- filt2_adapt(y, width=5)
	x5 <- filt2_diff(y, niter=5)
	x6 <- filt2_guide(y, width=5)

	expect_lt(sum((x3 - x)^2), sum((y - x)^2))
	expect_lt(sum((x4 - x)^2), sum((y - x)^2))
	expect_lt(sum((x5 - x)^2), sum((y - x)^2))
	expect_lt(sum((x6 - x)^2), sum((y - x)^2))

	set.seed(1, kind="default")
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

test_that("warp + align 2d", {

	set.seed(1, kind="default")
	x <- matrix(0, nrow=32, ncol=32)
	x[9:24,9:24] <- 10
	x <- x + runif(length(x))

	y <- trans2d(x, rotate=15, translate=c(-5, 5))

	z1 <- warp2_trans(y, x, metric="cor")
	z2 <- warp2_trans(y, x, metric="mse")
	z3 <- warp2_trans(y, x, metric="mi")

	expect_gt(mi(z1, x), mi(y, x))
	expect_gt(mi(z2, x), mi(y, x))
	expect_gt(mi(z3, x), mi(y, x))

	x2 <- array(x, dim=c(dim(x), 3)) + rnorm(3 * length(x))
	y2 <- trans2d(x2, rotate=15, translate=c(-5, 5))
	
	z4 <- warp2_trans(y2, x2, metric="cor")

	expect_gt(mi(z4, x2), mi(y2, x2))

})

test_that("contrast enhancement", {

	set.seed(1, kind="default")
	x <- matrix(0, nrow=32, ncol=32)
	x[9:24,9:24] <- 10
	y <- x + rlnorm(length(x))

	z1 <- enhance_hist(y)
	z2 <- enhance_adapt(y)

	h <- tabulate(cut(y, breaks=256L))
	h1 <- tabulate(cut(z1, breaks=256L))
	h2 <- tabulate(cut(z2, breaks=256L))

	expect_equal(median(z1), median(y))
	expect_equal(median(z2), median(y))
	expect_equal(IQR(z1), IQR(y))
	expect_equal(IQR(z2), IQR(y))
	expect_gt(mean(which(h1 > 0)), mean(which(h > 0)))
	expect_gt(mean(which(h2 > 0)), mean(which(h > 0)))
	expect_lt(max(z1), max(y))
	expect_lt(max(z1), max(y))

})

test_that("rasterization", {

	set.seed(1, kind="default")
	i <- seq(-4, 4, length.out=12)
	j <- seq(1, 3, length.out=9)
	co <- expand.grid(x=i, y=j)
	z <- matrix(atan(co$x / co$y), nrow=12, ncol=9)
	z <- 10 * (z - min(z)) / diff(range(z))
	d1 <- expand.grid(x=1:12, y=1:9)
	d1$vals <- as.vector(z)

	expect_equal(c(x=12, y=9), estdim(d1[1:2]))
	expect_equal(z, to_raster(d1$x, d1$y, d1$vals))

	d2 <- d1
	d2$x <- jitter(d2$x)
	d2$y <- jitter(d2$y)

	expect_equal(c(x=12, y=9), estdim(d2[1:2]))
	expect_equal(z, to_raster(d2$x, d2$y, d2$vals))

	set.seed(1, kind="default")
	zn <- z
	rm <- rbinom(length(z), 1, 0.2)
	rm <- which(rm > 0)
	zn[rm] <- NA
	dn1 <- d1[-rm,,drop=FALSE]
	dn2 <- d2[-rm,,drop=FALSE]

	expect_equal(zn, to_raster(dn1$x, dn1$y, dn1$vals))

	za <- array(c(z, z, z), dim=c(x=12, y=9, z=3))
	d3 <- expand.grid(x=1:12, y=1:9, z=1:3)
	d3$vals <- as.vector(za)

	expect_equal(za, to_raster3(d3$x, d3$y, d3$z, d3$vals))

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

	set.seed(1, kind="default")
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
