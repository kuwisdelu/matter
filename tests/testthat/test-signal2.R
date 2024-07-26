require(testthat)
require(matter)

context("signal-processing-2")

test_that("filter 2d", {

	set.seed(1, kind="default")
	i <- seq(-4, 4, length.out=12)
	j <- seq(1, 3, length.out=9)
	co <- expand.grid(i=i, j=j)
	y <- matrix(atan(co$i / co$j), nrow=12, ncol=9)
	y <- 10 * (y - min(y)) / diff(range(y))
	x <- y + 2.5 * runif(length(y))

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

	x3 <- filt2_bi(x, width=5)
	x4 <- filt2_adapt(x, width=5)
	x5 <- filt2_diff(x, niter=5)
	x6 <- filt2_guide(x, width=5)

	expect_lt(sum((x3 - y)^2), sum((x - y)^2))
	expect_lt(sum((x4 - y)^2), sum((x - y)^2))
	expect_lt(sum((x5 - y)^2), sum((x - y)^2))
	expect_lt(sum((x6 - y)^2), sum((x - y)^2))

	z <- array(rep.int(x, 3), dim=c(dim(x), 3))
	
	z1 <- filt2_ma(z, width=5)
	z2 <- filt2_gauss(z, width=5)
	z3 <- filt2_bi(z, width=5)
	z4 <- filt2_adapt(z, width=5)
	z5 <- filt2_diff(z, niter=5)
	z6 <- filt2_guide(z, width=5)
	z7 <- filt2_guide(z, width=5, guide=x)

	expect_equal(z1[,,1], x1)
	expect_equal(z2[,,1], x2)
	expect_equal(z3[,,1], x3)
	expect_equal(z4[,,1], x4)
	expect_equal(z5[,,1], x5)
	expect_equal(z6[,,1], x6)
	expect_equal(z7[,,1], x6)

	expect_equal(z1[,,2], x1)
	expect_equal(z2[,,2], x2)
	expect_equal(z3[,,2], x3)
	expect_equal(z4[,,2], x4)
	expect_equal(z5[,,2], x5)
	expect_equal(z6[,,2], x6)
	expect_equal(z7[,,2], x6)

	expect_equal(z1[,,3], x1)
	expect_equal(z2[,,3], x2)
	expect_equal(z3[,,3], x3)
	expect_equal(z4[,,3], x4)
	expect_equal(z5[,,3], x5)
	expect_equal(z6[,,3], x6)
	expect_equal(z7[,,3], x6)

	set.seed(1, kind="default")
	u <- diag(10)
	i <- upper.tri(u, TRUE)
	u[i] <- u[i] + 10
	v <- u + rnorm(length(u))
	
	v1 <- filt2_bi(v, width=5)
	v2 <- filt2_adapt(v, width=5)
	v3 <- filt2_guide(v, width=5, sdreg=1)

	expect_lt(median(abs(v1 - u)), median(abs(v - u)))
	expect_lt(median(abs(v2 - u)), median(abs(v - u)))
	expect_lt(median(abs(v3 - u)), median(abs(v - u)))

})

test_that("filter nd (2d)", {

	set.seed(1, kind="default")
	i <- seq(-4, 4, length.out=12)
	j <- seq(1, 3, length.out=9)
	co <- expand.grid(i=i, j=j)
	y <- matrix(atan(co$i / co$j), nrow=12, ncol=9)
	y <- 10 * (y - min(y)) / diff(range(y))
	x <- y + 2.5 * runif(length(y))

	x1a <- filtn_ma(x, k=9)
	x1b <- filt2_ma(x, width=3)

	expect_equal(x1a[3,3], x1b[3,3])
	expect_equal(x1a[3,7], x1b[3,7])
	expect_equal(x1a[10,3], x1b[10,3])
	expect_equal(x1a[10,7], x1b[10,7])

	x2a <- filtn_gauss(x, k=9, sd=1)
	x2b <- filt2_gauss(x, width=3, sd=1)

	expect_equal(x2a[3,3], x2b[3,3])
	expect_equal(x2a[3,7], x2b[3,7])
	expect_equal(x2a[10,3], x2b[10,3])
	expect_equal(x2a[10,7], x2b[10,7])

	x3a <- filtn_bi(x, k=9, sddist=1, sdrange=1)
	x3b <- filt2_bi(x, width=3, sddist=1, sdrange=1)

	expect_equal(x3a[3,3], x3b[3,3])
	expect_equal(x3a[3,7], x3b[3,7])
	expect_equal(x3a[10,3], x3b[10,3])
	expect_equal(x3a[10,7], x3b[10,7])

	x4a <- filtn_adapt(x, k=9, spar=1)
	x4b <- filt2_adapt(x, width=3, spar=1)

	expect_equal(x4a[3,3], x4b[3,3])
	expect_equal(x4a[3,7], x4b[3,7])
	expect_equal(x4a[10,3], x4b[10,3])
	expect_equal(x4a[10,7], x4b[10,7])

})

test_that("warp + align 2d", {

	set.seed(1, kind="default")
	x <- matrix(0, nrow=32, ncol=32)
	x[9:24,9:24] <- 10
	x <- x + runif(length(x))

	y <- trans2d(x, rotate=15, translate=c(-5, 5))

	x1 <- warp2_trans(y, x, metric="cor")
	x2 <- warp2_trans(y, x, metric="mse")
	x3 <- warp2_trans(y, x, metric="mi")

	expect_gt(mi(x1, x), mi(y, x))
	expect_gt(mi(x2, x), mi(y, x))
	expect_gt(mi(x3, x), mi(y, x))

	u <- array(x, dim=c(dim(x), 3)) + rnorm(3 * length(x))
	v <- trans2d(u, rotate=15, translate=c(-5, 5))
	
	u2 <- warp2_trans(v, u, metric="cor")

	expect_gt(mi(u2, u), mi(v, u))

})

test_that("contrast enhancement", {

	set.seed(1, kind="default")
	x <- matrix(0, nrow=32, ncol=32)
	x[9:24,9:24] <- 10
	y <- x + rlnorm(length(x))
	z <- array(rep.int(y, 3), dim=c(dim(y), 3))

	x1 <- enhance_adj(y)
	x2 <- enhance_hist(y)
	x3 <- enhance_adapt(y)

	expect_equal(median(x1), median(y))
	expect_equal(median(x2), median(y))
	expect_equal(median(x3), median(y))
	expect_equal(IQR(x1), IQR(y))
	expect_equal(IQR(x2), IQR(y))
	expect_equal(IQR(x3), IQR(y))

	z1 <- enhance_adj(z)
	z2 <- enhance_hist(z)
	z3 <- enhance_adapt(z)

	expect_equal(z1[,,1], x1)
	expect_equal(z2[,,1], x2)
	expect_equal(z3[,,1], x3)

	h <- tabulate(cut(y, breaks=256L))
	h2 <- tabulate(cut(x2, breaks=256L))
	h3 <- tabulate(cut(x3, breaks=256L))

	expect_gt(mean(which(h2 > 0)), mean(which(h > 0)))
	expect_gt(mean(which(h3 > 0)), mean(which(h > 0)))
	expect_lt(max(x2), max(y))
	expect_lt(max(x3), max(y))

})

test_that("knnmax", {

	y <- c(
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0,
		0, 0, 0, 2, 0, 0, 1, 4, 2, 0, 1, 1, 0, 0, 0,
		0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 3, 2, 1, 0, 0,
		0, 0, 0, 0, 1, 3, 3, 0, 0, 1, 4, 4, 3, 1, 0,
		0, 0, 0, 0, 0, 3, 2, 0, 1, 0, 3, 2, 3, 0, 0,
		0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 2, 2, 3, 0, 0)
	
	x <- matrix(y, nrow=7, ncol=15, byrow=TRUE)
	rownames(x) <- seq_len(nrow(x))
	colnames(x) <- seq_len(ncol(x))
	
	co <- expand.grid(t1=1:nrow(x), t2=1:ncol(x))

	m1 <- which(knnmax(x, co, k=5), arr.ind=TRUE)

	expect_equal(nrow(m1), 6)
	expect_equal(m1[,"row"], c(3, 5, 3, 6, 5, 2))
	expect_equal(m1[,"col"], c(4, 6, 8, 9, 11, 14))

	m2 <- which(knnmax(x, co, k=25), arr.ind=TRUE)

	expect_equal(nrow(m2), 2)
	expect_equal(m2[,"row"], c(3, 5))
	expect_equal(m2[,"col"], c(8, 11))

})

test_that("findpeaks (knn)", {

	y <- c(
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0,
		0, 0, 0, 2, 0, 0, 1, 4, 2, 0, 1, 1, 0, 0, 0,
		0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 3, 2, 1, 0, 0,
		0, 0, 0, 0, 1, 3, 3, 0, 0, 1, 4, 4, 3, 1, 0,
		0, 0, 0, 0, 0, 3, 2, 0, 1, 0, 3, 2, 3, 0, 0,
		0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 2, 2, 3, 0, 0)
	
	x <- matrix(y, nrow=7, ncol=15, byrow=TRUE)

	p1 <- findpeaks_knn(x, k=5)
	
	expect_equal(nrow(p1), 6)
	expect_equal(p1[,"row"], c(3, 5, 3, 6, 5, 2))
	expect_equal(p1[,"col"], c(4, 6, 8, 9, 11, 14))

	expect_equal(attr(p1, "relheight"), x[p1] / max(x))

	ind <- matrix(seq_along(x), nrow=nrow(x), ncol=ncol(x))

	co <- expand.grid(t1=1:nrow(x), t2=1:ncol(x))
	p2 <- findpeaks_knn(as.vector(x), co, k=5)

	expect_equal(p2[1L], ind[3, 4])
	expect_equal(p2[2L], ind[5, 6])
	expect_equal(p2[3L], ind[3, 8])
	expect_equal(p2[4L], ind[6, 9])
	expect_equal(p2[5L], ind[5, 11])
	expect_equal(p2[6L], ind[2, 14])

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
