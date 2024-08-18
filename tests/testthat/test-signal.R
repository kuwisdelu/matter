require(testthat)
require(matter)

context("signal-processing")

test_that("filter 1d", {

	set.seed(1, kind="default")
	t <- seq(from=0, to=6 * pi, length.out=5000)
	y <- sin(t) + 0.6 * sin(2.6 * t)
	x <- y + runif(length(y))

	w <- 5L
	r <- w %/% 2

	x0 <- rep.int(x[1L], r)
	xn <- rep.int(x[length(x)], r)
	xr <- c(x0, x, xn)
	xr <- roll(xr, w)[(r + 1):(length(xr) - r)]
	xm <- vapply(xr, mean, numeric(1), na.rm=TRUE)

	wt <- dnorm((-r):r)
	wt <- wt / sum(wt)
	f <- function(z) sum(wt * z)
	xg <- vapply(xr, f, numeric(1))
	
	x1 <- filt1_ma(x, w)
	x2 <- filt1_gauss(x, w)
	x2c <- filt1_conv(x, wt)

	expect_equal(x1, xm)
	expect_equal(x2, xg)
	expect_equal(x2c, xg)

	x3 <- filt1_bi(x, w)
	x4 <- filt1_adapt(x, w)
	x5 <- filt1_diff(x, w)
	x6 <- filt1_guide(x, w)
	x7 <- filt1_pag(x, w)
	x8 <- filt1_sg(x, w)

	expect_lt(sum((x3 - y)^2), sum((x - y)^2))
	expect_lt(sum((x4 - y)^2), sum((x - y)^2))
	expect_lt(sum((x5 - y)^2), sum((x - y)^2))
	expect_lt(sum((x6 - y)^2), sum((x - y)^2))
	expect_lt(sum((x7 - y)^2), sum((x - y)^2))
	expect_lt(sum((x8 - y)^2), sum((x - y)^2))
	
	expect_gt(cor(x3, y), cor(x, y))
	expect_gt(cor(x4, y), cor(x, y))
	expect_gt(cor(x5, y), cor(x, y))
	expect_gt(cor(x6, y), cor(x, y))
	expect_gt(cor(x7, y), cor(x, y))
	expect_gt(cor(x8, y), cor(x, y))

	i <- roll(seq_along(x), width=5)
	wts <- rep_len(list(rep.int(1/5, 5)), length(i))
	x9 <- convolve_at(x, i, wts)
	x10 <- convolve_at(x, i, wt)
	
	expect_equal(x1[3:4998], x9[3:4998])
	expect_equal(x2[3:4998], x10[3:4998])

	set.seed(1, kind="default")
	z <- matrix(rlnorm(5000), nrow=100, ncol=50)
	i1 <- roll(seq_len(nrow(z)), width=5)
	i2 <- roll(seq_len(ncol(z)), width=5)
	w1 <- rep_len(list(rep.int(1/5, 5)), length(i1))
	w2 <- rep_len(list(rep.int(1/5, 5)), length(i2))

	FUN1 <- function(i, w, ...) colSums(w * z[i,,drop=FALSE], ...)
	FUN2 <- function(i, w, ...) colSums(w * t(z[,i,drop=FALSE]), ...)
	z1 <- t(mapply(FUN1, i1, w1))
	z2 <- mapply(FUN2, i2, w2)

	expect_equal(convolve_at(z, i1, w1, margin=1), z1)
	expect_equal(convolve_at(z, i2, w2, margin=2), z2)

})

test_that("filter nd (1d)", {

	set.seed(1, kind="default")
	t <- seq(from=0, to=6 * pi, length.out=200)
	y <- sin(t) + 0.6 * sin(2.6 * t)
	x <- y + runif(length(y))

	x1a <- filtn_ma(x, k=5)
	x1b <- filt1_ma(x, width=5)

	expect_equal(
		x1a[3L:(length(x)-3L)],
		x1b[3L:(length(x)-3L)])

	x2a <- filtn_gauss(x, k=5, sd=1)
	x2b <- filt1_gauss(x, width=5, sd=1)

	expect_equal(
		x2a[3L:(length(x)-3L)],
		x2b[3L:(length(x)-3L)])

	x3a <- filtn_bi(x, k=5, sddist=1, sdrange=1)
	x3b <- filt1_bi(x, width=5, sddist=1, sdrange=1)

	expect_equal(
		x3a[3L:(length(x)-3L)],
		x3b[3L:(length(x)-3L)])

	x4a <- filtn_adapt(x, k=5, spar=1)
	x4b <- filt1_adapt(x, width=5, spar=1)

	expect_equal(
		x4a[3L:(length(x)-3L)],
		x4b[3L:(length(x)-3L)])

})

test_that("warp + align 1d", {

	t <- seq(from=0, to=6 * pi, length.out=1000)
	dt <- 0.2 * (sin(t) + 0.6 * sin(2.6 * t))
	x <- sin(t) + 0.6 * sin(2.6 * t)
	y <- sin(t + dt) + 0.6 * sin(2.6 * (t + dt))
	z <- sin(t - dt) + 0.6 * sin(2.6 * (t - dt))

	px <- which(locmax(x))
	i <- seq_along(px)

	y2 <- warp1_loc(y, x)
	z2 <- warp1_loc(z, x)
	py2 <- which(locmax(y2))
	pz2 <- which(locmax(z2))

	expect_equivalent(py2[i], px, tolerance=1)
	expect_equivalent(pz2[i], px, tolerance=1)

	y3 <- warp1_dtw(y, x)
	z3 <- warp1_dtw(z, x)
	py3 <- which(locmax(y3))
	pz3 <- which(locmax(z3))

	y4 <- warp1_dtw(y, x, tol=Inf)
	z4 <- warp1_dtw(z, x, tol=Inf)
	py4 <- which(locmax(y4))
	pz4 <- which(locmax(z4))

	expect_equivalent(py3[i], px, tolerance=1)
	expect_equivalent(pz3[i], px, tolerance=1)
	expect_equivalent(py4[i], px, tolerance=1)
	expect_equivalent(pz4[i], px, tolerance=1)

	set.seed(1, kind="default")
	x <- 1:17
	y <- 1:18 + runif(18)
	rxy <- cor(approx(seq_along(x), x, n=length(y))$y, y)
	ryx <- cor(approx(seq_along(y), y, n=length(x))$y, x)
	
	expect_equal(icor(x, y), rxy)
	expect_equal(icor(y, x), ryx)

})

test_that("binvec + rollvec", {

	set.seed(1, kind="default")
	x <- runif(50)
	l <- seq(from=1, to=50, by=10)
	u <- seq(from=10, to=50, by=10)
	binfun <- function(x, l, u, fun, ...) {
		mapply(function(i, j) fun(x[i:j], ...), l, u)
	}
	rollfun <- function(x, width, fun, ...) {
		sapply(roll(x, width, na.drop=TRUE), fun, ...)
	}

	expect_equal(binfun(x, l, u, sum), binvec(x, l, u, "sum"))
	expect_equal(binfun(x, l, u, mean), binvec(x, l, u, "mean"))
	expect_equal(binfun(x, l, u, max), binvec(x, l, u, "max"))
	expect_equal(binfun(x, l, u, min), binvec(x, l, u, "min"))
	expect_equal(binfun(x, l, u, sd), binvec(x, l, u, "sd"))
	expect_equal(binfun(x, l, u, var), binvec(x, l, u, "var"))
	expect_equal(binfun(x, l, u, mad), binvec(x, l, u, "mad"))
	
	expect_equivalent(
		binfun(x, l, u, quantile, probs=0, type=3),
		binvec(x, l, u, "quantile", prob=0))
	expect_equivalent(
		binfun(x, l, u, quantile, probs=1/3, type=3),
		binvec(x, l, u, "quantile", prob=1/3))
	expect_equivalent(
		binfun(x, l, u, quantile, probs=2/3, type=3),
		binvec(x, l, u, "quantile", prob=2/3))
	expect_equivalent(
		binfun(x, l, u, quantile, probs=1, type=3),
		binvec(x, l, u, "quantile", prob=1))

	expect_equal(binvec(x, l, u), binvec(x, lower=l))
	expect_equal(binvec(x, l, u), binvec(x, upper=u))

	expect_equal(rollfun(x, 7L, sum), rollvec(x, 7L, "sum"))
	expect_equal(rollfun(x, 7L, mean), rollvec(x, 7L, "mean"))
	expect_equal(rollfun(x, 7L, max), rollvec(x, 7L, "max"))
	expect_equal(rollfun(x, 7L, min), rollvec(x, 7L, "min"))
	expect_equal(rollfun(x, 7L, sd), rollvec(x, 7L, "sd"))
	expect_equal(rollfun(x, 7L, var), rollvec(x, 7L, "var"))
	expect_equal(rollfun(x, 7L, mad), rollvec(x, 7L, "mad"))

	expect_equivalent(
		rollfun(x, 11L, quantile, probs=0, type=3),
		rollvec(x, 11L, "quantile", prob=0))
	expect_equivalent(
		rollfun(x, 11L, quantile, probs=1/3, type=3),
		rollvec(x, 11L, "quantile", prob=1/3))
	expect_equivalent(
		rollfun(x, 11L, quantile, probs=2/3, type=3),
		rollvec(x, 11L, "quantile", prob=2/3))
	expect_equivalent(
		rollfun(x, 11L, quantile, probs=1, type=3),
		rollvec(x, 11L, "quantile", prob=1))

})

test_that("rescale", {

	set.seed(1, kind="default")
	x <- rnorm(1000)
	i <- 1L

	x2 <- rescale_rms(x, 1)
	x3 <- rescale_sum(x, 1)
	x4 <- rescale_ref(x, ref=i, scale=1)
	x5 <- rescale_range(x, c(0, 1))
	x6 <- rescale_iqr(x, 1)
	
	expect_equal(sqrt(mean(x2^2)), 1)
	expect_equal(sum(abs(x3)), 1)
	expect_equal(x4[i], 1)
	expect_equal(range(x5), c(0, 1))
	expect_equal(IQR(x6), 1)

})

test_that("downsample", {

	set.seed(1, kind="default")
	n <- 200
	t <- seq(from=0, to=6 * pi, length.out=5000)
	x <- sin(t) + 0.6 * sin(2.6 * t)
	x <- x + runif(length(x))

	x2 <- downsample(x, n, method="lttb")
	
	expect_length(x2, n)
	expect_equal(x[1L], x2[1L])
	expect_equal(x[length(x)], x2[length(x2)])
	expect_true(all(x2 %in% x))

	x3 <- downsample(x, n, method="ltob")
	
	expect_length(x3, n)
	expect_equal(x[1L], x3[1L])
	expect_equal(x[length(x)], x3[length(x3)])
	expect_true(all(x3 %in% x))

	x4 <- downsample(x, n, method="dynamic")
	
	expect_length(x4, n)
	expect_equal(x[1L], x4[1L])
	expect_equal(x[length(x)], x4[length(x4)])
	expect_true(all(x4 %in% x))

})

test_that("locmax", {

	x <- c(0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 2, 2, 3, 0, 1)
	names(x) <- seq_along(x)
	m1 <- which(locmax(x))

	expect_equal(m1, c(4, 7, 13))

	x[11] <- 3
	m2 <- which(locmax(x))

	expect_equal(m2, c(4, 7, 11))

	x <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
	m3 <- which(locmax(x))

	expect_equal(m3, 5)

})

test_that("findpeaks", {

	x <- c(0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 2, 2, 3, 0, 1)
	names(x) <- seq_along(x)
	p1 <- findpeaks(x)

	expect_equivalent(p1, c(4, 7, 13))
	expect_equal(attr(p1, "left_bounds"), c(3, 6, 10))
	expect_equal(attr(p1, "right_bounds"), c(5, 10, 14))

	x[11] <- 3
	p2 <- findpeaks(x)

	expect_equivalent(p2, c(4, 7, 11))
	expect_equal(attr(p2, "left_bounds"), c(3, 6, 10))
	expect_equal(attr(p2, "right_bounds"), c(5, 10, 14))

	x <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)

	p3 <- findpeaks(x)

	expect_equivalent(p3, 5)
	expect_equal(attr(p3, "left_bounds"), 4)
	expect_equal(attr(p3, "right_bounds"), 6)

	x <- c(0, 1, 0, 3, 1, 3, 0, 4, 0)
	names(x) <- seq_along(x)
	p4 <- findpeaks(x, prominence=TRUE)

	expect_equivalent(p4, c(4, 8))
	expect_equal(attr(p4, "left_bases"), c(3, 7))
	expect_equal(attr(p4, "right_bases"), c(7, 9))
	expect_equal(attr(p4, "prominence"), c(3, 4))

	t <- seq(from=0, to=6 * pi, length.out=1000)
	x <- sin(t) + 0.6 * sin(2.6 * t)
	p5 <- findpeaks(x, prominence=TRUE, relheight=NULL)

	ref <- c(1.2415949, 0.4784017, 0.2847052, 3.1071679,
		0.2846030, 0.4782249, 2.4834026, 0.4782249)

	expect_equivalent(attr(p5, "prominences"), ref, tolerance=1e-3)

})

test_that("binpeaks + mergepeaks", {

	t <- seq(from=0, to=6 * pi, length.out=1000)
	x1 <- sin(t) + 0.6 * sin(2.6 * t)
	x2 <- sin(t + 0.02) + 0.6 * sin(2.6 * (t + 0.02))
	x3 <- sin(t - 0.02) + 0.6 * sin(2.6 * (t - 0.02))
	p1 <- findpeaks(x1)
	p2 <- findpeaks(x2)
	p3 <- findpeaks(x3)

	pb <- binpeaks(list(p1, p2, p3), tol=15, merge=FALSE)
	pm <- mergepeaks(sort(c(p1, p2, p3)), tol=15,
		n=rep.int(1, 3 * length(p1)))

	expect_equivalent(unclass(pb), p1, tolerance=0.5)
	expect_equivalent(unclass(pm), p1, tolerance=0.5)

	p <- c(1, 1.11, 1.2, 1.3, 1.4, 2, 2.1, 2.22, 2.3, 2.4)
	n <- c(1, 1, 4, 3, 1, 1, 2, 5, 6, 4)
	pm2 <- mergepeaks(p, n, tol=0.5)
	
	pwm1 <- sum((n * p)[1:5]) / sum(n[1:5])
	pwm2 <- sum((n * p)[6:10]) / sum(n[6:10])

	expect_equal(as.numeric(pm2), c(pwm1, pwm2))

})

test_that("peakwidths", {

	t <- seq(from=0, to=6 * pi, length.out=1000)
	x <- sin(t) + 0.6 * sin(2.6 * t)
	p <- findpeaks(x, relheight=NULL)
	
	w <- peakwidths(x, p, ref="prominence")

	ref <- c(64.25172825, 41.29465463, 35.46943289, 104.71586081,
        35.46729324, 41.30429622, 181.93835853, 45.37078546)

	expect_equivalent(w, ref, tolerance=1e-3)

	t2 <- seq(from=-4, to=4, length.out=1000)
	x2 <- dnorm(t2)
	p2 <- findpeaks(x2)

	w2 <- peakwidths(x2, p2, domain=t2, ref="height")

	expect_equivalent(w2, 2 * sqrt(2 * log(2)), tolerance=1e-3)

})

test_that("peakareas", {

	t <- seq(from=-4, to=4, length.out=1000)
	x <- dnorm(t)
	p <- findpeaks(x)

	a <- peakareas(x, p, domain=t)

	expect_equivalent(a, 1, tolerance=1e-3)

})

test_that("peakheights", {

	t <- seq(from=-4, to=4, length.out=1000)
	x <- dnorm(t)
	p <- findpeaks(x)

	h <- peakheights(x, p)

	expect_equivalent(h, max(x), tolerance=1e-3)

})

test_that("estres", {

	x <- 1:100
	y <- seq(1001L, 1100L, by=3L)
	z <- seq_rel(501, 600, 1e-3)
	u <- runif(100)

	expect_equal(estres(x), c(absolute=1L))
	expect_equal(estres(y), c(absolute=3L))
	expect_equal(estres(z), c(relative=1e-3))
	expect_equal(estres(u), c(absolute=min(diff(sort(u)))))

	expect_equal(estres(x, tol=1e-6), c(absolute=1L))
	expect_equal(estres(y, tol=1e-6), c(absolute=3L))
	expect_equal(estres(z, tol=1e-6), c(relative=1e-3))
	expect_equal(estres(u, tol=1e-6), c(absolute=NA_real_))

	p <- expand.grid(x=1:10, y=1:10)
	q <- expand.grid(
		x=seq(1L, 28L, by=3L),
		y=seq(1L, 28L, by=3L))
	r <- jitter(as.matrix(p))

	expect_equal(estres(p[,1L], tol=1e-6), c(absolute=1L))
	expect_equal(estres(p[,2L], tol=1e-6), c(absolute=1L))
	expect_equal(estres(q[,1L], tol=1e-6), c(absolute=3L))
	expect_equal(estres(q[,2L], tol=1e-6), c(absolute=3L))
	expect_equal(estres(r[,1L], tol=1e-6), c(absolute=NA_real_))
	expect_equal(estres(r[,2L], tol=1e-6), c(absolute=NA_real_))

})

test_that("approx1 (sorted)", {

	x <- c(1.0, 1.01, 1.11, 2.0, 2.22, 3.0, 3.33, 3.333, 4.0)
	y <- x
	xi <- c(1.0, 2.0, 3.33, 5.0)

	expect_equal(c(1.0, 2.0, 3.33, 4.0), approx1(x, y, xi))
	expect_equal(c(1.0, 2.0, 3.33, NA), approx1(x, y, xi, tol=0))
	expect_equal(c(1.0, 2.0, 3.33, NA), approx1(x, y, xi, tol=0.5))

	test1 <- c(1.0+1.01+1.11, 2.0+2.22, 3.0+3.33+3.333, NA)
	expect_equal(test1, approx1(x, y, xi, tol=0.5, interp="sum"))

	test2 <- c((1.0+1.01+1.11)/3, (2.0+2.22)/2, (3.0+3.33+3.333)/3, NA)
	expect_equal(test2, approx1(x, y, xi, tol=0.5, interp="mean"))

	test3 <- c(1.11, 2.22, 3.333, NA)
	expect_equal(test3, approx1(x, y, xi, tol=0.5, interp="max"))
	expect_equal(test3^2, approx1(x, y^2, xi, tol=0.5, interp="max"))

	test4 <- c(1, 2, 3, NA)
	expect_equal(test4, approx1(x, y, xi, tol=0.5, interp="min"))
	expect_equal(test4^2, approx1(x, y^2, xi, tol=0.5, interp="min"))

	xi1 <- x[abs(x - 2) <= 1]
	test5 <- c(dnorm(xi1, mean=2, sd=1/2))
	test5 <- sum(xi1 * test5 / sum(test5))
	expect_equal(test5, approx1(x, y, 2, tol=1, interp="gaussian"))

	xi2 <- seq(from=1, to=3, by=0.2)
	expect_equal(xi2, approx1(x, y, xi2, tol=1, interp="linear"))
	expect_equal(xi2, approx1(x, y, xi2, tol=2, interp="cubic"))

	xi3 <- seq(from=1, to=3, by=0.05)
	expect_equal(xi3, approx1(x, y, xi3, tol=1, interp="linear"))

	t <- seq(from=-4, to=4, length.out=1000)
	s <- dnorm(t)
	expect_equal(1, approx1(t, s, 0, tol=4, interp="area"), tolerance=1e-3)

	test6 <- c(2.0, 3.0)
	expect_equal(test6, approx1(x, y, c(2, 3), tol=0.1, interp="max"))

})

test_that("approx1 (unsorted)", {

	x <- rev(c(1.0, 1.01, 1.11, 2.0, 2.22, 3.0, 3.33, 3.333, 4.0))
	y <- x
	xi <- c(1.0, 2.0, 3.33, 5.0)

	expect_equal(c(1.0, 2.0, 3.33, 4.0), approx1(x, y, xi))
	expect_equal(c(1.0, 2.0, 3.33, NA), approx1(x, y, xi, tol=0))
	expect_equal(c(1.0, 2.0, 3.33, NA), approx1(x, y, xi, tol=0.5))

	test1 <- c(1.0+1.01+1.11, 2.0+2.22, 3.0+3.33+3.333, NA)
	expect_equal(test1, approx1(x, y, xi, tol=0.5, interp="sum"))

	test2 <- c((1.0+1.01+1.11)/3, (2.0+2.22)/2, (3.0+3.33+3.333)/3, NA)
	expect_equal(test2, approx1(x, y, xi, tol=0.5, interp="mean"))

	test3 <- c(1.11, 2.22, 3.333, NA)
	expect_equal(test3, approx1(x, y, xi, tol=0.5, interp="max"))
	expect_equal(test3^2, approx1(x, y^2, xi, tol=0.5, interp="max"))

	test4 <- c(1, 2, 3, NA)
	expect_equal(test4, approx1(x, y, xi, tol=0.5, interp="min"))
	expect_equal(test4^2, approx1(x, y^2, xi, tol=0.5, interp="min"))

	xi1 <- x[abs(x - 2) <= 1]
	test5 <- c(dnorm(xi1, mean=2, sd=1/2))
	test5 <- sum(xi1 * test5 / sum(test5))
	expect_equal(test5, approx1(x, y, 2, tol=1, interp="gaussian"))

	xi2 <- seq(from=1, to=3, by=0.2)
	expect_equal(xi2, approx1(x, y, xi2, tol=1, interp="linear"))
	expect_equal(xi2, approx1(x, y, xi2, tol=2, interp="cubic"))

	xi3 <- seq(from=1, to=3, by=0.05)
	expect_equal(xi3, approx1(x, y, xi3, tol=1, interp="linear"))

	test6 <- c(2.0, 3.0)
	expect_equal(test6, approx1(x, y, c(2, 3), tol=0.1, interp="max"))

})
