require(testthat)
require(matter)

context("signal-processing")

test_that("binvec", {

	set.seed(1)
	x <- runif(50)
	u <- seq(from=1, to=50, by=10)
	v <- seq(from=10, to=50, by=10)
	binfun <- function(x, u, v, fun) {
		mapply(function(i, j) fun(x[i:j]), u, v)
	}

	expect_equal(binfun(x, u, v, sum), binvec(x, u, v, "sum"))
	expect_equal(binfun(x, u, v, mean), binvec(x, u, v, "mean"))
	expect_equal(binfun(x, u, v, max), binvec(x, u, v, "max"))
	expect_equal(binfun(x, u, v, min), binvec(x, u, v, "min"))
	expect_equal(binfun(x, u, v, sd), binvec(x, u, v, "sd"))
	expect_equal(binfun(x, u, v, var), binvec(x, u, v, "var"))

	f <- seq(from=1, to=51, by=10)
	
	expect_equal(binvec(x, u, v), binvec(x, f))

})

test_that("downsample", {

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
	p5 <- findpeaks(x, prominence=TRUE)

	ref <- c(1.2415949, 0.4784017, 0.2847052, 3.1071679,
		0.2846030, 0.4782249, 2.4834026, 0.4782249)

	expect_equivalent(attr(p5, "prominences"), ref, tolerance=1e-3)

})

test_that("peakwidths", {

	t <- seq(from=0, to=6 * pi, length.out=1000)
	x <- sin(t) + 0.6 * sin(2.6 * t)
	p <- findpeaks(x)
	
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
