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

	f <- seq(from=1, to=51, by=10)
	
	expect_equal(binvec(x, u, v), binvec(x, f))

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

})

