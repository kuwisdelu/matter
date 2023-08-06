require(testthat)
require(matter)

context("matter-list")

test_that("matter list", {

	x <- list(
		raw=as.raw(0:5),
		lgl=c(TRUE, FALSE, NA),
		int=c(1L, 2L, 3L, 4L, 5L, 6L),
		dbl=c(1.11, 2.22, 3.33),
		chr="hello, world!")
	y <- matter_list(x)

	expect_equal(x, y[])
	expect_equal(x, as.list(y))
	expect_equal(x[1], y[1])
	expect_equal(x[[1]], y[[1]])
	expect_equal(x[1:3], y[1:3])
	expect_equal(x[3:1], y[3:1])
	expect_equal(x[[3]][1:3], y[[3,1:3]])
	expect_equal(x[[3]][3:1], y[[3,3:1]])
	expect_equal(x[[5]], y[[5]])
	expect_equal("hello", y[[5,1:5]])
	expect_equal(x$chr, y$chr)

	z <- y[1:3,drop=NULL]

	expect_is(z, "matter_list")
	expect_equal(x[1:3], z[])

	expect_equal(c(x, x), c(y, y)[])

	y$dbl[3L] <- 3.333
	expect_equal(3.333, y$dbl[3L])

})

test_that("matter struct", {

	x <- struct(first=c(int=1), second=c(double=1))
	x$first <- 2L
	x$second <- 3.333

	expect_equal(2L, x$first)
	expect_equal(3.333, x$second)

	y <- struct(first=c(short=2), second=c(long=2))
	y$first <- c(0L, 1L)
	y$second <- c(1000L, 9999L)

	expect_equal(c(0L, 1L), y$first)
	expect_equal(c(1000L, 9999L), y$second)

	y$first[1L] <- 10L	
	y$second[1L] <- -99L

	expect_equal(c(10L, 1L), y$first)
	expect_equal(c(-99L, 9999L), y$second)

})
