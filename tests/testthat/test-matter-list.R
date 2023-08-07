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

	x$dbl[3L] <- 3.333
	y$dbl[3L] <- 3.333
	expect_equal(3.333, y$dbl[3L])

	x2 <- list(
		extra1=c(100, 101, 102),
		extra2=c(2020, 2021, 2022))
	y2 <- matter_list(x2, path=path(y), append=TRUE)
	
	expect_equal(c(x, x2), c(y, y2)[])
	expect_equal(path(y), path(y2))

})

test_that("matter list coercion", {

	x <- list(
		a=c(1L, 2L, 3L, 4L, 5L, 6L),
		b=c(1.0, 1.01, 1.11, 2.0, 2.22, 3.33))
	y <- matter_list(x)

	y2 <- as(y, "matter_arr")
	y3 <- as(y, "matter_vec")
	y4 <- as(y, "matter_mat")
	
	expect_equivalent(unlist(x), y2[])
	expect_equivalent(unlist(x), y3[])
	expect_equal(simplify2array(x), y4[])

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
