require(testthat)
require(matter)

context("matter-list")

test_that("matter list indexing", {

	x <- list(
		raw=as.raw(0:5),
		lgl=c(TRUE, FALSE, NA),
		int=c(1L, 2L, 3L, 4L, 5L, 6L),
		dbl=c(1.11, 2.22, 3.33),
		chr="hello, world!")
	y <- matter_list(x)

	expect_equal(x, y[])
	expect_equal(x[1], y[1])
	expect_equal(x[[1]], y[[1]])
	expect_equal(x[1:3], y[1:3])
	expect_equal(x[3:1], y[3:1])
	expect_equal(x[[3]][1:3], y[[3,1:3]])
	expect_equal(x[[3]][3:1], y[[3,3:1]])
	expect_equal(x[[5]], y[[5]])
	expect_equal("hello", y[[5,1:5]])
	expect_equal(x$chr, y$chr)

})

