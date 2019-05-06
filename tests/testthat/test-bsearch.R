require(testthat)
require(matter)

context("bsearch")

test_that("binary search - integers", {

	x <- c(1L, 2L, 3L, 4L, 5L, 8L, 9L, 100L, 101L, 102L)

	keys <- c(1L, 4L, 6L, 99L, 102L)

	expect_equal(c(1, 4, NA, NA, 10), bsearch(keys, x))

	expect_equal(c(1, 4, 5, 8, 10), bsearch(keys, x, nearest=TRUE))

})

test_that("binary search - doubles", {

	x <- c(1.11, 2.22, 3.33, 4.0, 5.0)

	keys <- c(1.11, 3.0, 3.33, 5.0, 5.1)

	expect_equal(c(1, NA, 3, 5, NA), bsearch(keys, x))

	expect_equal(c(1, 3, 3, 5, 5), bsearch(keys, x, nearest=TRUE))

	expect_equal(NA_integer_, bsearch(3.0, x, tol=0))

	expect_equal(3, bsearch(3.0, x, tol=0.5))

	expect_equal(NA_integer_, bsearch(3.0, x, tol=0.1, tol.ref="key"))

	expect_equal(3, bsearch(3.0, x, tol=0.1, tol.ref="key", nearest=TRUE))

	expect_equal(3, bsearch(3.0, x, tol=0.1, tol.ref="values"))

})

test_that("binary search - strings", {

	x <- c("abc", "bc", "bcd", "cde", "def")

	keys <- c("abc", "b", "bcde", "cde", "def")

	expect_equal(c(1, NA, NA, 4, 5), bsearch(keys, x))

	expect_equal(c(1, 2, 3, 4, 5), bsearch(keys, x, nearest=TRUE))

	expect_equal(NA_integer_, bsearch("z", x))

	expect_equal(5, bsearch("z", x, nearest=TRUE))

})

