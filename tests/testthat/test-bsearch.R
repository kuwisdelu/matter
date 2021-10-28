require(testthat)
require(matter)

context("bsearch")

test_that("binary search - relative diff", {

	expect_equal(reldiff(3L, 4L), abs(3 - 4))

	expect_equal(reldiff(3L, 4L, ref="x"), abs(3 - 4) / 3)

	expect_equal(reldiff(3L, 4L, ref="y"), abs(3 - 4) / 4)

	expect_equal(reldiff(3, 3.14), abs(3 - 3.14))

	expect_equal(reldiff(3, 3.14, ref="x"), abs(3 - 3.14) / 3)

	expect_equal(reldiff(3, 3.14, ref="y"), abs(3 - 3.14) / 3.14)

	expect_equal(reldiff("abc", "abc"), 0)

	expect_equal(reldiff("abc", "abcd"), 1)

	expect_equal(reldiff("abc", "abbd"), 2)

	expect_equal(reldiff("abc", "bbbd"), 4)

	expect_equal(reldiff("abc", "bbc"), 3)

	expect_equal(reldiff("abc", "abcd", ref="x"), 1 / 3)

	expect_equal(reldiff("abc", "abcd", ref="y"), 1 / 4)

	expect_equal(reldiff("abc", "abbd", ref="x"), 2 / 3)

	expect_equal(reldiff("abc", "abbd", ref="y"), 2 / 4)

})

test_that("linear search - integers", {

	table <- c(1L, 2L, 3L, 4L, 5L, 8L, 9L, 100L, 101L, 102L)

	x <- c(1L, 4L, 6L, 99L, 102L)

	expect_equal(c(1, 4, NA, NA, 10), lsearch(x, table))

	expect_equal(c(1, 4, 5, 8, 10), lsearch(x, table, nearest=TRUE))

})

test_that("linear search - doubles", {

	table <- c(1.11, 2.22, 3.33, 4.0, 5.0)

	x <- c(1.11, 3.0, 3.33, 5.0, 5.1)

	expect_equal(c(1, NA, 3, 5, NA), lsearch(x, table))

	expect_equal(c(1, 3, 3, 5, 5), lsearch(x, table, nearest=TRUE))

	expect_equal(NA_integer_, lsearch(3.0, table, tol=0))

	expect_equal(3, lsearch(3.0, table, tol=0.5))

	expect_equal(NA_integer_, lsearch(3.0, table, tol=0.1, tol.ref="x"))

	expect_equal(3, lsearch(3.0, table, tol=0.1, tol.ref="x", nearest=TRUE))

	expect_equal(3, lsearch(3.0, table, tol=0.1, tol.ref="y"))

})

test_that("linear search - strings", {

	table <- c("abc", "bc", "bcd", "cde", "def")

	x <- c("abc", "b", "bcde", "cde", "def")

	expect_equal(c(1, NA, NA, 4, 5), lsearch(x, table))

	expect_equal(c(1, 2, 3, 4, 5), lsearch(x, table, nearest=TRUE))

	expect_equal(NA_integer_, lsearch("z", table))

})

test_that("binary search - integers", {

	table <- c(1L, 2L, 3L, 4L, 5L, 8L, 9L, 100L, 101L, 102L)

	x <- c(1L, 4L, 6L, 99L, 102L)

	expect_equal(c(1, 4, NA, NA, 10), bsearch(x, table))

	expect_equal(c(1, 4, 5, 8, 10), bsearch(x, table, nearest=TRUE))

})

test_that("binary search - doubles", {

	table <- c(1.11, 2.22, 3.33, 4.0, 5.0)

	x <- c(1.11, 3.0, 3.33, 5.0, 5.1)

	expect_equal(c(1, NA, 3, 5, NA), bsearch(x, table))

	expect_equal(c(1, 3, 3, 5, 5), bsearch(x, table, nearest=TRUE))

	expect_equal(NA_integer_, bsearch(3.0, table, tol=0))

	expect_equal(3, bsearch(3.0, table, tol=0.5))

	expect_equal(NA_integer_, bsearch(3.0, table, tol=0.1, tol.ref="x"))

	expect_equal(3, bsearch(3.0, table, tol=0.1, tol.ref="x", nearest=TRUE))

	expect_equal(3, bsearch(3.0, table, tol=0.1, tol.ref="y"))

})

test_that("binary search - strings", {

	table <- c("abc", "bc", "bcd", "cde", "def")

	x <- c("abc", "b", "bcde", "cde", "def")

	expect_equal(c(1, NA, NA, 4, 5), bsearch(x, table))

	expect_equal(c(1, 2, 3, 4, 5), bsearch(x, table, nearest=TRUE))

	expect_equal(NA_integer_, bsearch("z", table))

	expect_equal(5, bsearch("z", table, nearest=TRUE)) # should this be expected??

})

