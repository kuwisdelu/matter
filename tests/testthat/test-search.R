require(testthat)
require(matter)

context("search")

test_that("relative difference", {

	expect_equal(reldiff(1:10, ref="abs"), rep.int(1, 9))
	expect_equal(reldiff(1:10, ref="x"), rep.int(1, 9) / 2:10)
	expect_equal(reldiff(1:10, ref="y"), rep.int(1, 9) / 1:9)
	expect_equal(reldiff(3L, 4L, ref="abs"), (3 - 4))
	expect_equal(reldiff(3L, 4L, ref="x"), (3 - 4) / 3)
	expect_equal(reldiff(3L, 4L, ref="y"), (3 - 4) / 4)
	expect_equal(reldiff(3, 3.14, ref="abs"), (3 - 3.14))
	expect_equal(reldiff(3, 3.14, ref="x"), (3 - 3.14) / 3)
	expect_equal(reldiff(3, 3.14, ref="y"), (3 - 3.14) / 3.14)
	expect_equal(reldiff("abc", "abc"), 0)
	expect_equal(reldiff("abc", "abcd", ref="abs"), -1)
	expect_equal(reldiff("abc", "abbd", ref="abs"), 2)
	expect_equal(reldiff("abc", "bbbd", ref="abs"), -4)
	expect_equal(reldiff("abc", "bbc", ref="abs"), -3)
	expect_equal(reldiff("abc", "abcd", ref="x"), -1 / 3)
	expect_equal(reldiff("abc", "abcd", ref="y"), -1 / 4)
	expect_equal(reldiff("abc", "abbd", ref="x"), 2 / 3)
	expect_equal(reldiff("abc", "abbd", ref="y"), 2 / 4)

})

test_that("quick select + median + mad", {

	set.seed(1)
	u1 <- as.numeric(sample(100L))
	u2 <- as.numeric(sample(101L))
	u3 <- c(0,1,0,1,0,0,3,2,2,2,4,4,8,2,0,0)
	u4 <- sample(colors())

	expect_equal(qorder(u1), order(u1))
	expect_equal(qorder(u2), order(u2))
	expect_equal(u3[qorder(u3)], u3[order(u3)])
	expect_equal(qorder(u4), order(u4))
	expect_equal(qselect(u1, 1L), min(u1))
	expect_equal(qselect(u1, 100L), max(u1))
	expect_equal(qselect(u2, 51L), median(u2))
	expect_equal(qselect(u3, 1L), 0)
	expect_equal(qselect(u3, 8L), 1)
	expect_equal(qselect(u3, 9L), 2)
	expect_equal(qselect(u3, 16L), 8)
	expect_equal(qselect(u4, 1L), sort(u4)[1L])
	expect_equal(qselect(u4, 100L), sort(u4)[100L])
	expect_equal(qselect(u4, length(u4)), sort(u4)[length(u4)])
	expect_equal(qmedian(u1), median(u1))
	expect_equal(qmedian(u2), median(u2))
	expect_equal(qmedian(u3), median(u3))
	expect_equal(qmad(u1), mad(u1))
	expect_equal(qmad(u2), mad(u2))
	expect_equal(qmad(u3), mad(u3))

})

test_that("binary search - integers", {

	table <- c(1L, 2L, 3L, 4L, 5L, 8L, 9L, 100L, 101L, 102L)
	x <- c(1L, 4L, 6L, 99L, 102L)
	
	expect_equal(c(1, 4, NA, NA, 10), bsearch(x, table))
	expect_equal(c(1, 4, 5, 8, 10), bsearch(x, table, tol=Inf))
	expect_equal(c(1, 4, 5, 8, 10), bsearch(x, table, nearest=TRUE))
	expect_equal(rep_len(NA_integer_, 3L), bsearch(c(-1L, 0L, 1L), integer(), tol=0.1))

})

test_that("binary search - doubles", {

	table <- c(1.11, 2.22, 3.33, 4.0, 5.0)
	x <- c(1.11, 3.0, 3.33, 5.0, 5.1)
	
	expect_equal(c(1, NA, 3, 5, NA), bsearch(x, table))
	expect_equal(c(1, 3, 3, 5, 5), bsearch(x, table, nearest=TRUE))
	expect_equal(NA_integer_, bsearch(3.0, table, tol=0))
	expect_equal(3, bsearch(3.0, table, tol=0.5))
	expect_equal(3, bsearch(3.0, table, tol=Inf))
	expect_equal(NA_integer_, bsearch(3.0, table, tol=0.1, tol.ref="x"))
	expect_equal(3, bsearch(3.0, table, tol=0.1, tol.ref="x", nearest=TRUE))
	expect_equal(3, bsearch(3.0, table, tol=0.1, tol.ref="y"))
	expect_equal(3, bsearch(3.0, table, tol=0.1, tol.ref="y"))
	expect_equal(rep_len(NA_integer_, 3L), bsearch(c(-1, 0, 1), numeric(), tol=0.1))

})

test_that("binary search - strings", {

	table <- c("abc", "bc", "bcd", "cde", "def")
	x <- c("abc", "b", "bcde", "cde", "def")
	
	expect_equal(c(1, NA, NA, 4, 5), bsearch(x, table))
	expect_equal(c(1, 2, 3, 4, 5), bsearch(x, table, nearest=TRUE))
	expect_equal(NA_integer_, bsearch("z", table))
	expect_equal(5, bsearch("z", table, nearest=TRUE)) # should this be expected??

})

test_that("approx search (sorted) - integers", {

	keys <- c(1L, 2L, 3L, 4L, 5L, 8L, 9L, 100L, 101L, 102L)
	vals <- keys^1.11
	x <- c(1L, 4L, 6L, 99L, 102L)

	expect_equal(vals[c(1, 4, NA, NA, 10)], asearch(x, keys, vals))
	expect_equal(vals[c(1, 4, 5, 8, 10)], asearch(x, keys, vals, tol=Inf))
	expect_equal(vals[rev(c(1, 4, NA, NA, 10))], asearch(rev(x), keys, vals))
	expect_equal(vals[rev(c(1, 4, 5, 8, 10))], asearch(rev(x), keys, vals, tol=Inf))
	expect_equal(10L, asearch(1L, 1L, 10L))
	expect_equal(NA_real_, asearch(NA_integer_, keys, vals))
	expect_equal(c(0, NA_real_), asearch(c(1.01, NA_integer_), keys, vals, nomatch=0))

})

test_that("approx search (unsorted) - integers", {

	keys <- rev(c(1L, 2L, 3L, 4L, 5L, 8L, 9L, 100L, 101L, 102L))
	vals <- keys^1.11
	x <- c(1L, 4L, 6L, 99L, 102L)

	expect_equal(vals[c(10, 7, NA, NA, 1)], asearch(x, keys, vals))
	expect_equal(vals[c(10, 7, 6, 3, 1)], asearch(x, keys, vals, tol=Inf))
	expect_equal(vals[rev(c(10, 7, NA, NA, 1))], asearch(rev(x), keys, vals))
	expect_equal(vals[rev(c(10, 7, 6, 3, 1))], asearch(rev(x), keys, vals, tol=Inf))

})

test_that("approx search (sorted) - doubles", {

	keys <- c(1.11, 2.22, 3.33, 4.0, 5.0)
	vals <- keys^1.11
	x <- c(1.11, 3.0, 3.33, 5.0, 5.1)

	expect_equal(vals[c(1, NA, 3, 5, NA)], asearch(x, keys, vals))
	expect_equal(vals[c(1, 3, 3, 5, 5)], asearch(x, keys, vals, tol=Inf))
	expect_equal(NA_real_, asearch(3.0, keys, vals, tol=0))
	expect_equal(vals[3], asearch(3.0, keys, vals, tol=0.5))
	expect_equal(vals[3], asearch(3.0, keys, vals, tol=Inf))
	expect_equal(NA_real_, asearch(3.0, keys, vals, tol=0.1, tol.ref="x"))
	expect_equal(vals[3], asearch(3.0, keys, vals, tol=0.2, tol.ref="x"))
	expect_equal(vals[3], asearch(3.0, keys, vals, tol=0.1, tol.ref="y"))
	expect_equal(1.11, asearch(1.01, 1.01, 1.11))
	expect_equal(NA_real_, asearch(NA_real_, keys, vals))
	expect_equal(c(0, NA_real_), asearch(c(1.01, NA_real_), keys, vals, nomatch=0))

	fun <- function(xi) which.min(abs(xi - keys))
	x2 <- seq(from=1, to=4, length.out=20)
	y2 <- vals[vapply(x2, fun, integer(1))]

	expect_equal(y2, asearch(x2, keys, vals, tol=Inf))

})

test_that("approx search (unsorted) - doubles", {

	keys <- rev(c(1.11, 2.22, 3.33, 4.0, 5.0))
	vals <- keys^1.11
	x <- c(1.11, 3.0, 3.33, 5.0, 5.1)

	expect_equal(vals[c(5, NA, 3, 1, NA)], asearch(x, keys, vals))
	expect_equal(vals[c(5, 3, 3, 1, 1)], asearch(x, keys, vals, tol=Inf))
	expect_equal(NA_real_, asearch(3.0, keys, vals, tol=0))
	expect_equal(vals[3], asearch(3.0, keys, vals, tol=0.5))
	expect_equal(vals[3], asearch(3.0, keys, vals, tol=Inf))
	expect_equal(NA_real_, asearch(3.0, keys, vals, tol=0.1, tol.ref="x"))
	expect_equal(vals[3], asearch(3.0, keys, vals, tol=0.2, tol.ref="x"))
	expect_equal(vals[3], asearch(3.0, keys, vals, tol=0.1, tol.ref="y"))

})

test_that("approx search (sorted) - strings", {

	keys <- c("a", "b", "c", "d", "e")
	vals <- c(1.11, 2.22, 3.33, 4.0, 5.0)
	x <- c("a", "c", "ee")

	expect_equal(vals[c(1, 3, NA)], asearch(x, keys, vals))
	expect_equal(vals[c(1, 3, 5)], asearch(x, keys, vals, tol=Inf))

})

test_that("approx search (unsorted) - strings", {

	keys <- rev(c("a", "b", "c", "d", "e"))
	vals <- rev(c(1.11, 2.22, 3.33, 4.0, 5.0))
	x <- c("a", "c", "ee")

	expect_equal(vals[c(5, 3, NA)], asearch(x, keys, vals))
	expect_equal(vals[c(5, 3, 1)], asearch(x, keys, vals, tol=Inf))

})

test_that("k-dimensional search", {

	d1 <- data.frame(
		x=c(2,5,9,4,8,7,9,8,9,6,3,1,9,2,8),
		y=c(3,4,6,7,1,2,4,4,7,3,4,6,5,1,7),
		z=c(3,2,7,9,5,6,1,2,8,1,5,8,3,3,6))
	i1 <- seq_len(nrow(d1))
	t1 <- kdtree(d1)
	ns1 <- as.matrix(t1$nodes + 1L)
	ks1a <- kdsearch(c(2,3,3), d1)
	ks1b <- kdsearch(c(7,2,6), d1, tol=2)

	expect_equal(t1$root + 1L, 6L)
	expect_equal(t1$root + 1L, which(!i1 %in% sort(ns1)))
	expect_setequal(ks1a, list(c(11L, 14L, 1L)))
	expect_setequal(ks1b, list(c(6L, 5L)))

	d2 <- expand.grid(x=1:5, y=1:5)
	i2 <- seq_len(nrow(d2))
	t2 <- kdtree(d2)
	ns2 <- as.matrix(t2$nodes + 1L)
	ks2a <- kdsearch(c(1,1), d2, tol=1)
	ks2b <- kdsearch(c(5,5), d2, tol=1)

	expect_equal(t2$root + 1L, which(!i2 %in% sort(ns2)))
	expect_setequal(ks2a, list(c(2L, 7L, 6L, 1L)))
	expect_setequal(ks2b, list(c(19L, 24L, 25L, 20L)))

	t3 <- kdtree(1:3)
	t4 <- kdtree(1)

	expect_equal(t3$root, 1L)
	expect_equal(t4$root, 0L)

})
