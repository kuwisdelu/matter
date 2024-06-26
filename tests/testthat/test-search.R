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

test_that("quick sort + friends", {

	set.seed(1, kind="default")
	u1 <- as.numeric(sample(100L))
	u2 <- as.numeric(sample(101L))
	u3 <- c(0,1,0,1,0,0,3,2,2,2,4,4,8,2,0,0)
	u4 <- c(0,1,NA,1,0,0,3,2,2,NA,4,4,8,2,0,0)
	u5 <- sample(colors())

	expect_equal(qorder(u1), order(u1))
	expect_equal(qorder(u2), order(u2))
	expect_equal(u3[qorder(u3)], u3[order(u3)])
	expect_equal(u4[qorder(u4)], u4[order(u4)])
	expect_equal(qorder(u5), order(u5))
	
	expect_equal(qrank(u1), rank(u1))
	expect_equal(qrank(u2), rank(u2))
	expect_equal(qrank(u3, ties.max=TRUE), rank(u3, ties.method="max"))
	expect_equal(qrank(u3, ties.max=FALSE), rank(u3, ties.method="min"))
	expect_equal(qrank(u4, ties.max=TRUE), rank(u4, ties.method="max", na.last="keep"))
	expect_equal(qrank(u4, ties.max=FALSE), rank(u4, ties.method="min", na.last="keep"))
	expect_equal(qrank(u5), rank(u5))
	
	expect_equal(qselect(u1, 1L), min(u1))
	expect_equal(qselect(u1, 100L), max(u1))
	expect_equal(qselect(u2, 51L), median(u2))
	expect_equal(qselect(u3, 1L), 0)
	expect_equal(qselect(u3, 8L), 1)
	expect_equal(qselect(u3, 9L), 2)
	expect_equal(qselect(u3, 16L), 8)
	expect_equal(qselect(u4, 1L), 0)
	expect_equal(qselect(u4, 8L), 2)
	expect_equal(qselect(u4, 9L), 2)
	expect_equal(qselect(u4, 16L), NA_real_)
	expect_equal(qselect(u5, 1L), sort(u5)[1L])
	expect_equal(qselect(u5, 100L), sort(u5)[100L])
	expect_equal(qselect(u5, length(u5)), sort(u5)[length(u5)])
	
	expect_equal(qmedian(u1), median(u1))
	expect_equal(qmedian(u2), median(u2))
	expect_equal(qmedian(u3), median(u3))
	expect_equal(qmedian(u4, na.rm=TRUE), median(u3, na.rm=TRUE))
	
	expect_equal(qmad(u1), mad(u1))
	expect_equal(qmad(u2), mad(u2))
	expect_equal(qmad(u3), mad(u3))
	expect_equal(qmad(u4, na.rm=TRUE), mad(u3, na.rm=TRUE))

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

test_that("k-dimensional search", {

	d1 <- data.frame(
		x=c(2,5,9,4,8,7,9,8,9,6,3,1,9,2,8),
		y=c(3,4,6,7,1,2,4,4,7,3,4,6,5,1,7),
		z=c(3,2,7,9,5,6,1,2,8,1,5,8,3,3,6))
	i1 <- seq_len(nrow(d1))
	t1 <- kdtree(d1)
	ns1 <- as.matrix(t1$nodes + 1L)
	ks1a <- kdsearch(c(2,3,3), d1, tol=c(2,2,4))
	ks1b <- kdsearch(c(7,2,6), d1, tol=2)

	expect_equal(t1$root + 1L, 6L)
	expect_equal(t1$root + 1L, which(!i1 %in% sort(ns1)))
	expect_setequal(ks1a[[1L]], c(11L, 14L, 1L))
	expect_setequal(ks1b[[1L]], c(6L, 5L))

	q1 <- rbind(c(2.22, 3.33, 3.33), c(8.1, 7.1, 6.1))
	kn1a <- knnsearch(q1, d1, k=1)
	kn1b <- knnsearch(q1, d1, k=3)
	kn1c <- knnsearch(q1, d1, k=3, metric="maximum")
	kn1d <- knnsearch(q1, d1, k=3, metric="manhattan")
	kn1e <- knnsearch(q1, d1, k=3, metric="minkowski", p=3)

	expect_setequal(kn1a, c(1L, 15L))
	expect_setequal(kn1b[1L,], c(1L, 11L, 14L))
	expect_setequal(kn1b[2L,], c(15L, 3L, 9L))
	expect_setequal(kn1c[1L,], c(1L, 11L, 14L))
	expect_setequal(kn1c[2L,], c(15L, 3L, 9L))
	expect_setequal(kn1d[1L,], c(1L, 11L, 14L))
	expect_setequal(kn1d[2L,], c(15L, 3L, 9L))
	expect_setequal(kn1e[1L,], c(1L, 11L, 14L))
	expect_setequal(kn1e[2L,], c(15L, 3L, 9L))

	d2 <- expand.grid(x=1:5, y=1:5)
	i2 <- seq_len(nrow(d2))
	t2 <- kdtree(d2)
	ns2 <- as.matrix(t2$nodes + 1L)
	ks2a <- kdsearch(c(1,1), d2, tol=1)
	ks2b <- kdsearch(c(5,5), d2, tol=1)

	q2 <- rbind(c(1.11, 1.11), c(2.22, 2.22))
	kn2a <- knnsearch(q2, d2, k=1)
	kn2b <- knnsearch(q2, d2, k=3)
	
	expect_setequal(kn2a, c(1L, 7L))
	expect_setequal(kn2b[1L,], c(1L, 2L, 6L))
	expect_setequal(kn2b[2L,], c(7L, 8L, 12L))

	expect_equal(t2$root + 1L, which(!i2 %in% sort(ns2)))
	expect_setequal(ks2a[[1L]], c(2L, 7L, 6L, 1L))
	expect_setequal(ks2b[[1L]], c(19L, 24L, 25L, 20L))

	t3 <- kdtree(1:3)
	t4 <- kdtree(1)

	expect_equal(t3$root, 1L)
	expect_equal(t4$root, 0L)

})

test_that("k-dimensional self search", {

	d1 <- data.frame(
		x=c(2,5,9,4,8,7,9,8,9,6,3,1,9,2,8),
		y=c(3,4,6,7,1,2,4,4,7,3,4,6,5,1,7),
		z=c(3,2,7,9,5,6,1,2,8,1,5,8,3,3,6))
	t1 <- kdtree(d1)
	
	kn1a <- knnsearch(d1, k=3)
	kn1b <- knnsearch(d1, t1, k=3)

	expect_equal(kn1a, kn1b)

	set.seed(1)
	n <- 100
	d2 <- data.frame(x=runif(n), y=runif(n))
	t2 <- kdtree(d2)

	kn2a <- knnsearch(d2, k=3)
	kn2b <- knnsearch(d2, t2, k=3)

	expect_equal(kn2a, kn2b)

	d3 <- expand.grid(x=1:9, y=1:9)
	t3 <- kdtree(d3)

	kn3a <- knnsearch(d3, k=5)
	kn3b <- knnsearch(d3, t3, k=5)

	ds3a <- rowdist_at(d3, ix=1:nrow(d3), iy=kn3a)
	ds3b <- rowdist_at(d3, ix=1:nrow(d3), iy=kn3b)

	expect_equal(ds3a, ds3b)
	expect_equal(kn3a, kn3b)

})

