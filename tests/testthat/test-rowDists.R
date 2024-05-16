require(testthat)
require(matter)

context("distance")

test_that("rowdist + coldist", {

	x <- expand.grid(x=1:4, y=1:4, z=1:2)
	y <- expand.grid(x=1:2, y=1:2, z=1)

	f_d <- function(x, ...) as.matrix(dist(x, ...))

	expect_equivalent(f_d(x), rowdist(x))
	expect_equivalent(f_d(y), rowdist(y))
	expect_equivalent(f_d(t(x)), coldist(x))
	expect_equivalent(f_d(t(y)), coldist(y))

	d <- matrix(nrow=nrow(x), ncol=nrow(y))
	for ( i in seq_len(nrow(x)))
		for ( j in seq_len(nrow(y)))
			d[i,j] <- sqrt(sum((x[i,] - y[j,])^2))

	expect_equal(d, rowdist(x, y))
	expect_equal(d, coldist(t(x), t(y)))

	expect_equivalent(
		f_d(x, method="maximum"),
		rowdist(x, metric="maximum"))
	expect_equivalent(
		f_d(x, method="manhattan"),
		rowdist(x, metric="manhattan"))
	expect_equivalent(
		f_d(x, method="minkowski", p=3),
		rowdist(x, metric="minkowski", p=3))

	expect_equivalent(
		f_d(t(x), method="maximum"),
		coldist(x, metric="maximum"))
	expect_equivalent(
		f_d(t(x), method="manhattan"),
		coldist(x, metric="manhattan"))
	expect_equivalent(
		f_d(t(x), method="minkowski", p=3),
		coldist(x, metric="minkowski", p=3))

	f_r <- function(x, i, y, j)
	{
		Map(function(ii, jj)
		{
			n <- max(length(ii), length(jj))
			ii <- rep_len(ii, n)
			jj <- rep_len(jj, n)
			xi <- x[ii,,drop=FALSE]
			yj <- y[jj,,drop=FALSE]
			sqrt(rowSums((xi - yj)^2))
		}, i, j)
	}

	f_c <- function(x, i, y, j)
	{
		Map(function(ii, jj)
		{
			n <- max(length(ii), length(jj))
			ii <- rep_len(ii, n)
			jj <- rep_len(jj, n)
			xi <- x[,ii,drop=FALSE]
			yj <- y[,jj,drop=FALSE]
			sqrt(colSums((xi - yj)^2))
		}, i, j)
	}

	set.seed(1, kind="default")
	x <- matrix(sort(rnorm(144)), nrow=12, ncol=12)
	y <- matrix(sort(rnorm(144)), nrow=12, ncol=12)
	i <- roll(1:12, width=3, na.drop=TRUE)
	j <- roll(12:1, width=3, na.drop=TRUE)
	
	d1a <- rowdist_at(x, 1:12, y, i)
	d1b <- f_r(x, 1:12, y, i)
	d2a <- rowdist_at(x, i, y, 1:12)
	d2b <- f_r(x, i, y, 1:12)
	d3a <- rowdist_at(x, i, y, j)
	d3b <- f_r(x, i, y, j)
	
	d4a <- coldist_at(x, 1:12, y, i)
	d4b <- f_c(x, 1:12, y, i)
	d5a <- coldist_at(x, i, y, 1:12)
	d5b <- f_c(x, i, y, 1:12)
	d6a <- coldist_at(x, i, y, j)
	d6b <- f_c(x, i, y, j)

	expect_equal(d1a, d1b)
	expect_equal(d2a, d2b)
	expect_equal(d3a, d3b)
	expect_equal(d4a, d4b)
	expect_equal(d5a, d5b)
	expect_equal(d6a, d6b)

})

test_that("rowDists + colDists", {

	register(SerialParam())
	set.seed(1, kind="default")
	x <- matrix(sort(rnorm(30)), nrow=5, ncol=6)
	y <- matrix(sort(rnorm(30)), nrow=5, ncol=6)

	expect_equivalent(rowdist(x), rowDists(x))
	expect_equivalent(rowdist(y), rowDists(y))
	expect_equivalent(coldist(x), colDists(x))
	expect_equivalent(coldist(y), colDists(y))

	expect_equivalent(rowdist(x, y), rowDists(x, y))
	expect_equivalent(rowdist(y, x), rowDists(y, x))
	expect_equivalent(coldist(x, y), colDists(x, y))
	expect_equivalent(coldist(y, x), colDists(y, x))

	expect_equivalent(rowdist(x), rowDists(x, iter.dim=2L))
	expect_equivalent(rowdist(y), rowDists(y, iter.dim=2L))
	expect_equivalent(coldist(x), colDists(x, iter.dim=1L))
	expect_equivalent(coldist(y), colDists(y, iter.dim=1L))

	expect_equivalent(
		rowdist(x, metric="maximum"),
		rowDists(x, metric="maximum"))
	expect_equivalent(
		rowdist(x, metric="manhattan"),
		rowDists(x, metric="manhattan"))
	expect_equivalent(
		rowdist(x, metric="minkowski", p=3),
		rowDists(x, metric="minkowski", p=3))
	expect_equivalent(
		coldist(x, metric="maximum"),
		colDists(x, metric="maximum"))
	expect_equivalent(
		coldist(x, metric="manhattan"),
		colDists(x, metric="manhattan"))
	expect_equivalent(
		coldist(x, metric="minkowski", p=3),
		colDists(x, metric="minkowski", p=3))

	expect_equivalent(
		rowdist(x, metric="maximum"),
		rowDists(x, metric="maximum", iter.dim=2L))
	expect_equivalent(
		rowdist(x, metric="manhattan"),
		rowDists(x, metric="manhattan", iter.dim=2L))
	expect_equivalent(
		rowdist(x, metric="minkowski", p=3),
		rowDists(x, metric="minkowski", p=3, iter.dim=2L))
	expect_equivalent(
		coldist(x, metric="maximum"),
		colDists(x, metric="maximum", iter.dim=1L))
	expect_equivalent(
		coldist(x, metric="manhattan"),
		colDists(x, metric="manhattan", iter.dim=1L))
	expect_equivalent(
		coldist(x, metric="minkowski", p=3),
		colDists(x, metric="minkowski", p=3, iter.dim=1L))

	ir <- roll(1:5, width=3, na.drop=TRUE)
	ic <- roll(1:6, width=3, na.drop=TRUE)

	expect_equal(
		rowdist_at(x, ix=1:5, iy=ir),
		rowDists(x, at=ir))
	
	expect_equal(
		coldist_at(x, ix=1:6, iy=ic),
		colDists(x, at=ic))

	ir <- roll(1:5, width=3, na.drop=FALSE)
	ic <- roll(1:6, width=3, na.drop=FALSE)

	expect_equal(
		rowdist_at(x, ix=1:5, iy=ir),
		rowDists(x, at=ir))
	
	expect_equal(
		coldist_at(x, ix=1:6, iy=ic),
		colDists(x, at=ic))

})

test_that("weighted rowDists + colDists", {

	register(SerialParam())
	set.seed(1, kind="default")
	x <- matrix(sort(rnorm(30)), nrow=5, ncol=6)
	y <- matrix(sort(rnorm(30)), nrow=5, ncol=6)

	wc <- runif(ncol(x))
	wr <- runif(nrow(x))

	dr <- apply(x, 1L,
		function(xi) {
			d <- colsweep(x, xi, "-")
			d <- rowSums(colsweep(d^2, wc, "*"))
			sqrt(d)
		})

	dc <- apply(x, 2L,
		function(xi) {
			d <- rowsweep(x, xi, "-")
			d <- colSums(rowsweep(d^2, wr, "*"))
			sqrt(d)
		})
	
	expect_equal(dr, rowdist(x, weights=wc))
	expect_equal(dr, rowDists(x, weights=wc, iter.dim=1L))
	expect_equal(dr, rowDists(x, weights=wc, iter.dim=2L))

	expect_equal(dc, coldist(x, weights=wr))
	expect_equal(dc, colDists(x, weights=wr, iter.dim=1L))
	expect_equal(dc, colDists(x, weights=wr, iter.dim=2L))

})

test_that("rowDists + colDists - matter matrix", {

	register(SerialParam())
	set.seed(1, kind="default")
	x <- matrix(rnorm(30), nrow=5, ncol=6)
	y <- matrix(rnorm(30), nrow=5, ncol=6)
	xx <- matter_mat(x)
	yy <- matter_mat(y)

	expect_equal(rowdist(x, y), rowDists(xx, y))
	expect_equal(rowdist(y, x), rowDists(y, xx))
	expect_equal(rowdist(x, y), rowDists(x, yy))
	expect_equal(rowdist(y, x), rowDists(yy, x))

	expect_equal(coldist(x, y), colDists(xx, y))
	expect_equal(coldist(y, x), colDists(y, xx))
	expect_equal(coldist(x, y), colDists(x, yy))
	expect_equal(coldist(y, x), colDists(yy, x))

	xx <- matter_mat(x, rowMaj=TRUE)
	yy <- matter_mat(y, rowMaj=TRUE)

	expect_equal(rowdist(x, y), rowDists(xx, y))
	expect_equal(rowdist(y, x), rowDists(y, xx))
	expect_equal(rowdist(x, y), rowDists(x, yy))
	expect_equal(rowdist(y, x), rowDists(yy, x))

	expect_equal(coldist(x, y), colDists(xx, y))
	expect_equal(coldist(y, x), colDists(y, xx))
	expect_equal(coldist(x, y), colDists(x, yy))
	expect_equal(coldist(y, x), colDists(yy, x))

})

test_that("rowDists + colDists - matter matrix", {

	register(SerialParam())
	set.seed(1, kind="default")
	x <- matrix(rnorm(30), nrow=5, ncol=6)
	y <- matrix(rnorm(30), nrow=5, ncol=6)
	xx <- matter_mat(x)
	yy <- matter_mat(y)

	expect_equal(rowdist(x, y), rowDists(xx, y))
	expect_equal(rowdist(y, x), rowDists(y, xx))
	expect_equal(rowdist(x, y), rowDists(x, yy))
	expect_equal(rowdist(y, x), rowDists(yy, x))

	expect_equal(coldist(x, y), colDists(xx, y))
	expect_equal(coldist(y, x), colDists(y, xx))
	expect_equal(coldist(x, y), colDists(x, yy))
	expect_equal(coldist(y, x), colDists(yy, x))

	ir <- roll(1:5, width=3, na.drop=TRUE)
	ic <- roll(1:6, width=3, na.drop=TRUE)

	expect_equal(
		rowdist_at(x, ix=1:5, iy=ir),
		rowDists(xx, at=ir))
	
	expect_equal(
		coldist_at(x, ix=1:6, iy=ic),
		colDists(xx, at=ic))

	xx <- matter_mat(x, rowMaj=TRUE)
	yy <- matter_mat(y, rowMaj=TRUE)

	expect_equal(rowdist(x, y), rowDists(xx, y))
	expect_equal(rowdist(y, x), rowDists(y, xx))
	expect_equal(rowdist(x, y), rowDists(x, yy))
	expect_equal(rowdist(y, x), rowDists(yy, x))

	expect_equal(coldist(x, y), colDists(xx, y))
	expect_equal(coldist(y, x), colDists(y, xx))
	expect_equal(coldist(x, y), colDists(x, yy))
	expect_equal(coldist(y, x), colDists(yy, x))

	expect_equal(
		rowdist_at(x, ix=1:5, iy=ir),
		rowDists(xx, at=ir))
	
	expect_equal(
		coldist_at(x, ix=1:6, iy=ic),
		colDists(xx, at=ic))

})

test_that("rowDists + colDists - sparse matrix", {

	register(SerialParam())
	set.seed(1, kind="default")
	x <- rbinom(30, 1, 0.5)
	x[x != 0] <- seq_len(sum(x != 0))
	dim(x) <- c(5, 6)
	y <- rbinom(30, 1, 0.5)
	y[y != 0] <- seq_len(sum(y != 0))
	dim(y) <- c(5, 6)
	xx <- sparse_mat(x)
	yy <- sparse_mat(y)

	expect_equal(rowdist(x, y), rowDists(xx, y))
	expect_equal(rowdist(y, x), rowDists(y, xx))
	expect_equal(rowdist(x, y), rowDists(x, yy))
	expect_equal(rowdist(y, x), rowDists(yy, x))

	expect_equal(coldist(x, y), colDists(xx, y))
	expect_equal(coldist(y, x), colDists(y, xx))
	expect_equal(coldist(x, y), colDists(x, yy))
	expect_equal(coldist(y, x), colDists(yy, x))

	ir <- roll(1:5, width=3, na.drop=TRUE)
	ic <- roll(1:6, width=3, na.drop=TRUE)

	expect_equal(
		rowdist_at(x, ix=1:5, iy=ir),
		rowDists(xx, at=ir))
	
	expect_equal(
		coldist_at(x, ix=1:6, iy=ic),
		colDists(xx, at=ic))

	xx <- sparse_mat(x, rowMaj=TRUE)
	yy <- sparse_mat(y, rowMaj=TRUE)

	expect_equal(rowdist(x, y), rowDists(xx, y))
	expect_equal(rowdist(y, x), rowDists(y, xx))
	expect_equal(rowdist(x, y), rowDists(x, yy))
	expect_equal(rowdist(y, x), rowDists(yy, x))

	expect_equal(coldist(x, y), colDists(xx, y))
	expect_equal(coldist(y, x), colDists(y, xx))
	expect_equal(coldist(x, y), colDists(x, yy))
	expect_equal(coldist(y, x), colDists(yy, x))

	expect_equal(
		rowdist_at(x, ix=1:5, iy=ir),
		rowDists(xx, at=ir))
	
	expect_equal(
		coldist_at(x, ix=1:6, iy=ic),
		colDists(xx, at=ic))

})

test_that("point in poly", {

	poly <- data.frame(
		x=c(3,5,5,3),
		y=c(3,3,5,5))
	xy <- data.frame(
		x=c(4,6,4,2,3,5,5,3,4,5,4,4,
			3.5,4.5,4.0,3.5),
		y=c(2,4,6,4,3,3,5,5,3,4,5,3,
			4.0,4.0,4.5,4.0),
		ref=c(
			rep("out", 4),
			rep("vertex", 4),
			rep("edge", 4),
			rep("in", 4)))

	xy$test <- inpoly(xy[,1:2], poly)
	
	inside <- which(xy$test)
	outside <- which(!xy$test)

	expect_setequal(inside, which(!xy$ref %in% "out"))
	expect_setequal(outside, which(xy$ref %in% "out"))

})
