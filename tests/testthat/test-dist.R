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

	set.seed(1)
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
