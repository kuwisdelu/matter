require(testthat)
require(matter)

context("apply")

test_that("chunk_apply", {

	set.seed(1)

	register(SerialParam())

	a <- replicate(100, rnorm(10), simplify=FALSE)

	b <- replicate(100, runif(10), simplify=FALSE)

	expect_equal(chunk_apply(a, mean), lapply(a, mean))

	expect_equal(chunk_apply(a, mean, chunks=10), lapply(a, mean))

	expect_equal(chunk_mapply(`+`, a, b),
		mapply(`+`, a, b, SIMPLIFY=FALSE))

	expect_equal(chunk_mapply(`+`, a, b, chunks=10),
		mapply(`+`, a, b, SIMPLIFY=FALSE))

	i <- replicate(100, sample(100, 5), simplify=FALSE)

	m <- simplify2array(a)

	out1 <- chunk_apply(m, sum, MARGIN=2, pattern=i, chunks=10)

	out2 <- lapply(i, function(j) sum(m[,j,drop=FALSE]))

	expect_equal(out1, out2)

	f <- function(x, y) sum(mapply(`*`, x, y, SIMPLIFY=TRUE))

	mout1 <- chunk_mapply(f, a, b, pattern=i, chunks=10)

	mout2 <- lapply(i, function(j) f(a[j], b[j]))

	expect_equal(mout1, mout2)

})

test_that("chunk_apply io", {

	set.seed(1)

	register(SerialParam())

	a <- replicate(100, rnorm(10), simplify=FALSE)

	b <- replicate(100, runif(10), simplify=FALSE)

	out1 <- chunk_apply(a, function(x) x + 1, chunks=10, outfile=tempfile())

	out2 <- lapply(a, function(x) x + 1)

	expect_equal(out1[], out2)

	mout1 <- chunk_mapply(`+`, a, b, chunks=10, outfile=tempfile())

	mout2 <- mapply(`+`, a, b, SIMPLIFY=FALSE)

	expect_equal(mout1[], mout2)

	i <- replicate(100, sample(100, 5), simplify=FALSE)

	m <- simplify2array(a)

	out1 <- chunk_apply(m, sum, MARGIN=2, pattern=i, chunks=10, outfile=tempfile())

	out2 <- lapply(i, function(j) sum(m[,j,drop=FALSE]))

	expect_equal(out1[], out2)

	f <- function(x, y) sum(mapply(`*`, x, y, SIMPLIFY=TRUE))

	mout1 <- chunk_mapply(f, a, b, pattern=i, chunks=10, outfile=tempfile())

	mout2 <- lapply(i, function(j) f(a[j], b[j]))

	expect_equal(mout1[], mout2)

})

test_that("apply", {

	register(SerialParam())

	x <- matrix(1:100, nrow=10, ncol=10)

	y <- matter_mat(x, nrow=10, ncol=10)

	expect_equal(apply(x, 1, sum), apply(y, 1, sum))

	expect_equal(apply(x, 2, sum), apply(y, 2, sum))

	x <- list(a=1:10, b=11:20, c=21:30)

	y <- matter_list(x, names=names(x))

	expect_equal(lapply(x, sum), lapply(y, sum))

	expect_equal(sapply(x, sum), sapply(y, sum))

})

