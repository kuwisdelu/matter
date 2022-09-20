require(testthat)
require(matter)

context("apply")

test_that("chunkLapply", {

	set.seed(1)
	register(SerialParam())
	a <- replicate(100, rnorm(10), simplify=FALSE)
	b <- replicate(100, runif(10), simplify=FALSE)

	expect_equal(
		chunkLapply(a, mean),
		lapply(a, mean))
	expect_equal(
		chunkLapply(a, mean, nchunks=10),
		lapply(a, mean))
	expect_equal(
		chunkLapply(a, mean, simplify=TRUE),
		sapply(a, mean))
	expect_equal(
		chunkMapply(`+`, a, b),
		mapply(`+`, a, b, SIMPLIFY=FALSE))
	expect_equal(
		chunkMapply(`+`, a, b, nchunks=10),
		mapply(`+`, a, b, SIMPLIFY=FALSE))

	set.seed(1)
	register(SerialParam())
	u <- sort(runif(100))
	v <- rev(u)
	ind <- roll(seq_along(u), width=5, na.drop=TRUE)
	f <- function(x, y) mean(x + y)

	expect_equal(
		chunkLapply(u, mean, depends=ind, simplify=TRUE),
		sapply(ind, function(i) mean(u[i])))
	expect_equal(
		chunkLapply(u, mean, depends=ind, simplify=TRUE, nchunks=10),
		sapply(ind, function(i) mean(u[i])))
	expect_equal(
		chunkMapply(f, u, v, depends=ind, simplify=TRUE),
		sapply(ind, function(i) mean(u[i] + v[i])))
	expect_equal(
		chunkMapply(f, u, v, depends=ind, simplify=TRUE, nchunks=10),
		sapply(ind, function(i) mean(u[i] + v[i])))

})

test_that("chunk_apply io", {

	# test outpath

})

