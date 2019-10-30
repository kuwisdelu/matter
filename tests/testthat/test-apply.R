require(testthat)
require(matter)

context("apply")

test_that("chunk_apply", {

	register(SerialParam())

	a <- replicate(100, rnorm(10), simplify=FALSE)

	b <- replicate(100, runif(10), simplify=FALSE)

	expect_equal(chunk_apply(a, mean), lapply(a, mean))

	expect_equal(chunk_apply(a, mean, chunks=10), lapply(a, mean))

	expect_equal(chunk_mapply(`+`, a, b),
		mapply(`+`, a, b, SIMPLIFY=FALSE))

	expect_equal(chunk_mapply(`+`, a, b, chunks=10),
		mapply(`+`, a, b, SIMPLIFY=FALSE))

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

