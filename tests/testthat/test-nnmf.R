require(testthat)
require(matter)

context("nnmf")

test_that("nnmf - multiplicative updates", {

	set.seed(1)
	nr <- 100
	nc <- 25
	vals1 <- sort(runif(2500))
	vals2 <- rev(sort(runif(2500)))
	x1 <- matrix(vals1, nrow=100, ncol=25)
	x2 <- matrix(vals2, nrow=100, ncol=25)
	x <- cbind(x1, x2)

	mf1 <- nnmf_mult(x, k=3L)
	mf2 <- nnmf_mult(x, k=3L, method="KL")
	mf3 <- nnmf_mult(x, k=3L, method="IS")
	mf1x <- mf1$x %*% t(mf1$activation)
	mf2x <- mf2$x %*% t(mf2$activation)
	mf3x <- mf3$x %*% t(mf2$activation)

	expect_equal(mf1x, x, tolerance=1e-2)
	expect_equal(mf2x, x, tolerance=1e-2)
	expect_equal(mf2x, x, tolerance=1e-2)

	expect_equivalent(
		predict(mf1, x),
		mf1$x, tolerance=2e-1)
	expect_equivalent(
		predict(mf2, x),
		mf2$x, tolerance=2e-1)
	expect_equivalent(
		predict(mf3, x),
		mf3$x, tolerance=2e-1)

})

test_that("nnmf - matter matrix", {

	set.seed(1)
	nr <- 100
	nc <- 25
	vals1 <- sort(runif(2500))
	vals2 <- rev(sort(runif(2500)))
	x1 <- matrix(vals1, nrow=100, ncol=25)
	x2 <- matrix(vals2, nrow=100, ncol=25)
	x <- cbind(x1, x2)
	y <- matter_mat(x)
	mf.x <- nnmf_mult(x)
	mf.y <- nnmf_mult(y)

	expect_equal(
		mf.x$x,
		mf.y$x, tolerance=1e-5)
	expect_equal(
		mf.x$activation,
		mf.y$activation, tolerance=1e-5)

})

test_that("nnmf - sparse matrix", {

	set.seed(1)
	x <- rbinom(5000, 1, 0.2)
	x[x != 0] <- seq_len(sum(x != 0))
	dim(x) <- c(100, 50)
	y <- sparse_mat(x)
	mf.x <- nnmf_mult(x)
	mf.y <- nnmf_mult(y)

	expect_equal(
		mf.x$x,
		mf.y$x, tolerance=1e-5)
	expect_equal(
		mf.x$activation,
		mf.y$activation, tolerance=1e-5)

})