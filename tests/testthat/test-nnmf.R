require(testthat)
require(matter)

context("nnmf")

test_that("nnmf - alternating least squares", {

	set.seed(1, kind="default")
	nr <- 100
	nc <- 25
	vals1 <- sort(runif(2500))
	vals2 <- rev(sort(runif(2500)))
	x1 <- matrix(vals1, nrow=100, ncol=25)
	x2 <- matrix(vals2, nrow=100, ncol=25)
	x <- cbind(x1, x2)

	mf <- nnmf_als(x, k=3L)
	mf.x <- mf$x %*% t(mf$activation)

	expect_equal(mf.x, x, tolerance=5e-2)
	expect_equivalent(
		predict(mf, x),
		mf$x, tolerance=2e-1)

	mft <- nnmf_als(t(x), k=3L, transpose=TRUE)
	mft.x <- mft$x %*% t(mft$activation)

	expect_equal(mft.x, x, tolerance=5e-2)
	expect_equivalent(
		predict(mft, t(x)),
		mft$x, tolerance=2e-1)

})

test_that("nnmf - multiplicative updates", {

	set.seed(1, kind="default")
	nr <- 100
	nc <- 25
	vals1 <- sort(runif(2500))
	vals2 <- rev(sort(runif(2500)))
	x1 <- matrix(vals1, nrow=100, ncol=25)
	x2 <- matrix(vals2, nrow=100, ncol=25)
	x <- cbind(x1, x2)

	mf1 <- nnmf_mult(x, k=3L)
	mf2 <- nnmf_mult(x, k=3L, cost="KL")
	mf3 <- nnmf_mult(x, k=3L, cost="IS")
	mf4 <- nnmf_mult(t(x), transpose=TRUE)
	mf1.x <- mf1$x %*% t(mf1$activation)
	mf2.x <- mf2$x %*% t(mf2$activation)
	mf3.x <- mf3$x %*% t(mf3$activation)
	mf4.x <- mf4$x %*% t(mf4$activation)

	expect_equal(mf1.x, x, tolerance=1e-1)
	expect_equal(mf2.x, x, tolerance=1e-1)
	expect_equal(mf3.x, x, tolerance=1e-1)
	expect_equal(mf4.x, x, tolerance=1e-1)

	expect_equivalent(
		predict(mf1, x),
		mf1$x, tolerance=2e-1)
	expect_equivalent(
		predict(mf2, x),
		mf2$x, tolerance=2e-1)
	expect_equivalent(
		predict(mf3, x),
		mf3$x, tolerance=2e-1)
	expect_equivalent(
		predict(mf4, t(x)),
		mf4$x, tolerance=2e-1)

})

test_that("nnmf - matter matrix", {

	register(SerialParam())
	set.seed(1, kind="default")
	nr <- 100
	nc <- 25
	vals1 <- sort(runif(nr * nc))
	vals2 <- rev(sort(runif(nr * nc)))
	x1 <- matrix(vals1, nrow=nr, ncol=nc)
	x2 <- matrix(vals2, nrow=nr, ncol=nc)
	x <- cbind(x1, x2)
	y <- matter_mat(x)
	mf1.x <- nnmf_als(x)
	mf1.y <- nnmf_als(y)
	mf2.x <- nnmf_mult(x)
	mf2.y <- nnmf_mult(y)

	expect_equal(
		mf1.x$x,
		mf1.y$x, tolerance=1e-5)
	expect_equal(
		mf2.x$x,
		mf2.y$x, tolerance=1e-5)
	expect_equal(
		mf1.x$activation,
		mf1.y$activation, tolerance=1e-5)
	expect_equal(
		mf2.x$activation,
		mf2.y$activation, tolerance=1e-5)

})

test_that("nnmf - sparse matrix", {

	register(SerialParam())
	set.seed(1, kind="default")
	nr <- 100
	nc <- 25
	x <- rbinom(nr * nc, 1, 0.2)
	x[x != 0] <- seq_len(sum(x != 0))
	dim(x) <- c(nr, nc)
	y <- sparse_mat(x)
	mf1.x <- nnmf_als(x)
	mf1.y <- nnmf_als(y)
	mf2.x <- nnmf_mult(x)
	mf2.y <- nnmf_mult(y)

	expect_equal(
		mf1.x$x,
		mf1.y$x, tolerance=1e-5)
	expect_equal(
		mf2.x$x,
		mf2.y$x, tolerance=1e-5)
	expect_equal(
		mf1.x$activation,
		mf1.y$activation, tolerance=1e-5)
	expect_equal(
		mf2.x$activation,
		mf2.y$activation, tolerance=1e-5)

})
