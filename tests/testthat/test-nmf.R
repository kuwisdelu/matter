require(testthat)
require(matter)

context("nmf")

test_that("nmf - multiplicative updates", {

	set.seed(1)
	nr <- 100
	nc <- 25
	vals1 <- sort(runif(2500))
	vals2 <- rev(sort(runif(2500)))
	x1 <- matrix(vals1, nrow=100, ncol=25)
	x2 <- matrix(vals2, nrow=100, ncol=25)
	x <- cbind(x1, x2)

	mf1 <- nmf_mu(x, k=3L)
	mf2 <- nmf_mu(x, k=3L, method="KL")
	mf3 <- nmf_mu(x, k=3L, method="IS")
	mf1x <- mf1$x %*% t(mf1$activation)
	mf2x <- mf2$x %*% t(mf2$activation)
	mf3x <- mf3$x %*% t(mf2$activation)
	
	expect_equal(mf1x, x, tolerance=1e-2)
	expect_equal(mf2x, x, tolerance=1e-2)
	expect_equal(mf2x, x, tolerance=1e-2)

})

test_that("nmf - matter matrix", {

	set.seed(1)
	nr <- 100
	nc <- 25
	vals1 <- sort(runif(2500))
	vals2 <- rev(sort(runif(2500)))
	x1 <- matrix(vals1, nrow=100, ncol=25)
	x2 <- matrix(vals2, nrow=100, ncol=25)
	x <- cbind(x1, x2)
	y <- matter_mat(x)
	mf.x <- nmf_mu(x)
	mf.y <- nmf_mu(y)

	expect_equal(
		mf.x$x,
		mf.y$x, tolerance=1e-5)
	expect_equal(
		mf.x$activation,
		mf.y$activation, tolerance=1e-5)

})

test_that("nmf - sparse matrix", {

	set.seed(1)
	x <- rbinom(5000, 1, 0.2)
	x[x != 0] <- seq_len(sum(x != 0))
	dim(x) <- c(100, 50)
	y <- sparse_mat(x)
	mf.x <- nmf_mu(x)
	mf.y <- nmf_mu(y)

	expect_equal(
		mf.x$x,
		mf.y$x, tolerance=1e-5)
	expect_equal(
		mf.x$activation,
		mf.y$activation, tolerance=1e-5)

})
