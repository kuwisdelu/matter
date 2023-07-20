require(testthat)
require(matter)

context("prcomp")

test_that("prcomp - irlba", {

	set.seed(1)
	x <- matrix(rnorm(5000), nrow=100, ncol=50)
	xt <- t(x)
	pca.x <- prcomp(x, rank.=3L)
	lcz.x <- prcomp_lanczos(x, k=3L)
	lcz.xt <- prcomp_lanczos(xt, k=3L, transpose=TRUE)

	expect_equal(pca.x$sdev[1:3], lcz.x$sdev)
	expect_equal(pca.x$sdev[1:3], lcz.xt$sdev)

	expect_equal(
		abs(pca.x$rotation),
		abs(lcz.x$rotation), tolerance=1e-5)
	expect_equal(
		abs(pca.x$rotation),
		abs(lcz.xt$rotation), tolerance=1e-5)
	
	expect_equal(
		abs(pca.x$x),
		abs(lcz.x$x), tolerance=1e-5)
	expect_equal(
		abs(pca.x$x),
		abs(lcz.xt$x), tolerance=1e-5)

	newdata <- matrix(rnorm(250), nrow=5, ncol=50)

	pca.pred <- predict(pca.x, newdata)
	lcz.pred <- predict(lcz.x, newdata)
	lcz.predt <- predict(lcz.xt, newdata)

	expect_equal(
		abs(pca.pred),
		abs(lcz.pred), tolerance=1e-5)
	expect_equal(
		abs(pca.pred),
		abs(lcz.predt), tolerance=1e-5)

})

test_that("prcomp - matter matrix", {

	set.seed(1)
	x <- matrix(rnorm(5000), nrow=100, ncol=50)
	y <- matter_mat(x, nrow=100, ncol=50)
	pca.x <- prcomp(x, rank.=3L)
	pca.y <- prcomp(y, k=3L)

	expect_equal(
		abs(pca.x$rotation),
		abs(pca.y$rotation), tolerance=1e-5)

	expect_equal(
		abs(pca.x$x),
		abs(pca.y$x), tolerance=1e-5)

	newdata <- matrix(rnorm(250), nrow=5, ncol=50)

	pred.x <- predict(pca.x, newdata)
	pred.y <- predict(pca.y, newdata)

	expect_equal(
		abs(pred.x),
		abs(pred.y), tolerance=1e-4)

})

test_that("prcomp - sparse matrix", {

	set.seed(1)
	x <- rbinom(5000, 1, 0.2)
	x[x != 0] <- seq_len(sum(x != 0))
	dim(x) <- c(100, 50)
	y <- sparse_mat(x)
	pca.x <- prcomp(x, rank.=3L)
	pca.y <- prcomp(y, k=3L)

	expect_equal(
		abs(pca.x$rotation),
		abs(pca.y$rotation), tolerance=1e-3)

	expect_equal(
		abs(pca.x$x),
		abs(pca.y$x), tolerance=1e-5)

	newdata <- matrix(rnorm(250), nrow=5, ncol=50)

	pred.x <- predict(pca.x, newdata)
	pred.y <- predict(pca.y, newdata)

	expect_equal(
		abs(pred.x[,1:3]),
		abs(pred.y[,1:3]), tolerance=1e-4)

})

