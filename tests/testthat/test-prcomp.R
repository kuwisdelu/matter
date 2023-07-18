require(testthat)
require(matter)

context("prcomp")

test_that("prcomp - matter matrix", {

	set.seed(1)
	x <- matrix(rnorm(5000), nrow=100, ncol=50)
	y <- matter_mat(x, nrow=100, ncol=50)
	pca.x <- prcomp(x)
	pca.y <- prcomp(y)

	expect_equal(
		abs(pca.x$rotation[,1:3]),
		abs(pca.y$rotation), tolerance=1e-3)

	newdata <- matrix(rnorm(250), nrow=5, ncol=50)

	pred.x <- predict(pca.x, newdata)
	pred.y <- predict(pca.y, newdata)

	expect_equal(
		abs(pred.x[,1:3]),
		abs(pred.y[,1:3]), tolerance=1e-4)

})

test_that("prcomp - sparse matrix", {

	set.seed(1)
	x <- rbinom(5000, 1, 0.2)
	x[x != 0] <- seq_len(sum(x != 0))
	dim(x) <- c(100, 50)
	y <- sparse_mat(x)
	pca.x <- prcomp(x)
	pca.y <- prcomp(y)

	expect_equal(
		abs(pca.x$rotation[,1:3]),
		abs(pca.y$rotation), tolerance=1e-3)

	newdata <- matrix(rnorm(250), nrow=5, ncol=50)

	pred.x <- predict(pca.x, newdata)
	pred.y <- predict(pca.y, newdata)

	expect_equal(
		abs(pred.x[,1:3]),
		abs(pred.y[,1:3]), tolerance=1e-4)

})

