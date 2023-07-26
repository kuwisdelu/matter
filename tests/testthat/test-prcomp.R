require(testthat)
require(matter)

context("prcomp")

test_that("prcomp - irlba", {

	set.seed(1)
	register(SerialParam())
	x <- matrix(rnorm(5000), nrow=100, ncol=50)
	xt <- t(x)
	pc.x <- prcomp(x, rank.=3L)
	lz.x <- prcomp_lanczos(x, k=3L)
	lz.xt <- prcomp_lanczos(xt, k=3L, transpose=TRUE)

	expect_equal(pc.x$sdev[1:3], lz.x$sdev)
	expect_equal(pc.x$sdev[1:3], lz.xt$sdev)

	expect_equal(
		abs(pc.x$rotation),
		abs(lz.x$rotation), tolerance=1e-5)
	expect_equal(
		abs(pc.x$rotation),
		abs(lz.xt$rotation), tolerance=1e-5)
	
	expect_equal(
		abs(pc.x$x),
		abs(lz.x$x), tolerance=1e-5)
	expect_equal(
		abs(pc.x$x),
		abs(lz.xt$x), tolerance=1e-5)

	newdata <- matrix(rnorm(250), nrow=5, ncol=50)

	pc.pred <- predict(pc.x, newdata)
	lz.pred <- predict(lz.x, newdata)
	lz.predt <- predict(lz.xt, newdata)

	expect_equal(
		abs(pc.pred),
		abs(lz.pred), tolerance=1e-5)
	expect_equal(
		abs(pc.pred),
		abs(lz.predt), tolerance=1e-5)

})

test_that("prcomp - matter matrix", {

	set.seed(1)
	register(SerialParam())
	x <- matrix(rnorm(5000), nrow=100, ncol=50)
	y <- matter_mat(x, nrow=100, ncol=50)
	pc.x <- prcomp(x, rank.=3L)
	pc.y <- prcomp(y, k=3L)

	expect_equal(
		abs(pc.x$rotation),
		abs(pc.y$rotation), tolerance=1e-5)

	expect_equal(
		abs(pc.x$x),
		abs(pc.y$x), tolerance=1e-5)

	newdata <- matrix(rnorm(250), nrow=5, ncol=50)

	pred.x <- predict(pc.x, newdata)
	pred.y <- predict(pc.y, newdata)

	expect_equal(
		abs(pred.x),
		abs(pred.y), tolerance=1e-4)

})

test_that("prcomp - sparse matrix", {

	set.seed(1)
	register(SerialParam())
	x <- rbinom(5000, 1, 0.2)
	x[x != 0] <- seq_len(sum(x != 0))
	dim(x) <- c(100, 50)
	y <- sparse_mat(x)
	pc.x <- prcomp(x, rank.=3L)
	pc.y <- prcomp(y, k=3L)

	expect_equal(
		abs(pc.x$rotation),
		abs(pc.y$rotation), tolerance=1e-3)

	expect_equal(
		abs(pc.x$x),
		abs(pc.y$x), tolerance=1e-5)

	newdata <- matrix(rnorm(250), nrow=5, ncol=50)

	pred.x <- predict(pc.x, newdata)
	pred.y <- predict(pc.y, newdata)

	expect_equal(
		abs(pred.x[,1:3]),
		abs(pred.y[,1:3]), tolerance=1e-4)

})

