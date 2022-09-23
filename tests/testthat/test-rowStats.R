require(testthat)
require(matter)

context("matrix-stats")

test_that("row-stats", {

	register(SerialParam())
	set.seed(1)
	x <- matrix(10 * runif(75 * 50), nrow=75, ncol=50)
	dnm <- list(paste0("row", 1:nrow(x)), paste0("col", 1:ncol(x)))
	dimnames(x) <- dnm
	group <- factor(sample(LETTERS[1:4], ncol(x), replace=TRUE))

	a1 <- rowStats(x, "mean", nchunks=4L)
	a2 <- apply(x, 1L, mean)
	expect_equal(a1, a2)

	a1 <- rowStats(x, "var", nchunks=4L)
	a2 <- apply(x, 1L, var)
	expect_equal(a1, a2)

	a1 <- rowStats(x, "mean", group=group, nchunks=4L)
	a2 <- t(aggregate(t(x), list(group), "mean")[-1L])
	expect_equal(a1, a2, check.attributes=FALSE)

	a1 <- rowStats(x, "var", group=group, nchunks=4L)
	a2 <- t(aggregate(t(x), list(group), "var")[-1L])
	expect_equal(a1, a2, check.attributes=FALSE)

	a1 <- rowStats(x, "sd", group=group, nchunks=4L)
	a2 <- t(aggregate(t(x), list(group), "sd")[-1L])
	expect_equal(a1, a2, check.attributes=FALSE)

	a1 <- rowStats(x, c("mean", "var"), nchunks=4L)
	a21 <- apply(x, 1L, mean)
	a22 <- apply(x, 1L, var)
	a2 <- cbind("mean"=a21, "var"=a22)
	expect_equal(a1, a2)

	a1 <- rowStats(x, c("mean", "var"), group=group, nchunks=4L)
	a21 <- t(aggregate(t(x), list(group), "mean")[-1L])
	a22 <- t(aggregate(t(x), list(group), "var")[-1L])
	expect_equal(a1[,,1L], a21, check.attributes=FALSE)
	expect_equal(a1[,,2L], a22, check.attributes=FALSE)

})

test_that("row-stats (iter by col)", {

	register(SerialParam())
	set.seed(1)
	x <- matrix(10 * runif(75 * 50), nrow=75, ncol=50)
	dnm <- list(paste0("row", 1:nrow(x)), paste0("col", 1:ncol(x)))
	dimnames(x) <- dnm
	group <- factor(sample(LETTERS[1:4], ncol(x), replace=TRUE))

	a1 <- rowStats(x, "mean", nchunks=4L, iter.dim=2L)
	a2 <- apply(x, 1L, mean)
	expect_equal(a1, a2)

	a1 <- rowStats(x, "var", nchunks=4L, iter.dim=2L)
	a2 <- apply(x, 1L, var)
	expect_equal(a1, a2)

	a1 <- rowStats(x, "mean", group=group, nchunks=4L, iter.dim=2L)
	a2 <- t(aggregate(t(x), list(group), "mean")[-1L])
	expect_equal(a1, a2, check.attributes=FALSE)

	a1 <- rowStats(x, "var", group=group, nchunks=4L, iter.dim=2L)
	a2 <- t(aggregate(t(x), list(group), "var")[-1L])
	expect_equal(a1, a2, check.attributes=FALSE)

	a1 <- rowStats(x, "sd", group=group, nchunks=4L, iter.dim=2L)
	a2 <- t(aggregate(t(x), list(group), "sd")[-1L])
	expect_equal(a1, a2, check.attributes=FALSE)

	a1 <- rowStats(x, c("mean", "var"), nchunks=4L, iter.dim=2L)
	a21 <- apply(x, 1L, mean)
	a22 <- apply(x, 1L, var)
	a2 <- cbind("mean"=a21, "var"=a22)
	expect_equal(a1, a2)

	a1 <- rowStats(x, c("mean", "var"), group=group, nchunks=4L, iter.dim=2L)
	a21 <- t(aggregate(t(x), list(group), "mean")[-1L])
	a22 <- t(aggregate(t(x), list(group), "var")[-1L])
	expect_equal(a1[,,1L], a21, check.attributes=FALSE)
	expect_equal(a1[,,2L], a22, check.attributes=FALSE)

})

test_that("col-stats", {

	register(SerialParam())
	set.seed(1)
	x <- matrix(10 * runif(75 * 50), nrow=75, ncol=50)
	dnm <- list(paste0("row", 1:nrow(x)), paste0("col", 1:ncol(x)))
	dimnames(x) <- dnm
	group <- factor(sample(LETTERS[1:4], nrow(x), replace=TRUE))

	a1 <- colStats(x, "mean", nchunks=4L)
	a2 <- apply(x, 2L, mean)
	expect_equal(a1, a2)

	a1 <- colStats(x, "var", nchunks=4L)
	a2 <- apply(x, 2L, var)
	expect_equal(a1, a2)

	a1 <- colStats(x, "mean", group=group, nchunks=4L)
	a2 <- t(aggregate(x, list(group), "mean")[-1L])
	expect_equal(a1, a2, check.attributes=FALSE)

	a1 <- colStats(x, "var", group=group, nchunks=4L)
	a2 <- t(aggregate(x, list(group), "var")[-1L])
	expect_equal(a1, a2, check.attributes=FALSE)

	a1 <- colStats(x, "sd", group=group, nchunks=4L)
	a2 <- t(aggregate(x, list(group), "sd")[-1L])
	expect_equal(a1, a2, check.attributes=FALSE)

	a1 <- colStats(x, c("mean", "var"), nchunks=4L)
	a21 <- apply(x, 2L, mean)
	a22 <- apply(x, 2L, var)
	a2 <- cbind("mean"=a21, "var"=a22)
	expect_equal(a1, a2)

	a1 <- colStats(x, c("mean", "var"), group=group, nchunks=4L)
	a21 <- t(aggregate(x, list(group), "mean")[-1L])
	a22 <- t(aggregate(x, list(group), "var")[-1L])
	expect_equal(a1[,,1L], a21, check.attributes=FALSE)
	expect_equal(a1[,,2L], a22, check.attributes=FALSE)

})

test_that("col-stats (iter by row)", {

	register(SerialParam())
	set.seed(1)
	x <- matrix(10 * runif(75 * 50), nrow=75, ncol=50)
	dnm <- list(paste0("row", 1:nrow(x)), paste0("col", 1:ncol(x)))
	dimnames(x) <- dnm
	group <- factor(sample(LETTERS[1:4], nrow(x), replace=TRUE))

	a1 <- colStats(x, "mean", nchunks=4L, iter.dim=1L)
	a2 <- apply(x, 2L, mean)
	expect_equal(a1, a2)

	a1 <- colStats(x, "var", nchunks=4L, iter.dim=1L)
	a2 <- apply(x, 2L, var)
	expect_equal(a1, a2)

	a1 <- colStats(x, "mean", group=group, nchunks=4L, iter.dim=1L)
	a2 <- t(aggregate(x, list(group), "mean")[-1L])
	expect_equal(a1, a2, check.attributes=FALSE)

	a1 <- colStats(x, "var", group=group, nchunks=4L, iter.dim=1L)
	a2 <- t(aggregate(x, list(group), "var")[-1L])
	expect_equal(a1, a2, check.attributes=FALSE)

	a1 <- colStats(x, "sd", group=group, nchunks=4L, iter.dim=1L)
	a2 <- t(aggregate(x, list(group), "sd")[-1L])
	expect_equal(a1, a2, check.attributes=FALSE)

	a1 <- colStats(x, c("mean", "var"), nchunks=4L, iter.dim=1L)
	a21 <- apply(x, 2L, mean)
	a22 <- apply(x, 2L, var)
	a2 <- cbind("mean"=a21, "var"=a22)
	expect_equal(a1, a2)

	a1 <- colStats(x, c("mean", "var"), group=group, nchunks=4L, iter.dim=1L)
	a21 <- t(aggregate(x, list(group), "mean")[-1L])
	a22 <- t(aggregate(x, list(group), "var")[-1L])
	expect_equal(a1[,,1L], a21, check.attributes=FALSE)
	expect_equal(a1[,,2L], a22, check.attributes=FALSE)

})

