require(testthat)
require(matter)

context("matrix-stats")

test_that("rowStats", {

	register(SerialParam())
	set.seed(1, kind="default")
	x <- matrix(10 * runif(75 * 50), nrow=75, ncol=50)
	dnm <- list(paste0("row", 1:nrow(x)), paste0("col", 1:ncol(x)))
	dimnames(x) <- dnm
	group <- factor(sample(LETTERS[1:4], ncol(x), replace=TRUE))
	copts <- list(nchunks=4L)

	a1 <- rowStats(x, "mean", chunkopts=copts)
	a2 <- apply(x, 1L, mean)
	expect_equal(a1, a2)

	a1 <- rowStats(x, "var", chunkopts=copts)
	a2 <- apply(x, 1L, var)
	expect_equal(a1, a2)

	a1 <- rowStats(x, "mean", group=group, chunkopts=copts)
	a2 <- t(aggregate(t(x), list(group), "mean")[-1L])
	expect_equivalent(a1, a2)

	a1 <- rowStats(x, "var", group=group, chunkopts=copts)
	a2 <- t(aggregate(t(x), list(group), "var")[-1L])
	expect_equivalent(a1, a2)

	a1 <- rowStats(x, "sd", group=group, chunkopts=copts)
	a2 <- t(aggregate(t(x), list(group), "sd")[-1L])
	expect_equivalent(a1, a2)

	a1 <- rowStats(x, c("mean", "var"), chunkopts=copts)
	a21 <- apply(x, 1L, mean)
	a22 <- apply(x, 1L, var)
	a2 <- cbind("mean"=a21, "var"=a22)
	expect_equal(a1, a2)

	a1 <- rowStats(x, c("mean", "var"), group=group, chunkopts=copts)
	a21 <- t(aggregate(t(x), list(group), "mean")[-1L])
	a22 <- t(aggregate(t(x), list(group), "var")[-1L])
	expect_equivalent(a1[,,1L], a21)
	expect_equivalent(a1[,,2L], a22)

	cgroup1 <- rep.int("a", ncol(x))
	rgroup1 <- rep.int("a", nrow(x))

	a1 <- rowStats(x, "mean", group=cgroup1)

})

test_that("rowStats (iter by col)", {

	register(SerialParam())
	set.seed(1, kind="default")
	x <- matrix(10 * runif(75 * 50), nrow=75, ncol=50)
	dnm <- list(paste0("row", 1:nrow(x)), paste0("col", 1:ncol(x)))
	dimnames(x) <- dnm
	group <- factor(sample(LETTERS[1:4], ncol(x), replace=TRUE))
	copts <- list(nchunks=4L)

	a1 <- rowStats(x, "mean", chunkopts=copts, iter.dim=2L)
	a2 <- apply(x, 1L, mean)
	expect_equal(a1, a2)

	a1 <- rowStats(x, "var", chunkopts=copts, iter.dim=2L)
	a2 <- apply(x, 1L, var)
	expect_equal(a1, a2)

	a1 <- rowStats(x, "mean", group=group, chunkopts=copts, iter.dim=2L)
	a2 <- t(aggregate(t(x), list(group), "mean")[-1L])
	expect_equivalent(a1, a2)

	a1 <- rowStats(x, "var", group=group, chunkopts=copts, iter.dim=2L)
	a2 <- t(aggregate(t(x), list(group), "var")[-1L])
	expect_equivalent(a1, a2)

	a1 <- rowStats(x, "sd", group=group, chunkopts=copts, iter.dim=2L)
	a2 <- t(aggregate(t(x), list(group), "sd")[-1L])
	expect_equivalent(a1, a2)

	a1 <- rowStats(x, c("mean", "var"), chunkopts=copts, iter.dim=2L)
	a21 <- apply(x, 1L, mean)
	a22 <- apply(x, 1L, var)
	a2 <- cbind("mean"=a21, "var"=a22)
	expect_equal(a1, a2)

	a1 <- rowStats(x, c("mean", "var"), group=group, chunkopts=copts, iter.dim=2L)
	a21 <- t(aggregate(t(x), list(group), "mean")[-1L])
	a22 <- t(aggregate(t(x), list(group), "var")[-1L])
	expect_equivalent(a1[,,1L], a21)
	expect_equivalent(a1[,,2L], a22)

})

test_that("colStats", {

	register(SerialParam())
	set.seed(1, kind="default")
	x <- matrix(10 * runif(75 * 50), nrow=75, ncol=50)
	dnm <- list(paste0("row", 1:nrow(x)), paste0("col", 1:ncol(x)))
	dimnames(x) <- dnm
	group <- factor(sample(LETTERS[1:4], nrow(x), replace=TRUE))
	copts <- list(nchunks=4L)

	a1 <- colStats(x, "mean", chunkopts=copts)
	a2 <- apply(x, 2L, mean)
	expect_equal(a1, a2)

	a1 <- colStats(x, "var", chunkopts=copts)
	a2 <- apply(x, 2L, var)
	expect_equal(a1, a2)

	a1 <- colStats(x, "mean", group=group, chunkopts=copts)
	a2 <- t(aggregate(x, list(group), "mean")[-1L])
	expect_equivalent(a1, a2)

	a1 <- colStats(x, "var", group=group, chunkopts=copts)
	a2 <- t(aggregate(x, list(group), "var")[-1L])
	expect_equivalent(a1, a2)

	a1 <- colStats(x, "sd", group=group, chunkopts=copts)
	a2 <- t(aggregate(x, list(group), "sd")[-1L])
	expect_equivalent(a1, a2)

	a1 <- colStats(x, c("mean", "var"), chunkopts=copts)
	a21 <- apply(x, 2L, mean)
	a22 <- apply(x, 2L, var)
	a2 <- cbind("mean"=a21, "var"=a22)
	expect_equal(a1, a2)

	a1 <- colStats(x, c("mean", "var"), group=group, chunkopts=copts)
	a21 <- t(aggregate(x, list(group), "mean")[-1L])
	a22 <- t(aggregate(x, list(group), "var")[-1L])
	expect_equivalent(a1[,,1L], a21)
	expect_equivalent(a1[,,2L], a22)

})

test_that("colStats (iter by row)", {

	register(SerialParam())
	set.seed(1, kind="default")
	x <- matrix(10 * runif(75 * 50), nrow=75, ncol=50)
	dnm <- list(paste0("row", 1:nrow(x)), paste0("col", 1:ncol(x)))
	dimnames(x) <- dnm
	group <- factor(sample(LETTERS[1:4], nrow(x), replace=TRUE))
	copts <- list(nchunks=4L)

	a1 <- colStats(x, "mean", chunkopts=copts, iter.dim=1L)
	a2 <- apply(x, 2L, mean)
	expect_equal(a1, a2)

	a1 <- colStats(x, "var", chunkopts=copts, iter.dim=1L)
	a2 <- apply(x, 2L, var)
	expect_equal(a1, a2)

	a1 <- colStats(x, "mean", group=group, chunkopts=copts, iter.dim=1L)
	a2 <- t(aggregate(x, list(group), "mean")[-1L])
	expect_equivalent(a1, a2)

	a1 <- colStats(x, "var", group=group, chunkopts=copts, iter.dim=1L)
	a2 <- t(aggregate(x, list(group), "var")[-1L])
	expect_equivalent(a1, a2)

	a1 <- colStats(x, "sd", group=group, chunkopts=copts, iter.dim=1L)
	a2 <- t(aggregate(x, list(group), "sd")[-1L])
	expect_equivalent(a1, a2)

	a1 <- colStats(x, c("mean", "var"), chunkopts=copts, iter.dim=1L)
	a21 <- apply(x, 2L, mean)
	a22 <- apply(x, 2L, var)
	a2 <- cbind("mean"=a21, "var"=a22)
	expect_equal(a1, a2)

	a1 <- colStats(x, c("mean", "var"), group=group, chunkopts=copts, iter.dim=1L)
	a21 <- t(aggregate(x, list(group), "mean")[-1L])
	a22 <- t(aggregate(x, list(group), "var")[-1L])
	expect_equivalent(a1[,,1L], a21)
	expect_equivalent(a1[,,2L], a22)

})

test_that("rowStats + colStats - matter matrix", {

	register(SerialParam())
	set.seed(1, kind="default")
	x <- matrix(runif(600), nrow=30, ncol=20)
	y <- matter_mat(x)

	expect_equal(
		rowStats(x, "mean"),
		rowStats(y, "mean"))
	expect_equal(
		rowStats(x, "var"),
		rowStats(y, "var"))
	expect_equal(
		colStats(x, "mean"),
		colStats(y, "mean"))
	expect_equal(
		colStats(x, "var"),
		colStats(y, "var"))

	y <- matter_mat(x, rowMaj=TRUE)

	expect_equal(
		rowStats(x, "mean"),
		rowStats(y, "mean"))
	expect_equal(
		rowStats(x, "var"),
		rowStats(y, "var"))
	expect_equal(
		colStats(x, "mean"),
		colStats(y, "mean"))
	expect_equal(
		colStats(x, "var"),
		colStats(y, "var"))

})

test_that("rowStats + colStats - sparse matrix", {

	register(SerialParam())
	set.seed(1, kind="default")
	x <- rbinom(600, 1, 0.2)
	x[x != 0] <- seq_len(sum(x != 0))
	dim(x) <- c(30, 20)
	y <- sparse_mat(x)

	expect_equal(
		rowStats(x, "mean"),
		rowStats(y, "mean"))
	expect_equal(
		rowStats(x, "var"),
		rowStats(y, "var"))
	expect_equal(
		colStats(x, "mean"),
		colStats(y, "mean"))
	expect_equal(
		colStats(x, "var"),
		colStats(y, "var"))

	y <- sparse_mat(x, rowMaj=TRUE)

	expect_equal(
		rowStats(x, "mean"),
		rowStats(y, "mean"))
	expect_equal(
		rowStats(x, "var"),
		rowStats(y, "var"))
	expect_equal(
		colStats(x, "mean"),
		colStats(y, "mean"))
	expect_equal(
		colStats(x, "var"),
		colStats(y, "var"))

})
