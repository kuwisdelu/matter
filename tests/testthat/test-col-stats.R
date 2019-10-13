require(testthat)
require(matter)

context("col-stats")

test_that("col-stats-single", {

	register(SerialParam())

	set.seed(1)

	dmn <- list(paste0("row", 1:100), paste0("col", 1:100))

	x <- matrix(runif(100^2), nrow=100, ncol=100, dimnames=dmn)

	groups <- as.factor(rep(letters[1:5], each=20))

	ans1 <- colStats(x, "mean", chunks=3L)

	ans2 <- colStats(x, "mean", chunks=3L, iter.dim="rows")

	ans3 <- colMeans(x)

	expect_equal(ans1, ans2)

	expect_equal(ans1, ans3)

	ans1 <- colStats(x, "var", chunks=3L)

	ans2 <- colStats(x, "var", chunks=3L, iter.dim="rows")

	ans3 <- apply(x, 2, var)

	expect_equal(ans1, ans2)

	expect_equal(ans1, ans3)

	ans1 <- colStats(x, "mean", groups=groups, chunks=3L)

	ans2 <- colStats(x, "mean", groups=groups, chunks=3L, iter.dim="rows")

	ans3 <- lapply(levels(groups), function(g) colMeans(x[groups == g,]))

	ans3 <- do.call(cbind, ans3); colnames(ans3) <- levels(groups)

	expect_equal(ans1, ans2)

	expect_equal(ans1, ans3)

	ans1 <- colStats(x, "var", groups=groups, chunks=3L)

	ans2 <- colStats(x, "var", groups=groups, chunks=3L, iter.dim="rows")

	ans3 <- lapply(levels(groups), function(g) apply(x[groups == g,], 2, var))
	ans3 <- do.call(cbind, ans3); colnames(ans3) <- levels(groups)

	expect_equal(ans1, ans2)

	expect_equal(ans1, ans3)

})

test_that("col-stats-multi", {

	register(SerialParam())

	set.seed(1)

	dmn <- list(paste0("row", 1:100), paste0("col", 1:100))

	x <- matrix(runif(100^2), nrow=100, ncol=100, dimnames=dmn)

	groups <- as.factor(rep(letters[1:5], each=20))

	ans1 <- colStats(x, c("mean", "var"), chunks=3L)

	ans2 <- colStats(x, c("mean", "var"), chunks=3L, iter.dim="rows")

	ans3 <- list(mean=colMeans(x), var=apply(x, 2, var))

	expect_equal(ans1, ans2)

	expect_equal(ans1, ans3)

	ans1 <- colStats(x, c("mean", "var"), groups=groups, chunks=3L)

	ans2 <- colStats(x, c("mean", "var"), groups=groups, chunks=3L, iter.dim="rows")

	ans3_1 <- lapply(levels(groups), function(g) apply(x[groups == g,], 2, mean))
	ans3_1 <- do.call(cbind, ans3_1); colnames(ans3_1) <- levels(groups)

	ans3_2 <- lapply(levels(groups), function(g) apply(x[groups == g,], 2, var))
	ans3_2 <- do.call(cbind, ans3_2); colnames(ans3_2) <- levels(groups)

	ans3 <- list(mean=ans3_1, var=ans3_2)

	expect_equal(ans1, ans2)

	expect_equal(ans1, ans3)

})

test_that("col-stats-multi", {

	register(SerialParam())

	set.seed(1)

	dmn <- list(paste0("row", 1:100), paste0("col", 1:100))

	x <- matrix(runif(100^2), nrow=100, ncol=100, dimnames=dmn)

	groups <- as.factor(rep(letters[1:5], each=20))

	center <- 1:100 - 50.5
	scale <- sqrt(1:100)

	ans1 <- colStats(x, "sum", col.center=center, col.scale=scale)

	ans2 <- colSums(scale(x, center=center, scale=scale))

	expect_equal(ans1, ans2)

	ans1 <- colStats(x, "sum", row.center=center, row.scale=scale)

	ans2 <- colSums((x - center) / scale)

	expect_equal(ans1, ans2)

	set.seed(2)

	u <- runif(length(center) * nlevels(groups))

	center <- matrix(center + u, ncol=nlevels(groups))
	scale <- matrix(scale + u, ncol=nlevels(groups))

	colnames(center) <- levels(groups)
	colnames(scale) <- levels(groups)

	ans1 <- colStats(x, "sum", groups=groups, col.center=center)

	ans2 <- sapply(levels(groups), function(g) {
		xc <- x - t(center[,groups])
		colSums(xc[g==groups,,drop=FALSE])
	})

	expect_equal(ans1, ans2)

})

