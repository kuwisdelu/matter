require(testthat)
require(matter)

context("scale")

test_that("sweep - matter matrix", {

	set.seed(1)
	vals <- sort(round(10 * runif(35), 2))
	x <- matrix(vals, nrow=5, ncol=7)
	y <- matter_mat(x)

	a1 <- 1:5
	b1 <- 1:7
	a2 <- 101:105
	b2 <- 101:107
	a <- cbind(a1, a2)
	b <- cbind(b1, b2)
	cgroup <- as.integer(c(1, 1, 1, 1, 2, 2, 2))
	rgroup <- as.integer(c(1, 1, 1, 2, 2))

	expect_equal(sweep(x, 1L, a1), rowsweep(x, a1))
	expect_equal(rowsweep(x, a1), rowsweep(y, a1)[])
	expect_equal(
		rowsweep(x, a, group=cgroup),
		rowsweep(y, a, group=cgroup)[])

	expect_equal(sweep(x, 2L, b1), colsweep(x, b1))
	expect_equal(colsweep(x, b1), colsweep(y, b1)[])
	expect_equal(
		colsweep(x, b, group=rgroup),
		colsweep(y, b, group=rgroup)[])

	cgroupNA <- replace(cgroup, 4L, NA)

	expect_equal(
		rowsweep(x, a, group=cgroupNA),
		rowsweep(y, a, group=cgroupNA)[])

})

test_that("sweep - sparse matrix", {

	set.seed(2)
	x <- rbinom(35, 1, 0.4)
	x[x != 0] <- seq_len(sum(x != 0)) + seq_len(sum(x != 0)) * 0.11
	dim(x) <- c(5, 7)
	y <- sparse_mat(x)

	a1 <- 1:5
	b1 <- 1:7
	a2 <- 101:105
	b2 <- 101:107
	a <- cbind(a1, a2)
	b <- cbind(b1, b2)
	cgroup <- as.integer(c(1, 1, 1, 1, 2, 2, 2))
	rgroup <- as.integer(c(1, 1, 1, 2, 2))

	expect_equal(sweep(x, 1L, a1), rowsweep(x, a1))
	expect_equal(rowsweep(x, a1), rowsweep(y, a1)[])
	expect_equal(
		rowsweep(x, a, group=cgroup),
		rowsweep(y, a, group=cgroup)[])

	expect_equal(sweep(x, 2L, b1), colsweep(x, b1))
	expect_equal(colsweep(x, b1), colsweep(y, b1)[])
	expect_equal(
		colsweep(x, b, group=rgroup),
		colsweep(y, b, group=rgroup)[])

	cgroupNA <- replace(cgroup, 4L, NA)

	expect_equal(
		rowsweep(x, a, group=cgroupNA),
		rowsweep(y, a, group=cgroupNA)[])

})

test_that("scale - matrix", {

	register(SerialParam())
	set.seed(1)
	vals <- sort(round(10 * runif(35), 2))
	x <- matrix(vals, nrow=5, ncol=7)

	expect_equivalent(scale(x), colscale(x))
	expect_equivalent(scale(x, scale=FALSE), colscale(x, scale=FALSE))
	expect_equivalent(scale(x, center=FALSE), colscale(x, center=FALSE))

	expect_equivalent(t(scale(t(x))), rowscale(x))
	expect_equivalent(t(scale(t(x), scale=FALSE)), rowscale(x, scale=FALSE))
	expect_equivalent(t(scale(t(x), center=FALSE)), rowscale(x, center=FALSE))

	cgroup <- as.integer(c(1, 1, 1, 1, 2, 2, 2))
	rgroup <- as.integer(c(1, 1, 1, 2, 2))

	center <- colStats(x, "mean", group=rgroup)
	x2 <- t(sapply(1:nrow(x), function(i) x[i,] - center[,rgroup[i]]))
	x3 <- colscale(x, scale=FALSE, group=rgroup)
	expect_equivalent(x2, x3)

	center <- rowStats(x, "mean", group=cgroup)
	x2 <- sapply(1:ncol(x), function(i) x[,i] - center[,cgroup[i]])
	x3 <- rowscale(x, scale=FALSE, group=cgroup)
	expect_equivalent(x2, x3)

	scale <- colStats(x^2, "sum", group=rgroup)
	scale <- sqrt(scale / pmax(1, (rep(tabulate(rgroup), each=ncol(x)) - 1)))
	x2 <- t(sapply(1:nrow(x), function(i) x[i,] / scale[,rgroup[i]]))
	x3 <- colscale(x, center=FALSE, group=rgroup)
	expect_equivalent(x2, x3)

	scale <- rowStats(x^2, "sum", group=cgroup)
	scale <- sqrt(scale / pmax(1, (rep(tabulate(cgroup), each=nrow(x)) - 1)))
	x2 <- sapply(1:ncol(x), function(i) x[,i] / scale[,cgroup[i]])
	x3 <- rowscale(x, center=FALSE, group=cgroup)
	expect_equivalent(x2, x3)

})

test_that("scale - matter matrix", {

	register(SerialParam())
	set.seed(1)
	vals <- sort(round(10 * runif(35), 2))
	x <- matrix(vals, nrow=5, ncol=7)
	y <- matter_mat(x)

	expect_equivalent(scale(x), colscale(y)[])
	expect_equivalent(scale(x, scale=FALSE), colscale(y, scale=FALSE)[])
	expect_equivalent(scale(x, center=FALSE), colscale(y, center=FALSE)[])

	expect_equivalent(t(scale(t(x))), rowscale(y)[])
	expect_equivalent(t(scale(t(x), scale=FALSE)), rowscale(y, scale=FALSE)[])
	expect_equivalent(t(scale(t(x), center=FALSE)), rowscale(y, center=FALSE)[])

	cgroup <- as.integer(c(1, 1, 1, 1, 2, 2, 2))
	rgroup <- as.integer(c(1, 1, 1, 2, 2))

	center <- colStats(x, "mean", group=rgroup)
	x2 <- t(sapply(1:nrow(x), function(i) x[i,] - center[,rgroup[i]]))
	x3 <- colscale(y, scale=FALSE, group=rgroup)[]
	expect_equivalent(x2, x3)

	center <- rowStats(x, "mean", group=cgroup)
	x2 <- sapply(1:ncol(x), function(i) x[,i] - center[,cgroup[i]])
	x3 <- rowscale(y, scale=FALSE, group=cgroup)[]
	expect_equivalent(x2, x3)

	scale <- colStats(x^2, "sum", group=rgroup)
	scale <- sqrt(scale / pmax(1, (rep(tabulate(rgroup), each=ncol(x)) - 1)))
	x2 <- t(sapply(1:nrow(x), function(i) x[i,] / scale[,rgroup[i]]))
	x3 <- colscale(y, center=FALSE, group=rgroup)[]
	expect_equivalent(x2, x3)

	scale <- rowStats(x^2, "sum", group=cgroup)
	scale <- sqrt(scale / pmax(1, (rep(tabulate(cgroup), each=nrow(x)) - 1)))
	x2 <- sapply(1:ncol(x), function(i) x[,i] / scale[,cgroup[i]])
	x3 <- rowscale(y, center=FALSE, group=cgroup)[]
	expect_equivalent(x2, x3)

})

test_that("scale - sparse matrix", {

	register(SerialParam())
	set.seed(2)
	x <- rbinom(35, 1, 0.4)
	x[x != 0] <- seq_len(sum(x != 0)) + seq_len(sum(x != 0)) * 0.11
	dim(x) <- c(5, 7)
	y <- sparse_mat(x)

	expect_equivalent(scale(x), colscale(y)[])
	expect_equivalent(scale(x, scale=FALSE), colscale(y, scale=FALSE)[])
	expect_equivalent(scale(x, center=FALSE), colscale(y, center=FALSE)[])

	expect_equivalent(t(scale(t(x))), rowscale(y)[])
	expect_equivalent(t(scale(t(x), scale=FALSE)), rowscale(y, scale=FALSE)[])
	expect_equivalent(t(scale(t(x), center=FALSE)), rowscale(y, center=FALSE)[])

	cgroup <- as.integer(c(1, 1, 1, 1, 2, 2, 2))
	rgroup <- as.integer(c(1, 1, 1, 2, 2))

	center <- colStats(x, "mean", group=rgroup)
	x2 <- t(sapply(1:nrow(x), function(i) x[i,] - center[,rgroup[i]]))
	x3 <- colscale(y, scale=FALSE, group=rgroup)[]
	expect_equivalent(x2, x3)

	center <- rowStats(x, "mean", group=cgroup)
	x2 <- sapply(1:ncol(x), function(i) x[,i] - center[,cgroup[i]])
	x3 <- rowscale(y, scale=FALSE, group=cgroup)[]
	expect_equivalent(x2, x3)

	scale <- colStats(x^2, "sum", group=rgroup)
	scale <- sqrt(scale / pmax(1, (rep(tabulate(rgroup), each=ncol(x)) - 1)))
	x2 <- t(sapply(1:nrow(x), function(i) x[i,] / scale[,rgroup[i]]))
	x3 <- colscale(y, center=FALSE, group=rgroup)[]
	expect_equivalent(x2, x3)

	scale <- rowStats(x^2, "sum", group=cgroup)
	scale <- sqrt(scale / pmax(1, (rep(tabulate(cgroup), each=nrow(x)) - 1)))
	x2 <- sapply(1:ncol(x), function(i) x[,i] / scale[,cgroup[i]])
	x3 <- rowscale(y, center=FALSE, group=cgroup)[]
	expect_equivalent(x2, x3)

})

