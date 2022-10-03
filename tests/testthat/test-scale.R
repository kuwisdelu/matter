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

	expect_equal(rowsweep(x, a1), rowsweep(y, a1)[])
	expect_equal(
		rowsweep(x, a, group=cgroup),
		rowsweep(y, a, group=cgroup)[])

	expect_equal(colsweep(x, b1), colsweep(y, b1)[])
	expect_equal(
		colsweep(x, b, group=rgroup),
		colsweep(y, b, group=rgroup)[])

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

	expect_equal(rowsweep(x, a1), rowsweep(y, a1)[])
	expect_equal(
		rowsweep(x, a, group=cgroup),
		rowsweep(y, a, group=cgroup)[])

	expect_equal(colsweep(x, b1), colsweep(y, b1)[])
	expect_equal(
		colsweep(x, b, group=rgroup),
		colsweep(y, b, group=rgroup)[])

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

	colscale(x, group=rgroup) # FIXME: test me!

})

