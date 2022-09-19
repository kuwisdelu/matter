require(testthat)
require(matter)

context("scale")

test_that("sweep", {

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

	expect_equal(rowsweep(x, 1, a1), rowsweep(y, 1, a1)[])
	expect_equal(rowsweep(x, cgroup, a), rowsweep(y, cgroup, a)[])

	expect_equal(colsweep(x, 1, b1), colsweep(y, 1, b1)[])
	expect_equal(colsweep(x, rgroup, b), colsweep(y, rgroup, b)[])

})

test_that("sparse sweep", {

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

	expect_equal(rowsweep(x, 1, a1), rowsweep(y, 1, a1)[])
	expect_equal(rowsweep(x, cgroup, a), rowsweep(y, cgroup, a)[])

	expect_equal(colsweep(x, 1, b1), colsweep(y, 1, b1)[])
	expect_equal(colsweep(x, rgroup, b), colsweep(y, rgroup, b)[])

})

