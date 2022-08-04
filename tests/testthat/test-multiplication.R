require(testthat)
require(matter)

context("matrix-multiplication")

test_that("matter matrix multiplication", {

	x <- matrix(1:100, nrow=10, ncol=10)

	y <- matter_mat(x, nrow=10, ncol=10)

	expect_equal(x %*% x, x %*% y)

	expect_equal(x %*% x, y %*% x)

	x <- matrix(1:100, nrow=10, ncol=10)

	y <- matter_mat(x, nrow=10, ncol=10, rowMaj=TRUE)

	expect_equal(x %*% x, x %*% y)

	expect_equal(x %*% x, y %*% x)

	x <- matrix(1:91, nrow=7, ncol=13)

	y <- matter_mat(x, nrow=7, ncol=13)

	expect_equal(x %*% t(x), x %*% t(y))

	expect_equal(t(x) %*% x, t(y) %*% x)

	x <- matrix(1:91, nrow=7, ncol=13)

	y <- matter_mat(x, nrow=7, ncol=13, rowMaj=TRUE)

	expect_equal(x %*% t(x), x %*% t(y))

	expect_equal(t(x) %*% x, t(y) %*% x)

})

test_that("sparse matrix multiplication", {

	x <- matrix(rbinom(100, 1, 0.2), nrow=10, ncol=10)

	y <- sparse_old_mat(x, keys=1:10 + (1:10) * 0.11)

	expect_equal(x %*% x, x %*% y)

	expect_equal(x %*% x, y %*% x)

	x <- matrix(rbinom(100, 1, 0.2), nrow=10, ncol=10)

	y <- sparse_old_mat(x, keys=1:10 + (1:10) * 0.11, rowMaj=TRUE)

	expect_equal(x %*% x, x %*% y)

	expect_equal(x %*% x, y %*% x)

	x <- matrix(rbinom(91, 1, 0.2), nrow=7, ncol=13)

	y <- sparse_old_mat(x, keys=1:7 + (1:7) * 0.11)

	expect_equal(x %*% t(x), x %*% t(y))

	expect_equal(t(x) %*% x, t(y) %*% x)

	x <- matrix(rbinom(91, 1, 0.2), nrow=7, ncol=13)

	y <- sparse_old_mat(x, keys=1:13 + (1:13) * 0.11, rowMaj=TRUE)

	expect_equal(x %*% t(x), x %*% t(y))

	expect_equal(t(x) %*% x, t(y) %*% x)

})

test_that("virtual matrix multiplication", {

	x <- matrix(runif(100), nrow=10, ncol=10)

	y <- virtual_mat(x)

	expect_equal(x %*% x, x %*% y)

	expect_equal(x %*% x, y %*% x)

	x <- matrix(runif(100), nrow=10, ncol=10)

	y <- virtual_mat(x, rowMaj=TRUE)

	expect_equal(x %*% x, x %*% y)

	expect_equal(x %*% x, y %*% x)

	x <- matrix(runif(91), nrow=7, ncol=13)

	y <- virtual_mat(x)

	expect_equal(x %*% t(x), x %*% t(y))

	expect_equal(t(x) %*% x, t(y) %*% x)

	x <- matrix(runif(91), nrow=7, ncol=13)

	y <- virtual_mat(x, rowMaj=TRUE)

	expect_equal(x %*% t(x), x %*% t(y))

	expect_equal(t(x) %*% x, t(y) %*% x)

})
