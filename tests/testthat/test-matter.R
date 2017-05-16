require(testthat)

context("matter-class")

test_that("delta run length encoding", {

	x <- c(1,1,1,1,1,6,7,8,9,10,21,32,33,34,15)

	y <- drle(x)

	expect_equal(x, y[])

	expect_equal(x[1], y[1])

	expect_equal(x[1:15], y[1:15])

})

test_that("vector subsetting", {

	x <- seq_len(100)

	y <- matter_vec(x, length=length(x))

	expect_equal(x, y[])

	expect_equal(x[1], y[1])

	expect_equal(x[1:10], y[1:10])

})

test_that("matrix subsetting", {

	x <- matrix(1:100, nrow=10, ncol=10)

	y <- matter_mat(x, nrow=10, ncol=10)

	expect_equal(x, y[])

	expect_equal(x[1,], y[1,])

	expect_equal(x[,1], y[,1])

	expect_equal(x[1,1], y[1,1])

	expect_equal(x[1:10,1:10], y[1:10,1:10])

})

test_that("matrix multiplication", {

	x <- matrix(1:100, nrow=10, ncol=10)

	y <- matter_mat(x, nrow=10, ncol=10)

	expect_equal(x %*% x, x %*% y)

	expect_equal(x %*% x, y %*% x)

	x <- matrix(1:100, nrow=10, ncol=10)

	y <- matter_mat(x, nrow=10, ncol=10, rowMaj=TRUE)

	expect_equal(x %*% x, x %*% y)

	expect_equal(x %*% x, y %*% x)

})

