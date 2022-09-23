require(testthat)
require(matter)

context("summary-statistics")

test_that("summary statistics", {

	x <- seq_len(100)

	y <- matter_vec(x, length=100)

	expect_equal(range(x), range(y))

	expect_equal(min(x), min(y))

	expect_equal(max(x), max(y))

	expect_equal(prod(x), prod(y))

	expect_equal(sum(x), sum(y))

	expect_equal(mean(x), mean(y))

	expect_equal(var(x), var(y))

	expect_equal(sd(x), sd(y))

	x <- rep(TRUE, 10)

	y <- matter_vec(x, length=10)

	expect_equal(any(x), any(y))

	expect_equal(all(x), all(y))

	x <- rep(FALSE, 10)

	y <- matter_vec(x, length=10)

	expect_equal(any(x), any(y))

	expect_equal(all(x), all(y))

	x <- c(rep(TRUE, 9), FALSE)

	y <- matter_vec(x, length=10)

	expect_equal(any(x), any(y))

	expect_equal(all(x), all(y))

	x <- c(rep(FALSE, 9), TRUE)

	y <- matter_vec(x, length=10)

	expect_equal(any(x), any(y))

	expect_equal(all(x), all(y))

	x <- matrix(1:100, nrow=10, ncol=10)

	y <- matter_mat(x, nrow=10, ncol=10)

	expect_equal(colSums(x), colSums(y))

	expect_equal(colMeans(x), colMeans(y))

	expect_equal(apply(x, 2, var), colVars(y))

	expect_equal(apply(x, 2, sd), colSds(y))

	expect_equal(rowSums(x), rowSums(y))

	expect_equal(rowMeans(x), rowMeans(y))

	expect_equal(apply(x, 1, var), rowVars(y))

	expect_equal(apply(x, 1, sd), rowSds(y))

})

