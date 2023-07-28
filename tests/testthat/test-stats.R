require(testthat)
require(matter)

context("summary-statistics")

test_that("summary statistics", {

	register(SerialParam())
	set.seed(1)
	x <- sort(100 * runif(100))
	y <- matter_vec(x)

	expect_equal(range(x), range(y))
	expect_equal(min(x), min(y))
	expect_equal(max(x), max(y))
	expect_equal(prod(x), prod(y))
	expect_equal(sum(x), sum(y))
	expect_equal(mean(x), mean(y))
	expect_equal(var(x), var(y))
	expect_equal(sd(x), sd(y))

	x <- rep(TRUE, 10)
	y <- matter_vec(x)

	expect_equal(any(x), any(y))
	expect_equal(all(x), all(y))

	x <- rep(FALSE, 10)
	y <- matter_vec(x)

	expect_equal(any(x), any(y))
	expect_equal(all(x), all(y))

	x <- c(rep(TRUE, 9), FALSE)
	y <- matter_vec(x)

	expect_equal(any(x), any(y))
	expect_equal(all(x), all(y))

	x <- c(rep(FALSE, 9), TRUE)
	y <- matter_vec(x)

	expect_equal(any(x), any(y))
	expect_equal(all(x), all(y))

	register(SerialParam())
	set.seed(1)
	x <- sort(100 * runif(100))
	x <- matrix(x, nrow=10, ncol=10)
	y <- matter_mat(x)

	expect_equal(rowSums(x), rowSums(y))
	expect_equal(rowMeans(x), rowMeans(y))
	expect_equal(colSums(x), colSums(y))
	expect_equal(colMeans(x), colMeans(y))

})

