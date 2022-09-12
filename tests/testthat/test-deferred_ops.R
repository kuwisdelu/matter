require(testthat)
require(matter)

context("delayed-ops")

test_that("delayed ops - integers", {

	x <- seq_len(10)
	y <- matter_vec(x)

	expect_equal(x + 1, (y + 1)[])

})

test_that("delayed ops - matrix", {

	set.seed(1)
	vals <- sort(round(10 * runif(35), 2))
	x <- matrix(vals, nrow=5, ncol=7)
	y <- matter_mat(x)

	expect_equal(x + 1, (y + 1)[])
	expect_equal(x + 5:1, (y + 5:1)[])
	expect_equal(x + as.matrix(5:1), (y + as.matrix(5:1))[])
	expect_equal(x + t(7:1), (y + t(7:1))[])

})

