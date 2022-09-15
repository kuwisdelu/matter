require(testthat)
require(matter)

context("scale")

test_that("sweep", {

	set.seed(1)
	vals <- sort(round(10 * runif(35), 2))
	x <- matrix(vals, nrow=5, ncol=7)
	y <- matter_arr(x)

	a1 <- 1:5
	b1 <- 1:7
	a2 <- 101:105
	b2 <- 101:107
	a <- cbind(a1, a2)
	b <- cbind(b1, b2)
	cg <- as.integer(c(1, 1, 1, 1, 2, 2, 2))
	rg <- as.integer(c(1, 1, 1, 2, 2))

	expect_equal(rowsweep(x, 1, a1), rowsweep(y, 1, a1)[])
	expect_equal(rowsweep(x, cg, a), rowsweep(y, cg, a)[])

	expect_equal(colsweep(x, 1, b1), colsweep(y, 1, b1)[])
	expect_equal(colsweep(x, rg, b), colsweep(y, rg, b)[])

})

