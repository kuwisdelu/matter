require(testthat)
require(matter)

context("vectools")

test_that("locmax", {

	x <- c(0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 2, 2, 3, 0, 1)

	names(x) <- seq_along(x)

	m1 <- locmax(x, findLimits=TRUE)

	expect_equal(as.integer(m1), c(4, 7, 13))

	x[11] <- 3

	m2 <- locmax(x, findLimits=TRUE)

	expect_equal(as.integer(m2), c(4, 7, 11))

	expect_equal(attr(m1, "lower"), attr(m2, "lower"))

	expect_equal(attr(m1, "upper"), attr(m2, "upper"))

})

test_that("binvec", {

	set.seed(1)

	x <- runif(50)

	u <- seq(from=1, to=50, by=10)

	v <- seq(from=10, to=50, by=10)

	binfun <- function(x, u, v, fun) {
		mapply(function(i, j) fun(x[i:j]), u, v)
	}

	expect_equal(binvec(x, u, v, "sum"), binfun(x, u, v, sum))

	expect_equal(binvec(x, u, v, "mean"), binfun(x, u, v, mean))

	expect_equal(binvec(x, u, v, "min"), binfun(x, u, v, min))

	expect_equal(binvec(x, u, v, "max"), binfun(x, u, v, max))

	f <- seq(from=1, to=51, by=10)

	expect_equal(binvec(x, u, v), binvec(x, f))

	g <- rep(letters[1:5], each=10)

	expect_equal(binvec(x, g, method="sum"), as.vector(tapply(x, g, sum)))

	expect_equal(binvec(x, g, method="mean"), as.vector(tapply(x, g, mean)))

	expect_equal(binvec(x, g, method="min"), as.vector(tapply(x, g, min)))

	expect_equal(binvec(x, g, method="max"), as.vector(tapply(x, g, max)))

})
