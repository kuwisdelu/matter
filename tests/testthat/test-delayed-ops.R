require(testthat)
require(matter)

context("delayed-ops")

test_that("delayed ops - integers", {

	x <- seq_len(10)

	y <- matter_vec(x, length=10)

	expect_equal(x + 1, (y + 1)[])

	expect_equal(2 * x + 1, (2 * y + 1)[])

	expect_equal(x / 2 - 1, (y / 2 - 1)[])

	expect_equal(x %% 2, (y %% 2)[])

	expect_equal(x %/% 2, (y %/% 2)[])

	expect_equal(log(x), log(x)[])

	expect_equal(exp(x), exp(x)[])

	expect_equal(x > 5, (y > 5)[])

	expect_equal(x >= 5, (y >= 5)[])

	expect_equal(x < 5, (y < 5)[])

	expect_equal(x <= 5, (y <= 5)[])

	expect_equal(x != 5, (y != 5)[])

	expect_equal(x > 2 & x < 8, (y > 2 & y < 8)[])

	expect_equal(x > 2 | x < 8, (y > 2 | y < 8)[])

	expect_equal(which(x == 5), which(y == 5))

})

test_that("delayed ops - doubles", {

	x <- seq_len(10) + 0.5

	y <- matter_vec(x, length=10)

	expect_equal(x + 1.1, (y + 1.1)[])

	expect_equal(2.2 * x + 1.1, (2.2 * y + 1.1)[])

	expect_equal(x / 2.2 - 1.1, (y / 2.2 - 1.1)[])

	expect_equal(x %% 2.2, (y %% 2.2)[])

	expect_equal(x %/% 2.2, (y %/% 2.2)[])

	expect_equal(log(x), log(x)[])

	expect_equal(exp(x), exp(x)[])

	expect_equal(x > 5.5, (y > 5.5)[])

	expect_equal(x >= 5.5, (y >= 5.5)[])

	expect_equal(x < 5.5, (y < 5.5)[])

	expect_equal(x <= 5.5, (y <= 5.5)[])

	expect_equal(x != 5.5, (y != 5.5)[])

	expect_equal(x > 2 & x < 8, (y > 2 & y < 8)[])

	expect_equal(x > 2 | x < 8, (y > 2 | y < 8)[])

	expect_equal(which(x == 5.5), which(y == 5.5))

})

test_that("delayed ops - factors", {

	x <- letters[1:5]

	y <- matter_fc(x)

	expect_equal(x == "a", (y == "a")[])

	expect_equal(x != "a", (y != "a")[])

	expect_equal(x == x, (y == y)[])

	expect_equal(x != x, (y != y)[])

	expect_equal(x == x, (y == x)[])

	expect_equal(x != x, (y != x)[])

	expect_equal(x == factor(x), (y == factor(x))[])

	expect_equal(x != factor(x), (y != factor(x))[])

	expect_equal(which(x == "b"), which(y == "b"))

})
