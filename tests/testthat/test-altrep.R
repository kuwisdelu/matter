require(testthat)
require(matter)

context("altrep")

register(SerialParam())

test_that("altrep integer", {

	x <- seq_len(100)

	y <- matter_vec(x)

	z <- matter:::as.altrep(y)

	expect_equal(head(x), head(z))

	expect_equal(mean(x), mean(z))

})

test_that("altrep real", {

	x <- as.numeric(seq_len(100))

	y <- matter_vec(x)

	z <- matter:::as.altrep(y)

	expect_equal(head(x), head(z))

	expect_equal(mean(x), mean(z))

})

