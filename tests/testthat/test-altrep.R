require(testthat)
require(matter)

context("altrep")

test_that("altrep", {

	register(SerialParam())

	x <- seq_len(100)

	y <- matter_vec(x)

	z <- matter:::make_matter_altrep(y)

	expect_equal(head(x), head(z))

	expect_equal(mean(x), mean(z))

})

