require(testthat)
require(matter)

context("apply")

test_that("apply", {

	register(SerialParam())

	x <- matrix(1:100, nrow=10, ncol=10)

	y <- matter_mat(x, nrow=10, ncol=10)

	expect_equal(apply(x, 1, sum), apply(y, 1, sum))

	expect_equal(apply(x, 2, sum), apply(y, 2, sum))

	x <- list(a=1:10, b=11:20, c=21:30)

	y <- matter_list(x, names=names(x))

	expect_equal(lapply(x, sum), lapply(y, sum))

	expect_equal(sapply(x, sum), sapply(y, sum))

})

