require(testthat)
require(matter)

context("altrep")

register(SerialParam())

test_that("altrep raw", {

	x <- charToRaw("hello, world!")

	y <- matter_vec(x)

	z <- matter:::as.altrep(y)

	expect_equal(head(x), head(z))

	s <- serialize(z, NULL)

	u <- unserialize(s)

	expect_equal(head(x), head(u))

})

test_that("altrep logical", {

	x <- c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)

	y <- matter_vec(x)

	z <- matter:::as.altrep(y)

	expect_equal(head(x), head(z))

	expect_equal(sum(x), sum(z))

	s <- serialize(z, NULL)

	u <- unserialize(s)

	expect_equal(head(x), head(u))

})

test_that("altrep integer", {

	x <- seq_len(100)

	y <- matter_vec(x)

	z <- matter:::as.altrep(y)

	expect_equal(head(x), head(z))

	expect_equal(sum(x), sum(z))

	s <- serialize(z, NULL)

	u <- unserialize(s)

	expect_equal(head(x), head(u))

})

test_that("altrep real", {

	x <- as.numeric(seq_len(100))

	y <- matter_vec(x)

	z <- matter:::as.altrep(y)

	expect_equal(head(x), head(z))

	expect_equal(sum(x), sum(z))

	s <- serialize(z, NULL)

	u <- unserialize(s)

	expect_equal(head(x), head(u))

})

test_that("altrep matrix", {

	x <- matrix(1:100, nrow=10, ncol=10)

	y <- matter_mat(x)

	z <- matter:::as.altrep(y)

	expect_equal(x[1:3,1:3], z[1:3,1:3])

	expect_equal(sum(x), sum(z))

	s <- serialize(z, NULL)

	u <- unserialize(s)

	expect_equal(head(x), head(u))

	expect_equal(dim(x), dim(u))

})

test_that("altrep array", {

	x <- array(1:125, dim=c(5,5,5))

	y <- matter_arr(x)

	z <- matter:::as.altrep(y)

	expect_equal(x[1:3,1:3,1:3], z[1:3,1:3,1:3])

	expect_equal(sum(x), sum(z))

	s <- serialize(z, NULL)

	u <- unserialize(s)

	expect_equal(head(x), head(u))

	expect_equal(dim(x), dim(u))

})


test_that("factor subsetting", {

	x <- factor(c("neon", "genesis", "evangelion"))

	y <- matter_fc(x)

	z <- matter:::as.altrep(y)

	expect_equal(head(x), head(z))

	expect_equal(levels(x), levels(z))

})

