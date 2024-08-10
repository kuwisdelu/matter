require(testthat)
require(matter)

context("shared")

test_that("shared coercion", {

	x0 <- as.shared(as.raw(1:10))

	expect_is(x0, "matter_vec")
	expect_equal(as.character(type(x0)), "raw")
	expect_true(is.shared(x0))

	x1 <- as.shared(rep.int(c(TRUE, FALSE), 5L))

	expect_is(x1, "matter_vec")
	expect_equal(as.character(type(x1)), "logical")
	expect_true(is.shared(x1))

	x2 <- as.shared(1:10)

	expect_is(x2, "matter_vec")
	expect_equal(as.character(type(x2)), "integer")
	expect_true(is.shared(x2))

	x3 <- as.shared(as.double(1:10))

	expect_is(x3, "matter_vec")
	expect_equal(as.character(type(x3)), "double")
	expect_true(is.shared(x3))

	x4 <- as.shared(matrix(1:9, nrow=3, ncol=3))

	expect_is(x4, "matter_mat")
	expect_equal(as.character(type(x4)), "integer")
	expect_true(is.shared(x4))

	x5 <- as.shared(matrix(as.double(1:9), nrow=3, ncol=3))

	expect_is(x5, "matter_mat")
	expect_equal(as.character(type(x5)), "double")
	expect_true(is.shared(x5))

	x6 <- as.shared(array(1:27, dim=c(3,3,3)))

	expect_is(x6, "matter_arr")
	expect_equal(as.character(type(x6)), "integer")
	expect_true(is.shared(x6))

	x7 <- as.shared(array(as.double(1:27), dim=c(3,3,3)))

	expect_is(x7, "matter_arr")
	expect_equal(as.character(type(x7)), "double")
	expect_true(is.shared(x7))

	x8 <- as.shared(list(1:3, c(1.11, 2.22, 3.33), c("hello world!")))

	expect_is(x8, "matter_list")
	expect_equal(as.character(type(x8)),
		c("integer", "double", "character"))
	expect_true(is.shared(x8))

	x9 <- as.shared(factor(rep.int(c("pos", "neg"), 5L)))

	expect_is(x9, "matter_fct")
	expect_equal(as.character(type(x9)), "integer")
	expect_true(is.shared(x9))

	x10 <- as.shared(c("neon", "genesis", "evangelion"))

	expect_is(x10, "matter_str")
	expect_equal(as.character(type(x10)), rep.int("character", 3L))
	expect_true(is.shared(x10))

	y <- matter(1:10, path=":memory:")
	z1 <- as.shared(y)
	z2 <- matter(y, path=":memory:")

	expect_true(all(path(y) == path(z1)))
	expect_true(all(path(y) != path(z2)))
	expect_true(is.shared(z1))
	expect_true(is.shared(z2))

})
