require(testthat)
require(matter)

context("matter")

test_that("matter constructor", {

	x0 <- matter(as.raw(1:10))

	expect_is(x0, "matter_vec")
	expect_equal(as.character(type(x0)), "raw")

	x1 <- matter(rep.int(c(TRUE, FALSE), 5L))

	expect_is(x1, "matter_vec")
	expect_equal(as.character(type(x1)), "logical")

	x2 <- matter(1:10)

	expect_is(x2, "matter_vec")
	expect_equal(as.character(type(x2)), "integer")

	x3 <- matter(as.double(1:10))

	expect_is(x3, "matter_vec")
	expect_equal(as.character(type(x3)), "double")

	x4 <- matter(matrix(1:9, nrow=3, ncol=3))

	expect_is(x4, "matter_mat")
	expect_equal(as.character(type(x4)), "integer")

	x5 <- matter(matrix(as.double(1:9), nrow=3, ncol=3))

	expect_is(x5, "matter_mat")
	expect_equal(as.character(type(x5)), "double")

	x6 <- matter(array(1:27, dim=c(3,3,3)))

	expect_is(x6, "matter_arr")
	expect_equal(as.character(type(x6)), "integer")

	x7 <- matter(array(as.double(1:27), dim=c(3,3,3)))

	expect_is(x7, "matter_arr")
	expect_equal(as.character(type(x7)), "double")

	x8 <- matter(list(1:3, c(1.11, 2.22, 3.33), c("hello world!")))

	expect_is(x8, "matter_list")
	expect_equal(as.character(type(x8)),
		c("integer", "double", "character"))

	x9 <- matter(factor(rep.int(c("pos", "neg"), 5L)))

	expect_is(x9, "matter_fct")
	expect_equal(as.character(type(x9)), "integer")

	x10 <- matter(c("neon", "genesis", "evangelion"))

	expect_is(x10, "matter_str")
	expect_equal(as.character(type(x10)), rep.int("character", 3L))

	y <- matter(1:10)
	z1 <- as.matter(y)
	z2 <- matter(y)

	expect_true(all(path(y) == path(z1)))
	expect_true(all(path(y) != path(z2)))

})
