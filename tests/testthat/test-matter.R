require(testthat)
require(matter)

context("matter")

test_that("as.matter", {

	x0 <- as.matter(as.raw(1:10))

	expect_is(x0, "matter_vec")
	expect_equal(as.character(type(x0)), "raw")

	x1 <- as.matter(rep.int(c(TRUE, FALSE), 5L))

	expect_is(x1, "matter_vec")
	expect_equal(as.character(type(x1)), "logical")

	x2 <- as.matter(1:10)

	expect_is(x2, "matter_vec")
	expect_equal(as.character(type(x2)), "integer")

	x3 <- as.matter(as.double(1:10))

	expect_is(x3, "matter_vec")
	expect_equal(as.character(type(x3)), "double")

	x4 <- as.matter(matrix(1:9, nrow=3, ncol=3))

	expect_is(x4, "matter_mat")
	expect_equal(as.character(type(x4)), "integer")

	x5 <- as.matter(matrix(as.double(1:9), nrow=3, ncol=3))

	expect_is(x5, "matter_mat")
	expect_equal(as.character(type(x5)), "double")

	x6 <- as.matter(array(1:27, dim=c(3,3,3)))

	expect_is(x6, "matter_arr")
	expect_equal(as.character(type(x6)), "integer")

	x7 <- as.matter(array(as.double(1:27), dim=c(3,3,3)))

	expect_is(x7, "matter_arr")
	expect_equal(as.character(type(x7)), "double")

	x8 <- as.matter(list(1:3, c(1.11, 2.22, 3.33), c("hello world!")))

	expect_is(x8, "matter_list")
	expect_equal(as.character(type(x8)),
		c("integer", "double", "character"))

	x9 <- as.matter(factor(rep.int(c("pos", "neg"), 5L)))

	expect_is(x9, "matter_fct")
	expect_equal(as.character(type(x9)), "integer")

	x10 <- as.matter(c("neon", "genesis", "evangelion"))

	expect_is(x10, "matter_str")
	expect_equal(as.character(type(x10)), rep.int("character", 3L))

	y <- as.matter(1:10)
	z1 <- as.matter(y)
	z2 <- matter(y)

	expect_true(all(path(y) == path(z1)))
	expect_true(all(path(y) != path(z2)))

})

test_that("as.shared", {

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

	y <- as.shared(1:10)
	z1 <- as.shared(y)
	z2 <- matter(y, path=":memory:")

	expect_true(all(path(y) == path(z1)))
	expect_true(all(path(y) != path(z2)))
	expect_true(is.shared(z1))
	expect_true(is.shared(z2))

})

test_that("fetch/flash - base", {

	register(SerialParam())

	x0 <- as.raw(1:10)
	x0a <- fetch(x0)
	x0b <- flash(x0)

	expect_equal(x0a[], x0[])
	expect_equal(x0b[], x0[])
	expect_true(is.shared(x0a))
	expect_true(is.matter(x0b) && !is.shared(x0b))

	x1 <- rep.int(c(TRUE, FALSE), 5L)
	x1a <- fetch(x1)
	x1b <- flash(x1)

	expect_equal(x1a[], x1[])
	expect_equal(x1b[], x1[])
	expect_true(is.shared(x1a))
	expect_true(is.matter(x1b) && !is.shared(x1b))

	x2 <- 1:10
	x2a <- fetch(x2)
	x2b <- flash(x2)

	expect_equal(x2a[], x2[])
	expect_equal(x2b[], x2[])
	expect_true(is.shared(x2a))
	expect_true(is.matter(x2b) && !is.shared(x2b))

	x3 <- as.double(1:10)
	x3a <- fetch(x3)
	x3b <- flash(x3)

	expect_equal(x3a[], x3[])
	expect_equal(x3b[], x3[])
	expect_true(is.shared(x3a))
	expect_true(is.matter(x3b) && !is.shared(x3b))

	x4 <- matrix(1:9, nrow=3, ncol=3)
	x4a <- fetch(x4)
	x4b <- flash(x4)

	expect_equal(x4a[], x4[])
	expect_equal(x4b[], x4[])
	expect_true(is.shared(x4a))
	expect_true(is.matter(x4b) && !is.shared(x4b))

	x5 <- matrix(as.double(1:9), nrow=3, ncol=3)
	x5a <- fetch(x5)
	x5b <- flash(x5)

	expect_equal(x5a[], x5[])
	expect_equal(x5b[], x5[])
	expect_true(is.shared(x5a))
	expect_true(is.matter(x5b) && !is.shared(x5b))

	x6 <- array(1:27, dim=c(3,3,3))
	x6a <- fetch(x6)
	x6b <- flash(x6)

	expect_equal(x6a[], x6[])
	expect_equal(x6b[], x6[])
	expect_true(is.shared(x6a))
	expect_true(is.matter(x6b) && !is.shared(x6b))

	x7 <- array(as.double(1:27), dim=c(3,3,3))
	x7a <- fetch(x7)
	x7b <- flash(x7)

	expect_equal(x7a[], x7[])
	expect_equal(x7b[], x7[])
	expect_true(is.shared(x7a))
	expect_true(is.matter(x7b) && !is.shared(x7b))

	x8 <- list(1:3, c(1.11, 2.22, 3.33), c("hello world!"))
	x8a <- fetch(x8)
	x8b <- flash(x8)

	expect_equal(x8a[], x8[])
	expect_equal(x8b[], x8[])
	expect_true(is.shared(x8a))
	expect_true(is.matter(x8b) && !is.shared(x8b))

	x9 <- as.matter(factor(rep.int(c("pos", "neg"), 5L)))
	x9a <- fetch(x9)
	x9b <- flash(x9)

	expect_equal(x9a[], x9[])
	expect_equal(x9b[], x9[])
	expect_true(is.shared(x9a))
	expect_true(is.matter(x9b) && !is.shared(x9b))

	x10 <- as.matter(c("neon", "genesis", "evangelion"))
	x10a <- fetch(x10)
	x10b <- flash(x10)

	expect_equal(x10a[], x10[])
	expect_equal(x10b[], x10[])
	expect_true(is.shared(x10a))
	expect_true(is.matter(x10b) && !is.shared(x10b))

})

test_that("fetch/flash - matter", {

	register(SerialParam())

	x0 <- as.matter(as.raw(1:10))
	x0a <- fetch(x0)
	x0b <- flash(x0)

	expect_equal(x0a[], x0[])
	expect_equal(x0b[], x0[])
	expect_true(is.shared(x0a))
	expect_true(is.matter(x0b) && !is.shared(x0b))

	x1 <- as.matter(rep.int(c(TRUE, FALSE), 5L))
	x1a <- fetch(x1)
	x1b <- flash(x1)

	expect_equal(x1a[], x1[])
	expect_equal(x1b[], x1[])
	expect_true(is.shared(x1a))
	expect_true(is.matter(x1b) && !is.shared(x1b))

	x2 <- as.matter(1:10)
	x2a <- fetch(x2)
	x2b <- flash(x2)

	expect_equal(x2a[], x2[])
	expect_equal(x2b[], x2[])
	expect_true(is.shared(x2a))
	expect_true(is.matter(x2b) && !is.shared(x2b))

	x3 <- as.matter(as.double(1:10))
	x3a <- fetch(x3)
	x3b <- flash(x3)

	expect_equal(x3a[], x3[])
	expect_equal(x3b[], x3[])
	expect_true(is.shared(x3a))
	expect_true(is.matter(x3b) && !is.shared(x3b))

	x4 <- as.matter(matrix(1:9, nrow=3, ncol=3))
	x4a <- fetch(x4)
	x4b <- flash(x4)

	expect_equal(x4a[], x4[])
	expect_equal(x4b[], x4[])
	expect_true(is.shared(x4a))
	expect_true(is.matter(x4b) && !is.shared(x4b))

	x5 <- as.matter(matrix(as.double(1:9), nrow=3, ncol=3))
	x5a <- fetch(x5)
	x5b <- flash(x5)

	expect_equal(x5a[], x5[])
	expect_equal(x5b[], x5[])
	expect_true(is.shared(x5a))
	expect_true(is.matter(x5b) && !is.shared(x5b))

	x6 <- as.matter(array(1:27, dim=c(3,3,3)))
	x6a <- fetch(x6)
	x6b <- flash(x6)

	expect_equal(x6a[], x6[])
	expect_equal(x6b[], x6[])
	expect_true(is.shared(x6a))
	expect_true(is.matter(x6b) && !is.shared(x6b))

	x7 <- as.matter(array(as.double(1:27), dim=c(3,3,3)))
	x7a <- fetch(x7)
	x7b <- flash(x7)

	expect_equal(x7a[], x7[])
	expect_equal(x7b[], x7[])
	expect_true(is.shared(x7a))
	expect_true(is.matter(x7b) && !is.shared(x7b))

	x8 <- as.matter(list(1:3, c(1.11, 2.22, 3.33), c("hello world!")))
	x8a <- fetch(x8)
	x8b <- flash(x8)

	expect_equal(x8a[], x8[])
	expect_equal(x8b[], x8[])
	expect_true(is.shared(x8a))
	expect_true(is.matter(x8b) && !is.shared(x8b))

	x9 <- as.matter(factor(rep.int(c("pos", "neg"), 5L)))
	x9a <- fetch(x9)
	x9b <- flash(x9)

	expect_equal(x9a[], x9[])
	expect_equal(x9b[], x9[])
	expect_true(is.shared(x9a))
	expect_true(is.matter(x9b) && !is.shared(x9b))

	x10 <- as.matter(c("neon", "genesis", "evangelion"))
	x10a <- fetch(x10)
	x10b <- flash(x10)

	expect_equal(x10a[], x10[])
	expect_equal(x10b[], x10[])
	expect_true(is.shared(x10a))
	expect_true(is.matter(x10b) && !is.shared(x10b))

})

