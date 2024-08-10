require(testthat)
require(matter)

context("shared-resources")

test_that("uuid", {

	set.seed(1, kind="default")
	oseed0 <- getRNGStream()
	id0 <- uuid()
	seed0 <- getRNGStream()

	expect_equal(seed0, oseed0)

	set.seed(1, kind="default")
	oseed1 <- getRNGStream()
	id1 <- uuid()
	seed1 <- getRNGStream()

	expect_equal(seed1, oseed1)

	expect_true(identical(seed0, seed1))
	expect_false(identical(id0, id1))

})

test_that("shared_file", {

	name <- tempfile()
	x <- matter_shared_resource(create=name)
	y <- x

	expect_error(matter_shared_resource(create=name))

	expect_true(name %in% matter_shared_resource_list())
	expect_true(file.exists(name))

	rm(x)
	gc(full=TRUE)

	expect_true(name %in% matter_shared_resource_list())
	expect_true(file.exists(name))

	rm(y)
	gc(full=TRUE)

	expect_false(name %in% matter_shared_resource_list())
	expect_false(file.exists(name))

})

test_that("shared_file - matter_arr", {

	set.seed(1, kind="default")
	u <- runif(100)
	x <- matter_vec(u, path=NULL)
	p <- path(x)

	expect_equal(x[], u)
	expect_true(file.exists(p))

	y <- x[1:10,drop=NULL]

	expect_equal(y[], u[1:10])
	expect_true(file.exists(p))

	rm(x)
	gc(full=TRUE)

	expect_true(file.exists(p))

	rm(y)
	gc(full=TRUE)

	expect_false(file.exists(p))

})

test_that("shared_file - matter_list", {

	l <- list(x=1:10, y=runif(10), z="hello, world!")
	x <- matter_list(l, path=NULL)
	p <- path(x)

	expect_equal(x[], l)
	expect_true(file.exists(p))

	y <- x[1:2,drop=NULL]

	expect_equal(y[], l[1:2])
	expect_true(file.exists(p))

	rm(x)
	gc(full=TRUE)

	expect_true(file.exists(p))

	rm(y)
	gc(full=TRUE)

	expect_false(file.exists(p))

})

test_that("shared_memory", {

	name <- tempmem()
	x <- matter_shared_resource(create=name)
	y <- x

	expect_error(matter_shared_resource(create=name))

	expect_true(name %in% matter_shared_resource_list())

	rm(x)
	gc(full=TRUE)

	expect_true(name %in% matter_shared_resource_list())

	rm(y)
	gc(full=TRUE)

	expect_false(name %in% matter_shared_resource_list())

})

test_that("shared_memory - matter_arr", {

	set.seed(1, kind="default")
	u <- runif(100)
	x <- matter_vec(u, path=":memory:")

	expect_equal(x[], u)
	expect_length(matter_shared_resource_list(), 1L)

	y <- x[1:10,drop=NULL]

	expect_equal(y[], u[1:10])
	expect_length(matter_shared_resource_list(), 1L)

	rm(x)
	gc(full=TRUE)

	expect_length(matter_shared_resource_list(), 1L)

	rm(y)
	gc(full=TRUE)

	expect_length(matter_shared_resource_list(), 0L)

})

test_that("shared_memory - matter_list", {

	l <- list(x=1:10, y=runif(10), z="hello, world!")
	x <- matter_list(l, path=":memory:")

	expect_equal(x[], l)
	expect_length(matter_shared_resource_list(), 1L)

	y <- x[1:2,drop=NULL]

	expect_equal(y[], l[1:2])
	expect_length(matter_shared_resource_list(), 1L)

	rm(x)
	gc(full=TRUE)

	expect_length(matter_shared_resource_list(), 1L)

	rm(y)
	gc(full=TRUE)

	expect_length(matter_shared_resource_list(), 0L)

})
