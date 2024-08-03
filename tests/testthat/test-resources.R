require(testthat)
require(matter)

context("shared-resources")

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

	expect_true(!name %in% matter_shared_resource_list())
	expect_true(!file.exists(name))

})

test_that("shared_file - matter_arr", {

	set.seed(1)
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

	expect_true(!file.exists(p))

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

	expect_true(!file.exists(p))

})
