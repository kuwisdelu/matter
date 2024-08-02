require(testthat)
require(matter)

context("shared-resources")

test_that("shared_file", {

	name <- tempfile()
	x <- matter_shared_resource(create=name)

	expect_true(name %in% matter_shared_resource_list())
	expect_true(file.exists(name))

	rm(x)
	gc(full=TRUE)

	expect_true(!name %in% matter_shared_resource_list())
	expect_true(!file.exists(name))

})
