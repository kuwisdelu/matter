require(testthat)
require(matter)

context("spatial")

test_that("point in poly", {

	poly <- data.frame(
		x=c(3,5,5,3),
		y=c(3,3,5,5))
	xy <- data.frame(
		x=c(4,6,4,2,3,5,5,3,4,5,4,4,
			3.5,4.5,4.0,3.5),
		y=c(2,4,6,4,3,3,5,5,3,4,5,3,
			4.0,4.0,4.5,4.0),
		ref=c(
			rep("out", 4),
			rep("vertex", 4),
			rep("edge", 4),
			rep("in", 4)))

	xy$test <- inpoly(xy[,1:2], poly)
	
	inside <- which(xy$test)
	outside <- which(!xy$test)

	expect_setequal(inside, which(!xy$ref %in% "out"))
	expect_setequal(outside, which(xy$ref %in% "out"))

})
