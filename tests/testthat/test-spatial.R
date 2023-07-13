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

test_that("rowdist + coldist", {

	x <- expand.grid(x=1:4, y=1:4, z=1:2)
	y <- expand.grid(x=1:2, y=1:2, z=1)
	d <- matrix(nrow=nrow(x), ncol=nrow(y))
	for ( i in seq_len(nrow(x)))
		for ( j in seq_len(nrow(y)))
			d[i,j] <- sqrt(sum((x[i,] - y[j,])^2))

	expect_equal(d, rowdist(x, y))
	expect_equal(d, coldist(t(x), t(y)))

	c1 <- expand.grid(x=1:3, y=1:3)
	c2 <- expand.grid(x=1:10, y=1:10)

})
