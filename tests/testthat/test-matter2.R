require(testthat)
require(matter)

context("matter-class")

test_that("matter array 1-D indexing", {

	set.seed(1)
	x <- sort(round(10 * runif(10), 2))
	y <- matter2_arr(x)

	expect_equal(x[1], y[1])
	expect_equal(x[1:10], y[1:10])
	expect_equal(x[10:1], y[10:1])
	expect_equal(x[c(1,3,5,7)], y[c(1,3,5,7)])
	expect_equal(x[c(7,5,3,1)], y[c(7,5,3,1)])
	expect_equal(x[c(1,NA,2,NA,3)], y[c(1,NA,2,NA,3)])

	x[1:5] <- x[1:5] + 100
	y[1:5] <- y[1:5] + 100

	expect_equal(x[], y[])
	expect_equal(x[1], y[1])
	expect_equal(x[1:10], y[1:10])
	expect_equal(x[10:1], y[10:1])

	x[c(1,3,5)] <- NA
	y[c(1,3,5)] <- NA

	expect_equal(x[1], y[1])
	expect_equal(x[1:10], y[1:10])
	expect_equal(x[10:1], y[10:1])

	set.seed(1)
	x <- sort(round(10 * runif(10), 2))
	y <- matter2_arr(x, rowMaj=TRUE)

	expect_equal(x[1], y[1])
	expect_equal(x[1:10], y[1:10])
	expect_equal(x[10:1], y[10:1])

})

test_that("matter array N-D indexing", {

	set.seed(1)
	vals <- sort(round(10 * runif(35), 2))
	x <- matrix(vals, nrow=5, ncol=7)
	y <- matter2_arr(x)

	expect_equal(x, y[])
	expect_equal(x[1,], y[1,])
	expect_equal(x[,1], y[,1])
	expect_equal(x[5,], y[5,])
	expect_equal(x[,7], y[,7])
	expect_equal(x[1:3,1:4], y[1:3,1:4])
	expect_equal(x[3:1,4:1], y[3:1,4:1])
	expect_equal(x[,c(1,NA,7)], y[,c(1,NA,7)])

	x[1,] <- x[1,] + 100
	y[1,] <- y[1,] + 100
	
	expect_equal(x, y[])

	x[,1] <- x[,1] + -100
	y[,1] <- y[,1] + -100
	
	expect_equal(x, y[])

	x[1,1] <- NA
	y[1,1] <- NA

	expect_equal(x, y[])

	set.seed(1)
	vals <- sort(round(10 * runif(24), 2))
	x <- array(vals, dim=c(4,3,2))
	y <- matter2_arr(x)
	i <- 1:2

	expect_equal(x, y[])
	expect_equal(x[1,,], y[1,,])
	expect_equal(x[,1,], y[,1,])
	expect_equal(x[,,1], y[,,1])
	expect_equal(x[4,,], y[4,,])
	expect_equal(x[,3,], y[,3,])
	expect_equal(x[,,2], y[,,2])
	expect_equal(x[i,i,], y[i,i,])
	expect_equal(x[,i,i], y[,i,i])

	x[,,1] <- x[,,1] + 100
	y[,,1] <- y[,,1] + 100

	expect_equal(x, y[])

})

test_that("matter array N-D indexing - rowMaj", {

	set.seed(1)
	vals <- sort(round(10 * runif(35), 2))
	x <- matrix(vals, nrow=5, ncol=7)
	y <- matter2_arr(x, rowMaj=TRUE)

	expect_equal(x, y[])
	expect_equal(x[1,], y[1,])
	expect_equal(x[,1], y[,1])
	expect_equal(x[5,], y[5,])
	expect_equal(x[,7], y[,7])
	expect_equal(x[1:3,1:4], y[1:3,1:4])
	expect_equal(x[3:1,4:1], y[3:1,4:1])
	expect_equal(x[,c(1,NA,7)], y[,c(1,NA,7)])

	x[1,] <- x[1,] + 100
	y[1,] <- y[1,] + 100
	
	expect_equal(x, y[])

	x[,1] <- x[,1] + -100
	y[,1] <- y[,1] + -100
	
	expect_equal(x, y[])

	x[1,1] <- NA
	y[1,1] <- NA

	expect_equal(x, y[])

	set.seed(1)
	vals <- sort(round(10 * runif(24), 2))
	x <- array(vals, dim=c(4,3,2))
	y <- matter2_arr(x, rowMaj=TRUE)
	i <- 1:2

	expect_equal(x, y[])
	expect_equal(x[1,,], y[1,,])
	expect_equal(x[,1,], y[,1,])
	expect_equal(x[,,1], y[,,1])
	expect_equal(x[4,,], y[4,,])
	expect_equal(x[,3,], y[,3,])
	expect_equal(x[,,2], y[,,2])
	expect_equal(x[i,i,], y[i,i,])
	expect_equal(x[,i,i], y[,i,i])

	x[,,1] <- x[,,1] + 100
	y[,,1] <- y[,,1] + 100

	expect_equal(x, y[])

})


