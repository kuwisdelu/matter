require(testthat)
require(matter)

context("matter-class")

test_that("delta run length encoding", {

	x <- c(1,1,1,1,1,6,7,8,9,10,21,32,33,34,15)

	y <- drle(x)

	expect_equal(x, y[])

	expect_equal(x[1], y[1])

	expect_equal(x[1:15], y[1:15])

	expect_equal(x[15:1], y[15:1])

})

test_that("vector subsetting", {

	x <- seq_len(100)

	y <- matter_vec(x, length=length(x))

	expect_equal(x, y[])

	expect_equal(x[1], y[1])

	expect_equal(x[1:10], y[1:10])

	expect_equal(x[10:1], y[10:1])

	expect_equivalent(as.matter(x), y)

})

test_that("matrix subsetting", {

	x <- matrix(1:100, nrow=10, ncol=10)

	y <- matter_mat(x)

	expect_equal(x, y[])

	expect_equal(x[1,], y[1,])

	expect_equal(x[,1], y[,1])

	expect_equal(x[1,1], y[1,1])

	expect_equal(x[1:10,1:10], y[1:10,1:10])

	expect_equal(x[10:1,10:1], y[10:1,10:1])

	expect_equivalent(as.matter(x), y)

})


test_that("array subsetting", {

	x <- array(1:125, dim=c(5,5,5))

	y <- matter_arr(x)

	expect_equal(x, y[])

	expect_equal(x, y[,,])

	expect_equal(x[1,,], y[1,,])

	expect_equal(x[,1,], y[,1,])

	expect_equal(x[,,1], y[,,1])

	expect_equal(x[1:5,,], y[1:5,,])

	expect_equal(x[,1:5,], y[,1:5,])

	expect_equal(x[,,1:5], y[,,1:5])

	expect_equal(x[1,1,], y[1,1,])

	expect_equal(x[,1,1], y[,1,1])

	expect_equal(x[5:1,5:1,], y[5:1,5:1,])

	expect_equal(x[,5:1,5:1], y[,5:1,5:1])

	expect_equivalent(as.matter(x), y)

})

test_that("list subsetting", {

	x <- list(1:5, 6:10, 11:15)

	y <- matter_list(x)

	expect_equal(x, y[])

	expect_equal(x[1], y[1][])

	expect_equal(x[[1]], y[[1]])

	expect_equal(x[[1]][1:5], y[1,1:5])

	expect_equal(x[[2]][5:1], y[2,5:1])

	expect_equivalent(as.matter(x), y)

})

test_that("string subsetting", {

	x <- c("neon", "genesis", "evangelion")

	y <- matter_str(x)

	expect_equal(x, y[])

	expect_equal(x[1], y[1])

	expect_equal(x[1:3], y[1:3])

	expect_equal(x[3:1], y[3:1])

	expect_equivalent(as.matter(x), y)

})

test_that("data frame subsetting", {

	x <- seq_len(10)

	x2 <- matter_vec(x)

	y <- letters[1:10]

	y2 <- matter_str(y)

	df1 <- data.frame(x=x, y=y, stringsAsFactors=FALSE)

	df2 <- matter_df(x=x2, y=y2)

	expect_equal(df1, df2[])

	expect_equal(df1[1:5,], df2[1:5,])

	expect_equal(df1[,"x"], df2[,"x"][])

	expect_equal(df1[1:5,"x"], df2[1:5,"x"])

	expect_equal(df1$x, df2$x[])

	expect_equal(df1$y[1:5], df2$y[1:5])

	expect_equivalent(as.matter(df1)[], df2[])

})
