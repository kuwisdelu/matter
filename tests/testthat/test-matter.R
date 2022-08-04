require(testthat)
require(matter)

context("matter-classes")

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

	expect_equal(x[1:5,1:5], y[1:5,1:5])

	expect_equal(x[5:1,5:1], y[5:1,5:1])

	expect_equivalent(as.matter(x), y)

})

test_that("matrix binding", {

	x1 <- matrix(1:100, nrow=10, ncol=10)

	rownames(x1) <- paste0("row", 1:nrow(x1))

	colnames(x1) <- paste0("col", 1:ncol(x1))

	x2 <- x1[,1:5]

	x3 <- x1[1:5,]

	y1 <- matter_mat(x1, dimnames=dimnames(x1))

	y2 <- matter_mat(x2, dimnames=dimnames(x2))

	y3 <- matter_mat(x3, dimnames=dimnames(x3))

	expect_equal(cbind(y1, y2)[], cbind(x1, x2))

	expect_equal(rbind(y1, y3)[], rbind(x1, x3))

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

	x <- list(a=c(TRUE, FALSE), b=1:5, c=c(1.11, 2.22))

	y <- matter_list(x, names=names(x))

	expect_equal(x, y[])

	expect_equal(x[1], y[1])

	expect_equal(x[[1]], y[[1]])

	expect_equal(x[1:2], y[1:2])

	expect_equal(x[[2]][1:5], y[[2,1:5]])

	expect_equal(x[[2]][5:1], y[[2,5:1]])

	expect_equal(x[[3]], y[[3]])

	expect_equal(x$b, y$b)

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

test_that("factor subsetting", {

	x <- factor(c("neon", "genesis", "evangelion"))

	y <- matter_fc(x)

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

	z <- factor(letters[11:20])

	z2 <- matter_fc(z)

	df1 <- data.frame(x=x, y=y, z=z, stringsAsFactors=FALSE)

	df2 <- matter_df(x=x2, y=y2, z=z2)

	expect_equal(df1, df2[])

	expect_equal(df1[1:5,], df2[1:5,])

	expect_equal(df1[,"x"], df2[,"x"])

	expect_equal(df1[,c("x","y")], df2[,c("x","y")])

	expect_equal(df1[1:5,c("x","y")], df2[1:5,c("x","y")])

	expect_equal(df1$x, df2$x[])

	expect_equivalent(as.matter(df1)[], df2[])

})

test_that("matrix binding", {

	x1 <- data.frame(
		V1=1:10, V2=11:20, V3=21:30, V4=31:40, V5=41:40,
		V6=letters[1:10], V7=letters[11:20],
		V8=factor(letters[1:10]), V9=factor(letters[1:10]),
		V10=rnorm(10), stringsAsFactors=FALSE)

	rownames(x1) <- paste0("row", 1:nrow(x1))

	colnames(x1) <- paste0("col", 1:ncol(x1))

	x2 <- x1[,1:5]

	x3 <- x1[1:5,]

	row.names(x3) <- paste0("row", 11:15)

	y1 <- matter_df(x1, row.names=row.names(x1), stringsAsFactors=FALSE)

	y2 <- matter_df(x2, row.names=row.names(x2), stringsAsFactors=FALSE)

	y3 <- matter_df(x3, row.names=row.names(x3), stringsAsFactors=FALSE)

	expect_equal(cbind(y1, y2)[], cbind(x1, x2))

	expect_equal(rbind(y1, y3)[], rbind(x1, x3))

})

test_that("sparse matrix subsetting", {

	x <- matrix(rbinom(100, 1, 0.2), nrow=10, ncol=10)

	y <- sparse_old_mat(x, keys=1:10 + (1:10) * 0.11)

	expect_equal(x, y[])

	expect_equal(x[1,], y[1,])

	expect_equal(x[,1], y[,1])

	expect_equal(x[1,1], y[1,1])

	expect_equal(x[1:10,1:10], y[1:10,1:10])

	expect_equal(x[10:1,10:1], y[10:1,10:1])

	keys(y) <- keys(y) + 0.001

	tolerance(y) <- c(absolute=0.025)

	expect_equal(x, y[])

	expect_equal(x[1,], y[1,])

	expect_equal(x[,1], y[,1])

	expect_equal(x[1,1], y[1,1])

	expect_equal(x[1:10,1:10], y[1:10,1:10])

	expect_equal(x[10:1,10:1], y[10:1,10:1])

	keys <- list(c(1, 1.11), c(2, 2.22), c(3, 3.33))

	values <- list(c(1, 100), c(1, 100), c(1, 100))

	init <- list(keys=keys, values=values)

	x <- diag(3)

	x2 <- 100 * x

	y <- sparse_old_mat(init, keys=c(1, 2, 3), nrow=3, ncol=3)

	expect_equal(x, y[])

	tolerance(y) <- c(absolute=0.5)

	expect_error(y[])

	combiner(y) <- "mean"

	expect_equal((x + x2) / 2, y[])

	combiner(y) <- "sum"

	expect_equal(x + x2, y[])

	combiner(y) <- "min"

	expect_equal(x, y[])

	combiner(y) <- "max"

	expect_equal(x2, y[])

})

test_that("virtual matrix subsetting", {

	x <- matrix(runif(50), nrow=5, ncol=10)

	y <- virtual_mat(x)

	expect_equal(x, y[])

	expect_equal(x[1,], y[1,])

	expect_equal(x[,1], y[,1])

	expect_equal(x[1,1], y[1,1])

	expect_equal(x[1:5,1:10], y[1:5,1:10])

	expect_equal(x[5:1,10:1], y[5:1,10:1])

	x <- rbind(cbind(x, x), cbind(x, x))

	y <- rbind(cbind(y, y), cbind(y, y))

	expect_equal(x, y[])

	expect_equal(x[1,], y[1,])

	expect_equal(x[,1], y[,1])

	expect_equal(x[1,1], y[1,1])

	expect_equal(x[1:5,1:10], y[1:5,1:10])

	expect_equal(x[5:1,10:1], y[5:1,10:1])

})

test_that("virtual replicated vectors", {

	init <- list(1:10, 11:20, 21:30)

	x <- rep(init, 100)

	y <- rep_vt(init, 100)

	expect_equal(x[1], y[1])

	expect_equal(x[1:10], y[1:10])

	expect_equal(x[[5]], y[[5]])

	expect_equal(length(x), length(y))

})
