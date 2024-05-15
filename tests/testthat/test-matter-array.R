require(testthat)
require(matter)

context("matter-array")

test_that("matter array raw", {

	x <- as.raw(1:10)
	y <- matter_arr(x)

	expect_equal(x, y[])
	expect_equal(x, as.raw(y))
	expect_equal(x[1:10], y[1:10])
	expect_equal(x[10:1], y[10:1])

})

test_that("matter array logical", {

	x <- rep_len(c(TRUE, FALSE), 10L)
	y <- matter_arr(x)

	expect_equal(x, y[])
	expect_equal(x, as.logical(y))
	expect_equal(x[1:10], y[1:10])
	expect_equal(x[10:1], y[10:1])

})

test_that("matter array integer", {

	x <- 1:10
	y <- matter_arr(x)

	expect_equal(x, y[])
	expect_equal(x, as.integer(y))
	expect_equal(x[1:10], y[1:10])
	expect_equal(x[10:1], y[10:1])

})

test_that("matter array double", {

	x <- 1:10 + 1:10 * 0.11
	y <- matter_arr(x)

	expect_equal(x, y[])
	expect_equal(x, as.double(y))
	expect_equal(x[1:10], y[1:10])
	expect_equal(x[10:1], y[10:1])

})

test_that("matter array 1-D indexing", {

	set.seed(1, kind="default")
	x <- sort(round(10 * runif(10), 2))
	y <- matter_vec(x)

	expect_equal(x, y[])
	expect_equal(x[1], y[1])
	expect_equal(x[1:10], y[1:10])
	expect_equal(x[10:1], y[10:1])
	expect_equal(x[c(1,3,5,7)], y[c(1,3,5,7)])
	expect_equal(x[c(7,5,3,1)], y[c(7,5,3,1)])
	expect_equal(x[c(1,NA,2,NA,3)], y[c(1,NA,2,NA,3)])
	expect_error(y[-1])
	expect_error(y[length(y) + 1])

	z <- y[1:5,drop=NULL]

	expect_is(z, "matter_vec")
	expect_equal(x[1:5], z[])

	x[1:5] <- x[1:5] + 100
	y[1:5] <- y[1:5] + 100

	expect_equal(x[], y[])
	expect_equal(x[1], y[1])
	expect_equal(x[1:10], y[1:10])
	expect_equal(x[10:1], y[10:1])

	x[c(1,3,5)] <- NA
	y[c(1,3,5)] <- NA

	expect_equal(x[], y[])
	expect_equal(x[1], y[1])
	expect_equal(x[1:10], y[1:10])
	expect_equal(x[10:1], y[10:1])

	set.seed(1, kind="default")
	x <- sort(round(10 * runif(10), 2))
	y <- matter_arr(x, rowMaj=TRUE)

	expect_equal(x[], y[])
	expect_equal(x[1], y[1])
	expect_equal(x[1:10], y[1:10])
	expect_equal(x[10:1], y[10:1])

	z <- matter_vec(type="double", path=path(y))
	expect_equal(x, z[])
	expect_null(dim(z))

	z <- matter_vec(0, length=10)
	expect_equal(rep(0, 10), z[])

	expect_is(matter_vec(), "matter_vec")

})

test_that("matter array 2-D indexing (col major)", {

	set.seed(1, kind="default")
	vals <- sort(round(10 * runif(35), 2))
	x <- matrix(vals, nrow=5, ncol=7)
	y <- matter_mat(x)

	expect_equal(x, y[])
	expect_equal(x[1,], y[1,])
	expect_equal(x[,1], y[,1])
	expect_equal(x[5,], y[5,])
	expect_equal(x[,7], y[,7])
	expect_equal(x[1:3,1:4], y[1:3,1:4])
	expect_equal(x[3:1,4:1], y[3:1,4:1])
	expect_equal(x[,c(1,NA,7)], y[,c(1,NA,7)])
	expect_equal(x[1,,drop=FALSE], as.matrix(y[1,,drop=NULL]))
	expect_error(y[,-1])
	expect_error(y[-1,])
	expect_error(y[,ncol(y) + 1])
	expect_error(y[nrow(y) + 1,])

	z <- y[1:3,1:3,drop=NULL]

	expect_is(z, "matter_mat")
	expect_equal(x[1:3,1:3], z[])

	x[1,] <- x[1,] + 100
	y[1,] <- y[1,] + 100
	
	expect_equal(x, y[])

	x[,1] <- x[,1] + -100
	y[,1] <- y[,1] + -100
	
	expect_equal(x, y[])

	x[1,1] <- NA
	y[1,1] <- NA

	expect_equal(x, y[])
	
	expect_equal(t(x), t(y)[])

	z <- matter_mat(0, nrow=5, ncol=7)
	expect_equal(matrix(0, nrow=5, ncol=7), z[])

	expect_is(matter_mat(), "matter_mat")

})

test_that("matter array 2-D indexing (row major)", {

	set.seed(1, kind="default")
	vals <- sort(round(10 * runif(35), 2))
	x <- matrix(vals, nrow=5, ncol=7)
	y <- matter_mat(x, rowMaj=TRUE)

	expect_equal(x, y[])
	expect_equal(x[1,], y[1,])
	expect_equal(x[,1], y[,1])
	expect_equal(x[5,], y[5,])
	expect_equal(x[,7], y[,7])
	expect_equal(x[1:3,1:4], y[1:3,1:4])
	expect_equal(x[3:1,4:1], y[3:1,4:1])
	expect_equal(x[,c(1,NA,7)], y[,c(1,NA,7)])
	expect_equal(x[1,,drop=FALSE], as.matrix(y[1,,drop=NULL]))
	expect_error(y[,-1])
	expect_error(y[-1,])
	expect_error(y[,ncol(y) + 1])
	expect_error(y[nrow(y) + 1,])

	z <- y[1:3,1:3,drop=NULL]

	expect_is(z, "matter_mat")
	expect_equal(x[1:3,1:3], z[])

	x[1,] <- x[1,] + 100
	y[1,] <- y[1,] + 100
	
	expect_equal(x, y[])

	x[,1] <- x[,1] + -100
	y[,1] <- y[,1] + -100
	
	expect_equal(x, y[])

	x[1,1] <- NA
	y[1,1] <- NA

	expect_equal(x, y[])

	expect_equal(t(x), t(y)[])

	z <- matter_mat(0, nrow=5, ncol=7, rowMaj=TRUE)
	expect_equal(matrix(0, nrow=5, ncol=7), z[])

})

test_that("matter array N-D indexing (col major)", {

	set.seed(1, kind="default")
	vals <- sort(round(10 * runif(24), 2))
	x <- array(vals, dim=c(4,3,2))
	y <- matter_arr(x)
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
	expect_equal(x[1,1,,drop=FALSE], as.array(y[1,1,,drop=NULL]))

	z <- y[1:2,1:2,1,drop=NULL]

	expect_is(z, "matter_arr")
	expect_equal(x[1:2,1:2,1], z[])

	x[,,1] <- x[,,1] + 100
	y[,,1] <- y[,,1] + 100

	expect_equal(x, y[])

	expect_error(t(y))

	z <- matter_arr(0, dim=c(4,3,2))
	expect_equal(array(0, dim=c(4,3,2)), z[])

	expect_is(matter_arr(), "matter_arr")

})

test_that("matter array N-D indexing (row major)", {

	set.seed(1, kind="default")
	vals <- sort(round(10 * runif(24), 2))
	x <- array(vals, dim=c(4,3,2))
	y <- matter_arr(x, rowMaj=TRUE)
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
	expect_equal(x[1,1,,drop=FALSE], as.array(y[1,1,,drop=NULL]))

	z <- y[1:2,1:2,1,drop=NULL]

	expect_is(z, "matter_arr")
	expect_equal(x[1:2,1:2,1], z[])

	x[,,1] <- x[,,1] + 100
	y[,,1] <- y[,,1] + 100

	expect_equal(x, y[])

	expect_error(t(y))

	z <- matter_arr(0, dim=c(4,3,2), rowMaj=TRUE)
	expect_equal(array(0, dim=c(4,3,2)), z[])

})

test_that("matter matrix combining", {

	set.seed(1, kind="default")
	v1 <- sort(round(10 * runif(10)))
	v2 <- sort(round(10 * runif(10)))
	x <- matter_vec(v1)
	y <- matter_vec(v2, path=path(x), append=TRUE)
	expect_equal(c(v1, v2), c(x, y)[])
	expect_equal(path(x), path(y))

	set.seed(1, kind="default")
	vals <- sort(round(10 * runif(35), 2))
	x <- matrix(vals, nrow=5, ncol=7)
	y <- matter_mat(x)
	a <- matter_vec(x[,1])

	expect_equal(c(x, x), c(y, y)[])
	expect_equal(cbind(x, x), cbind(y, y)[])
	expect_equal(cbind(x[,1], x), cbind(a, y)[])

	y <- matter_mat(x, rowMaj=TRUE)
	a <- matter_vec(x[1,])

	expect_equal(rbind(x, x), rbind(y, y)[])
	expect_equal(rbind(x[1,], x), rbind(a, y)[])

})

test_that("matter array coercion", {

	x <- array(1:24, dim=c(4,3,2))
	y <- matter_arr(x)

	expect_error(as(y, "matter_mat"))
	expect_is(as(y, "matter_vec"), "matter_vec")
	expect_equal(x, as.array(y))
	expect_equal(as.vector(x), as.vector(y))

	x <- 1:10
	y <- matter_vec(x)

	expect_is(as(y, "matter_arr"), "matter_arr")
	expect_is(as(y, "matter_mat"), "matter_mat")
	expect_equal(x, as.vector(y))
	expect_equal(as.array(x), as.array(y))

	x <- matrix(1:9, nrow=3, ncol=3)
	y <- matter_mat(x)

	expect_is(as(y, "matter_arr"), "matter_arr")
	expect_is(as(y, "matter_vec"), "matter_vec")
	expect_equal(x, as.matrix(y))
	expect_equal(as.vector(x), as.vector(y))

})

test_that("matter matrix multiplication", {

	set.seed(1, kind="default")
	vals1 <- sort(round(10 * runif(35), 2))
	vals2 <- sort(round(10 * runif(35), 2))
	x <- matrix(vals1, nrow=5, ncol=7)
	y <- matrix(vals2, nrow=7, ncol=5)

	options(matter.matmul.bpparam=NULL)
	xx <- matter_mat(x)
	yy <- matter_mat(y)

	expect_equal(x %*% y, xx %*% y)
	expect_equal(x %*% y, x %*% yy)
	expect_equal(y %*% x, yy %*% x)
	expect_equal(y %*% x, y %*% xx)
	expect_equal(crossprod(x, x), crossprod(xx, x))
	expect_equal(crossprod(x, x), crossprod(x, xx))
	expect_equal(tcrossprod(x, x), tcrossprod(xx, x))
	expect_equal(tcrossprod(x, x), tcrossprod(x, xx))

	options(matter.matmul.bpparam=SerialParam())

	expect_equal(x %*% y, xx %*% y)
	expect_equal(x %*% y, x %*% yy)
	expect_equal(y %*% x, yy %*% x)
	expect_equal(y %*% x, y %*% xx)
	expect_equal(crossprod(x, x), crossprod(xx, x))
	expect_equal(crossprod(x, x), crossprod(x, xx))
	expect_equal(tcrossprod(x, x), tcrossprod(xx, x))
	expect_equal(tcrossprod(x, x), tcrossprod(x, xx))

	options(matter.matmul.bpparam=NULL)
	xx <- matter_mat(x, rowMaj=TRUE)
	yy <- matter_mat(y, rowMaj=TRUE)

	expect_equal(x %*% y, xx %*% y)
	expect_equal(x %*% y, x %*% yy)
	expect_equal(y %*% x, yy %*% x)
	expect_equal(y %*% x, y %*% xx)
	expect_equal(crossprod(x, x), crossprod(xx, x))
	expect_equal(crossprod(x, x), crossprod(x, xx))
	expect_equal(tcrossprod(x, x), tcrossprod(xx, x))
	expect_equal(tcrossprod(x, x), tcrossprod(x, xx))

	options(matter.matmul.bpparam=SerialParam())

	expect_equal(x %*% y, xx %*% y)
	expect_equal(x %*% y, x %*% yy)
	expect_equal(y %*% x, yy %*% x)
	expect_equal(y %*% x, y %*% xx)
	expect_equal(crossprod(x, x), crossprod(xx, x))
	expect_equal(crossprod(x, x), crossprod(x, xx))
	expect_equal(tcrossprod(x, x), tcrossprod(xx, x))
	expect_equal(tcrossprod(x, x), tcrossprod(x, xx))

})
