require(testthat)
require(matter)

context("delayed-ops")

test_that("delayed ops - logical", {

	x <- rep_len(c(TRUE, FALSE), 10L)
	y <- matter_vec(x)

	expect_equal(x + 1, (y + 1)[])
	expect_equal(x + 1:10, (y + 1:10)[])
	expect_equal((x + 1:10)[1:5], (y + 1:10)[1:5])
	expect_equal(x + c(NA,2:9,NA), (y + c(NA,2:9,NA))[])
	expect_error(y + 1:5)
	expect_error(y + "foo")

})

test_that("delayed ops - integer", {

	x <- seq_len(10)
	y <- matter_vec(x)

	expect_equal(x + 1, (y + 1)[])
	expect_equal(x + 1:10, (y + 1:10)[])
	expect_equal((x + 1:10)[1:5], (y + 1:10)[1:5])
	expect_equal(x + c(NA,2:9,NA), (y + c(NA,2:9,NA))[])
	expect_error(y + 1:5)
	expect_error(y + "foo")

})

test_that("delayed ops - sparse integer", {

	set.seed(2)
	x <- rbinom(10, 1, 0.4)
	x[x != 0] <- seq_len(sum(x != 0))
	y <- sparse_vec(x)

	expect_equal(x + 1, (y + 1)[])
	expect_equal(x + 1:10, (y + 1:10)[])
	expect_equal((x + 1:10)[1:5], (y + 1:10)[1:5])
	expect_equal(x + c(NA,2:9,NA), (y + c(NA,2:9,NA))[])
	expect_error(y + 1:5)
	expect_error(y + "foo")

})

test_that("delayed ops - double", {

	x <- 1:10 + 1:10 * 0.11
	y <- matter_vec(x)

	expect_equal(x + 1, (y + 1)[])
	expect_equal(x + 1:10, (y + 1:10)[])
	expect_equal((x + 1:10)[1:5], (y + 1:10)[1:5])
	expect_equal(x + c(NA,2:9,NA), (y + c(NA,2:9,NA))[])
	expect_error(y + 1:5)
	expect_error(y + "foo")

})

test_that("delayed ops - sparse double", {

	set.seed(2)
	x <- rbinom(10, 1, 0.4)
	x[x != 0] <- seq_len(sum(x != 0)) + seq_len(sum(x != 0)) * 0.11
	y <- sparse_vec(x)

	expect_equal(x + 1, (y + 1)[])
	expect_equal(x + 1:10, (y + 1:10)[])
	expect_equal((x + 1:10)[1:5], (y + 1:10)[1:5])
	expect_equal(x + c(NA,2:9,NA), (y + c(NA,2:9,NA))[])
	expect_error(y + 1:5)
	expect_error(y + "foo")

})

test_that("delayed ops - matrix", {

	set.seed(1)
	vals <- sort(round(10 * runif(35), 2))
	x <- matrix(vals, nrow=5, ncol=7)
	y <- matter_mat(x)
	
	a <- matrix(rep.int(1:5, 7), ncol=7)
	b <- matrix(rep.int(1:7, 5), nrow=5, byrow=TRUE)
	a1 <- a[,1,drop=FALSE]
	b1 <- b[1,,drop=FALSE]

	expect_equal(x + 1, (y + 1)[])
	expect_equal(x + 1:5, (y + 1:5)[])
	expect_equal(x + c(NA,2:4,NA), (y + c(NA,2:4,NA))[])
	expect_equal(x + a, (y + a1)[])
	expect_equal((x + a)[1,], (y + a1)[1,])
	expect_equal((x + a)[,1], (y + a1)[,1])
	expect_equal(x + b, (y + b1)[])
	expect_equal((x + b)[1,], (y + b1)[1,])
	expect_equal((x + b)[,1], (y + b1)[,1])
	expect_equal(x + a + b, (y + a1 + b1)[])
	expect_equal(a - x + b, (a1 - y + b1)[])
	expect_equal(a * x - b, (a1 * y - b1)[])

})

test_that("delayed ops - sparse matrix", {

	set.seed(2)
	x <- rbinom(35, 1, 0.4)
	x[x != 0] <- seq_len(sum(x != 0)) + seq_len(sum(x != 0)) * 0.11
	dim(x) <- c(5, 7)
	y <- sparse_mat(x)
	
	a <- matrix(rep.int(1:5, 7), ncol=7)
	b <- matrix(rep.int(1:7, 5), nrow=5, byrow=TRUE)
	a1 <- a[,1,drop=FALSE]
	b1 <- b[1,,drop=FALSE]

	expect_equal(x + 1, (y + 1)[])
	expect_equal(x + 1:5, (y + 1:5)[])
	expect_equal(x + c(NA,2:4,NA), (y + c(NA,2:4,NA))[])
	expect_equal(x + a, (y + a1)[])
	expect_equal((x + a)[1,], (y + a1)[1,])
	expect_equal((x + a)[,1], (y + a1)[,1])
	expect_equal(x + b, (y + b1)[])
	expect_equal((x + b)[1,], (y + b1)[1,])
	expect_equal((x + b)[,1], (y + b1)[,1])
	expect_equal(x + a + b, (y + a1 + b1)[])
	expect_equal(a - x + b, (a1 - y + b1)[])
	expect_equal(a * x - b, (a1 * y - b1)[])

})

test_that("delayed ops - array", {

	set.seed(1)
	vals <- sort(round(10 * runif(24), 2))
	x <- array(vals, dim=c(4,3,2))
	y <- matter_arr(x)
	
	a <- array(rep_len(1:4, length(x)), dim=dim(x))
	a11 <- a[,1,1,drop=FALSE]

	expect_equal(x + 1, (y + 1)[])
	expect_equal(x + 1:4, (y + 1:4)[])
	expect_equal(x + a, (y + a11)[])
	expect_equal((x + a)[1,,], (y + a11)[1,,])
	expect_equal((x + a)[,1,], (y + a11)[,1,])
	expect_equal((x + a)[,,1], (y + a11)[,,1])
	expect_equal(x - a, (y - a11)[])
	expect_equal(a * x - a, (a11 * y - a11)[])

})

