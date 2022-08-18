require(testthat)
require(matter)

context("sparse-classes")

test_that("sparse vector subsetting", {

	set.seed(1)

	x <- rbinom(100, 1, 0.2)

	x[x != 0] <- seq_len(sum(x != 0))

	y <- sparse_vec(x)

	expect_equal(x, y[])

	expect_equal(x[1:10], y[1:10])

	expect_equal(x[10:1], y[10:1])

	domain(y) <- seq_along(y)

	expect_equal(x, y[])

	expect_equal(x[1:10], y[1:10])

	expect_equal(x[10:1], y[10:1])

	atomindex(y) <- as.matter(atomindex(y))

	atomdata(y) <- as.matter(atomdata(y))

	expect_equal(x, y[])

	expect_equal(x[1:10], y[1:10])

	expect_equal(x[10:1], y[10:1])

})

test_that("sparse vector subsetting w/ interpolation", {

	z <- sparse_vec(index=c(1.0, 1.01, 1.11,
							2.0, 2.22,
							3.0, 3.33, 3.333,
							4.0),
					data=c(1.0, 1.01, 1.11,
							2.0, 2.22,
							3.0, 3.33, 3.333,
							4.0),
					domain=seq(from=0, to=4, by=0.5))

	test1 <- c(0, 0, 1, 0, 2, 0, 3, 0, 4)

	expect_equal(test1, z[])

	tolerance(z) <- 0.25

	test2 <- c(0, 0, 1, 0, 2, 0, 3, 3.333, 4)

	expect_equal(test2, z[])

	sampler(z) <- "sum"

	test3 <- c(0, 0, 1.0+1.01+1.11, 0, 2.0+2.22, 0, 3, 3.33+3.333, 4)

	expect_equal(test3, z[])

	sampler(z) <- "mean"

	test4 <- c(0, 0, (1.0+1.01+1.11)/3, 0, (2.0+2.22)/2, 0, 3, (3.33+3.333)/2, 4)

	expect_equal(test4, z[])

	sampler(z) <- "linear"

	tolerance(z) <- 1.0

	test5 <- c(0, 1, 1, 1.5, 2, 2.5, 3, 3.5, 4)

	expect_equal(test5, z[])

	z2 <- sparse_vec(index=rev(aindex(z)),
					data=rev(adata(z)),
					domain=domain(z))

	expect_equal(test1, z2[])

	z3 <- sparse_vec(index=seq_len(11),
					data=c(rep_len(1, 5), 10,
						rep_len(1, 5)),
					domain=seq_len(11))

	tolerance(z3) <- 2

	sampler(z3) <- "sum"

	test6 <- c(3, 4, 5, 14, 14, 14, 14, 14, 5, 4, 3)

	expect_equal(test6, z3[])

	sampler(z3) <- "mean"

	wts <- rep_len(1, 5)

	test7 <- filter(adata(z3), wts / sum(wts), circular=TRUE)

	expect_equal(as.vector(test7), z3[])

	sampler(z3) <- "gaussian"

	wts <- dnorm((-2):2, sd=5/4)

	test8 <- filter(adata(z3), wts / sum(wts), circular=TRUE)

	expect_equal(as.vector(test8), z3[])

})

test_that("sparse matrix subsetting", {

	set.seed(1)

	x <- rbinom(200, 1, 0.2)

	x[x != 0] <- seq_len(sum(x != 0))

	dim(x) <- c(20, 10)

	y <- sparse_mat2(x)

	expect_equal(x, y[])

	expect_equal(x[,1], y[,1])

	expect_equal(x[1,], y[1,])

	expect_equal(x[,10], y[,10])

	expect_equal(x[20,], y[20,])

	expect_equal(x[1:10,1:5], y[1:10,1:5])

	expect_equal(x[10:1,5:1], y[10:1,5:1])

	expect_equal(x[11:20,6:10], y[11:20,6:10])

	y <- sparse_mat2(x, rowMaj=TRUE)

	expect_equal(x, y[])

	expect_equal(x[,1], y[,1])

	expect_equal(x[1,], y[1,])

	expect_equal(x[,10], y[,10])

	expect_equal(x[20,], y[20,])

	expect_equal(x[1:10,1:5], y[1:10,1:5])

	expect_equal(x[10:1,5:1], y[10:1,5:1])

	expect_equal(x[11:20,6:10], y[11:20,6:10])

})

test_that("sparse matrix subsetting w/ interpolation", {

	set.seed(1)

	x <- rbinom(200, 1, 0.2)

	x[x != 0] <- seq_len(sum(x != 0))

	dim(x) <- c(20, 10)

	d <- seq_len(nrow(x)) + round(runif(nrow(x))/4, 2)

	y <- sparse_mat2(x, domain=d, tolerance=0.5)

	expect_equal(x, y[])

	expect_equal(x[,1], y[,1])

	expect_equal(x[1,], y[1,])

	expect_equal(x[,10], y[,10])

	expect_equal(x[20,], y[20,])

	expect_equal(x[1:10,1:5], y[1:10,1:5])

	expect_equal(x[10:1,5:1], y[10:1,5:1])

	expect_equal(x[11:20,6:10], y[11:20,6:10])

	d <- seq_len(ncol(x)) + round(runif(ncol(x))/4, 2)

	y <- sparse_mat2(x, domain=d, tolerance=0.5, rowMaj=TRUE)

	expect_equal(x, y[])

	expect_equal(x[,1], y[,1])

	expect_equal(x[1,], y[1,])

	expect_equal(x[,10], y[,10])

	expect_equal(x[20,], y[20,])

	expect_equal(x[1:10,1:5], y[1:10,1:5])

	expect_equal(x[10:1,5:1], y[10:1,5:1])

	expect_equal(x[11:20,6:10], y[11:20,6:10])

})


test_that("sparse matrix timing", {

	set.seed(1)

	x <- rbinom(500 * 1000, 1, 0.2)

	x[x != 0] <- seq_len(sum(x != 0))

	dim(x) <- c(500, 1000)

	y <- sparse_mat2(x)

	z <- sparse_mat(x)

	expect_equal(y[], z[])

	bench::mark(y[,1:100], z[,1:100])

	bench::mark(y[1:100,], z[1:100,])

	bench::mark(y[1:100,1:100], z[1:100,1:100])

	d <- seq_len(nrow(x)) + round(runif(nrow(x))/4, 2)

	y2 <- sparse_mat2(x, domain=d, tolerance=0.5)

	z2 <- sparse_mat(x, domain=d, tolerance=0.5)

	expect_equal(y2[], z2[])

	bench::mark(y2[,1:100], z2[,1:100])

	bench::mark(y2[1:100,], z2[1:100,])

	bench::mark(y2[1:100,1:100], z2[1:100,1:100])

})

