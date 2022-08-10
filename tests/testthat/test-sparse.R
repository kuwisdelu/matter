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

	combiner(z) <- "sum"

	test3 <- c(0, 0, 1.0+1.01+1.11, 0, 2.0+2.22, 0, 3, 3.33+3.333, 4)

	expect_equal(test3, z[])

	combiner(z) <- "mean"

	test4 <- c(0, 0, (1.0+1.01+1.11)/3, 0, (2.0+2.22)/2, 0, 3, (3.33+3.333)/2, 4)

	expect_equal(test4, z[])

	combiner(z) <- "max"

	test5 <- c(0, 0, 1.11, 0, 2.22, 0, 3, 3.333, 4)

	expect_equal(test5, z[])

	combiner(z) <- "linear"

	tolerance(z) <- 1.0

	test6 <- seq(from=0, to=4, by=0.5)

	expect_equal(test6, z[])

	z2 <- sparse_vec(index=rev(aindex(z)),
					data=rev(adata(z)),
					domain=domain(z))

	expect_equal(test1, z2[])

})

test_that("sparse vector timing", {

	set.seed(1)

	x <- rbinom(100000, 1, 0.05)

	x[x != 0] <- seq_len(sum(x != 0))

	y <- sparse_vec(x)

	z <- sparse_old_mat(list(keys=list(y@index),
							values=list(y@data)),
						nrow=length(y), ncol=1)

	expect_equal(x, y[])

	bench::mark(y[])
	bench::mark(z[])

	bench::mark(y[1:1000])
	bench::mark(z[1:1000,])

	bench::mark(y[1000:1])
	bench::mark(z[1000:1,])

	set.seed(1)

	x <- rbinom(100000, 1, 0.1)

	x[x != 0] <- seq_len(sum(x != 0))

	y <- sparse_vec(x, domain=1:length(x), tolerance=0.1)

	z <- sparse_old_mat(list(keys=list(y@index),
							values=list(y@data)),
						nrow=length(y), ncol=1,
						keys=1:length(y),
						tolerance=0.1)

	bench::mark(y[])
	bench::mark(z[])

	bench::mark(y[1:1000])
	bench::mark(z[1:1000,])

	bench::mark(y[1000:1])
	bench::mark(z[1000:1,])

})
