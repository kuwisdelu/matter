require(testthat)
require(matter)

context("stream-statistics")

test_that("streaming univariate statistics", {

	set.seed(1)

	x <- sample(1:100, size=10)

	y <- sample(1:100, size=10)

	expect_equal(range(x), as.numeric(s_range(x)))

	expect_equal(min(x), as.numeric(s_min(x)))

	expect_equal(max(x), as.numeric(s_max(x)))

	expect_equal(prod(x), as.numeric(s_prod(x)))

	expect_equal(sum(x), as.numeric(s_sum(x)))

	expect_equal(mean(x), as.numeric(s_mean(x)))

	expect_equal(var(x), as.numeric(s_var(x)))

	expect_equal(sd(x), as.numeric(s_sd(x)))

	expect_equal(any(x > 50), as.logical(s_any(x > 50)))

	expect_equal(all(x > 50), as.logical(s_all(x > 50)))

	expect_equal(nnzero(x > 50), as.numeric(s_nnzero(x > 50)))

	xy <- c(x, y)

	s_x <- s_range(x); s_y <- s_range(y)

	expect_equal(range(xy), as.numeric(stat_c(s_x, s_y)))

	s_x <- s_min(x); s_y <- s_min(y)

	expect_equal(min(xy), as.numeric(stat_c(s_x, s_y)))

	s_x <- s_max(x); s_y <- s_max(y)

	expect_equal(max(xy), as.numeric(stat_c(s_x, s_y)))

	s_x <- s_prod(x); s_y <- s_prod(y)

	expect_equal(prod(xy), as.numeric(stat_c(s_x, s_y)))

	s_x <- s_sum(x); s_y <- s_sum(y)

	expect_equal(sum(xy), as.numeric(stat_c(s_x, s_y)))

	s_x <- s_mean(x); s_y <- s_mean(y)

	expect_equal(mean(xy), as.numeric(stat_c(s_x, s_y)))

	s_x <- s_var(x); s_y <- s_var(y)

	expect_equal(var(xy), as.numeric(stat_c(s_x, s_y)))

	expect_equal(var(c(xy, 99)), as.numeric(stat_c(s_x, s_y, 99)))

	s_x <- s_sd(x); s_y <- s_sd(y)

	expect_equal(sd(xy), as.numeric(stat_c(s_x, s_y)))

	expect_equal(sd(c(xy, 99)), as.numeric(stat_c(s_x, s_y, 99)))

	s_x <- s_any(x > 50); s_y <- s_any(y > 50)

	expect_equal(any(xy > 50), as.logical(stat_c(s_x, s_y)))

	s_x <- s_all(x > 50); s_y <- s_all(y > 50)

	expect_equal(all(xy > 50), as.logical(stat_c(s_x, s_y)))

	s_x <- s_nnzero(x > 50); s_y <- s_nnzero(y > 50)

	expect_equal(nnzero(xy > 50), as.numeric(stat_c(s_x, s_y)))

	x[1] <- NA; y[1:5] <- NA

	xy <- c(x, y)

	expect_true(is.na(s_mean(x)))

	expect_true(is.na(s_mean(y)))

	s_x <- s_mean(x, na.rm=TRUE); s_y <- s_mean(y, na.rm=TRUE)

	expect_equal(mean(xy, na.rm=TRUE), as.numeric(stat_c(s_x, s_y)))

	s_x <- s_var(x, na.rm=TRUE); s_y <- s_var(y, na.rm=TRUE)

	expect_equal(var(xy, na.rm=TRUE), as.numeric(stat_c(s_x, s_y)))

})

test_that("streaming matrix statistics", {

	set.seed(1)

	x <- matrix(rnorm(100), nrow=10, ncol=10)

	y <- matrix(rnorm(100), nrow=10, ncol=10)

	xy <- cbind(x, y)

	s_x <- rowstreamStats(x, "range"); s_y <- rowstreamStats(y, "range")

	expect_equal(as.numeric(apply(xy, 1, range)), as.numeric(stat_c(s_x, s_y)))

	s_x <- rowstreamStats(x, "min"); s_y <- rowstreamStats(y, "min")

	expect_equal(as.numeric(apply(xy, 1, min)), as.numeric(stat_c(s_x, s_y)))

	s_x <- rowstreamStats(x, "max"); s_y <- rowstreamStats(y, "max")

	expect_equal(as.numeric(apply(xy, 1, max)), as.numeric(stat_c(s_x, s_y)))

	s_x <- rowstreamStats(x, "prod"); s_y <- rowstreamStats(y, "prod")

	expect_equal(as.numeric(apply(xy, 1, prod)), as.numeric(stat_c(s_x, s_y)))

	s_x <- rowstreamStats(x, "sum"); s_y <- rowstreamStats(y, "sum")

	expect_equal(as.numeric(apply(xy, 1, sum)), as.numeric(stat_c(s_x, s_y)))

	s_x <- rowstreamStats(x, "mean"); s_y <- rowstreamStats(y, "mean")

	expect_equal(as.numeric(apply(xy, 1, mean)), as.numeric(stat_c(s_x, s_y)))

	s_x <- rowstreamStats(x, "var"); s_y <- rowstreamStats(y, "var")

	expect_equal(as.numeric(apply(xy, 1, var)), as.numeric(stat_c(s_x, s_y)))

	expect_equal(as.numeric(apply(cbind(xy, 9), 1, var)), as.numeric(stat_c(s_x, s_y, 9)))

	s_x <- rowstreamStats(x, "sd"); s_y <- rowstreamStats(y, "sd")

	expect_equal(as.numeric(apply(xy, 1, sd)), as.numeric(stat_c(s_x, s_y)))

	expect_equal(as.numeric(apply(cbind(xy, 9), 1, sd)), as.numeric(stat_c(s_x, s_y, 9)))

	s_x <- rowstreamStats(x > 0, "any"); s_y <- rowstreamStats(y > 0, "any")

	expect_equal(as.logical(apply(xy > 0, 1, any)), as.logical(stat_c(s_x, s_y)))

	s_x <- rowstreamStats(x > 0, "all"); s_y <- rowstreamStats(y > 0, "all")

	expect_equal(as.logical(apply(xy > 0, 1, all)), as.logical(stat_c(s_x, s_y)))

	s_x <- rowstreamStats(x > 0, "nnzero"); s_y <- rowstreamStats(y > 0, "nnzero")

	expect_equal(as.numeric(apply(xy > 0, 1, nnzero)), as.numeric(stat_c(s_x, s_y)))

	x[,1] <- NA; y[1,] <- NA

	xy <- cbind(x, y)

	expect_true(all(is.na(rowstreamStats(x, "mean"))))

	expect_true(any(is.na(rowstreamStats(y, "mean"))))

	s_x <- rowstreamStats(x, "mean", na.rm=TRUE); s_y <- rowstreamStats(y, "mean", na.rm=TRUE)

	expect_equal(as.numeric(apply(xy, 1, mean, na.rm=TRUE)), as.numeric(stat_c(s_x, s_y)))

	s_x <- rowstreamStats(x, "var", na.rm=TRUE); s_y <- rowstreamStats(y, "var", na.rm=TRUE)

	expect_equal(as.numeric(apply(xy, 1, var, na.rm=TRUE)), as.numeric(stat_c(s_x, s_y)))

})

test_that("streaming variance + standard deviation", {

	set.seed(1)

	x <- sample(1:100, size=10)

	s1_a <- sd(x[1:3])

	s1_b <- s_sd(x[1], x[2], x[3])

	expect_equal(s1_a, as.numeric(s1_b))

	s1_a <- sd(x[1:2])

	s1_b <- stat_c(s_sd(x[1]), s_sd(x[2]))

	expect_equal(s1_a, as.numeric(s1_b))

	s1_a <- sd(x[1:5])

	s1_b <- stat_c(s_sd(x[1:4]), s_sd(x[5]))

	expect_equal(s1_a, as.numeric(s1_b))

	s1_a <- sd(x[1:5])

	s1_b <- stat_c(s_sd(x[1]), s_sd(x[2:5]))

	expect_equal(s1_a, as.numeric(s1_b))

	s2_a <- var(x[1:3])

	s2_b <- s_var(x[1], x[2], x[3])

	expect_equal(s2_a, as.numeric(s2_b))

	s2_a <- var(x[1:2])

	s2_b <- stat_c(s_var(x[1]), s_var(x[2]))

	expect_equal(s2_a, as.numeric(s2_b))

	s2_a <- var(x[1:5])

	s2_b <- stat_c(s_var(x[1:4]), s_var(x[5]))

	expect_equal(s2_a, as.numeric(s2_b))

	s2_a <- var(x[1:5])

	s2_b <- stat_c(s_var(x[1]), s_var(x[2:5]))

	expect_equal(s2_a, as.numeric(s2_b))

})

