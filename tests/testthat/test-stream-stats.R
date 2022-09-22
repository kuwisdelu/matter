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
	
	sx <- s_range(x)
	sy <- s_range(y)
	expect_equal(range(xy), as.numeric(stat_c(sx, sy)))
	
	sx <- s_min(x)
	sy <- s_min(y)
	expect_equal(min(xy), as.numeric(stat_c(sx, sy)))
	
	sx <- s_max(x)
	sy <- s_max(y)
	expect_equal(max(xy), as.numeric(stat_c(sx, sy)))
	
	sx <- s_prod(x)
	sy <- s_prod(y)
	expect_equal(prod(xy), as.numeric(stat_c(sx, sy)))
	
	sx <- s_sum(x)
	sy <- s_sum(y)
	expect_equal(sum(xy), as.numeric(stat_c(sx, sy)))
	
	sx <- s_mean(x)
	sy <- s_mean(y)
	expect_equal(mean(xy), as.numeric(stat_c(sx, sy)))
	
	sx <- s_var(x)
	sy <- s_var(y)
	expect_equal(var(xy), as.numeric(stat_c(sx, sy)))
	expect_equal(var(c(xy, 99)), as.numeric(stat_c(sx, sy, 99)))
	
	sx <- s_sd(x)
	sy <- s_sd(y)
	expect_equal(sd(xy), as.numeric(stat_c(sx, sy)))
	expect_equal(sd(c(xy, 99)), as.numeric(stat_c(sx, sy, 99)))
	
	sx <- s_any(x > 50)
	sy <- s_any(y > 50)
	expect_equal(any(xy > 50), as.logical(stat_c(sx, sy)))
	
	sx <- s_all(x > 50)
	sy <- s_all(y > 50)
	expect_equal(all(xy > 50), as.logical(stat_c(sx, sy)))
	
	sx <- s_nnzero(x > 50)
	sy <- s_nnzero(y > 50)
	expect_equal(nnzero(xy > 50), as.numeric(stat_c(sx, sy)))

	x[1] <- NA
	y[1:5] <- NA
	xy <- c(x, y)
	expect_true(is.na(s_mean(x)))
	expect_true(is.na(s_mean(y)))

	sx <- s_mean(x, na.rm=TRUE)
	sy <- s_mean(y, na.rm=TRUE)
	expect_equal(mean(xy, na.rm=TRUE), as.numeric(stat_c(sx, sy)))
	
	sx <- s_var(x, na.rm=TRUE)
	sy <- s_var(y, na.rm=TRUE)
	expect_equal(var(xy, na.rm=TRUE), as.numeric(stat_c(sx, sy)))

})

test_that("streaming variance + standard deviation", {

	set.seed(1)
	x <- sample(1:100, size=10)

	s1a <- sd(x[1:3])
	s1b <- s_sd(x[1], x[2], x[3])
	expect_equal(s1a, as.numeric(s1b))

	s1a <- sd(x[1:2])
	s1b <- stat_c(s_sd(x[1]), s_sd(x[2]))
	expect_equal(s1a, as.numeric(s1b))

	s1a <- sd(x[1:5])
	s1b <- stat_c(s_sd(x[1:4]), s_sd(x[5]))
	expect_equal(s1a, as.numeric(s1b))

	s1a <- sd(x[1:5])
	s1b <- stat_c(s_sd(x[1]), s_sd(x[2:5]))
	expect_equal(s1a, as.numeric(s1b))

	s2a <- var(x[1:3])
	s2b <- s_var(x[1], x[2], x[3])
	expect_equal(s2a, as.numeric(s2b))

	s2a <- var(x[1:2])
	s2b <- stat_c(s_var(x[1]), s_var(x[2]))
	expect_equal(s2a, as.numeric(s2b))

	s2a <- var(x[1:5])
	s2b <- stat_c(s_var(x[1:4]), s_var(x[5]))
	expect_equal(s2a, as.numeric(s2b))

	s2a <- var(x[1:5])
	s2b <- stat_c(s_var(x[1]), s_var(x[2:5]))
	expect_equal(s2a, as.numeric(s2b))

})


test_that("streaming matrix statistics", {

	set.seed(1)
	x <- matrix(rnorm(150), nrow=15, ncol=10)
	y <- matrix(rnorm(150), nrow=15, ncol=10)

	xy <- cbind(x, y)

	sx <- s_rowstats(x, "range")
	sy <- s_rowstats(y, "range")
	expect_equal(as.numeric(t(apply(xy, 1, range))), as.numeric(stat_c(sx, sy)))

	sx <- s_rowstats(x, "min")
	sy <- s_rowstats(y, "min")
	expect_equal(as.numeric(apply(xy, 1, min)), as.numeric(stat_c(sx, sy)))

	sx <- s_rowstats(x, "max")
	sy <- s_rowstats(y, "max")
	expect_equal(as.numeric(apply(xy, 1, max)), as.numeric(stat_c(sx, sy)))

	sx <- s_rowstats(x, "prod")
	sy <- s_rowstats(y, "prod")
	expect_equal(as.numeric(apply(xy, 1, prod)), as.numeric(stat_c(sx, sy)))

	sx <- s_rowstats(x, "sum")
	sy <- s_rowstats(y, "sum")
	expect_equal(as.numeric(apply(xy, 1, sum)), as.numeric(stat_c(sx, sy)))

	sx <- s_rowstats(x, "mean")
	sy <- s_rowstats(y, "mean")
	expect_equal(as.numeric(apply(xy, 1, mean)), as.numeric(stat_c(sx, sy)))

	sx <- s_rowstats(x, "var")
	sy <- s_rowstats(y, "var")
	expect_equal(as.numeric(apply(xy, 1, var)), as.numeric(stat_c(sx, sy)))
	expect_equal(as.numeric(apply(cbind(xy, 9), 1, var)), as.numeric(stat_c(sx, sy, 9)))

	sx <- s_rowstats(x, "sd")
	sy <- s_rowstats(y, "sd")
	expect_equal(as.numeric(apply(xy, 1, sd)), as.numeric(stat_c(sx, sy)))
	expect_equal(as.numeric(apply(cbind(xy, 9), 1, sd)), as.numeric(stat_c(sx, sy, 9)))

	sx <- s_rowstats(x > 0, "any")
	sy <- s_rowstats(y > 0, "any")
	expect_equal(as.logical(apply(xy > 0, 1, any)), as.logical(stat_c(sx, sy)))

	sx <- s_rowstats(x > 0, "all")
	sy <- s_rowstats(y > 0, "all")
	expect_equal(as.logical(apply(xy > 0, 1, all)), as.logical(stat_c(sx, sy)))

	sx <- s_rowstats(x > 0, "nnzero")
	sy <- s_rowstats(y > 0, "nnzero")
	expect_equal(as.numeric(apply(xy > 0, 1, nnzero)), as.numeric(stat_c(sx, sy)))

	x[,1] <- NA
	y[1,] <- NA
	xy <- cbind(x, y)

	expect_true(all(is.na(s_rowstats(x, "mean"))))
	expect_true(any(is.na(s_rowstats(y, "mean"))))

	sx <- s_rowstats(x, "mean", na.rm=TRUE)
	sy <- s_rowstats(y, "mean", na.rm=TRUE)
	expect_equal(as.numeric(apply(xy, 1, mean, na.rm=TRUE)), as.numeric(stat_c(sx, sy)))

	sx <- s_rowstats(x, "var", na.rm=TRUE)
	sy <- s_rowstats(y, "var", na.rm=TRUE)
	expect_equal(as.numeric(apply(xy, 1, var, na.rm=TRUE)), as.numeric(stat_c(sx, sy)))

})

test_that("streaming matrix statistics (grouped)", {

	set.seed(1)
	x <- matrix(rnorm(1000), nrow=50, ncol=20)
	y <- matrix(rnorm(1000), nrow=50, ncol=20)
	
	cgroupx <- sample(4, ncol(x), replace=TRUE)
	cgroupy <- sample(4, ncol(y), replace=TRUE)
	
	rgroupx <- sample(6, nrow(x), replace=TRUE)
	rgroupy <- sample(6, nrow(y), replace=TRUE)

	cxy <- cbind(x, y)
	rxy <- rbind(x, y)

	sx <- s_rowstats(x, "mean", cgroupx)
	sy <- s_rowstats(y, "mean", cgroupy)
	ans1 <- stat_c(sx, sy)
	ans2 <- t(aggregate(t(cxy), list(c(cgroupx, cgroupy)), "mean")[-1L])
	expect_equal(unclass(ans1), ans2, check.attributes=FALSE)

	sx <- s_colstats(x, "mean", rgroupx)
	sy <- s_colstats(y, "mean", rgroupy)
	ans1 <- stat_c(sx, sy)
	ans2 <- t(aggregate(rxy, list(c(rgroupx, rgroupy)), "mean")[-1L])
	expect_equal(unclass(ans1), ans2, check.attributes=FALSE)

	sx <- s_rowstats(x, "var", cgroupx)
	sy <- s_rowstats(y, "var", cgroupy)
	ans1 <- stat_c(sx, sy)
	ans2 <- t(aggregate(t(cxy), list(c(cgroupx, cgroupy)), "var")[-1L])
	expect_equal(unclass(ans1), ans2, check.attributes=FALSE)

	sx <- s_colstats(x, "var", rgroupx)
	sy <- s_colstats(y, "var", rgroupy)
	ans1 <- stat_c(sx, sy)
	ans2 <- t(aggregate(rxy, list(c(rgroupx, rgroupy)), "var")[-1L])
	expect_equal(unclass(ans1), ans2, check.attributes=FALSE)

})


