require(testthat)
require(matter)

context("stream-statistics")

test_that("streaming statistics", {

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

	x[1] <- NA; y[1:5] <- NA

	xy <- c(x, y)

	expect_true(is.na(s_mean(x)))

	expect_true(is.na(s_mean(y)))

	s_x <- s_mean(x, na.rm=TRUE); s_y <- s_mean(y, na.rm=TRUE)

	expect_equal(mean(xy, na.rm=TRUE), as.numeric(stat_c(s_x, s_y)))

	s_x <- s_var(x, na.rm=TRUE); s_y <- s_var(y, na.rm=TRUE)

	expect_equal(var(xy, na.rm=TRUE), as.numeric(stat_c(s_x, s_y)))

})

