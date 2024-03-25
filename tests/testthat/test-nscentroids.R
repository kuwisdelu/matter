require(testthat)
require(matter)

context("nscentroids")

test_that("nscentroids", {

	register(SerialParam())
	set.seed(1)
	n <- 100
	p <- 5
	x <- matrix(rnorm(n * p), nrow=n, ncol=p)
	colnames(x) <- paste0("x", seq_len(p))
	y <- ifelse(x[,1L] > 0 | x[,2L] < 0, "a", "b")

	sc0 <- nscentroids(x, y, s=0)
	sc1 <- nscentroids(x, y, s=1)
	sc2 <- nscentroids(x, y, s=2)
	sct0 <- nscentroids(t(x), y, s=0, transpose=TRUE)
	sct1 <- nscentroids(t(x), y, s=1, transpose=TRUE)
	sct2 <- nscentroids(t(x), y, s=2, transpose=TRUE)

	sc0f <- predict(sc0)
	sc1f <- predict(sc1)
	sc2f <- predict(sc2)
	sc0p <- predict(sc0, x)
	sc1p <- predict(sc1, x)
	sc2p <- predict(sc2, x)

	sct0f <- predict(sct0)
	sct1f <- predict(sct1)
	sct2f <- predict(sct2)
	sct0p <- predict(sct0, t(x))
	sct1p <- predict(sct1, t(x))
	sct2p <- predict(sct2, t(x))

	expect_equal(fitted(sc0), sc0f)
	expect_equal(fitted(sct0), sct0f)
	expect_equal(fitted(sc0), sc0p)
	expect_equal(fitted(sct0), sct0p)

	expect_equal(fitted(sc0, "class"), sc0$class)
	expect_equal(fitted(sct0, "class"), sct0$class)

	expect_equal(rowSums(fitted(sc0)), rep.int(1, n))
	expect_equal(rowSums(fitted(sc1)), rep.int(1, n))
	expect_equal(rowSums(fitted(sc2)), rep.int(1, n))
	expect_equal(rowSums(fitted(sct0)), rep.int(1, n))
	expect_equal(rowSums(fitted(sct1)), rep.int(1, n))
	expect_equal(rowSums(fitted(sct2)), rep.int(1, n))

	expect_gt(sum(sc0$statistic != 0), sum(sc1$statistic != 0))
	expect_gt(sum(sc1$statistic != 0), sum(sc2$statistic != 0))

	sc012 <- nscentroids(x, y, s=0:2)

	expect_equal(sc012[[1L]], sc0)
	expect_equal(sc012[[2L]], sc1)
	expect_equal(sc012[[3L]], sc2)

	expect_warning(nscentroids(x, y, s=3))

})
