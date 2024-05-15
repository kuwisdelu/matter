require(testthat)
require(matter)

context("apply")

test_that("apply RNG", {

	n <- 10
	ns <- rep.int(n, 100)

	# check that local seed is forwarded
	register(SerialParam())
	set.seed(1, kind="Mersenne-Twister")
	x1 <- runif(n)
	set.seed(1, kind="Mersenne-Twister")
	ans1 <- chunkLapply(ns, runif, RNG=TRUE)

	expect_equal(ans1[[1L]], x1)

	# check that original seed is reset
	register(SerialParam())
	set.seed(1, kind="Mersenne-Twister")
	s <- getRNGStream()
	ans1 <- chunkLapply(ns, runif, RNG=TRUE)

	expect_equal(getRNGStream(), s)

	# check that parallel seed is iterated
	register(SerialParam())
	set.seed(1, kind="L'Ecuyer-CMRG")
	s1 <- getRNGStream()
	s2 <- parallel::nextRNGSubStream(s1$seed)
	setRNGStream(s1)
	y1 <- runif(n)
	setRNGStream(s2)
	y2 <- runif(n)
	set.seed(1, kind="L'Ecuyer-CMRG")
	ans2 <- chunkLapply(ns, runif, RNG=TRUE)

	expect_equal(ans2[[1L]], y1)
	expect_equal(ans2[[2L]], y2)

	# check that parallel seed is iterated (MC)
	register(MulticoreParam())
	set.seed(1, kind="L'Ecuyer-CMRG")
	ans3 <- chunkLapply(ns, runif, RNG=TRUE)

	expect_equal(ans3[[1L]], y1)
	expect_equal(ans3[[2L]], y2)

	# check that seeds is independent of nchunks
	register(MulticoreParam())
	set.seed(1, kind="L'Ecuyer-CMRG")
	ans4 <- chunkLapply(ns, runif, nchunks=10, RNG=TRUE)
	set.seed(1, kind="L'Ecuyer-CMRG")
	ans5 <- chunkLapply(ns, runif, nchunks=50, RNG=TRUE)

	expect_equal(ans4, ans5)

	# restore defaults for other tests
	RNGkind("default", "default", "default")

})

test_that("chunkLapply + chunkMapply", {

	register(SerialParam())
	set.seed(1, kind="default")
	a <- replicate(100, rnorm(10), simplify=FALSE)
	b <- replicate(100, runif(10), simplify=FALSE)

	expect_equal(
		chunkLapply(a, mean),
		lapply(a, mean))
	expect_equal(
		chunkLapply(a, mean, nchunks=10),
		lapply(a, mean))
	expect_equal(
		chunkLapply(a, mean, simplify=TRUE),
		sapply(a, mean))
	
	expect_equal(
		chunkMapply(`+`, a, b),
		mapply(`+`, a, b, SIMPLIFY=FALSE))
	expect_equal(
		chunkMapply(`+`, a, b, nchunks=10),
		mapply(`+`, a, b, SIMPLIFY=FALSE))

	register(SerialParam())
	set.seed(1, kind="default")	
	u <- sort(runif(100))
	v <- rev(u)
	ind <- roll(seq_along(u), width=5, na.drop=TRUE)
	f <- function(x, y) mean(x + y)

	expect_equal(
		chunkLapply(u, mean, depends=ind, simplify=TRUE),
		sapply(ind, function(i) mean(u[i])))
	expect_equal(
		chunkLapply(u, mean, depends=ind, simplify=TRUE, nchunks=10),
		sapply(ind, function(i) mean(u[i])))
	
	expect_equal(
		chunkMapply(f, u, v, depends=ind, simplify=TRUE),
		sapply(ind, function(i) mean(u[i] + v[i])))
	expect_equal(
		chunkMapply(f, u, v, depends=ind, simplify=TRUE, nchunks=10),
		sapply(ind, function(i) mean(u[i] + v[i])))

})

test_that("chunkLapply + chunkMapply i/o", {

	register(SerialParam())
	set.seed(1, kind="default")
	y <- replicate(100, rexp(10), simplify=FALSE)
	path <- tempfile()

	ans1 <- chunkLapply(y, log1p, outpath=path)
	ans2 <- chunkLapply(y, log1p, outpath=path, simplify=TRUE)
	ans3 <- chunkLapply(y, mean, outpath=path, simplify=TRUE)

	expect_equal(ans1[], lapply(y, log1p))
	expect_equal(ans2[], sapply(y, log1p))
	expect_equal(ans3[], sapply(y, mean))

	register(SerialParam())
	set.seed(1, kind="default")
	u <- replicate(10, runif(10), simplify=FALSE)
	v <- replicate(10, runif(10), simplify=FALSE)
	f <- function(x, y) x + y

	ans4 <- chunkMapply(f, u, v, outpath=path)

	expect_equal(ans4[], Map(f, u, v))

})

test_that("chunkApply", {

	register(SerialParam())
	set.seed(1, kind="default")
	vals <- sort(round(10 * rexp(140), 2))
	x <- matrix(vals, nrow=20, ncol=7)

	expect_equal(
		chunkApply(x, 1L, mean),
		apply(x, 1L, mean, simplify=FALSE))
	expect_equal(
		chunkApply(x, 1L, mean, nchunks=10),
		apply(x, 1L, mean, simplify=FALSE))
	expect_equal(
		chunkApply(x, 1L, mean, simplify=TRUE),
		apply(x, 1L, mean))

	expect_equal(
		chunkApply(x, 2L, mean),
		apply(x, 2L, mean, simplify=FALSE))
	expect_equal(
		chunkApply(x, 2L, mean, nchunks=10),
		apply(x, 2L, mean, simplify=FALSE))
	expect_equal(
		chunkApply(x, 2L, mean, simplify=TRUE),
		apply(x, 2L, mean))

	expect_equal(
		chunkApply(x, 1L, log1p),
		apply(x, 1L, log1p, simplify=FALSE))
	expect_equal(
		chunkApply(x, 1L, log1p, nchunks=10),
		apply(x, 1L, log1p, simplify=FALSE))
	expect_equal(
		chunkApply(x, 1L, log1p, simplify=TRUE),
		apply(x, 1L, log1p))

	expect_equal(
		chunkApply(x, 2L, log1p),
		apply(x, 2L, log1p, simplify=FALSE))
	expect_equal(
		chunkApply(x, 2L, log1p, nchunks=10),
		apply(x, 2L, log1p, simplify=FALSE))
	expect_equal(
		chunkApply(x, 2L, log1p, simplify=TRUE),
		apply(x, 2L, log1p))

})

test_that("chunkApply i/o", {

	register(SerialParam())
	set.seed(1, kind="default")
	vals <- sort(round(10 * rexp(140), 2))
	x <- matrix(vals, nrow=20, ncol=7)
	path <- tempfile()

	ans1 <- chunkApply(x, 1L, log1p, outpath=path)
	ans2 <- chunkApply(x, 1L, log1p, outpath=path, simplify=TRUE)
	ans3 <- chunkApply(x, 1L, mean, outpath=path, simplify=TRUE)

	expect_equal(ans1[], apply(x, 1L, log1p, simplify=FALSE))
	expect_equal(ans2[], apply(x, 1L, log1p))
	expect_equal(ans3[], apply(x, 1L, mean))

	ans4 <- chunkApply(x, 2L, log1p, outpath=path)
	ans5 <- chunkApply(x, 2L, log1p, outpath=path, simplify=TRUE)
	ans6 <- chunkApply(x, 2L, mean, outpath=path, simplify=TRUE)

	expect_equal(ans4[], apply(x, 2L, log1p, simplify=FALSE))
	expect_equal(ans5[], apply(x, 2L, log1p))
	expect_equal(ans6[], apply(x, 2L, mean))

})

