require(testthat)
require(matter)

context("apply")

test_that("chunked - memory", {

	register(SerialParam())
	set.seed(1)
	x <- runif(100)
	y <- replicate(100, rnorm(10), simplify=FALSE)
	z <- matrix(rnorm(100^2), nrow=100, ncol=100)
	i <- chunkify(seq_len(100))
	i1 <- i[[1L]]

	nc <- 20L
	xc <- chunked_vector(x, nchunks=nc)
	yc <- chunked_vector(y, nchunks=nc)
	zc1 <- chunked_matrix(z, 1L, nchunks=nc)
	zc2 <- chunked_matrix(z, 2L, nchunks=nc)
	mc <- chunked_list(x, y, nchunks=nc)

	expect_equivalent(xc[[1L]], x[i1])
	expect_equivalent(yc[[1L]], y[i1])
	expect_equivalent(zc1[[1L]], z[i1,,drop=FALSE])
	expect_equivalent(zc2[[1L]], z[,i1,drop=FALSE])

	expect_equivalent(xc[1:3], list(xc[[1L]], xc[[2L]], xc[[3L]]))
	expect_equivalent(yc[1:3], list(yc[[1L]], yc[[2L]], yc[[3L]]))
	expect_equivalent(zc1[1:3], list(zc1[[1L]], zc1[[2L]], zc1[[3L]]))
	expect_equivalent(zc2[1:3], list(zc2[[1L]], zc2[[2L]], zc2[[3L]]))
	
	expect_equivalent(mc[[1L]][[1L]], xc[[1L]])
	expect_equivalent(mc[[1L]][[2L]], yc[[1L]])
	expect_equivalent(mc[1:2], list(mc[[1L]], mc[[2L]]))

	xcm_bp <- bplapply(xc, mean)
	xcm_l <- lapply(as.list(xc), mean)

	expect_equal(xcm_bp, xcm_l)

	register(SerialParam())
	set.seed(1)	
	u <- sort(runif(100))
	ind <- roll(seq_along(u), width=5, na.drop=TRUE)

	uc <- chunked_vector(u, nchunks=nc, depends=ind)

	expect_equal(attr(uc[[1L]], "chunksize"), length(i1))
	expect_is(attr(uc[[1L]], "depends"), "list")
	expect_equal(
		length(attr(uc[[1L]], "depends")),
		length(attr(uc[[1L]], "index")))

})

test_that("chunked - matter", {

	register(SerialParam())
	set.seed(1)
	x <- as.matter(runif(100))
	y <- as.matter(replicate(100, rnorm(10), simplify=FALSE))
	z <- as.matter(matrix(rnorm(100^2), nrow=100, ncol=100))
	i <- chunkify(seq_len(100))
	i1 <- i[[1L]]

	nc <- 20L
	xl <- chunked_vector(x, nchunks=nc, local=TRUE)
	yl <- chunked_vector(y, nchunks=nc, local=TRUE)
	zl1 <- chunked_matrix(z, 1L, nchunks=nc, local=TRUE)
	zl2 <- chunked_matrix(z, 2L, nchunks=nc, local=TRUE)
	ml <- chunked_list(x, y, nchunks=nc, local=TRUE)

	expect_equivalent(xl[[1L]], x[i1,drop=NULL])
	expect_equivalent(yl[[1L]], y[i1,drop=NULL])
	expect_equivalent(zl1[[1L]], z[i1,,drop=NULL])
	expect_equivalent(zl2[[1L]], z[,i1,drop=NULL])

	nc <- 20L
	xc <- chunked_vector(x, nchunks=nc, local=FALSE)
	yc <- chunked_vector(y, nchunks=nc, local=FALSE)
	zc1 <- chunked_matrix(z, 1L, nchunks=nc, local=FALSE)
	zc2 <- chunked_matrix(z, 2L, nchunks=nc, local=FALSE)
	mc <- chunked_list(x, y, nchunks=nc, local=FALSE)

	expect_equivalent(xc[[1L]], x[i1])
	expect_equivalent(yc[[1L]], y[i1])
	expect_equivalent(zc1[[1L]], z[i1,,drop=TRUE])
	expect_equivalent(zc2[[1L]], z[,i1,drop=TRUE])

})

test_that("chunkLapply + chunkMapply", {

	register(SerialParam())
	set.seed(1)
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
	set.seed(1)	
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
	set.seed(1)
	y <- replicate(100, rexp(10), simplify=FALSE)
	path <- tempfile()

	ans1 <- chunkLapply(y, log1p, outpath=path)
	ans2 <- chunkLapply(y, log1p, outpath=path, simplify=TRUE)
	ans3 <- chunkLapply(y, mean, outpath=path, simplify=TRUE)

	expect_equal(ans1[], lapply(y, log1p))
	expect_equal(ans2[], sapply(y, log1p))
	expect_equal(ans3[], sapply(y, mean))

	register(SerialParam())
	set.seed(1)
	u <- replicate(10, runif(10), simplify=FALSE)
	v <- replicate(10, runif(10), simplify=FALSE)
	f <- function(x, y) x + y

	ans4 <- chunkMapply(f, u, v, outpath=path)

	expect_equal(ans4[], Map(f, u, v))

})

test_that("chunkApply", {

	register(SerialParam())
	set.seed(1)
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
	set.seed(1)
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

