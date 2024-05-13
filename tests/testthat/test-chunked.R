require(testthat)
require(matter)

context("chunked")

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
