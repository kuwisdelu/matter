require(testthat)
require(matter)

context("chunked")

test_that("chunked - memory", {

	register(SerialParam())

	set.seed(1, kind="default")
	x <- runif(100)
	y <- replicate(100, rnorm(10), simplify=FALSE)
	z <- matrix(rnorm(100^2), nrow=100, ncol=100)
	i <- chunkify(seq_len(100))
	i1 <- i[[1L]]

	nchunks <- 20L
	xc <- chunked_vec(x, nchunks=nchunks)
	yc <- chunked_vec(y, nchunks=nchunks)
	zc1 <- chunked_mat(z, 1L, nchunks=nchunks)
	zc2 <- chunked_mat(z, 2L, nchunks=nchunks)
	mc <- chunked_list(x, y, nchunks=nchunks)

	expect_equivalent(xc[[1L]], x[i1])
	expect_equivalent(yc[[1L]], y[i1])
	expect_equivalent(zc1[[1L]], z[i1,,drop=FALSE])
	expect_equivalent(zc2[[1L]], z[,i1,drop=FALSE])

	expect_equivalent(xc[1:3], list(xc[[1L]], xc[[2L]], xc[[3L]]))
	expect_equivalent(yc[1:3], list(yc[[1L]], yc[[2L]], yc[[3L]]))
	expect_equivalent(zc1[1:3], list(zc1[[1L]], zc1[[2L]], zc1[[3L]]))
	expect_equivalent(zc2[1:3], list(zc2[[1L]], zc2[[2L]], zc2[[3L]]))

	expect_equivalent(xc[18:20], list(xc[[18L]], xc[[19L]], xc[[20L]]))
	expect_equivalent(yc[18:20], list(yc[[18L]], yc[[19L]], yc[[20L]]))
	expect_equivalent(zc1[18:20], list(zc1[[18L]], zc1[[19L]], zc1[[20L]]))
	expect_equivalent(zc2[18:20], list(zc2[[18L]], zc2[[19L]], zc2[[20L]]))
	
	expect_equivalent(mc[[1L]][[1L]], xc[[1L]])
	expect_equivalent(mc[[1L]][[2L]], yc[[1L]])
	expect_equivalent(mc[1:2], list(mc[[1L]], mc[[2L]]))

	xcm_bp <- bplapply(xc, mean)
	xcm_l <- lapply(as.list(xc), mean)

	expect_equal(xcm_bp, xcm_l)

	i2 <- sample.int(100)
	i3 <- list(sample.int(50), 50 + sample.int(50))
	xr1 <- chunked_vec(x, permute=TRUE)
	xr2 <- chunked_vec(x, permute=i2)
	xr3 <- chunked_vec(x, permute=i3)
	
	xrl1 <- unlist(as.list(xr1))
	xrl2 <- unlist(as.list(xr2))
	xrl3 <- unlist(as.list(xr3))

	expect_setequal(xrl1, x)
	expect_setequal(xrl2, x)
	expect_setequal(xrl3, x)

	expect_equal(xrl2, x[i2])
	expect_true(any(x[i3[[1L]]] %in% xr3[[1L]]))
	expect_true(any(x[i3[[2L]]] %in% xr3[[1L]]))

	set.seed(1, kind="default")	
	u <- sort(runif(100))
	ind <- roll(seq_along(u), width=5, na.drop=TRUE)
	uc <- chunked_vec(u, nchunks=nchunks, depends=ind)
	a <- attr(uc[[1L]], "chunkinfo")

	expect_equal(a$chunklen, length(i1))
	expect_is(a$depends, "list")
	expect_equal(
		length(a$depends),
		length(a$depends))

})

test_that("chunked - matter", {

	set.seed(1, kind="default")
	x <- as.matter(runif(100))
	y <- as.matter(replicate(100, rnorm(10), simplify=FALSE))
	z <- as.matter(matrix(rnorm(100^2), nrow=100, ncol=100))
	i <- chunkify(seq_len(100))
	i1 <- i[[1L]]

	nchunks <- 20L
	drop <- FALSE
	xc <- chunked_vec(x, nchunks=nchunks, drop=drop)
	yc <- chunked_vec(y, nchunks=nchunks, drop=drop)
	zc1 <- chunked_mat(z, 1L, nchunks=nchunks, drop=drop)
	zc2 <- chunked_mat(z, 2L, nchunks=nchunks, drop=drop)
	mc <- chunked_list(x, y, nchunks=nchunks, drop=drop)

	expect_equivalent(xc[[1L]], x[i1])
	expect_equivalent(yc[[1L]], y[i1])
	expect_equivalent(zc1[[1L]], z[i1,,drop=FALSE])
	expect_equivalent(zc2[[1L]], z[,i1,drop=FALSE])

	nchunks <- 20L
	drop <- NULL
	xl <- chunked_vec(x, nchunks=nchunks, drop=drop)
	yl <- chunked_vec(y, nchunks=nchunks, drop=drop)
	zl1 <- chunked_mat(z, 1L, nchunks=nchunks, drop=drop)
	zl2 <- chunked_mat(z, 2L, nchunks=nchunks, drop=drop)
	ml <- chunked_list(x, y, nchunks=nchunks, drop=drop)

	expect_equivalent(xl[[1L]], x[i1,drop=NULL])
	expect_equivalent(yl[[1L]], y[i1,drop=NULL])
	expect_equivalent(zl1[[1L]], z[i1,,drop=NULL])
	expect_equivalent(zl2[[1L]], z[,i1,drop=NULL])

})

