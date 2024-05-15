require(testthat)
require(matter)

context("chunked")

test_that("chunked - memory", {

	set.seed(1, kind="default")
	x <- runif(100)
	y <- replicate(100, rnorm(10), simplify=FALSE)
	z <- matrix(rnorm(100^2), nrow=100, ncol=100)
	i <- chunkify(seq_len(100))
	i1 <- i[[1L]]

	nc <- 20L
	xc <- chunked_vec(x, nchunks=nc)
	yc <- chunked_vec(y, nchunks=nc)
	zc1 <- chunked_mat(z, 1L, nchunks=nc)
	zc2 <- chunked_mat(z, 2L, nchunks=nc)
	mc <- chunked_list(x, y, nchunks=nc)

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

	set.seed(1, kind="default")	
	u <- sort(runif(100))
	ind <- roll(seq_along(u), width=5, na.drop=TRUE)
	uc <- chunked_vec(u, nchunks=nc, depends=ind)
	a <- attr(uc[[1L]], "chunkinfo")

	expect_equal(a$chunksize, length(i1))
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

	# TODO reimplement local=TRUE

	# nc <- 20L
	# loc <- TRUE
	# xl <- chunked_vec(x, nchunks=nc, local=loc)
	# yl <- chunked_vec(y, nchunks=nc, local=loc)
	# zl1 <- chunked_mat(z, 1L, nchunks=nc, local=loc)
	# zl2 <- chunked_mat(z, 2L, nchunks=nc, local=loc)
	# ml <- chunked_list(x, y, nchunks=nc, local=loc)

	# expect_equivalent(xl[[1L]], x[i1,drop=NULL])
	# expect_equivalent(yl[[1L]], y[i1,drop=NULL])
	# expect_equivalent(zl1[[1L]], z[i1,,drop=NULL])
	# expect_equivalent(zl2[[1L]], z[,i1,drop=NULL])

	nc <- 20L
	loc <- FALSE
	xc <- chunked_vec(x, nchunks=nc, local=loc)
	yc <- chunked_vec(y, nchunks=nc, local=loc)
	zc1 <- chunked_mat(z, 1L, nchunks=nc, local=loc)
	zc2 <- chunked_mat(z, 2L, nchunks=nc, local=loc)
	mc <- chunked_list(x, y, nchunks=nc, local=loc)

	expect_equivalent(xc[[1L]], x[i1])
	expect_equivalent(yc[[1L]], y[i1])
	expect_equivalent(zc1[[1L]], z[i1,,drop=TRUE])
	expect_equivalent(zc2[[1L]], z[,i1,drop=TRUE])

})

