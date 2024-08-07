require(testthat)
require(matter)

context("parallel")

test_that("SnowfastParam", {

	set.seed(1)
	n <- 25000
	len <- 500
	x <- replicate(len, runif(n), simplify=FALSE)

	sp0 <- SnowfastParam(workers=4)

	expect_is(sp0, "SnowfastParam")
	expect_is(sp0, "SnowParam")
	expect_is(bpbackend(sp0), "NULLcluster")
	expect_true(bpisup(bpstart(sp0)))
	expect_is(bpbackend(sp0), "SOCKcluster")
	expect_false(bpisup(bpstop(sp0)))

	zs0 <- chunkLapply(x, sum, BPPARAM=sp0)
	zn0 <- chunkLapply(x, sum, BPPARAM=NULL)

	expect_equal(zs0, zn0)

	sp1 <- SnowfastParam(workers=4, tasks=100)
	zs1 <- chunkLapply(x, sum, BPPARAM=sp1)
	zn1 <- chunkLapply(x, sum, BPPARAM=NULL)

	expect_equal(zs1, zn1)

	sp2 <- SnowfastParam(workers=4, progressbar=TRUE)
	zs2 <- chunkLapply(x, sum, BPPARAM=sp2)
	zn2 <- chunkLapply(x, sum, BPPARAM=NULL)

	expect_equal(zs2, zn2)

	bpstop(sp0)
	bpstop(sp1)
	bpstop(sp2)

})

test_that("SnowfastParam - matter", {

	set.seed(1)
	n <- 2500
	len <- 50
	x <- replicate(len, runif(n), simplify=FALSE)
	y <- as.matter(x)

	sp0 <- SnowfastParam(workers=4)

	zs0 <- chunkLapply(y, sum,
		chunkopts=list(nchunks=5), BPPARAM=sp0)
	zn0 <- chunkLapply(y, sum,
		chunkopts=list(nchunks=5), BPPARAM=NULL)

	expect_equal(zs0, zn0)

	bpstop(sp0)

})


