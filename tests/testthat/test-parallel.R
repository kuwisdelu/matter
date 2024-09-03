require(testthat)
require(matter)

context("parallel")

test_that("RNG", {

	n <- 10
	ns <- rep.int(n, 100)

	# check that local seed is forwarded
	register(SerialParam())
	set.seed(1, kind="Mersenne-Twister")
	x1 <- runif(n)
	set.seed(1, kind="Mersenne-Twister")
	suppressWarnings(ans1 <- chunkLapply(ns, runif, RNG=TRUE))

	expect_equal(ans1[[1L]], x1)

	# check that local seed is iterated
	register(SerialParam())
	set.seed(1, kind="Mersenne-Twister")
	s1 <- getRNGStream()
	runif(1)
	s2 <- getRNGStream()
	setRNGStream(s1)
	y1 <- runif(n)
	setRNGStream(s2)
	y2 <- runif(n)
	set.seed(1, kind="Mersenne-Twister")
	suppressWarnings(ans2 <- chunkLapply(ns, runif, RNG=TRUE))

	expect_equal(ans2[[1L]], y1)
	expect_equal(ans2[[2L]], y2)

	# check that local seeds are independent of nchunks
	register(SerialParam())
	set.seed(1, kind="Mersenne-Twister")
	suppressWarnings(ans4 <- chunkLapply(ns, runif,
		chunkopts=list(nchunks=10), RNG=TRUE))
	set.seed(1, kind="Mersenne-Twister")
	suppressWarnings(ans5 <- chunkLapply(ns, runif,
		chunkopts=list(nchunks=50), RNG=TRUE))

	expect_equal(ans4, ans5)

	# check that original seed is reset
	register(SerialParam())
	set.seed(1, kind="Mersenne-Twister")
	s <- getRNGStream()
	suppressWarnings(ans1 <- chunkLapply(ns, runif, RNG=TRUE))

	expect_equal(getRNGStream(), s)

	# check that parallel seed is iterated (SC)
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

	# check that parallel seeds are independent of nchunks
	register(MulticoreParam())
	set.seed(1, kind="L'Ecuyer-CMRG")
	ans4 <- chunkLapply(ns, runif, chunkopts=list(nchunks=10), RNG=TRUE)
	set.seed(1, kind="L'Ecuyer-CMRG")
	ans5 <- chunkLapply(ns, runif, chunkopts=list(nchunks=50), RNG=TRUE)

	expect_equal(ans4, ans5)

	# restore defaults for other tests
	RNGkind("default", "default", "default")

})

test_that("isolated closures", {

	mkfun <- function() isofun(function() NULL)
	mkclos <- function(x = 0) isoclos(function() NULL)

	f0 <- isofun(function() NULL)
	f1 <- mkfun()
	f2 <- mkclos()

	expect_identical(environment(f0), baseenv())
	expect_identical(environment(f1), baseenv())
	
	expect_equal(ls(environment(f2)), "x")
	expect_identical(parent.env(environment(f2)), baseenv())

})

test_that("SnowfastParam", {

	set.seed(1, kind="default")
	n <- 2500
	len <- 50
	x <- replicate(len, runif(n), simplify=FALSE)

	sp0 <- SnowfastParam(workers=2)

	expect_is(sp0, "SnowfastParam")
	expect_is(sp0, "SnowParam")
	expect_is(bpbackend(sp0), "NULLcluster")
	expect_true(bpisup(bpstart(sp0)))
	expect_is(bpbackend(sp0), "SOCKcluster")
	expect_false(bpisup(bpstop(sp0)))

	zs0 <- chunkLapply(x, sum, BPPARAM=sp0)
	zn0 <- chunkLapply(x, sum, BPPARAM=NULL)

	expect_equal(zs0, zn0)

	sp1 <- SnowfastParam(workers=2, tasks=len)
	zs1 <- chunkLapply(x, sum, BPPARAM=sp1)
	zn1 <- chunkLapply(x, sum, BPPARAM=NULL)

	expect_equal(zs1, zn1)

	sp2 <- SnowfastParam(workers=2, progressbar=TRUE)
	zs2 <- chunkLapply(x, sum, BPPARAM=sp2)
	zn2 <- chunkLapply(x, sum, BPPARAM=NULL)

	expect_equal(zs2, zn2)

	bpstop(sp0)
	bpstop(sp1)
	bpstop(sp2)

})

test_that("SnowfastParam - matter", {

	set.seed(1, kind="default")
	n <- 2500
	len <- 50
	x <- replicate(len, runif(n), simplify=FALSE)
	y <- as.matter(x)

	sp0 <- SnowfastParam(workers=2)

	zs0 <- chunkLapply(y, sum,
		chunkopts=list(nchunks=5), BPPARAM=sp0)
	zn0 <- chunkLapply(y, sum,
		chunkopts=list(nchunks=5), BPPARAM=NULL)

	expect_equal(zs0, zn0)

	bpstop(sp0)

})
