require(testthat)
require(matter)

context("fastmap")

test_that("fastmap", {

	register(SerialParam())

	# Arithmetic example from Faloutsos and Lin

	d1 <- c(0, 1, 1, 100, 100)
	d2 <- c(1, 0, 1, 100, 100)
	d3 <- c(1, 1, 0, 100, 100)
	d4 <- c(100, 100, 100, 0, 1)
	d5 <- c(100, 100, 100, 1, 0)

	d <- rbind(d1, d2, d3, d4, d5)
	
	o1 <- c(1/sqrt(2), 0, 0, 0, 0, 0)
	o2 <- c(0, 1/sqrt(2), 0, 0, 0, 0)
	o3 <- c(0, 0, 1/sqrt(2), 0, 0, 0)
	o4 <- c(0, 0, 0, 0, sqrt(100^2 - (1/sqrt(2))^2 - (1/2)^2), 1/2)
	o5 <- c(0, 0, 0, 0, sqrt(100^2 - (1/sqrt(2))^2 - (1/2)^2), -1/2)

	x <- rbind(o1, o2, o3, o4, o5)

	expect_equivalent(as.matrix(dist(x)), d)

	# Note: due to ties in the example data
	# there are multiple "correct" pivot
	# so initialization matters --
	# the seed below reproduces paper results

	f1 <- c(0, 0.005, 0.005, 100, 99.995)
	f2 <- c(0.707089, 1.41418, 1.06062, 0.707089, 0)
	f3 <- c(0.668149, 0.935411, 0, 0.668149, 1)

	# EXCEPT for component #3
	# where the ties are broken
	# by floating point error...
	# so only compare #1 and #2

	f <- cbind(f1, f2, f3)

	set.seed(1, kind="Mersenne-Twister")
	fm <- fastmap(x)

	expect_equivalent(f[,1:2], fm$x[,1:2], tolerance=1e-5)

	p1 <- c(1, 4)
	p2 <- c(5, 2)
	p3 <- c(3, 5)

	p <- rbind(p1, p2, p3)

	expect_equivalent(p[1:2,], fm$pivots[1:2,1:2])

	pred <- predict(fm, x)

	expect_equivalent(fm$x, pred)
	
})

test_that("fastmap - matter matrix", {

	register(SerialParam())
	set.seed(1, kind="default")
	nr <- 100
	nc <- 25
	vals1 <- sort(runif(2500))
	vals2 <- rev(sort(runif(2500)))
	x1 <- matrix(vals1, nrow=100, ncol=25)
	x2 <- matrix(vals2, nrow=100, ncol=25)
	x <- cbind(x1, x2)
	y <- matter_mat(x)

	set.seed(1, kind="default")
	fmx <- fastmap(x)
	set.seed(1, kind="default")
	fmy <- fastmap(y)

	expect_equal(fmx$x, fmy$x)

})

test_that("fastmap - sparse matrix", {

	register(SerialParam())
	set.seed(1, kind="default")
	x <- rbinom(5000, 1, 0.2)
	x[x != 0] <- seq_len(sum(x != 0))
	dim(x) <- c(100, 50)
	y <- sparse_mat(x)

	set.seed(1, kind="default")
	fmx <- fastmap(x)
	set.seed(1, kind="default")
	fmy <- fastmap(y)

	expect_equal(fmx$x, fmy$x)

})

