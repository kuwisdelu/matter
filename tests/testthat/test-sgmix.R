require(testthat)
require(matter)

context("sgmix")

test_that("sgmix", {

	set.seed(1)
	nr <- 128
	nc <- 128
	x <- matrix(rnorm(nr * nc), nrow=nr, ncol=nc)
	i <- (nr %/% 8):(3 * nr %/% 8)
	j <- (nc %/% 8):(3 * nc %/% 8)
	x[i,j] <- rnorm(length(i) * length(j), mean=1)
	i <- (3 * nr %/% 8):(5 * nr %/% 8)
	j <- (3 * nc %/% 8):(5 * nc %/% 8)
	x[i,j] <- rnorm(length(i) * length(j), mean=2)
	i <- (5 * nr %/% 8):(7 * nr %/% 8)
	j <- (5 * nc %/% 8):(7 * nc %/% 8)
	x[i,j] <- rnorm(length(i) * length(j), mean=3)

	set.seed(2)
	gm02 <- sgmix(x, r=2, k=2, weights="gaussian")
	gm03 <- sgmix(x, r=2, k=3, weights="gaussian")
	gm04 <- sgmix(x, r=2, k=4, weights="gaussian")
	gm12 <- sgmix(x, r=2, k=2, weights="bilateral")
	gm22 <- sgmix(x, r=2, k=2, weights="adaptive")

	expect_length(gm02$mu, 2)
	expect_length(gm03$mu, 3)
	expect_length(gm04$mu, 4)

	expect_false(is.unsorted(gm02$mu))
	expect_false(is.unsorted(gm03$mu))
	expect_false(is.unsorted(gm04$mu))

	expect_true(all(gm02$sigma > 0))
	expect_true(all(gm03$sigma > 0))
	expect_true(all(gm04$sigma > 0))

	expect_equal(fitted(gm02), gm02$probability)
	expect_equal(fitted(gm12), gm12$probability)
	expect_equal(fitted(gm22), gm22$probability)

	expect_equal(fitted(gm02, type="class"), gm02$class)
	expect_equal(fitted(gm12, type="class"), gm12$class)
	expect_equal(fitted(gm22, type="class"), gm22$class)

	expect_setequal(fitted(gm02, type="mean"), gm02$mu)
	expect_setequal(fitted(gm03, type="mean"), gm03$mu)
	expect_setequal(fitted(gm04, type="mean"), gm04$mu)

	expect_equal(rowSums(gm02$probability), rep.int(1, length(x)))
	expect_equal(rowSums(gm03$probability), rep.int(1, length(x)))
	expect_equal(rowSums(gm04$probability), rep.int(1, length(x)))

})

test_that("sgmixn", {

	set.seed(1)
	nr <- 128
	nc <- 128
	x <- matrix(rnorm(nr * nc), nrow=nr, ncol=nc)
	i <- (nr %/% 8):(3 * nr %/% 8)
	j <- (nc %/% 8):(3 * nc %/% 8)
	x[i,j] <- rnorm(length(i) * length(j), mean=2)
	i <- (5 * nr %/% 8):(7 * nr %/% 8)
	j <- (5 * nc %/% 8):(7 * nc %/% 8)
	x[i,j] <- rnorm(length(i) * length(j), mean=4)
	group <- rep(c(1L, 2L), each=length(x) %/% 2, length.out=length(x))

	set.seed(2)
	gmn02 <- sgmixn(x, r=2, k=2, group=group)

	expect_length(gmn02$mu, 4)
	expect_true(all(gmn02$sigma > 0))

	expect_equal(fitted(gmn02), gmn02$probability)
	expect_equal(fitted(gmn02, type="class"), gmn02$class)
	expect_setequal(fitted(gmn02, type="mean"), gmn02$mu)
	
	expect_equal(rowSums(gmn02$probability), rep.int(1, length(x)))
	expect_equal(rowSums(gmn02$probability[,1:2]), as.numeric(group == 1L))
	expect_equal(rowSums(gmn02$probability[,3:4]), as.numeric(group == 2L))

})
