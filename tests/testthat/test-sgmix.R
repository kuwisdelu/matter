require(testthat)
require(matter)

context("sgmix")

test_that("sgmix", {

	set.seed(1)
	nr <- 64
	nc <- 64
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

	expect_length(gm02$mu, 2)
	expect_length(gm03$mu, 3)
	expect_length(gm04$mu, 4)

	expect_false(is.unsorted(rev(gm02$mu)))
	expect_false(is.unsorted(rev(gm03$mu)))
	expect_false(is.unsorted(rev(gm04$mu)))

	expect_true(all(gm02$sigma > 0))
	expect_true(all(gm03$sigma > 0))
	expect_true(all(gm04$sigma > 0))

	expect_setequal(fitted(gm02), gm02$mu)
	expect_setequal(fitted(gm03), gm03$mu)
	expect_setequal(fitted(gm04), gm04$mu)

	expect_equal(rowSums(gm02$probability), rep.int(1, length(x)))
	expect_equal(rowSums(gm03$probability), rep.int(1, length(x)))
	expect_equal(rowSums(gm04$probability), rep.int(1, length(x)))

	set.seed(3)
	gm12 <- sgmix(x, r=2, k=2, weights="bilateral")
	gm22 <- sgmix(x, r=2, k=2, weights="adaptive")

	expect_equal(fitted(gm02, type="class"), gm02$class)
	expect_equal(fitted(gm12, type="class"), gm12$class)
	expect_equal(fitted(gm22, type="class"), gm22$class)

	set.seed(4)
	gm02c <- sgmix(x, r=2, k=2, weights="gaussian", compress=TRUE)

	expect_is(gm02c$class, "drle_fct")
	expect_null(gm02c$probability)

})

test_that("sgmix (grouped)", {

	set.seed(1)
	nr <- 64
	nc <- 64
	x <- matrix(rnorm(nr * nc), nrow=nr, ncol=nc)
	i <- (nr %/% 8):(3 * nr %/% 8)
	j <- (nc %/% 8):(3 * nc %/% 8)
	x[i,j] <- rnorm(length(i) * length(j), mean=2)
	i <- (5 * nr %/% 8):(7 * nr %/% 8)
	j <- (5 * nc %/% 8):(7 * nc %/% 8)
	x[i,j] <- rnorm(length(i) * length(j), mean=4)
	group <- rep(c("A", "B"), each=length(x) %/% 2, length.out=length(x))

	set.seed(2)
	gm02 <- sgmix(x, r=2, k=2, group=group)

	expect_length(gm02$mu, 4)
	expect_true(all(gm02$sigma > 0))

	expect_equal(fitted(gm02, type="mu"), gm02$mu)
	expect_equal(fitted(gm02, type="sigma"), gm02$sigma)
	expect_equal(fitted(gm02, type="class"), gm02$class)
	
	gs <- attr(gm02$probability, "group")

	expect_equal(rowSums(gm02$probability), rep.int(1, length(x)))
	expect_equal(
		rowSums(gm02$probability[,gs %in% "A"]),
		as.numeric(group %in% "A"))
	expect_equal(
		rowSums(gm02$probability[,gs %in% "B"]),
		as.numeric(group %in% "B"))

})

test_that("sgmix (degenerate)", {

	set.seed(1)
	nr <- 64
	nc <- 64
	x <- matrix(rnorm(nr * nc), nrow=nr, ncol=nc)
	y <- matrix(rbinom(nr * nc, 1, 0.5), nrow=nr, ncol=nc)

	set.seed(2)
	gm01 <- sgmix(x, r=2, k=1)

	expect_setequal(gm01$class, 1L)
	expect_setequal(as.numeric(gm01$probability), 1)

	set.seed(3)
	gm02 <- sgmix(y, r=2, k=2)

	expect_setequal(gm02$mu, unique(y))
	expect_setequal(gm02$sigma, 0)

	set.seed(4)
	expect_warning(sgmix(y, r=2, k=3))

})

test_that("sgmixn", {

	register(SerialParam())
	
	f <- function()
	{
		nr <- 64
		nc <- 64
		x <- matrix(rnorm(nr * nc), nrow=nr, ncol=nc)
		i <- (nr %/% 8):(3 * nr %/% 8)
		j <- (nc %/% 8):(3 * nc %/% 8)
		x[i,j] <- rnorm(length(i) * length(j), mean=2)
		i <- (5 * nr %/% 8):(7 * nr %/% 8)
		j <- (5 * nc %/% 8):(7 * nc %/% 8)
		x[i,j] <- rnorm(length(i) * length(j), mean=4)
		as.vector(x)
	}

	set.seed(1)
	vals <- replicate(5, f())
	co <- expand.grid(x=1:64, y=1:64)
	group <- rep(c("A", "B"), each=nrow(vals) %/% 2, length.out=nrow(vals))

	set.seed(2)
	gmn02 <- sgmixn(co$x, co$y, vals, r=2, k=2, group=group)

	expect_length(gmn02$class, 5)
	expect_equal(dim(gmn02$mu), c(2,2,5))
	expect_equal(dim(gmn02$sigma), c(2,2,5))
	expect_equal(dim(gmn02$alpha), c(2,2,5))
	expect_equal(dim(gmn02$beta), c(2,5))
	
	expect_true(all(gmn02$sigma > 0))

	expect_equal(fitted(gmn02, type="mu"), gmn02$mu)
	expect_equal(fitted(gmn02, type="sigma"), gmn02$sigma)
	expect_equal(fitted(gmn02, type="class"), gmn02$class)
	
	expect_true(all(logLik(gmn02) > 0))

	set.seed(3)
	gmn02c <- sgmixn(co$x, co$y, vals, r=2, k=2, group=group, compress=TRUE)

	expect_is(gmn02c$class[[1L]], "drle_fct")

})

