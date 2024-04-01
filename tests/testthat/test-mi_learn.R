require(testthat)
require(matter)

context("mi_learn")

test_that("mi_learn", {

	register(SerialParam())
	set.seed(1)
	n <- 100
	p <- 5
	g <- 5
	group <- rep(paste0("s", seq_len(g)), each=n %/% g)
	group <- factor(rep_len(group, n))
	x <- matrix(rnorm(n * p), nrow=n, ncol=p)
	colnames(x) <- paste0("x", seq_len(p))

	# create bagged labels
	y <- ifelse(group %in% c("s1", "s2", "s3"), "pos", "neg")
	y <- factor(y, levels=c("pos", "neg"))
	ipos <- y %in% "pos"
	ineg <- y %in% "neg"
	z <- y

	# create "true" labels (with some within-bag noise)
	z[ipos] <- sample(c("pos", "neg"), sum(ipos), replace=TRUE)
	jpos <- z %in% "pos"
	jneg <- z %in% "neg"

	# create data
	x[jpos,] <- x[jpos,] + rnorm(p * sum(jpos), mean=1)
	x[jneg,] <- x[jneg,] - rnorm(p * sum(jneg), mean=1)

	# create "observed" labels
	yj <- c("pos", "pos", "pos", "neg", "neg")
	yj <- factor(yj, levels=c("pos", "neg"))

	fit0 <- nscentroids(x=x, y=y)
	fit1 <- mi_learn(nscentroids, x=x, y=y, group=group, priors=1)
	fit2 <- mi_learn(nscentroids, x=x, y=yj, group=group, priors=1)

	y0 <- fitted(fit0, "class")
	y1 <- fitted(fit1, "class")
	y2 <- fitted(fit2, "class")

	expect_equal(class(fit0), class(fit1))
	expect_equal(fit1, fit2)
	expect_equal(y1, y2)
	expect_gt(mean(y1 == z), mean(y0 == z))
	expect_gt(mean(y2 == z), mean(y0 == z))

})

test_that("predscore", {

	set.seed(1)
	n <- 1000
	s <- c("a", "b", "c")
	x <- sample(s, n, replace=TRUE)
	y <- ifelse(runif(n) > 0.1, x, sample(s, n, replace=TRUE))
	ps <- predscore(x, y)
	
	expect_equal(ps["a","Recall"], mean((x == y)[y == "a"]))
	expect_equal(ps["b","Recall"], mean((x == y)[y == "b"]))
	expect_equal(ps["c","Recall"], mean((x == y)[y == "c"]))
	expect_equal(ps["a","Precision"], mean((x == y)[x == "a"]))
	expect_equal(ps["b","Precision"], mean((x == y)[x == "b"]))
	expect_equal(ps["c","Precision"], mean((x == y)[x == "c"]))

	set.seed(2)
	n <- 1000
	x <- rnorm(n)
	y <- jitter(runif(1) * rnorm(n))
	ps2 <- predscore(x, y)

	expect_equivalent(ps2["RMSE"], sqrt(mean((y - x)^2)))
	expect_equivalent(ps2["MAE"], mean(abs(y - x)))
	expect_equivalent(ps2["MAPE"], mean(abs(y - x) / y))

})
