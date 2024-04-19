require(testthat)
require(matter)

context("mi_learn")

test_that("mi_learn", {

	register(SerialParam())
	set.seed(1)
	n <- 200
	p <- 5
	g <- 8
	samp <- rep(paste0("s", seq_len(g)), each=n %/% g)
	samp <- factor(rep_len(samp, n))
	folds <- ifelse(samp %in% c("s1", "s2", "s5", "s6"), "f1", "f2")
	x <- matrix(rnorm(n * p), nrow=n, ncol=p)
	colnames(x) <- paste0("x", seq_len(p))

	# create bagged labels
	y <- ifelse(samp %in% c("s1", "s2", "s3", "s4"), "pos", "neg")
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
	yj <- rep(c("pos", "neg"), each=g %/% 2)
	yj <- factor(yj, levels=c("pos", "neg"))

	fit0 <- nscentroids(x=x, y=y)
	fit1 <- mi_learn(nscentroids, x=x, y=y, bags=samp, priors=1)
	fit2 <- mi_learn(nscentroids, x=x, y=yj, bags=samp, priors=1)
	fitcv0 <- cv_do(nscentroids, x=x, y=z, bags=samp, folds=folds, priors=1)

	y0 <- fitted(fit0, "class")
	y1 <- fitted(fit1, "class")
	y2 <- fitted(fit2, "class")
	ycv0 <- fitted(fitcv0, type="class")

	expect_equal(class(fit0), class(fit1))
	expect_equal(fit1, fit2)
	expect_equal(y1, y2)
	expect_gt(mean(y1 == z), mean(y0 == z))
	expect_gt(mean(y2 == z), mean(y0 == z))
	expect_gt(mean(ycv0 == z), mean(y0 == z))

	fit3 <- pls_nipals(x=x, y=y, k=2, center=FALSE)
	fit4 <- mi_learn(pls_nipals, x=x, y=y, bags=samp, k=2, center=FALSE)

	y3 <- fitted(fit3, "class")
	y4 <- fitted(fit4, "class")

	expect_gt(mean(y4 == z), mean(y3 == z))

})

