require(testthat)
require(matter)

context("bigglm")

test_that("bigglm - matter matrix", {

	set.seed(1)
	x <- matrix(rnorm(1000), nrow=100, ncol=10)
	y <- matter_mat(x, nrow=100, ncol=10)
	colnames(x) <- c(paste0("x", 1:9), "y")
	colnames(y) <- c(paste0("x", 1:9), "y")

	fm <- paste0("y ~ ", paste0(paste0("x", 1:9), collapse=" + "))
	fm <- as.formula(fm)

	fit.x <- lm(fm, data=as.data.frame(x))
	gfit.x <- glm(fm, data=as.data.frame(x))
	gfit.y <- bigglm(fm, data=y)

	expect_equal(coef(fit.x), coef(gfit.y), tolerance=0.1)
	expect_equal(coef(gfit.x), coef(gfit.y), tolerance=0.1)

})

test_that("bigglm - sparse matrix", {

	set.seed(1)
	x <- rbinom(1000, 1, 0.2)
	x[x != 0] <- seq_len(sum(x != 0))
	dim(x) <- c(100, 10)
	y <- sparse_mat(x)
	colnames(x) <- c(paste0("x", 1:9), "y")
	colnames(y) <- c(paste0("x", 1:9), "y")

	fm <- paste0("y ~ ", paste0(paste0("x", 1:9), collapse=" + "))
	fm <- as.formula(fm)

	fit.x <- lm(fm, data=as.data.frame(x))
	gfit.x <- glm(fm, data=as.data.frame(x))
	gfit.y <- bigglm(fm, data=y)

	expect_equal(coef(fit.x), coef(gfit.y), tolerance=0.1)
	expect_equal(coef(gfit.x), coef(gfit.y), tolerance=0.1)

})


