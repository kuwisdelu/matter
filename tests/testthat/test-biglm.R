require(testthat)
require(matter)

context("bigglm")

test_that("bigglm", {

	set.seed(1)
	register(SerialParam())
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

