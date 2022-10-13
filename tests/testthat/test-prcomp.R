require(testthat)
require(matter)

context("prcomp")

test_that("prcomp", {

	set.seed(1)
	x <- matrix(rnorm(1000), nrow=100, ncol=10)
	y <- matter_mat(x, nrow=100, ncol=10)
	pca.x <- prcomp(x)
	pca.y <- prcomp(y)

	expect_equal(
		abs(pca.x$rotation[,1:3]),
		abs(pca.y$rotation),
		tolerance=0.1)

})
