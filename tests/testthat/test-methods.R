require(testthat)
require(matter)

context("matter-methods")

test_that("matrix multiplication", {

	x <- matrix(1:100, nrow=10, ncol=10)

	y <- matter_mat(x, nrow=10, ncol=10)

	expect_equal(x %*% x, x %*% y)

	expect_equal(x %*% x, y %*% x)

	x <- matrix(1:100, nrow=10, ncol=10)

	y <- matter_mat(x, nrow=10, ncol=10, rowMaj=TRUE)

	expect_equal(x %*% x, x %*% y)

	expect_equal(x %*% x, y %*% x)

	x <- matrix(1:91, nrow=7, ncol=13)

	y <- matter_mat(x, nrow=7, ncol=13)

	expect_equal(x %*% t(x), x %*% t(y))

	expect_equal(t(x) %*% x, t(y) %*% x)

	x <- matrix(1:91, nrow=7, ncol=13)

	y <- matter_mat(x, nrow=7, ncol=13, rowMaj=TRUE)

	expect_equal(x %*% t(x), x %*% t(y))

	expect_equal(t(x) %*% x, t(y) %*% x)

})

test_that("summary statistics", {

	x <- seq_len(100)

	y <- matter_vec(x, length=100)

	expect_equal(sum(x), sum(y))

	expect_equal(mean(x), mean(y))

	expect_equal(var(x), var(y))

	expect_equal(sd(x), sd(y))

	x <- matrix(1:100, nrow=10, ncol=10)

	y <- matter_mat(x, nrow=10, ncol=10)

	expect_equal(colSums(x), colSums(y))

	expect_equal(colMeans(x), colMeans(y))

	expect_equal(apply(x, 2, var), colVars(y))

	expect_equal(apply(x, 2, sd), colSds(y))

	expect_equal(rowSums(x), rowSums(y))

	expect_equal(rowMeans(x), rowMeans(y))

	expect_equal(apply(x, 1, var), rowVars(y))

	expect_equal(apply(x, 1, sd), rowSds(y))

})

test_that("delayed ops 1", {

	x <- seq_len(10)

	y <- matter_vec(x, length=10)

	expect_equal(x + 1, (y + 1)[])

	expect_equal(2 * x + 1, (2 * y + 1)[])

	expect_equal(x / 2 - 1, (y / 2 - 1)[])

	expect_equal(log(x), log(x)[])

	expect_equal(exp(x), exp(x)[])

	expect_equal(x > 5, (y > 5)[])

	expect_equal(x >= 5, (y >= 5)[])

	expect_equal(x < 5, (y < 5)[])

	expect_equal(x <= 5, (y <= 5)[])

	expect_equal(x != 5, (y != 5)[])

	expect_equal(which(x == 5), which(y == 5))

}

test_that("delayed ops 2", {

	x <- seq_len(10) + 0.5

	y <- matter_vec(x, length=10)

	expect_equal(x + 1.1, (y + 1.1)[])

	expect_equal(2.2 * x + 1.1, (2.2 * y + 1.1)[])

	expect_equal(x / 2.2 - 1.1, (y / 2.2 - 1.1)[])

	expect_equal(log(x), log(x)[])

	expect_equal(exp(x), exp(x)[])

	expect_equal(x > 5.5, (y > 5.5)[])

	expect_equal(x >= 5.5, (y >= 5.5)[])

	expect_equal(x < 5.5, (y < 5.5)[])

	expect_equal(x <= 5.5, (y <= 5.5)[])

	expect_equal(x != 5.5, (y != 5.5)[])

	expect_equal(which(x == 5.5), which(y == 5.5))

})

test_that("apply", {

	x <- matrix(1:100, nrow=10, ncol=10)

	y <- matter_mat(x, nrow=10, ncol=10)

	expect_equal(apply(x, 1, sum), apply(y, 1, sum))

	expect_equal(apply(x, 2, sum), apply(y, 2, sum))

})

test_that("scale", {

	x <- matrix(1:100, nrow=10, ncol=10)

	y <- matter_mat(x, nrow=10, ncol=10)

	expect_equivalent(scale(x), scale(y)[])

	x <- t(x)

	y <- t(y)

	expect_equivalent(scale(x), scale(y)[])

	x <- matrix(1:91, nrow=7, ncol=13)

	y <- matter_mat(x, nrow=7, ncol=13)

	expect_equivalent(scale(x), scale(y)[])

	x <- t(x)

	y <- t(y)

	expect_equivalent(scale(x), scale(y)[])

})

test_that("bigglm", {

	set.seed(1)

	x <- matrix(rnorm(1000), nrow=100, ncol=10)

	y <- matter_mat(x, nrow=100, ncol=10)

	colnames(x) <- c(paste0("x", 1:9), "y")

	colnames(y) <- c(paste0("x", 1:9), "y")

	fm <- paste0("y ~ ", paste0(paste0("x", 1:9), collapse=" + "))
	fm <- as.formula(fm)

	fit.x <- glm(fm, data=as.data.frame(x))

	fit.y <- bigglm(fm, data=y, chunksize=100)

	expect_equal(coef(fit.x), coef(fit.y), tolerance=0.5)

	x2 <- as.data.frame(x)

	y2 <- as.matter(x2)

	fit.y2 <- bigglm(fm, data=y2, chunksize=100)

	expect_equal(coef(fit.x), coef(fit.y2), tolerance=0.5)

})

test_that("prcomp", {

	set.seed(1)

	x <- matrix(rnorm(1000), nrow=100, ncol=10)

	y <- matter_mat(x, nrow=100, ncol=10)

	pca.x <- prcomp(x)

	pca.y <- prcomp(y)

	expect_equal(pca.x$rotation[,1:3], pca.y$rotation, tolerance=0.5)

})

