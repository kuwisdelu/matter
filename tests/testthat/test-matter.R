require(testthat)

context("matter")

test_that("vector subsetting", {

	x <- seq_len(100)

	y <- matter_vec(x, length=length(x))

	expect_equal(x, y[])

	expect_equal(x[1], y[1])

	expect_equal(x[1:10], y[1:10])

})

test_that("matrix subsetting", {

	x <- matrix(1:100, nrow=10, ncol=10)

	y <- matter_mat(x, nrow=10, ncol=10)

	expect_equal(x, y[])

	expect_equal(x[1,], y[1,])

	expect_equal(x[,1], y[,1])

	expect_equal(x[1,1], y[1,1])

	expect_equal(x[1:10,1:10], y[1:10,1:10])

})

test_that("matrix multiplication", {

	x <- matrix(1:100, nrow=10, ncol=10)

	y <- matter_mat(x, nrow=10, ncol=10)

	expect_equal(x %*% x, x %*% y)

	expect_equal(x %*% x, y %*% x)

	x <- matrix(1:100, nrow=10, ncol=10)

	y <- matter_mat(x, nrow=10, ncol=10, rowMaj=TRUE)

	expect_equal(x %*% x, x %*% y)

	expect_equal(x %*% x, y %*% x)

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

test_that("apply", {

	x <- matrix(1:100, nrow=10, ncol=10)

	y <- matter_mat(x, nrow=10, ncol=10)

	expect_equal(apply(x, 1, sum), apply(y, 1, sum))

	expect_equal(apply(x, 2, sum), apply(y, 2, sum))

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

	expect_equal(coef(fit.x), coef(fit.y))

})

# test_that("irlba", {

# 	set.seed(1)

# 	x <- matrix(rnorm(1000), nrow=100, ncol=10)

# 	y <- matter_mat(x, nrow=100, ncol=10)

# 	require(irlba)

# 	fit.x <- irlba(x, nu=0, nv=2)

# 	fit.y <- irlba(y, nu=0, nv=2, mult=`%*%`)

# 	expect_equal(fit.x$v, fit.y$v, tolerance=0.5)

# })




