require(testthat)
require(matter)

context("cross validation")

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
	expect_equivalent(ps2["MAPE"], mean(abs((y - x) / y)))

})

test_that("cv_do classification", {

	register(SerialParam())
	set.seed(1)
	n <- 100
	p <- 5
	nfolds <- 3
	folds <- rep(paste0("set", seq_len(nfolds)), each=n)
	y <- rep(c(rep.int("yes", 60), rep.int("no", 40)), nfolds)
	x <- matrix(rnorm(nfolds * n * p), nrow=nfolds * n, ncol=p)
	x[,1L] <- x[,1L] + 2 * ifelse(y == "yes", runif(n), -runif(n))
	x[,2L] <- x[,2L] + 2 * ifelse(y == "no", runif(n), -runif(n))
	colnames(x) <- paste0("x", seq_len(p))

	k <- 1:5
	cv <- cv_do(pls_nipals, x, y, k=k, folds=folds, verbose=TRUE)

	expect_length(cv$scores, nfolds)
	expect_equal(nrow(cv$average), length(k))

})

test_that("cv_do regression", {

	register(SerialParam())
	set.seed(1)
	n <- 100
	p <- 5
	nfolds <- 3
	folds <- rep(paste0("set", seq_len(nfolds)), each=n)
	x <- matrix(rnorm(nfolds * n * p), nrow=nfolds * n, ncol=p)
	colnames(x) <- paste0("x", seq_len(p))
	y <- runif(nrow(x))
	y <- y + 10 * runif(1) * x[,1L] - 10 * runif(1) * x[,2L]
	y <- y + rnorm(nrow(x))

	k <- 1:5
	cv <- cv_do(pls_nipals, x, y, k=1:5, folds=folds, verbose=TRUE)

	expect_length(cv$scores, nfolds)
	expect_equal(nrow(cv$average), length(k))

})
