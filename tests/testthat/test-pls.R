require(testthat)
require(matter)

context("pls")

test_that("pls - nipals", {

	x0 <- cbind(
		c(-1, 1, -1, 1),
		c(-1, -1, 1, 1))
	x1 <- cbind(
		c(-2.18, 1.84, -0.48, 0.83),
		c(-2.18, -0.16, 1.52, 0.83))
	y <- as.matrix(c(2, 2, 0, -4))

	w10 <- c(-0.45, -0.89)
	p10 <- c(-0.45, -0.89)
	b0 <- c(-1, -2)

	w11 <- c(-0.45, -0.89)
	p11 <- c(-0.69, -0.77)
	b1 <- c(0.08, -1.08)

	np0 <- pls_nipals(x0, y, k=1, center=FALSE)
	np1 <- pls_nipals(x1, y, k=2, center=FALSE)
	npt0 <- pls_nipals(t(x0), y, k=1, center=FALSE, transpose=TRUE)
	npt1 <- pls_nipals(x1, y, k=2, center=FALSE)

	np0f <- predict(np0)
	np1f <- predict(np1)
	np0p <- predict(np0, x0)
	np1p <- predict(np1, x1)

	expect_equal(coef(np0), np0$coefficients)
	expect_equal(resid(np0), np0$residuals)
	expect_equal(fitted(np0), np0$fitted.values)
	expect_equal(loadings(np0), np0$loadings)

	expect_equal(np0$fitted.values, np0f)
	expect_equal(np1$fitted.values, np1f)
	expect_equal(np0$fitted.values, np0p)
	expect_equal(np1$fitted.values, np1p)

	expect_equivalent(np0$weights, w10, tolerance=1e-2)
	expect_equivalent(np0$loadings, p10, tolerance=1e-2)
	expect_equivalent(np0$coefficients, b0, tolerance=1e-2)

	expect_equivalent(np1$weights[,1L], w11, tolerance=1e-2)
	expect_equivalent(np1$loadings[,1L], p11, tolerance=1e-2)
	expect_equivalent(np1$coefficients, b1, tolerance=1e-2)

	expect_equivalent(npt0$weights, w10, tolerance=1e-2)
	expect_equivalent(npt0$loadings, p10, tolerance=1e-2)
	expect_equivalent(npt0$coefficients, b0, tolerance=1e-2)

	expect_equivalent(npt1$weights[,1L], w11, tolerance=1e-2)
	expect_equivalent(npt1$loadings[,1L], p11, tolerance=1e-2)
	expect_equivalent(npt1$coefficients, b1, tolerance=1e-2)

})

test_that("pls - da", {

	x <- cbind(
		c(-1, -5, -7.5, 10, -2.5, 5, 5),
		c(-1, -2.5, -7.5, 7.5, 12.5, 10, 5))
	y <- factor(c("a", "a", "a", "b", "a", "b", "b"))

	np1 <- pls_nipals(x, y, k=2)
	yf <- apply(fitted(np1), 1L, which.max)
	yp <- predict(np1, x, type="class")

	expect_equal(y, predict(np1, type="class"))
	expect_equal(fitted(np1), predict(np1, x))
	expect_equal(as.integer(y), yf)
	expect_equal(y, yp)

})

test_that("opls - nipals", {

	x1 <- cbind(
		c(-2.18, 1.84, -0.48, 0.83),
		c(-2.18, -0.16, 1.52, 0.83))
	y <- as.matrix(c(2, 2, 0, -4))

	no1 <- opls_nipals(x1, y, k=1, center=FALSE)
	not1 <- opls_nipals(t(x1), y, k=1, center=FALSE, transpose=TRUE)
	nop1 <- pls_nipals(no1$x, y, k=1, center=FALSE)

	wo1 <- c(-0.89, 0.45)
	po1 <- c(-1.16, -0.09)
	to1 <- c(0.97, -1.71, 1.11, -0.37)
	
	w10 <- c(-0.45, -0.89)
	p10 <- c(-0.45, -0.89)
	bo1 <- c(-0.41, -0.82)

	expect_equivalent(no1$weights, wo1, tolerance=1e-2)
	expect_equivalent(no1$loadings, po1, tolerance=1e-2)
	expect_equivalent(no1$scores, to1, tolerance=1e-2)

	expect_equivalent(not1$weights, wo1, tolerance=1e-2)
	expect_equivalent(not1$loadings, po1, tolerance=1e-2)
	expect_equivalent(not1$scores, to1, tolerance=1e-2)

	expect_equivalent(nop1$weights, w10, tolerance=1e-2)
	expect_equivalent(nop1$loadings, p10, tolerance=1e-2)
	expect_equivalent(nop1$coefficients, bo1, tolerance=1e-2)

	no1f <- predict(no1)
	no1p <- predict(no1, x1, k=1)

	not1f <- predict(not1)
	not1p <- predict(not1, t(x1), k=1)

	expect_equal(no1f, no1$x)
	expect_equal(no1p, no1$x)
	expect_equal(not1f, not1$x)
	expect_equal(not1p, not1$x)

	no2 <- opls_nipals(x1, y, k=2, center=FALSE)
	nop2 <- pls_nipals(no2$x, y, k=1, center=FALSE)

	no2f <- predict(no2)
	no2p1 <- predict(no2, x1, k=1)
	no2p2 <- predict(no2, x1, k=2)

	expect_equal(no2f, no2$x)
	expect_equal(no2p1, no1$x)
	expect_equal(no2p2, no2$x)

})

test_that("opls - da", {

	x <- cbind(
		c(-1, -5, -7.5, 10, -2.5, 5, 5),
		c(-1, -2.5, -7.5, 7.5, 12.5, 10, 5))
	y <- factor(c("a", "a", "a", "b", "a", "b", "b"))

	no1 <- opls_nipals(x, y, k=1)
	np1 <- pls_nipals(no1$x, y, k=1)
	yf <- apply(fitted(np1), 1L, which.max)
	yp <- predict(np1, no1$x, type="class")

	expect_equal(no1$x, predict(no1))

	expect_equal(y, predict(np1, type="class"))
	expect_equal(fitted(np1), predict(np1, no1$x))
	expect_equal(as.integer(y), yf)
	expect_equal(y, yp)

})
