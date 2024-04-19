require(testthat)
require(matter)

context("pls")

test_that("pls - nipals", {

	register(SerialParam())
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
	npt1 <- pls_nipals(t(x1), y, k=2, center=FALSE, transpose=TRUE)

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

	np0f <- predict(np0)
	np1f <- predict(np1)
	np0p <- predict(np0, x0)
	np1p <- predict(np1, x1)

	expect_equal(np0$fitted.values, np0f)
	expect_equal(np1$fitted.values, np1f)
	expect_equal(np0$fitted.values, np0p)
	expect_equal(np1$fitted.values, np1p)

	expect_equal(coef(np0), np0$coefficients)
	expect_equal(resid(np0), np0$residuals)
	expect_equal(fitted(np0), np0$fitted.values)
	expect_equal(loadings(np0), np0$loadings)

	np1f1 <- predict(np1, x1, k=1)
	np1f2 <- predict(np1, x1, k=2)
	np1f12 <- predict(np1, x1, k=1:2)

	expect_equivalent(np1f1, np1f12[,,1L])
	expect_equivalent(np1f2, np1f12[,,2L])

})

test_that("pls - simpls", {

	register(SerialParam())
	x0 <- cbind(
		c(-1, 1, -1, 1),
		c(-1, -1, 1, 1))
	x1 <- cbind(
		c(-2.18, 1.84, -0.48, 0.83),
		c(-2.18, -0.16, 1.52, 0.83))
	y <- as.matrix(c(2, 2, 0, -4))

	p10 <- c(-0.45, -0.89)
	b0 <- c(-1, -2)

	p11 <- c(-0.69, -0.77)
	b1 <- c(0.08, -1.08)

	sp0 <- pls_simpls(x0, y, k=1, center=FALSE)
	sp1 <- pls_simpls(x1, y, k=2, center=FALSE)
	spt0 <- pls_simpls(t(x0), y, k=1, center=FALSE, transpose=TRUE)
	spt1 <- pls_simpls(t(x1), y, k=2, center=FALSE, transpose=TRUE)

	expect_equivalent(sp0$loadings, p10, tolerance=1e-2)
	expect_equivalent(sp0$coefficients, b0, tolerance=1e-2)

	expect_equivalent(sp1$loadings[,1L], p11, tolerance=1e-2)
	expect_equivalent(sp1$coefficients, b1, tolerance=1e-2)

	expect_equivalent(spt0$loadings, p10, tolerance=1e-2)
	expect_equivalent(spt0$coefficients, b0, tolerance=1e-2)

	expect_equivalent(spt1$loadings[,1L], p11, tolerance=1e-2)
	expect_equivalent(spt1$coefficients, b1, tolerance=1e-2)

	sp0f <- predict(sp0)
	sp1f <- predict(sp1)
	sp0p <- predict(sp0, x0)
	sp1p <- predict(sp1, x1)

	expect_equal(sp0$fitted.values, sp0f)
	expect_equal(sp1$fitted.values, sp1f)
	expect_equal(sp0$fitted.values, sp0p)
	expect_equal(sp1$fitted.values, sp1p)

	expect_equal(coef(sp0), sp0$coefficients)
	expect_equal(resid(sp0), sp0$residuals)
	expect_equal(fitted(sp0), sp0$fitted.values)
	expect_equal(loadings(sp0), sp0$loadings)

	sp1f1 <- predict(sp1, x1, k=1)
	sp1f2 <- predict(sp1, x1, k=2)
	sp1f12 <- predict(sp1, x1, k=1:2)

	expect_equivalent(sp1f1, sp1f12[,,1L])
	expect_equivalent(sp1f2, sp1f12[,,2L])

})

test_that("pls - kernel #1", {

	register(SerialParam())
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

	kp0 <- pls_kernel(x0, y, k=1, center=FALSE)
	kp1 <- pls_kernel(x1, y, k=2, center=FALSE)
	kpt0 <- pls_kernel(t(x0), y, k=1, center=FALSE, transpose=TRUE)
	kpt1 <- pls_kernel(t(x1), y, k=2, center=FALSE, transpose=TRUE)

	expect_equivalent(kp0$weights, w10, tolerance=1e-2)
	expect_equivalent(kp0$loadings, p10, tolerance=1e-2)
	expect_equivalent(kp0$coefficients, b0, tolerance=1e-2)

	expect_equivalent(kp1$weights[,1L], w11, tolerance=1e-2)
	expect_equivalent(kp1$loadings[,1L], p11, tolerance=1e-2)
	expect_equivalent(kp1$coefficients, b1, tolerance=1e-2)

	expect_equivalent(kpt0$weights, w10, tolerance=1e-2)
	expect_equivalent(kpt0$loadings, p10, tolerance=1e-2)
	expect_equivalent(kpt0$coefficients, b0, tolerance=1e-2)

	expect_equivalent(kpt1$weights[,1L], w11, tolerance=1e-2)
	expect_equivalent(kpt1$loadings[,1L], p11, tolerance=1e-2)
	expect_equivalent(kpt1$coefficients, b1, tolerance=1e-2)

	kp0f <- predict(kp0)
	kp1f <- predict(kp1)
	kp0p <- predict(kp0, x0)
	kp1p <- predict(kp1, x1)

	expect_equal(kp0$fitted.values, kp0f)
	expect_equal(kp1$fitted.values, kp1f)
	expect_equal(kp0$fitted.values, kp0p)
	expect_equal(kp1$fitted.values, kp1p)

	expect_equal(coef(kp0), kp0$coefficients)
	expect_equal(resid(kp0), kp0$residuals)
	expect_equal(fitted(kp0), kp0$fitted.values)
	expect_equal(loadings(kp0), kp0$loadings)

	kp1f1 <- predict(kp1, x1, k=1)
	kp1f2 <- predict(kp1, x1, k=2)
	kp1f12 <- predict(kp1, x1, k=1:2)

	expect_equivalent(kp1f1, kp1f12[,,1L])
	expect_equivalent(kp1f2, kp1f12[,,2L])

})

test_that("pls - kernel #2", {

	register(SerialParam())
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

	kp0 <- pls_kernel(x0, y, k=1, method=2, center=FALSE)
	kp1 <- pls_kernel(x1, y, k=2, method=2, center=FALSE)
	kpt0 <- pls_kernel(t(x0), y, k=1, method=2, center=FALSE, transpose=TRUE)
	kpt1 <- pls_kernel(t(x1), y, k=2, method=2, center=FALSE, transpose=TRUE)

	expect_equivalent(kp0$weights, w10, tolerance=1e-2)
	expect_equivalent(kp0$loadings, p10, tolerance=1e-2)
	expect_equivalent(kp0$coefficients, b0, tolerance=1e-2)

	expect_equivalent(kp1$weights[,1L], w11, tolerance=1e-2)
	expect_equivalent(kp1$loadings[,1L], p11, tolerance=1e-2)
	expect_equivalent(kp1$coefficients, b1, tolerance=1e-2)

	expect_equivalent(kpt0$weights, w10, tolerance=1e-2)
	expect_equivalent(kpt0$loadings, p10, tolerance=1e-2)
	expect_equivalent(kpt0$coefficients, b0, tolerance=1e-2)

	expect_equivalent(kpt1$weights[,1L], w11, tolerance=1e-2)
	expect_equivalent(kpt1$loadings[,1L], p11, tolerance=1e-2)
	expect_equivalent(kpt1$coefficients, b1, tolerance=1e-2)

	kp0f <- predict(kp0)
	kp1f <- predict(kp1)
	kp0p <- predict(kp0, x0)
	kp1p <- predict(kp1, x1)

	expect_equal(kp0$fitted.values, kp0f)
	expect_equal(kp1$fitted.values, kp1f)
	expect_equal(kp0$fitted.values, kp0p)
	expect_equal(kp1$fitted.values, kp1p)

	expect_equal(coef(kp0), kp0$coefficients)
	expect_equal(resid(kp0), kp0$residuals)
	expect_equal(fitted(kp0), kp0$fitted.values)
	expect_equal(loadings(kp0), kp0$loadings)

	kp1f1 <- predict(kp1, x1, k=1)
	kp1f2 <- predict(kp1, x1, k=2)
	kp1f12 <- predict(kp1, x1, k=1:2)

	expect_equivalent(kp1f1, kp1f12[,,1L])
	expect_equivalent(kp1f2, kp1f12[,,2L])

})

test_that("pls - da", {

	register(SerialParam())
	x <- cbind(
		c(-1, -5, -7.5, 10, -2.5, 5, 5),
		c(-1, -2.5, -7.5, 7.5, 12.5, 10, 5))
	y <- factor(c("a", "a", "a", "b", "a", "b", "b"))

	np1 <- pls_nipals(x, y, k=2)
	ynf1 <- apply(fitted(np1), 1L, which.max)
	ynp1 <- predict(np1, x, type="class")

	expect_equal(y, predict(np1, type="class"))
	expect_equal(fitted(np1), predict(np1, x))
	expect_equal(as.integer(y), ynf1)
	expect_equal(y, ynp1)

	sp1 <- pls_nipals(x, y, k=2)
	ysf1 <- apply(fitted(sp1), 1L, which.max)
	ysp1 <- predict(sp1, x, type="class")

	expect_equal(y, predict(sp1, type="class"))
	expect_equal(fitted(sp1), predict(sp1, x))
	expect_equal(as.integer(y), ysf1)
	expect_equal(y, ysp1)

	kp1 <- pls_kernel(x, y, k=2, method=1)
	ykf1 <- apply(fitted(kp1), 1L, which.max)
	ykp1 <- predict(kp1, x, type="class")

	expect_equal(y, predict(kp1, type="class"))
	expect_equal(fitted(kp1), predict(kp1, x))
	expect_equal(as.integer(y), ykf1)
	expect_equal(y, ykp1)

	kp2 <- pls_kernel(x, y, k=2, method=2)
	ykf2 <- apply(fitted(kp2), 1L, which.max)
	ykp2 <- predict(kp2, x, type="class")

	expect_equal(y, predict(kp1, type="class"))
	expect_equal(fitted(kp1), predict(kp1, x))
	expect_equal(as.integer(y), ykf2)
	expect_equal(y, ykp2)

	ynf11 <- predict(np1, x, k=1, type="class")
	ynf12 <- predict(np1, x, k=2, type="class")
	ynf112 <- predict(np1, x, k=1:2, type="class")
	ynf112l <- predict(np1, x, k=1:2, type="class", simplify=FALSE)
	ynf112df <- data.frame("k=1"=ynf11, "k=2"=ynf12, check.names=FALSE)

	expect_equivalent(ynf11, ynf112[,1L])
	expect_equivalent(ynf12, ynf112[,2L])
	expect_equal(ynf112, ynf112df)
	expect_equal(list("k=1"=ynf11, "k=2"=ynf12), ynf112l)

})

test_that("opls - nipals", {

	register(SerialParam())
	x1 <- cbind(
		c(-2.18, 1.84, -0.48, 0.83),
		c(-2.18, -0.16, 1.52, 0.83))
	y <- as.matrix(c(2, 2, 0, -4))

	no1 <- opls_nipals(x1, y, k=1, center=FALSE)
	not1 <- opls_nipals(t(x1), y, k=1, center=FALSE, transpose=TRUE)
	nop1 <- pls_nipals(no1$x, y, k=1, center=FALSE)
	nor1 <- no1$regressions[[1L]]

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

	expect_equivalent(nor1$weights, w10, tolerance=1e-2)
	expect_equivalent(nor1$loadings, p10, tolerance=1e-2)
	expect_equivalent(nor1$coefficients, bo1, tolerance=1e-2)

	no1f <- predict(no1, type="x")
	no1p <- predict(no1, x1, k=1, type="x")

	not1f <- predict(not1, type="x")
	not1p <- predict(not1, t(x1), k=1, type="x")

	expect_equal(no1f, no1$x)
	expect_equal(no1p, no1$x)
	expect_equal(not1f, not1$x)
	expect_equal(not1p, not1$x)

	no2 <- opls_nipals(x1, y, k=2, center=FALSE)
	nop2 <- pls_nipals(no2$x, y, k=1, center=FALSE)
	nor2 <- no2$regressions[[2L]]
	x2 <- fitted(no2, type="x")

	no2f <- predict(no2, type="x")
	no2p1 <- predict(no2, x1, k=1, type="x")
	no2p2 <- predict(no2, x1, k=2, type="x")
	no2pr12 <- predict(no2, x1, k=1:2)

	expect_equal(no2f, no2$x)
	expect_equal(no2p1, no1$x)
	expect_equal(no2p2, no2$x)

	expect_equal(nor2, nop2)
	expect_equal(coef(no2), coef(nop2))
	expect_equal(resid(no2), resid(nop2))
	expect_equal(fitted(no2), fitted(nop2))
	expect_equal(predict(no2, x1), predict(nop2, x2))
	expect_equal(no2pr12[,,2L], drop(predict(nop2, x2)))

})

test_that("opls - da", {

	register(SerialParam())
	x <- cbind(
		c(-1, -5, -7.5, 10, -2.5, 5, 5),
		c(-1, -2.5, -7.5, 7.5, 12.5, 10, 5))
	y <- factor(c("a", "a", "a", "b", "a", "b", "b"))

	no1 <- opls_nipals(x, y, k=1)
	np1 <- pls_nipals(no1$x, y, k=1, center=FALSE, scale.=FALSE)
	ynf1 <- apply(fitted(np1), 1L, which.max)
	ynp1 <- predict(np1, no1$x, type="class")
	xe <- predict(no1, type="x")

	expect_equal(y, predict(np1, type="class"))
	expect_equal(fitted(np1), predict(np1, no1$x))
	expect_equal(as.integer(y), ynf1)
	expect_equal(y, ynp1)

	expect_equal(fitted(no1), fitted(np1))
	expect_equal(predict(no1, x), predict(np1, xe))

	expect_equal(fitted(no1, type="class"), fitted(np1, type="class"))
	expect_equal(predict(no1, x, type="class"), predict(np1, xe, type="class"))

})
