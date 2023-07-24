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

	no1 <- opls_nipals(x1, y, k=1, center=FALSE)
	not1 <- opls_nipals(t(x1), y, k=1, center=FALSE, transpose=TRUE)
	nop1 <- pls_nipals(no1$x, y, k=1, center=FALSE)

	wo1 <- c(-0.89, 0.45)
	po1 <- c(-1.16, -0.09)
	to1 <- c(0.97, -1.71, 1.11, -0.37)
	
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

})
