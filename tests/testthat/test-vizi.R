require(testthat)
require(matter)

context("vizi")

test_that("vizi - eval", {

	set.seed(1, kind="default")
	xla <- replicate(10, sort(rnorm(10)), simplify=FALSE)
	xlb <- replicate(10, sort(rnorm(10)), simplify=FALSE)
	names(xla) <- paste0("a", seq_along(xla))
	names(xlb) <- paste0("b", seq_along(xlb))
	xl <- list(a=xla, b=xlb)

	xmu <- matrix(sort(rnorm(100)), nrow=10, ncol=10)
	xmv <- matrix(sort(rnorm(100)), nrow=10, ncol=10)
	xm <- list(u=xmu, v=xmv)

	expect_equal(eval_expr(quote(a), data=xl), xla)
	expect_equal(eval_expr(quote(b), data=xl), xlb)
	expect_equal(eval_expr(quote(u), data=xm), xmu)
	expect_equal(eval_expr(quote(v), data=xm), xmv)

	e1 <- eval_expr(quote(a), data=xl, i1=1:2)
	e2 <- eval_expr(quote(a), data=xl, i1=1:2, i2=1:5)

	expect_equal(e1, xla[1:2])
	expect_equal(e2, list(a1=xla[[1L]][1:5], a2=xla[[2L]][1:5]))

	e3 <- eval_expr(quote(a + b), data=xl, i1=1:2)
	e4 <- eval_expr(quote(a + b), data=xl, i1=1:2, i2=1:5)

	expect_equal(e3[[1L]], xla[[1L]] + xlb[[1L]])
	expect_equal(e3[[2L]], xla[[2L]] + xlb[[2L]])
	expect_equal(e4[[1L]], (xla[[1L]] + xlb[[1L]])[1:5])
	expect_equal(e4[[2L]], (xla[[2L]] + xlb[[2L]])[1:5])

	ii <- c("1"=1, "1"=2, "2"=3, "2"=4)
	e5 <- eval_expr(quote(a), data=xl, i1=ii)
	
	expect_equal(e5[[1L]], xla[[1L]] + xla[[2L]])
	expect_equal(e5[[2L]], xla[[3L]] + xla[[4L]])

	gg <- names(ii)
	e6 <- eval_expr(quote(u), data=xm, i1=1:3, margin=1L)
	e7 <- eval_expr(quote(u), data=xm, i1=1:3, margin=2L)
	e8 <- eval_expr(quote(u), data=xm, i1=ii, margin=1L)

	expect_equal(do.call(rbind, e6), xmu[1:3,,drop=FALSE])
	expect_equal(do.call(cbind, e7), xmu[,1:3,drop=FALSE])
	expect_equal(do.call(rbind, e8), rowsum(xmu[1:4,,drop=FALSE], group=gg))

	e9 <- eval_expr(quote(u + v), data=xm, i1=1:2, margin=2L)

	expect_equal(e9[[1L]], xmu[,1L,drop=TRUE] + xmv[,1L,drop=TRUE])
	expect_equal(e9[[2L]], xmu[,2L,drop=TRUE] + xmv[,2L,drop=TRUE])

})

test_that("vizi - plot", {

	set.seed(1, kind="default")
	x <- rnorm(100)
	y <- rnorm(100)
	g <- factor(rep(c("a", "b", "c", "d"), each=25))
	df <- data.frame(VAR1=x, VAR2=y, GROUP=g)

	v1 <- vizi(x=x, y=y, color=g)

	expect_setequal(x, v1$encoding$x)
	expect_setequal(y, v1$encoding$y)
	expect_setequal(g, v1$encoding$color)
	expect_equal("x", v1$channels$x$label)
	expect_equal("y", v1$channels$y$label)
	expect_equal("color", v1$channels$color$label)
	expect_equal(range(x), v1$channels$x$limits)
	expect_equal(range(y), v1$channels$y$limits)
	expect_equal(levels(g), v1$channels$color$limits)

	v2 <- vizi(df, x=~VAR1, y=~VAR2, color=~GROUP)

	expect_setequal(x, v2$encoding$x)
	expect_setequal(y, v2$encoding$y)
	expect_setequal(g, v2$encoding$color)
	expect_equal("VAR1", v2$channels$x$label)
	expect_equal("VAR2", v2$channels$y$label)
	expect_equal("GROUP", v2$channels$color$label)
	expect_equal(range(x), v2$channels$x$limits)
	expect_equal(range(y), v2$channels$y$limits)
	expect_equal(levels(g), v2$channels$color$limits)

})

test_that("vizi - facets", {

	set.seed(1, kind="default")
	x <- rnorm(100)
	y <- rnorm(100)
	g <- factor(rep(c("a", "b", "c", "d"), each=25))
	df <- data.frame(VAR1=x, VAR2=y, GROUP=g)

	ia <- which(g == "a")
	ib <- which(g == "b")
	ic <- which(g == "c")
	id <- which(g == "d")

	xa <- x[ia]
	xb <- x[ib]
	xc <- x[ic]
	xd <- x[id]

	ya <- y[ia]
	yb <- y[ib]
	yc <- y[ic]
	yd <- y[id]

	v1 <- vizi(x=x, y=y)
	v1 <- add_facets(v1, by=g)

	expect_setequal(xa, v1$plots[[1L]]$encoding$x)
	expect_setequal(xb, v1$plots[[2L]]$encoding$x)
	expect_setequal(xc, v1$plots[[3L]]$encoding$x)
	expect_setequal(xd, v1$plots[[4L]]$encoding$x)
	expect_setequal(ya, v1$plots[[1L]]$encoding$y)
	expect_setequal(yb, v1$plots[[2L]]$encoding$y)
	expect_setequal(yc, v1$plots[[3L]]$encoding$y)
	expect_setequal(yd, v1$plots[[4L]]$encoding$y)
	expect_equal("x", v1$channels$x$label)
	expect_equal("y", v1$channels$y$label)
	expect_equal(range(x), v1$channels$x$limits)
	expect_equal(range(y), v1$channels$y$limits)
	expect_equal(levels(g), v1$labels)
	expect_setequal(ia, v1$subscripts[[1L]])
	expect_setequal(ib, v1$subscripts[[2L]])
	expect_setequal(ic, v1$subscripts[[3L]])
	expect_setequal(id, v1$subscripts[[4L]])

	v2 <- vizi(df, x=~VAR1, y=~VAR2)
	v2 <- add_facets(v2, by=~GROUP, data=df)

	expect_equal(v1$plots, v2$plots)
	expect_equal("VAR1", v2$channels$x$label)
	expect_equal("VAR2", v2$channels$y$label)
	expect_equal(range(x), v2$channels$x$limits)
	expect_equal(range(y), v2$channels$y$limits)
	expect_equal(levels(g), v2$labels)
	expect_equal(v1$subscripts, v2$subscripts)

	v3a <- vizi(x=xa, y=ya)
	v3b <- vizi(x=xb, y=yb)
	v3c <- vizi(x=xc, y=yc)
	v3d <- vizi(x=xd, y=yd)

	v3 <- as_facets(list(v3a, v3b, v3c, v3d))
	expect_equal(v1$plots, v3$plots)
	expect_equal(v1$channels, v3$channels)

	v4 <- as_facets(list(v2, v2))
	expect_equal(c(v2$plots, v2$plots), v4$plots)
	expect_equal(v2$channels, v4$channels)

})
