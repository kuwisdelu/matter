require(testthat)
require(matter)

context("vizi")

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
