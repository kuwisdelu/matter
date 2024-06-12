require(testthat)
require(matter)

context("vizi")

test_that("vizi - parse", {

	set.seed(1, kind="default")
	x1 <- runif(10)
	x2 <- runif(10)
	x3 <- runif(10)
	y1 <- rnorm(10)
	y2 <- rnorm(10)
	g1 <- sample(c("a", "b"), 10, replace=TRUE)
	g2 <- sample(c("c", "d"), 10, replace=TRUE)
	data <- data.frame(
		x1=x1, x2=x2, x3=x3,
		y1=y1, y2=y2,
		g1=g1, g2=g2)

	p1 <- parse_formula(y ~ x)

	expect_length(p1$lhs, 1L)
	expect_length(p1$rhs, 1L)
	expect_length(p1$g, 0L)
	
	expect_equal(p1$lhs[[1L]], quote(y))
	expect_equal(p1$rhs[[1L]], quote(x))

	p2 <- parse_formula(y ~ x1 * x2)

	expect_length(p2$lhs, 1L)
	expect_length(p2$rhs, 2L)
	expect_length(p2$g, 0L)

	expect_equal(p2$lhs[[1L]], quote(y))
	expect_equal(p2$rhs[[1L]], quote(x1))
	expect_equal(p2$rhs[[2L]], quote(x2))

	p3 <- parse_formula(y1 + y2 ~ x1 * x2)

	expect_length(p3$lhs, 2L)
	expect_length(p3$rhs, 2L)
	expect_length(p3$g, 0L)

	expect_equal(p3$lhs[[1L]], quote(y1))
	expect_equal(p3$lhs[[2L]], quote(y2))
	expect_equal(p3$rhs[[1L]], quote(x1))
	expect_equal(p3$rhs[[2L]], quote(x2))

	p4 <- parse_formula(y ~ x1 * x2 | g)

	expect_length(p4$lhs, 1L)
	expect_length(p4$rhs, 2L)
	expect_length(p4$g, 1L)

	expect_equal(p4$lhs[[1L]], quote(y))
	expect_equal(p4$rhs[[1L]], quote(x1))
	expect_equal(p4$rhs[[2L]], quote(x2))
	expect_equal(p4$g[[1L]], quote(g))

	p5 <- parse_formula(y1 + y2 ~ x1 * x2 | g1 * g2)

	expect_length(p5$lhs, 2L)
	expect_length(p5$rhs, 2L)
	expect_length(p5$g, 2L)

	expect_equal(p5$lhs[[1L]], quote(y1))
	expect_equal(p5$lhs[[2L]], quote(y2))
	expect_equal(p5$rhs[[1L]], quote(x1))
	expect_equal(p5$rhs[[2L]], quote(x2))
	expect_equal(p5$g[[1L]], quote(g1))
	expect_equal(p5$g[[2L]], quote(g2))

	p6 <- parse_formula(y1 + y2 ~ x1 * x2, envir=data)

	expect_equal(p6$lhs[[1L]], y1)
	expect_equal(p6$lhs[[2L]], y2)
	expect_equal(p6$rhs[[1L]], x1)
	expect_equal(p6$rhs[[2L]], x2)

	p7 <- parse_formula(y1 ~ x1 | g1 * g2, envir=data)

	expect_equal(p7$lhs[[1L]], y1)
	expect_equal(p7$rhs[[1L]], x1)
	expect_equal(p7$g[[1L]], g1)
	expect_equal(p7$g[[2L]], g2)

})

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

	es <- eval_exprs(list(foo=quote(a), bar=quote(a + b)), data=xl, i1=1:2)

	expect_equal(es[[1L]], e1)
	expect_equal(es[[2L]], e3)
	expect_true(attr(es, "recursive"))

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

test_that("vizi - mark - xy", {

	set.seed(1, kind="default")
	n <- 500
	g <- factor(sample(c("a", "b"), n, replace=TRUE))
	x <- rnorm(n)
	y <- runif(2)[as.integer(g)] * x + rnorm(n, sd=sqrt(0.1))

	v1 <- vizi(x=x, y=y)
	v1 <- add_mark(v1, "points")

	expect_no_error(plot(v1))

	v2 <- vizi(x=x, y=y, color=g, shape=g)
	v2 <- add_mark(v2, "points")

	expect_no_error(plot(v2))

	v3 <- vizi(x=x, y=y, color=g, linetype=g)
	v3 <- add_mark(v3, "lines",
		trans=list(sort=TRUE, n=100, downsampler="lttb"))

	expect_no_error(plot(v3))

	v4 <- vizi(x=x, y=y, color=g)
	v4 <- add_mark(v4, "peaks")

	expect_no_error(plot(v4))

	z <- rnorm(n)

	v5 <- vizi(x=x, y=y, z=z, color=g, shape=g)
	v5 <- add_mark(v5, "points")
	v5 <- set_par(v5, theta=30, phi=30)

	expect_no_error(plot(v5))

})

test_that("vizi - mark - text", {

	set.seed(1, kind="default")
	n <- 100
	g <- factor(sample(c("a", "b"), n, replace=TRUE))
	x <- rnorm(n)
	y <- runif(2)[as.integer(g)] * x + rnorm(n, sd=sqrt(0.1))

	v1 <- vizi(x=x, y=y, text=g)
	v1 <- add_mark(v1, "text")

	expect_no_error(plot(v1))

	v2 <- vizi(x=x, y=y, text=g, color=g)
	v2 <- add_mark(v2, "text")

	expect_no_error(plot(v2))

})

test_that("vizi - mark - rules", {

	set.seed(1, kind="default")
	n <- 100
	x <- rnorm(n)
	y <- runif(1) * x + rnorm(n, sd=sqrt(0.1))

	v1 <- vizi(x=x, y=y)
	v1 <- add_mark(v1, "points")
	v1 <- add_mark(v1, "rules", x=numeric(0), y=0)

	expect_no_error(plot(v1))

	v2 <- vizi(x=x, y=y)
	v2 <- add_mark(v2, "points")
	v2 <- add_mark(v2, "rules", x=0, y=numeric(0))

	expect_no_error(plot(v2))

	v3 <- vizi(x=x, y=y)
	v3 <- add_mark(v3, "points")
	v3 <- add_mark(v3, "rules", x=0, y=0)

	expect_no_error(plot(v3))

})

test_that("vizi - mark - bars", {

	set.seed(1, kind="default")
	n <- 5
	x <- letters[seq_len(n)]
	y <- runif(n)

	v1 <- vizi(x=x, y=y)
	v1 <- add_mark(v1, "bars")

	expect_no_error(plot(v1))

	v2 <- vizi(x=x, y=y, fill=x)
	v2 <- add_mark(v2, "bars", params=list(width=0.8))

	expect_no_error(plot(v2))

	x2 <- rep.int(x, 2)
	y2 <- c(y, runif(n))
	g <- rep(c("foo", "bar"), each=n)

	v3 <- vizi(x=x2, y=y2, fill=g)
	v3 <- add_mark(v3, "bars", params=list(width=0.8))

	expect_no_error(plot(v3))

	v4 <- vizi(x=x2, y=y2, fill=g)
	v4 <- add_mark(v4, "bars", params=list(width=0.8, stack=TRUE))
	v4 <- set_coord(v4, ylim=c(0, 2))

	expect_no_error(plot(v4))

})

test_that("vizi - mark - intervals", {

	set.seed(1, kind="default")
	n <- 6
	x <- letters[seq_len(n)]
	y1 <- runif(n)
	y2 <- runif(n) + 1
	g <- rep(c("foo", "bar"), each=n %/% 2)

	v1 <- vizi(x=x, ymin=y1, ymax=y2)
	v1 <- add_mark(v1, "intervals")

	expect_no_error(plot(v1))

	v2 <- vizi(x=x, ymin=y1, ymax=y2, color=g)
	v2 <- add_mark(v2, "intervals")

	expect_no_error(plot(v2))

})

test_that("vizi - mark - boxplot", {

	set.seed(1, kind="default")
	n <- 500
	x <- factor(sample(letters[1:6], n, replace=TRUE))
	y <- rnorm(n)
	g <- rep(c("foo", "bar"), each=n %/% 2)

	v1 <- vizi(x=x, y=y)
	v1 <- add_mark(v1, "boxplot")

	expect_no_error(plot(v1))

	v2 <- vizi(x=x, y=y, fill=g)
	v2 <- add_mark(v2, "boxplot")

	expect_no_error(plot(v2))

})

test_that("vizi - mark - image", {

	set.seed(1, kind="default")
	nr <- 32
	nc <- 32
	img <- matrix(rlnorm(nr * nc), nrow=nr, ncol=nc)
	i <- (nr %/% 3):(2 * nr %/% 3)
	j <- (nc %/% 3):(2 * nc %/% 3)
	img[i,j] <- img[i,j] + max(img)
	img <- rescale_range(img, c(0, 1))

	v1 <- vizi(xmin=0, xmax=1, ymin=0, ymax=1, image=list(img))
	v1 <- add_mark(v1, "image")

	expect_no_error(plot(v1))

	v2 <- vizi(xmin=0, xmax=1, ymin=0, ymax=1, image=img)
	v2 <- add_mark(v2, "image")

	expect_error(plot(v2))

})

test_that("vizi - mark - pixels/voxels", {

	set.seed(1, kind="default")
	nr <- 32
	nc <- 32
	x <- seq(-4, 4, length.out=nr)
	y <- seq(1, 3, length.out=nc)
	co <- expand.grid(x=x, y=y)
	x <- co$x
	y <- co$y
	vals <- matrix(atan(x / y), nrow=nr, ncol=nc)
	vals <- 10 * (vals - min(vals)) / diff(range(vals))
	vals <- vals + 2.5 * runif(length(vals))

	v1 <- vizi(x=x, y=y, color=vals)
	v1 <- add_mark(v1, "pixels")

	expect_no_error(plot(v1))

	v2 <- vizi(x=x, y=y, color=vals)
	v2 <- add_mark(v2, "pixels", trans=list(smooth="gaussian"))

	expect_no_error(plot(v2))

	v3 <- vizi(x=x, y=y, color=vals)
	v3 <- add_mark(v3, "pixels", trans=list(enhance="histeq"))

	expect_no_error(plot(v3))

	v4 <- vizi(x=x, y=y, color=vals)
	v4 <- add_mark(v4, "pixels", trans=list(scale=TRUE))

	expect_no_error(plot(v4))

	v5 <- vizi(x=x, y=y, color=vals)
	v5 <- add_mark(v5, z=0, "pixels")
	v5 <- add_mark(v5, z=1, "pixels")
	v5 <- add_mark(v5, z=2, "pixels")

	expect_no_error(plot(v5))

	x2 <- rep.int(x, 5)
	y2 <- rep.int(y, 5)
	z2 <- rep(1:5, each=nr * nc)
	vals2 <- c(vals, 2 * vals, 3 * vals, 4 * vals, 5 * vals)

	v6 <- vizi(x=x2, y=y2, z=z2, color=vals2)
	v6 <- add_mark(v6, "voxels")

	expect_no_error(plot(v6))

})

