
#### 2D Filtering and Smoothing ####
## -----------------------------

filt2_ma <- function(x, width = 5L)
{
	if ( !is.matrix(x) )
		stop("x must be a matrix")
	if ( width %% 2L != 1L )
		width <- 1 + 2 * (width %/% 2)
	.Call(C_meanFilter2, x, width, PACKAGE="matter")
}

filt2_gauss <- function(x, width = 5L, sd = (width %/% 2) / 2)
{
	if ( !is.matrix(x) )
		stop("x must be a matrix")
	if ( width %% 2L != 1L )
		width <- 1L + 2L * as.integer(width %/% 2)
	radius <- width %/% 2
	i <- seq(from=-radius, to=radius, by=1L)
	weights <- dnorm(i, sd=sd) %o% dnorm(i, sd=sd)
	.Call(C_linearFilter2, x, weights, PACKAGE="matter")
}

filt2_bi <- function(x, width = 5L,
	sddist = (width %/% 2) / 2, sdrange = 2 * qmad(x))
{
	if ( !is.matrix(x) )
		stop("x must be a matrix")
	if ( width %% 2L != 1L )
		width <- 1L + 2L * as.integer(width %/% 2)
	.Call(C_bilateralFilter2, x, width,
		sddist, sdrange, NA_real_, PACKAGE="matter")
}

filt2_adapt <- function(x, width = 5L, spar = 1)
{
	if ( !is.matrix(x) )
		stop("x must be a matrix")
	if ( width %% 2L != 1L )
		width <- 1L + 2L * as.integer(width %/% 2)
	.Call(C_bilateralFilter2, x, width,
		NA_real_, NA_real_, spar, PACKAGE="matter")
}

#### 2D Resampling with interpolation ####
## ---------------------------------------

approx2 <- function(x, y, z, xout, yout,
	interp = "cubic", nx = length(x), ny = length(y),
	tol = NA_real_, tol.ref = "abs", extrap = NA_real_)
{
	if ( is.matrix(x) && missing(y) && missing(z) ) {
		z <- x
		x <- seq_len(nrow(z))
		y <- seq_len(ncol(z))
	}
	if ( missing(xout) )
		xout <- seq(from=min(x), to=max(x), length.out=nx)
	if ( missing(yout) )
		yout <- seq(from=min(y), to=max(y), length.out=ny)
	lx <- length(x)
	ly <- length(y)
	if ( is.matrix(z) && all(c(lx, ly) == dim(z)) ) {
		co <- expand.grid(x=x, y=y)
		x <- co$x
		y <- co$y
	}
	nx <- length(xout)
	ny <- length(yout)
	out <- expand.grid(x=xout, y=yout)
	xi <- out$x
	yi <- out$y
	if ( length(x) != length(y) || length(y) != length(z) )
		stop("x, y, z must all be the same length")
	xy <- as.matrix(cbind(x, y))
	if ( is.integer(xy) && (is.double(xi) || is.double(yi)) )
		storage.mode(xy) <- "double"
	if ( is.double(xy) && is.integer(xi) )
		storage.mode(xi) <- "double"
	if ( is.double(xy) && is.integer(yi) )
		storage.mode(yi) <- "double"
	tol <- rep_len(tol, 2L)
	if ( anyNA(tol) ) {
		# guess tol as ~1x/~2x the max gap between samples
		ref <- ifelse(tol.ref == "abs", "abs", "y")
		mul <- ifelse(interp %in% c("none", "linear"), 1, 2)
		tolx <- mul * max(abs(reldiff(sort(x), ref=ref)))
		toly <- mul * max(abs(reldiff(sort(y), ref=ref)))
		newtol <- c(tolx, toly)
		tol[is.na(tol)] <- newtol[is.na(tol)]
	}
	extrap <- as.numeric(extrap)
	zi <- .Call(C_Approx2, xi, yi, xy, z, tol, as_tol_ref(tol.ref),
		extrap, as_interp(interp), PACKAGE="matter")
	dim(zi) <- c(nx, ny)
	zi
}

