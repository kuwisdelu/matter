
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

filt2_conv <- function(x, weights)
{
	if ( !is.matrix(x) )
		stop("x must be a matrix")
	if ( !is.matrix(weights) )
		stop("weights must be a matrix")
	.Call(C_linearFilter2, x, weights, PACKAGE="matter")
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

filt2_bi <- function(x, width = 5L, sddist = (width %/% 2) / 2,
	sdrange = mad(x, na.rm = TRUE))
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

filt2_diff <- function(x, niter = 3L, kappa = 50,
	rate = 0.25, method = 1L)
{
	if ( !is.matrix(x) )
		stop("x must be a matrix")
	if ( kappa < 1 )
		warning("kappa should be > 1")
	if ( rate <= 0 || rate > 0.25 )
		warning("rate should be positive and < 0.25")
	.Call(C_diffusionFilter2, x, niter,
		kappa, rate, method, PACKAGE="matter")
}

filt2_guide <- function(x, width = 5L, guide = x,
	sdreg = mad(x, na.rm = TRUE))
{
	if ( !is.matrix(x) )
		stop("x must be a matrix")
	if ( !is.matrix(guide) )
		stop("guide must be a matrix")
	if ( width %% 2L != 1L )
		width <- 1L + 2L * as.integer(width %/% 2)
	if ( is.integer(x) && is.double(guide) )
		storage.mode(x) <- "double"
	if ( is.double(x) && is.integer(guide) )
		storage.mode(guide) <- "double"
	.Call(C_guidedFilter2, x, guide, width,
		sdreg, PACKAGE="matter")
}

filt2_fun <- function(method)
{
	if ( is.character(method) )
		method <- tolower(method)
	options <- list(
		"ma" = 			filt2_ma,
		"mean" = 		filt2_ma,
		"gauss" = 		filt2_gauss,
		"gaussian" = 	filt2_gauss,
		"bi" = 			filt2_bi,
		"bilateral" = 	filt2_bi,
		"adapt" = 		filt2_adapt,
		"adaptive" = 	filt2_adapt,
		"diff" = 		filt2_diff,
		"diffusion" = 	filt2_diff,
		"guide" = 		filt2_guide,
		"guided" = 		filt2_guide)
	fn <- options[[method, exact=FALSE]]
	if ( is.null(fn) )
		stop("couldn't find method ", sQuote(method))
	fn
}

#### 2D Alignment and warping ####
## ---------------------------------

warp2_trans <- function(x, y, control = list(),
	trans = c("rigid", "similarity", "affine"),
	metric = c("cor", "mse", "mi"), nbins = 64L,
	scale = TRUE, dimout = dim(y))
{
	# scale x and y and remove NAs
	xs <- x
	ys <- y
	xs[is.na(xs)] <- min(x, na.rm=TRUE)
	ys[is.na(ys)] <- min(y, na.rm=TRUE)
	if ( scale ) {
		xs <- (xs - min(xs)) / max(xs)
		ys <- (ys - min(ys)) / max(ys)
	}
	# prepare metric function
	metric <- match.arg(metric)
	if ( metric == "cor" ) {
		score <- function(x, y) {			
			-(icor(x, y)^2)
		}
	} else if ( metric == "mse" ) {
		score <- function(x, y) {
			mean((x - y)^2)
		}
	} else if ( metric == "mi" ) {
		score <- function(x, y) {
			-mi(x, y, nbins)
		}
	}
	# prepare transformation
	trans <- match.arg(trans)
	if ( trans == "rigid" ) {
		init <- c(0, 0, 0)
		tform <- function(par, xi) {
			rot <- par[1L]
			tl <- par[2L:3L]
			trans2d(xi, rotate=rot, translate=tl,
				dimout=dim(y))
		}
	} else if ( trans == "similarity" ) {
		init <- c(0, 0, 0, 1, 1)
		tform <- function(par, xi) {
			rot <- par[1L]
			tl <- par[2L:3L]
			sc <- par[4L:5L]
			trans2d(xi, rotate=rot, translate=tl, scale=sc,
				dimout=dim(y))
		}
	} else if ( trans == "affine" ) {
		init <- rbind(diag(2), c(0, 0))
		tform <- function(par, xi) {
			pmat <- matrix(par, nrow=3L, ncol=2L)
			trans2d(xi, pmat=pmat,
				dimout=dim(y))
		}
	}
	# find optimal warp
	fn <- function(par) {
		xt <- tform(par, xs)
		xt[is.na(xt)] <- min(xt, na.rm=TRUE)
		score(xt, ys)
	}
	best <- optim(init, fn=fn, control=control)
	z <- tform(best$par, x)
	structure(z, optim=best, trans=trans, metric=metric)
}

mi <- function(x, y, n = 64L)
{
	if ( length(x) != length(y) )
		stop("x and y must be the same length")
	# calculate the 2D bin locations
	n = min(n, length(x) %/% 4L)
	rx <- range(x, na.rm=TRUE)
	ry <- range(y, na.rm=TRUE)
	x[is.na(x)] <- rx[1L]
	y[is.na(y)] <- ry[1L]
	nx <- ny <- floor(sqrt(n))
	xi <- seq(rx[1L], rx[2L], length.out=nx)
	yi <- seq(rx[1L], rx[2L], length.out=ny)
	# calculate the 2D histogram
	halfbw <- 0.5 * c(diff(xi[1L:2L]), diff(yi[1L:2L]))
	i <- bsearch(x, xi, tol=halfbw[1L])
	j <- bsearch(y, yi, tol=halfbw[2L])
	H <- matrix(0L, nrow=nx, ncol=ny)
	for ( k in seq_along(i) )
		H[i[k],j[k]] <- H[i[k],j[k]] + 1L
	# calculate mutual information
	pxy <- H / sum(H)
	px <- rowSums(pxy)
	py <- colSums(pxy)
	px <- matrix(px, nrow=nx, ncol=ny)
	py <- matrix(py, nrow=nx, ncol=ny, byrow=TRUE)
	nz <- pxy > 0
	sum(pxy[nz] * log2(pxy[nz] / (px[nz] * py[nz])))
}

warp2_fun <- function(method)
{
	if ( is.character(method) )
		method <- tolower(method)
	options <- list(
		"trans" = 	warp2_trans)
	fn <- options[[method, exact=FALSE]]
	if ( is.null(fn) )
		stop("couldn't find method ", sQuote(method))
	fn
}

#### Contrast enhancement ####
## ---------------------------

enhance_hist <- function(x, nbins = 256L)
{
	y <- .Call(C_histEq, x, nbins, PACKAGE="matter")
	rescale_iqr(y, IQR(x, na.rm=TRUE), median(x, na.rm=TRUE))
}

enhance_adapt <- function(x, width = sqrt(length(x)) %/% 5L,
	clip = 0.1, nbins = 256L)
{
	y <- .Call(C_adaptHistEq, x, width, clip, nbins, PACKAGE="matter")
	rescale_iqr(y, IQR(x, na.rm=TRUE), median(x, na.rm=TRUE))
}

enhance_fun <- function(method)
{
	if ( is.character(method) )
		method <- tolower(method)
	options <- list(
		"hist" = 		enhance_hist,
		"histeq" = 		enhance_hist,
		"histogram" = 	enhance_hist,
		"adapt" = 		enhance_adapt,
		"adaptive" = 	enhance_adapt,
		"clahe" =	 	enhance_adapt)
	fn <- options[[method, exact=FALSE]]
	if ( is.null(fn) )
		stop("couldn't find method ", sQuote(method))
	fn
}

#### Rasterize a scattered image ####
## ----------------------------------

is_gridded <- function(x, tol = 0.5)
{
	if ( length(dim(x)) != 2L )
		stop("x must be a matrix or data frame")
	!anyNA(apply(x, 2L, estres,
		tol = tol, tol.ref="abs"))
}

to_raster <- function(x, y, vals)
{
	xy <- cbind(x, y)
	gridded <- is_gridded(xy)
	dm <- estdim(xy)
	nx <- dm[1L]
	ny <- dm[2L]
	xr <- range(x, na.rm=TRUE)
	yr <- range(y, na.rm=TRUE)
	rx <- (xr[2L] - xr[1L]) / (nx - 1)
	ry <- (yr[2L] - yr[1L]) / (ny - 1)
	rx <- ifelse(is.na(rx), 1, rx)
	ry <- ifelse(is.na(ry), 1, ry)
	if ( gridded ) {
		rows <- as.integer(round((x - xr[1L]) / rx))
		cols <- as.integer(round((y - yr[1L]) / ry))
		init <- as.vector(NA, mode=typeof(vals))
		rs <- matrix(init, nrow=nx, ncol=ny)
		ind <- ((cols * nx) + rows) + 1L
		vals <- vals[!is.na(ind)]
		ind <- ind[!is.na(ind)]
		ind[ind > length(rs)] <- length(rs)
		rs[ind] <- vals
	} else {
		rs <- approx2(x, y, vals, interp="none",
			nx=nx, ny=ny, tol=c(rx, ry) / 2)
	}
	rs
}

to_raster3 <- function(x, y, z, vals)
{
	xyz <- cbind(x, y, z)
	gridded <- is_gridded(xyz)
	if ( !gridded )
		warning("data is not gridded")
	dm <- estdim(xyz)
	xr <- range(x, na.rm=TRUE)
	yr <- range(y, na.rm=TRUE)
	zr <- range(z, na.rm=TRUE)
	nx <- dm[1L]
	ny <- dm[2L]
	nz <- dm[3L]
	rx <- (xr[2L] - xr[1L]) / (nx - 1)
	ry <- (yr[2L] - yr[1L]) / (ny - 1)
	rz <- (zr[2L] - zr[1L]) / (nz - 1)
	rx <- ifelse(is.na(rx), 1, rx)
	ry <- ifelse(is.na(ry), 1, ry)
	rz <- ifelse(is.na(rz), 1, rz)
	ix <- as.integer(round((x - xr[1]) / rx))
	iy <- as.integer(round((y - yr[1]) / ry))
	iz <- as.integer(round((z - zr[1]) / rz))
	init <- as.vector(NA, mode=typeof(vals))
	rs <- array(init, dim=c(nx, ny, nz))
	ind <- ((iz * nx * ny) + (iy * nx) + ix) + 1L
	ind[ind > length(rs)] <- length(rs)
	rs[ind] <- vals
	rs
}

estdim <- function(x, tol = 1e-6)
{
	if ( length(dim(x)) != 2L )
		stop("x must be a matrix or data frame")
	res <- apply(x, 2L, estres, tol = tol, tol.ref="abs")
	if ( anyNA(res) ) {
		if ( ncol(x) == 2L ) {
			xr <- range(x[,1L], na.rm=TRUE)
			yr <- range(x[,2L], na.rm=TRUE)
			asp <- diff(yr) / diff(xr)
			nx <- round(sqrt(nrow(x) / asp))
			ny <- round(asp * nx)
			dm <- c(nx, ny)
		} else if ( ncol(x) == 3L ) {
			xr <- range(x[,1L], na.rm=TRUE)
			yr <- range(x[,2L], na.rm=TRUE)
			zr <- range(x[,3L], na.rm=TRUE)
			axy <- diff(yr) / diff(xr)
			axz <- diff(zr) / diff(xr)
			ayz <- diff(zr) / diff(yr)
			nx <- round((nrow(x) / (axz * axy))^(1/3))
			ny <- round((axz / ayz) * nx)
			nz <- round((ayz * axy) * nx)
			dm <- c(nx, ny, nz)
		} else {
			stop("only 2 or 3 dimensions supported",
				" for irregular coordinates")
		}
		names(dm) <- colnames(x)
	} else {
		dx <- apply(x, 2L, function(xi) diff(range(xi)))
		dm <- (dx / res) + 1
	}
	dm
}

#### 2D Resampling with interpolation ####
## ---------------------------------------

approx2 <- function(x, y, z, xout, yout,
	interp = "linear", nx = length(x), ny = length(y),
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
		# guess tol as ~1x/~2x the estimated gap between samples
		ref <- ifelse(tol.ref == "abs", "abs", "y")
		mul <- ifelse(interp %in% c("none", "linear"), 1, 2)
		tolx <- mul * reldiff(range(x), ref=ref) / sqrt(length(x))
		toly <- mul * reldiff(range(y), ref=ref) / sqrt(length(y))
		newtol <- c(tolx, toly)
		tol[is.na(tol)] <- newtol[is.na(tol)]
	}
	extrap <- as.numeric(extrap)
	zi <- .Call(C_Approx2, xi, yi, xy, z, tol, as_tol_ref(tol.ref),
		extrap, as_interp(interp), PACKAGE="matter")
	dim(zi) <- c(nx, ny)
	zi
}

#### Affine transformation ####
## ----------------------------

trans2d <- function(x, y, z, pmat,
	rotate = 0, translate = c(0, 0), scale = c(1, 1),
	interp = "linear", dimout = dim(z), ...)
{
	if ( missing(pmat) ) {
		angle <- rotate * pi / 180
		sc <- rep_len(scale, 2L)
		r1 <- sc[1L] * c(cos(angle), -sin(angle))
		r2 <- sc[2L] * c(sin(angle), cos(angle))
		tl <- rep_len(translate, 2L)
		pmat <- rbind(r1, r2, tl)
	} else {
		pmat <- as.matrix(pmat)
	}
	if ( !identical(dim(pmat), c(3L, 2L)) )
		stop("pmat must be a 3 x 2 matrix")
	if ( is.matrix(x) && missing(y) && missing(z) ) {
		z <- x
		x <- seq_len(nrow(z))
		y <- seq_len(ncol(z))
		co <- expand.grid(x=x, y=y)
		x <- co$x
		y <- co$y
	}
	co <- cbind(x, y, 1) %*% pmat
	co <- data.frame(x=co[,1], y=co[,2])
	if ( !missing(z) ) {
		xi <- seq(from=min(x), to=max(x), length.out=dimout[1L])
		yi <- seq(from=min(y), to=max(y), length.out=dimout[2L])
		zi <- approx2(co$x, co$y, z=z, interp=interp,
			xout=xi, yout=yi, ...)
		structure(zi, coord=co)
	} else {
		co
	}
}
