
#### Wavelets and Kernels ####
## ---------------------------

ricker <- function(n, a = 1)
{
	x <- seq(-n, n, length.out=n) / 2
	C <- 2 / (sqrt(3 * a) * pi^(0.25))
	m <- (1 - (x / a)^2)
	g <- exp(-(x / a)^2 / 2)
	C * m * g
}

#### Filtering and Smoothing ####
## -----------------------------

filt1_ma <- function(x, width = 5L)
{
	if ( !is.null(dim(x)) && length(dim(x)) != 1L )
		stop("x must be a vector")
	if ( width %% 2L != 1L )
		width <- 1 + 2 * (width %/% 2)
	.Call(C_meanFilter, x, width, PACKAGE="matter")
}

filt1_conv <- function(x, weights)
{
	if ( !is.null(dim(x)) && length(dim(x)) != 1L )
		stop("x must be a vector")
	if ( length(weights) %% 2L != 1L )
		stop("length of weights must be odd")
	.Call(C_linearFilter, x, weights, PACKAGE="matter")
}

filt1_gauss <- function(x, width = 5L, sd = (width %/% 2) / 2)
{
	if ( !is.null(dim(x)) && length(dim(x)) != 1L )
		stop("x must be a vector")
	if ( width %% 2L != 1L )
		width <- 1L + 2L * as.integer(width %/% 2)
	radius <- width %/% 2
	z <- seq(from=-radius, to=radius, by=1L)
	weights <- dnorm(z, sd=sd)
	.Call(C_linearFilter, x, weights, PACKAGE="matter")
}

filt1_bi <- function(x, width = 5L, sddist = (width %/% 2) / 2,
	sdrange = mad(x, na.rm = TRUE))
{
	if ( !is.null(dim(x)) && length(dim(x)) != 1L )
		stop("x must be a vector")
	if ( width %% 2L != 1L )
		width <- 1L + 2L * as.integer(width %/% 2)
	.Call(C_bilateralFilter, x, width,
		sddist, sdrange, NA_real_, PACKAGE="matter")
}

filt1_adapt <- function(x, width = 5L, spar = 1)
{
	if ( !is.null(dim(x)) && length(dim(x)) != 1L )
		stop("x must be a vector")
	if ( width %% 2L != 1L )
		width <- 1L + 2L * as.integer(width %/% 2)
	.Call(C_bilateralFilter, x, width,
		NA_real_, NA_real_, spar, PACKAGE="matter")
}

filt1_diff <- function(x, niter = 3L, kappa = 50,
	rate = 0.25, method = 1L)
{
	if ( !is.null(dim(x)) && length(dim(x)) != 1L )
		stop("x must be a vector")
	if ( kappa < 1 )
		warning("kappa should be > 1")
	if ( rate <= 0 || rate > 0.25 )
		warning("rate should be positive and <= 0.25")
	.Call(C_diffusionFilter, x, niter,
		kappa, rate, method, PACKAGE="matter")
}

filt1_guide <- function(x, width = 5L, guide = x,
	sdreg = mad(x, na.rm = TRUE))
{
	if ( !is.null(dim(x)) && length(dim(x)) != 1L )
		stop("x must be a vector")
	if ( width %% 2L != 1L )
		width <- 1L + 2L * as.integer(width %/% 2)
	if ( is.integer(x) && is.double(guide) )
		x <- as.double(x)
	if ( is.double(x) && is.integer(guide) )
		guide <- as.double(guide)
	.Call(C_guidedFilter, x, guide, width,
		sdreg, NA_real_, PACKAGE="matter")
}

filt1_pag <- function(x, width = 5L, guide = NULL,
	sdreg = mad(x, na.rm = TRUE), ftol = 1/10)
{
	if ( !is.null(dim(x)) && length(dim(x)) != 1L )
		stop("x must be a vector")
	if ( width %% 2L != 1L )
		width <- 1L + 2L * as.integer(width %/% 2)
	if ( is.null(guide) )
		guide <- filt1_diff(x, niter=3L, method=3L)
	if ( is.integer(x) && is.double(guide) )
		x <- as.double(x)
	if ( is.double(x) && is.integer(guide) )
		guide <- as.double(guide)
	.Call(C_guidedFilter, x, guide, width,
		sdreg, ftol, PACKAGE="matter")
}

filt1_sg <- function(x, width = 5L, order = min(3L, width - 2L),
	deriv = 0, delta = 1)
{
	if ( !is.null(dim(x)) && length(dim(x)) != 1L )
		stop("x must be a vector")
	if ( width %% 2L != 1L )
		width <- 1L + 2L * as.integer(width %/% 2)
	if ( width <= order )
		stop("width must be larger than order")
	b <- (1:width - (width %/% 2 + 1)) %*% t(rep.int(1, order + 1))
	b <- b^(as.matrix(rep.int(1, width)) %*% (0:order))
	weights <- pinv(b)[1 + deriv,]
	if ( deriv > 0 )
		weights <- weights * prod(1:deriv) / delta^deriv
	.Call(C_linearFilter, x, weights, PACKAGE="matter")
}

convolve_at <- function(x, index, weights, ...)
{
	if ( !is.list(index) )
		stop("index must be a list")
	if ( length(index) != length(x) )
		stop("index and x must have the same length")
	if ( is.numeric(weights) )
		weights <- list(weights)
	weights <- rep_len(weights, length(x))
	if ( !all(lengths(index) == lengths(weights)) )
		stop("lengths of index and weights must match")
	vapply(seq_along(x),
		function(i) {
			ii <- index[[i]]
			sum(weights[[i]] * x[ii], ...)
		}, numeric(1L))
}

#### Alignment and warping ####
## ----------------------------

warp1_loc <- function(x, y, tx = seq_along(x), ty = seq_along(y),
	events = c("maxmin", "max", "min"), lx = NULL, ly = NULL,
	interp = c("linear", "loess", "spline"), n = length(y),
	tol = NA_real_, tol.ref = "abs")
{
	events <- match.arg(events)
	# find events in each signal
	if ( events == "maxmin" ) {
		if ( is.null(lx) )
			lx <- tx[which(locmax(x) | locmin(x))]
		if ( is.null(ly) )
			ly <- ty[which(locmax(y) | locmin(y))]
	} else if ( events == "max" ) {
		if ( is.null(lx) )
			lx <- tx[which(locmax(x))]
		if ( is.null(ly) )
			ly <- ty[which(locmax(y))]
	} else if ( events == "min" ) {
		if ( is.null(lx) )
			lx <- tx[which(locmin(x))]
		if ( is.null(ly) )
			ly <- ty[which(locmin(y))]
	}
	if ( is.na(tol) ) {
		# guess tol as ~5% of the signal length
		xref <- ifelse(tol.ref == "abs", "abs", "y")
		tol <- 0.05 * length(x) * mean(reldiff(tx, ref=xref))
	}
	# match events between signals
	tout <- approx(tx, tx, n=n)$y
	i <- bsearch(lx, ly, tol=tol, tol.ref=tol.ref)
	loc <- !is.na(i)
	if ( sum(loc) >= 1 ) {
		# find shifts between events
		lx <- lx[loc]
		ly <- ly[i[loc]]
		path <- data.frame(x=lx, y=ly)
		dt <- ly - lx
		dt <- c(0, dt, 0)
		lx <- c(tx[1L], lx, tx[length(tx)])
		interp <- match.arg(interp)
		# interpolate shifts
		if ( interp == "loess" ) {
			shift <- loess(dt ~ locs)
			shift <- predict(shift, t)
		} else if ( interp == "spline" ) {
			shift <- spline(lx, dt, xout=tout)$y
		} else {
			shift <- approx(lx, dt, xout=tout)$y
		}
		tshift <- tout + shift
	} else {
		warning("no landmarks matched")
		tshift <- tout
		path <- NULL
	}
	# warp signal x to align with y
	xout <- spline(tshift, x, xout=tout,
		ties=list("ordered", mean))$y
	attr(xout, "path") <- path
	attr(xout, "tol") <- setNames(tol, tol.ref)
	xout
}

warp1_dtw <- function(x, y, tx = seq_along(x), ty = seq_along(y),
	n = length(y), tol = NA_real_, tol.ref = "abs")
{
	if ( is.integer(x) && is.double(y) )
		x <- as.double(x)
	if ( is.double(x) && is.integer(y) )
		y <- as.double(y)
	if ( is.integer(tx) && is.double(ty) )
		tx <- as.double(tx)
	if ( is.double(tx) && is.integer(ty) )
		ty <- as.double(ty)
	if ( is.na(tol) ) {
		# guess tol as ~5% of the signal length
		xref <- ifelse(tol.ref == "abs", "abs", "y")
		tol <- 0.05 * length(x) * mean(reldiff(tx, ref=xref))
	}
	# dynamic time warping to align signals
	d0 <- abs(reldiff(tx[1L], ty[1L], ref=tol.ref))
	dn <- abs(reldiff(tx[length(tx)], ty[length(ty)], ref=tol.ref))
	if ( tol < d0 || tol < dn ) {
		tol <- max(d0, dn)
		warning("'tol' must be greater than ", tol)
	}
	path <- .Call(C_warpDTW, x, y, tx, ty,
		tol, as_tol_ref(tol.ref), PACKAGE="matter")
	i <- rev(path[!is.na(path[,1L]),1L]) + 1L
	j <- rev(path[!is.na(path[,2L]),2L]) + 1L
	path <- data.frame(x=tx[i], y=ty[j])
	# warp signal x to align with y
	tout <- approx(ty[path$y], tx[path$x],
		ties=list("ordered", mean), n=n)$y
	xout <- approx(tx, x, xout=tout)$y
	attr(xout, "path") <- path
	attr(xout, "tol") <- setNames(tol, tol.ref)
	xout
}

warp1_cow <- function(x, y, tx = seq_along(x), ty = seq_along(y),
	nbins = NA_integer_, n = length(y),
	tol = NA_real_, tol.ref = "abs")
{
	if ( is.integer(x) && is.double(y) )
		x <- as.double(x)
	if ( is.double(x) && is.integer(y) )
		y <- as.double(y)
	if ( is.integer(tx) && is.double(ty) )
		tx <- as.double(tx)
	if ( is.double(tx) && is.integer(ty) )
		ty <- as.double(ty)
	if ( is.na(tol) ) {
		# guess tol as ~5% of the signal length
		xref <- ifelse(tol.ref == "abs", "abs", "y")
		tol <- 0.05 * length(x) * mean(reldiff(tx, ref=xref))
	}
	if ( is.na(nbins) ) {
		# guess nbins so that bin widths are ~tol
		xref <- ifelse(tol.ref == "abs", "abs", "y")
		nbins <- abs(reldiff(tx[1L], tx[length(tx)], ref=xref)) %/% tol
	}
	# initialize nodes
	if ( nbins < 2L )
		stop("need at least 2 bins")
	xb <- findbins(x, nbins=nbins, dynamic=FALSE, limits.only=TRUE)
	ix <- c(1L, xb$upper)
	yb <- findbins(y, nbins=nbins, dynamic=FALSE, limits.only=TRUE)
	iy <- c(1L, yb$upper)
	if ( any(xb$size < 3L) || any(yb$size < 3L) )
		stop("too many bins (need at least 3 samples per bin)")
	dmax <- max(abs(reldiff(tx[ix], ty[iy], ref=tol.ref)))
	if ( tol < dmax ) {
		tol <- dmax
		warning("'tol' must be greater than ", tol)
	}
	# correlation optimized warping to align signals
	path <- .Call(C_warpCOW, x, y, tx, ty,
		as.integer(ix - 1L), as.integer(iy - 1L),
		tol, as_tol_ref(tol.ref), PACKAGE="matter")
	i <- path[,1L] + 1L
	j <- path[,2L] + 1L
	path <- data.frame(x=tx[i], y=ty[j])
	tout <- unique(unlist(mapply(seq, from=i[-length(i)], to=i[-1L],
		length.out=j[-1L] - j[-length(j)] + 1L, SIMPLIFY=FALSE)))
	tout <- approx(seq_along(tout), tout,
		ties=list("ordered", mean), n=n)$y
	xout <- approx(tx, x, xout=tout)$y
	attr(xout, "path") <- path
	attr(xout, "tol") <- setNames(tol, tol.ref)
	xout
}

icor <- function(x, y)
{
	if ( is.integer(x) && is.double(y) )
		x <- as.double(x)
	if ( is.double(x) && is.integer(y) )
		y <- as.double(y)
	if ( anyNA(x) )
		x <- x[!is.na(x)]
	if ( anyNA(y) )
		y <- y[!is.na(x)]
	.Call(C_iCorr, x, y, PACKAGE="matter")
}

#### Binning and downsampling ####
## -------------------------------

binvec <- function(x, lower, upper, stat = "sum", prob = 0.5)
{
	if ( missing(lower) && missing(upper) ) {
		stop("must specify one of 'lower' or 'upper'")
	} else if ( missing(upper) ) {
		upper <- c(lower[-1] - 1L, length(x))
	} else if ( missing(lower) ) {
		lower <- c(1L, upper[-length(upper)] + 1L)
	}
	.Call(C_binVector, x, as.integer(lower - 1L), as.integer(upper - 1L),
		as_binstat(stat), prob, PACKAGE="matter")
}

rollvec <- function(x, width, stat = "sum", prob = 0.5)
{
	if ( width %% 2L != 1L )
		width <- 1L + 2L * as.integer(width %/% 2)
	end <- as.integer(length(x) - 1L)
	r <- as.integer(width %/% 2)
	i <- seq(0L, end, by=1L)
	lower <- pmax(i - r, 0L)
	upper <- pmin(i + r, end)
	.Call(C_binVector, x, lower, upper,
		as_binstat(stat), prob, PACKAGE="matter")
}

findbins <- function(x, nbins, dynamic = TRUE,
	niter = NA, limits.only = FALSE)
{
	n <- floor(nbins)
	width <- ceiling(length(x) / n)
	breaks <- floor(seq(from=width, to=length(x) - width, length.out=n - 1L))
	lower <- c(1L, breaks + 1L)
	upper <- c(breaks, length(x))
	if ( is.na(niter) ) {
		niter <- floor(10 * length(x) / n)
		check_converge <- TRUE
	} else {
		check_converge <- FALSE
	}
	if ( dynamic ) {
		if ( nbins < 3L )
			stop("need >= 3 bins for dynamic binning")
		# calculate SSE from linear regression in each bin
		sse <- binvec(x, lower, upper, stat="sse")
		trace <-  numeric(niter + 1L)
		trace[1L] <- sum(sse)
		for ( i in seq_len(niter) )
		{
			# update bins by merging min SSE's and splitting max SSE
			update <- .Call(C_binUpdate, sse, as.integer(lower - 1L),
				as.integer(upper - 1L), PACKAGE="matter")
			lower_new <- update[[1L]] + 1L
			upper_new <- update[[2L]] + 1L
			# check if new bins are better (lower sum of SSE)
			sse <- binvec(x, lower_new, upper_new, stat="sse")
			score <- sum(sse)
			if ( score < trace[i] || !check_converge ) {
				trace[i + 1L] <- score
				lower <- lower_new
				upper <- upper_new
			} else {
				trace <- trace[seq_len(i)]
				break
			}
		}
		bins <- structure(data.frame(lower=lower, upper=upper,
			size=upper - lower + 1L, sse=sse), trace=trace)
	} else {
		bins <- data.frame(lower=lower, upper=upper, size=upper - lower + 1L)
	}
	if ( limits.only ) {
		bins <- data.frame(lower=lower, upper=upper, size=upper - lower + 1L)
		if ( dynamic ) {
			attr(bins, "trace") <- trace
			bins$sse <- sse
		}
	} else {
		fun <- function(i, j) x[i:j]
		bins <- mapply(fun, lower, upper, SIMPLIFY=FALSE)
		names(bins) <- paste0("[", lower, ":", upper, "]")
		bins <- structure(bins, lower=lower, upper=upper)
		if ( dynamic ) {
			attr(bins, "trace") <- trace
			attr(bins, "sse") <- sse
		}
	}
	bins
}

ltob <- function(x, t, lower, upper)
{
	lower <- as.integer(lower - 1L)
	upper <- as.integer(upper - 1L)
	.Call(C_downsampleLTOB, x, t, lower, upper, PACKAGE="matter")
}

lttb <- function(x, t, lower, upper)
{
	lower <- as.integer(lower - 1L)
	upper <- as.integer(upper - 1L)
	.Call(C_downsampleLTTB, x, t, lower, upper, PACKAGE="matter")
}

downsample <- function(x, n = length(x) / 10L, domain = NULL,
	method = c("lttb", "ltob", "dynamic"))
{
	method <- match.arg(method)
	if ( missing(domain) || is.null(domain) )
		domain <- as.numeric(seq_along(x))
	# calculate dynamic bin widths if requested
	if ( method == "dynamic" ) {
		buckets <- findbins(x, n - 2L, dynamic=TRUE, limits.only=TRUE)
	} else {
		buckets <- findbins(x, n - 2L, dynamic=FALSE, limits.only=TRUE)
	}
	# use largest-triangle-1-bucket or largest-triangle-3-buckets
	if ( method == "ltob" ) {
		sample <- ltob(x, domain, buckets$lower, buckets$upper)
	} else {
		sample <- lttb(x, domain, buckets$lower, buckets$upper)
	}
	sample <- c(1L, sample, length(x))
	structure(x[sample], sample=sample)
}

#### Continuum estimation ####
## ----------------------------

estbase_loc <- function(x,
	smooth = c("none", "loess", "spline"),
	span = 1/10, spar = NULL, upper = FALSE)
{
	if ( upper ) {
		locs <- which(locmax(x))
	} else {
		locs <- which(locmin(x))
	}
	if ( length(locs) >= 2 ) {
		smooth <- match.arg(smooth)
		locs <- c(1L, locs, length(x))
		y <- approx(locs, x[locs], xout=seq_along(x))$y
		if ( smooth == "loess" )
			y <- lowess(y, f=span)$y
		if ( smooth == "spline" )
			y <- smooth.spline(y, spar=spar)$y
	} else {
		if ( upper ) {
			y <- rep.int(max(x, na.rm=TRUE), length(x))
		} else {
			y <- rep.int(min(x, na.rm=TRUE), length(x))
		}
	}
	y
}

estbase_hull <- function(x, upper = FALSE)
{
	t <- seq_along(x)
	if ( !is.integer(x) ) {
		x <- as.double(x)
		t <- as.double(t)
	}
	if ( length(x) >= 3 ) {
		hull <- .Call(C_convexHull, t, x, isTRUE(upper), PACKAGE="matter")
	} else {
		hull <- t - 1L
	}
	approx(hull, x[hull + 1L], xout=t)$y
}

estbase_snip <- function(x, width = 100L, decreasing = TRUE)
{
	.Call(C_smoothSNIP, as.double(x), as.integer(width),
		isTRUE(decreasing), PACKAGE="matter")
}

estbase_med <- function(x, width = 100L)
{
	runmed(x, k = 1L + 2L * (width %/% 2L))
}

#### Noise estimation ####
## -----------------------

estnoise_quant <- function(x, width = 25L, prob = 0.95, niter = 3L)
{
	y <- filt1_diff(x, method=3L, niter=niter)
	x <- abs(y - x)
	width <- min(width, length(x))
	rollvec(x, width, stat="quantile", prob=prob)
}

estnoise_diff <- function(x, nbins = 1L, dynamic = FALSE)
{
	fn <- function(xi) {
		# mean absolute deviation of signal derivative
		dx <- mean(diff(xi), na.rm=TRUE)
		ns <- mean(abs(xi - dx), na.rm=TRUE)
		noise <- rep.int(ns, length(xi))
	}
	if ( nbins > 1L ) {
		xb <- findbins(x, nbins=nbins, dynamic=dynamic)
		ns <- unlist(lapply(xb, fn))
		noise <- lowess(ns)$y
	} else {
		noise <- fn(x)
	}
	noise
}

estnoise_filt <- function(x, snr = 2, nbins = 1L,
	threshold = 0.5, peaks = FALSE)
{
	if ( nbins > 1L ) {
		# Gallia et al (2013) but with lowess
		xb <- findbins(x, nbins=nbins, dynamic=FALSE)
		noise <- lapply(xb, estnoise_filt, snr=snr,
			threshold=threshold, peaks=peaks)
		noise <- unlist(noise)
		noise <- lowess(noise)$y
	} else {
		# Xu and Freitas (2010) dynamic noise level
		if ( isFALSE(peaks) || is.null(peaks) ) {
			y <- sort(x[findpeaks(x)])
		} else {
			y <- sort(x[x > 0])
		}
		if ( length(y) <= 1L )
			return(rep.int(y, length(x)))
		noise <- (1 + threshold) * y[1L]
		i <- 2L
		snr_i <- y[2L] / noise
		# fit linear model
		fit <- lr(1L:i, y[1L:i])
		while ( snr_i < snr )
		{
			i <- i + 1L
			# predict noise level
			noise <- lr_predict(fit, i)
			snr_i <- y[i] / noise
			# update linear model
			fit <- lr_update(fit, i, y[i])
		}
		noise <- rep.int(noise, length(x))
	}
	noise
}

lr <- function(x, y) {
	ux <- mean(x)
	uy <- mean(y)
	sxx <- var(x)
	sxy <- cov(x, y)
	b <- sxy / sxx
	a <- uy - b * ux
	list(coef=c(a, b), n=length(x),
		x=ux, y=uy, xx=sxx, xy=sxy)
}

lr_update <- function(fit, x, y) {
	n <- fit$n + length(x)
	ux <- (fit$n * fit$x + sum(x)) / n
	uy <- (fit$n * fit$y + sum(y)) / n
	qx <- sqrt(fit$n) * fit$x + sqrt(n) * ux
	qx <- qx / (sqrt(fit$n) + sqrt(n))
	qy <- sqrt(fit$n) * fit$y + sqrt(n) * uy
	qy <- qy / (sqrt(fit$n) + sqrt(n))
	sxx <- (fit$n - 1) * fit$xx + sum((x - qx) * (x - qx))
	sxx <- sxx / (n - 1)
	sxy <- (fit$n - 1) * fit$xy + sum((x - qx) * (y - qy))
	sxy <- sxy / (n - 1)
	b <- sxy / sxx
	a <- uy - b * ux
	list(coef=c(a, b), n=n,
		x=ux, y=uy, xx=sxx, xy=sxy)
}

lr_predict <- function(fit, xi) {
	sum(fit$coef * c(1, xi))
}

estnoise_sd <- function(x, width = 25L, wavelet = ricker)
{
	if ( is.function(wavelet) )
		x <- cwt(x, wavelet, scales=1)
	width <- min(width, length(x))
	rollvec(x, width, stat="sd")
}

estnoise_mad <- function(x, width = 25L, wavelet = ricker)
{
	if ( is.function(wavelet) )
		x <- cwt(x, wavelet, scales=1)
	width <- min(width, length(x))
	rollvec(x, width, stat="sd")
}

#### Peak detection ####
## ---------------------

locmax <- function(x, width = 5L)
{
	.Call(C_localMaxima, x, as.integer(width), PACKAGE="matter")
}

locmin <- function(x, width = 5L)
{
	.Call(C_localMaxima, -x, as.integer(width), PACKAGE="matter")
}

findpeaks <- function(x, width = 5L, prominence = NULL,
	snr = NULL, noise = c("quant", "diff", "filt", "sd", "mad"),
	relheight = 0.005, bounds = TRUE, ...)
{
	peaks <- which(locmax(x, width))
	ann <- data.frame(row.names=seq_along(peaks))
	if ( isTRUE(bounds) )
	{
		# find peak boundaries (nearest local maxima)
		bounds <- .Call(C_peakBoundaries, x,
			as.integer(peaks - 1L), PACKAGE="matter")
		ann$left_bounds <- bounds[[1L]] + 1L
		ann$right_bounds <- bounds[[2L]] + 1L
	}
	if ( isTRUE(prominence) || is.numeric(prominence) )
	{
		# find peak bases (minima between peaks and next higher peaks)
		bases <- .Call(C_peakBases, x,
			as.integer(peaks - 1L), PACKAGE="matter")
		ann$left_bases <- bases[[1L]] + 1L
		ann$right_bases <- bases[[2L]] + 1L
		# lower of peak bases is lowest contour line (key col)
		contour <- pmax(x[ann$left_bases], x[ann$right_bases])
		# prominence is height of peak above lowest contour line
		ann$prominences <- x[peaks] - contour
		if ( is.numeric(prominence) )
		{
			# filter based on prominence if requested
			keep <- ann$prominence >= prominence
			peaks <- peaks[keep]
			ann <- ann[keep,,drop=FALSE]
		}
	}
	if ( isTRUE(relheight) || is.numeric(relheight) )
	{
		ann$relheight <- x[peaks] / max(x[peaks])
		if ( is.numeric(relheight) )
		{
			# filter based on relative height if requested
			keep <- ann$relheight >= relheight
			peaks <- peaks[keep]
			ann <- ann[keep,,drop=FALSE]
		}
	}
	if ( isTRUE(snr) || is.numeric(snr) )
	{
		noise <- match.arg(noise)
		fn <- get(paste0("estnoise_", noise))
		noise <- fn(x, ...)
		ann$snr <- x[peaks] / noise[peaks]
		if ( is.numeric(snr) )
		{
			# filter based on SNR
			keep <- ann$snr >= snr
			peaks <- peaks[keep]
			ann <- ann[keep,,drop=FALSE]
		}
	}
	if ( length(ann) > 0L )
		attributes(peaks) <- ann
	peaks
}

findpeaks_cwt <- function(x, snr = 2, wavelet = ricker, scales = NULL,
	maxdists = scales, ngaps = 3L, ridgelen = length(scales) %/% 4L,
	qnoise = 0.95, width = length(x) %/% 20L, bounds = TRUE)
{
	if ( is.null(scales) )
		scales <- c(1, seq(2, 30, 2), seq(32, 64, 4))
	# continuous wavelet transform
	coefs <- cwt(x, wavelet, scales)
	# find ridge lines (include raw signal)
	ridges <- findridges(cbind(x, coefs), c(1, maxdists), ngaps)
	ridges <- lapply(ridges,
		function(ridge) {
			ridge[,2L] <- ridge[,2L] - 1L
			ridge
		})
	# filter based on ridge length
	ridges <- ridges[vapply(ridges, nrow, integer(1L)) >= ridgelen]
	# get peak locations
	peaks <- vapply(ridges, function(ridge) ridge[1L,1L], integer(1L))
	# get signal-to-noise ratio
	width <- max(5L, width)
	halfwidth <- width %/% 2L
	i <- pmax(peaks - halfwidth, 1L)
	j <- pmin(peaks + halfwidth, nrow(coefs))
	signal <- vapply(ridges, function(ridge) max(coefs[ridge]), numeric(1L))
	noise <- binvec(abs(coefs[,1L]), i, j, stat="quantile", prob=qnoise)
	# filter based on SNR
	snrs <- signal / noise
	ridges <- ridges[snrs >= snr]
	peaks <- peaks[snrs >= snr]
	snrs <- snrs[snrs >= snr]
	# get peak annotations
	ann <- data.frame(ridges=I(ridges), snr=snrs)
	if ( isTRUE(bounds) )
	{
		# find peak boundaries (nearest local maxima)
		bounds <- .Call(C_peakBoundaries, x,
			as.integer(peaks - 1L), PACKAGE="matter")
		ann$left_bounds <- bounds[[1L]] + 1L
		ann$right_bounds <- bounds[[2L]] + 1L
	}
	attributes(peaks) <- ann
	peaks
}

findridges <- function(x, maxdists, ngaps)
{
	maxs <- matrix(nrow=nrow(x), ncol=ncol(x))
	for ( i in seq_len(ncol(x)) )
		maxs[,i] <- locmax(x[,i])
	# initialize ridges at highest column
	start <- max(which(colSums(maxs) > 0))
	ridges <- lapply(which(maxs[,start]), 
		function(row) {
			ridge <- matrix(NA_integer_, nrow=start, ncol=2L)
			ridge[1L,] <- c(row, start)
			ridge
		})
	outridges <- list()
	gaps <- integer(length(ridges))
	ns <- rep.int(1L, length(ridges))
	# iterate toward lowest column
	for ( col in (start - 1L):1L )
	{
		# get last rows from current ridges
		prev <- vapply(seq_along(ridges),
			function(i) ridges[[i]][ns[i],1L], integer(1L))
		gaps <- gaps + 1L
		# connect existing ridges
		rows <- which(maxs[,col])
		lines <- bsearch(prev, rows, tol=maxdists[col])
		lines <- rows[lines]
		for ( i in seq_along(lines) )
		{
			row <- lines[i]
			if ( is.na(row) )
				next
			ns[i] <- ns[i] + 1L
			ridges[[i]][ns[i],] <- c(row, col)
			gaps[i] <- 0L
		}
		# prepare list of new ridges
		rows <- setdiff(rows, lines)
		newlen <- length(rows)
		if ( newlen > 0L ) {
			newridges <- lapply(rows,
				function(row) {
					ridge <- matrix(NA_integer_, nrow=col, ncol=2L)
					ridge[1L,] <- c(row, col)
					ridge
				})
			ridges <- c(ridges, newridges)
			gaps <- c(gaps, rep.int(0L, newlen))
			ns <- c(ns, rep.int(1L, newlen))
		}
		# save and remove ridges with large gaps
		rm <- gaps > ngaps
		if ( any(rm) ) {
			outridges <- c(outridges, ridges[rm])
			ridges <- ridges[!rm]
			gaps <- gaps[!rm]
			ns <- ns[!rm]
		}
	}
	outridges <- c(outridges, ridges)
	for ( i in seq_along(outridges) )
	{
		# sort from lowest column to highest
		ridge <- outridges[[i]]
		if ( anyNA(ridge) ) {
			na <- is.na(ridge[,1L])
			ridge <- ridge[!na,,drop=FALSE]
		}
		outridges[[i]] <- ridge[nrow(ridge):1L,,drop=FALSE]
	}
	# sort by ridge locations at lowest column
	locations <- vapply(outridges,
		function(ridge) ridge[1L,1L], integer(1L))
	outridges <- outridges[order(locations)]
	outridges
}

cwt <- function(x, wavelet = ricker, scales = NULL)
{
	if ( is.null(scales) )
		scales <- c(1, seq(2, 30, 2), seq(32, 64, 4))
	# find best length for fft
	nx <- length(x)
	nw <- pmin(10 * scales, nx)
	n <- nextn(nx + max(nw) - 1L, 2L)
	y <- fft(c(x, rep.int(0, n - nx)))
	# prepare coefficient matrix
	w <- numeric(n)
	z <- matrix(nrow=nx, ncol=length(scales))
	scales <- sort(scales)
	for ( i in seq_along(scales) ) {
		# calculate padded wavelet
		w[seq_len(nw[i])] <- wavelet(nw[i], scales[i])
		# perform the convolution
		zi <- Re(fft(y * Conj(fft(w)), inverse=TRUE))
		# shift by half wavelet length
		h <- nw[i] %/% 2L
		right <- 1L:(nx - h)
		left <- (n - h + 1L):n
		zi <- zi[c(left, right)] / n
		# assign coefficients
		z[,i] <- zi
	}
	z
}

peakwidths <- function(x, peaks, domain = NULL,
	fmax = 0.5, ref = c("height", "prominence"))
{
	ref <- match.arg(ref)
	if ( is.null(domain) )
		domain <- seq_along(x)
	if ( is.unsorted(domain) )
		stop("'domain' must be sorted")
	if ( ref == "height" )
	{
		# find peak boundaries if not provided
		left_end <- attr(peaks, "left_bounds")
		right_end <- attr(peaks, "right_bounds")
		if ( is.null(left_end) || is.null(right_end) )
		{
			bounds <- .Call(C_peakBoundaries, x,
				as.integer(peaks - 1L), PACKAGE="matter")
			left_end <- bounds[[1L]] + 1L
			right_end <- bounds[[2L]] + 1L
		}
		p <- x[peaks] - min(x)
	} else
	{
		# find peak bases if not provided
		p <- attr(peaks, "prominences")
		left_end <- attr(peaks, "left_bases")
		right_end <- attr(peaks, "right_bases")
		if ( is.null(p) || is.null(left_end) || is.null(right_end) )
		{
			bases <- .Call(C_peakBases, x,
				as.integer(peaks - 1L), PACKAGE="matter")
			left_end <- bases[[1L]] + 1L
			right_end <- bases[[2L]] + 1L
			contour <- pmax(x[left_end], x[right_end])
			p <- x[peaks] - contour
		}
	}
	# descend peaks to find height at fraction of max
	heights <- x[peaks] - (1 - fmax) * p
	thresholds <- .Call(C_peakWidths, x, as.integer(peaks - 1L),
		as.double(domain), as.integer(left_end - 1L), as.integer(right_end - 1L),
		as.double(heights), PACKAGE="matter")
	ann <- data.frame(row.names=seq_along(peaks))
	ann$width_heights <- heights
	# calculate widths at intersections of reference height
	ann$left_ips <- thresholds[[1L]]
	ann$right_ips <- thresholds[[2L]]
	widths <- ann$right_ips - ann$left_ips
	attributes(widths) <- ann
	widths
}

peakareas <- function(x, peaks, domain = NULL)
{
	if ( is.null(domain) )
		domain <- seq_along(x)
	if ( is.unsorted(domain) )
		stop("'domain' must be sorted")
	left_bounds <- as.integer(attr(peaks, "left_bounds") - 1L)
	right_bounds <- as.integer(attr(peaks, "right_bounds") - 1L)
	if ( is.null(left_bounds) || is.null(right_bounds) )
	{
		# find peak boundaries if not provided
		bounds <- .Call(C_peakBoundaries, x,
			as.integer(peaks - 1L), PACKAGE="matter")
		left_bounds <- bounds[[1L]]
		right_bounds <- bounds[[2L]]
	}
	ann <- data.frame(row.names=seq_along(peaks))
	ann$left_bounds <- left_bounds
	ann$right_bounds <- right_bounds
	# calculate peak areas by numeric integration
	areas <- .Call(C_peakAreas, x, as.integer(peaks - 1L),
		as.double(domain), left_bounds, right_bounds, PACKAGE="matter")
	attributes(areas) <- ann
	areas
}

binpeaks <- function(peaklist, domain = NULL, xlist = peaklist,
	tol = NA_real_, tol.ref = "abs", merge = FALSE, na.drop = TRUE)
{
	if ( any(lengths(peaklist) != lengths(xlist)) )
		stop("lengths of 'peaklist' and 'xlist' must match")
	if ( is.na(tol) ) {
		# guess tol as ~1/2 the min gap between same-signal peaks
		ref <- ifelse(tol.ref == "abs", "abs", "y")
		fun <- function(peaks) min(reldiff(peaks, ref=ref), na.rm=TRUE)
		tol <- 0.5 * min(vapply(peaklist, fun, numeric(1)), na.rm=TRUE)
	}
	if ( is.null(domain) ) {
		# guess domain
		lims <- vapply(peaklist, range, numeric(2), na.rm=TRUE)
		lims <- range(lims, na.rm=TRUE)
		if ( tol.ref == "abs" ) {
			domain <- seq(from=lims[1L], to=lims[2L], by=tol)
		} else {
			domain <- seq_rel(from=lims[1L], to=lims[2L], by=tol)
		}
	}
	if ( is.unsorted(domain) )
		stop("'domain' must be sorted")
	peaks <- numeric(length(domain))
	x <- numeric(length(domain))
	n <- numeric(length(domain))
	for ( i in seq_along(peaklist) ) {
		# bin peaks to reference
		p <- bsearch(peaklist[[i]], domain, tol=tol, tol.ref=tol.ref)
		dup <- duplicated(p, NA_integer_)
		while ( any(dup) ) {
			# remove duplicates
			pdup <- p[which(dup)[1L]]
			pdup <- which(p == p[pdup])
			diff <- reldiff(peaklist[[i]][pdup],
				domain[p[pdup]], ref=tol.ref)
			p[pdup[diff > min(diff)]] <- NA_integer_
			dup <- duplicated(p, NA_integer_)
		}
		# add peaks to bin sum
		match <- !is.na(p)
		p <- p[match]
		peaks[p] <- peaks[p] + peaklist[[i]][match]
		x[p] <- x[p] + xlist[[i]][match]
		n[p] <- n[p] + 1
	}
	# average binned peaks
	nz <- n != 0
	peaks[nz] <- peaks[nz] / n[nz]
	peaks[!nz] <- NA_real_
	x[nz] <- x[nz] / n[nz]
	x[!nz] <- NA_real_
	names(tol) <- ifelse(tol.ref == "abs", "absolute", "relative")
	if ( merge )
		x <- mergepeaks(peaks, n=n, x=x,
			tol=tol, tol.ref=tol.ref, na.drop=FALSE)
	# create output peaks
	peaks <- structure(x, na.rm=TRUE, nobs=n,
		class=c("stream_mean", "stream_stat"))
	if ( na.drop && anyNA(peaks) )
		peaks <- peaks[!is.na(peaks)]
	attr(peaks, "tolerance") <- as_tol(tol)
	attr(peaks, "domain") <- domain
	peaks
}

mergepeaks <- function(peaks, n = nobs(peaks), x = peaks,
	tol = NA_real_, tol.ref = "abs", na.drop = TRUE)
{
	if ( length(peaks) != length(x) )
		stop("length of 'peaks' and 'x' must match")
	if ( is.unsorted(peaks) )
		stop("'peaks' must be sorted")
	if ( is.na(tol) ) {
		# guess tol as ~1/100 of average gap between peaks
		ref <- ifelse(tol.ref == "abs", "abs", "y")
		tol <- 0.01 * mean(reldiff(peaks, ref=ref), na.rm=TRUE)
	}
	# find smallest gap between peaks
	p <- which(!is.na(peaks))
	d <- reldiff(peaks[p], ref=tol.ref)
	dmin <- which.min(d)
	while ( d[dmin] <= tol )
	{
		# average overlapping peaks
		i <- p[dmin]
		j <- p[dmin + 1L]
		ni <- n[i]
		nj <- n[j]
		mpeaks <- (ni * peaks[i] + nj * peaks[j]) / (ni + nj)
		mx <- (ni * x[i] + nj * x[j]) / (ni + nj)
		# update overlapping peaks with average
		if ( ni > nj ) {
			peaks[i] <- mpeaks
			x[i] <- mx
			n[i] <- ni + nj
			peaks[j] <- NA_real_
			x[j] <- NA_real_
			n[j] <- 0
		} else {
			peaks[j] <- mpeaks
			x[j] <- mx
			n[j] <- ni + nj
			peaks[i] <- NA_real_
			x[i] <- NA_real_
			n[i] <- 0
		}
		# find next smallest gap between peaks
		p <- which(!is.na(peaks))
		d <- reldiff(peaks[p], ref=tol.ref)
		dmin <- which.min(d)
	}
	# create output peaks
	peaks <- structure(x, na.rm=TRUE, nobs=n,
		class=c("stream_mean", "stream_stat"))
	if ( na.drop && anyNA(peaks) )
		peaks <- peaks[!is.na(peaks)]
	attr(peaks, "tolerance") <- as_tol(tol)
	peaks
}

#### Resolution (rate) estimation ####
## -----------------------------------

estres <- function(x, tol = 1e-6, tol.ref = NA_character_)
{
	if ( length(x) <= 1L )
		return(NA_real_)
	x <- sort(x)
	from <- x[-length(x)]
	to <- x[-1L]
	rx <- 2 * ((to / from ) - 1) / ((to / from) + 1)
	rx <- ifelse(is.na(rx), Inf, rx)
	dx <- diff(x)
	if ( (is.na(tol.ref) && diff(range(rx)) > tol) || 
		(!is.na(tol.ref) && tol.ref == "abs" ) )
	{
		dx <- dx[dx > tol]
		if ( all(dx %% min(dx) <= tol) ) {
			res <- c(absolute = min(dx))
		} else {
			res <- c(absolute = NA_real_)
		}
	} else
	{
		rx <- rx[rx > tol]
		if ( all(rx %% min(rx) <= tol) ) {
			res <- c(relative = min(rx))
		} else {
			res <- c(relative = NA_real_)
		}
	}
	res
}

#### Resampling with interpolation ####
## ------------------------------------

approx1 <- function(x, y, xout, interp = "linear", n = length(x),
	tol = NA_real_, tol.ref = "abs", extrap = NA_real_)
{
	if ( missing(xout) )
		xout <- seq(from=min(x), to=max(x), length.out=n)
	if ( is.integer(x) && is.double(xout) )
		x <- as.double(x)
	if ( is.double(x) && is.integer(xout) )
		xout <- as.double(xout)
	if ( is.na(tol) ) {
		# guess tol as ~2x the max gap between samples
		ref <- ifelse(tol.ref == "abs", "abs", "y")
		tol <- 2 * max(abs(reldiff(sort(x), ref=ref)))
	}
	extrap <- as.numeric(extrap)
	.Call(C_Approx1, xout, x, y, tol, as_tol_ref(tol.ref),
		extrap, as_interp(interp), PACKAGE="matter")
}

#### Simulation ####
## -----------------

simspec <- function(n = 1L, npeaks = 50L,
	x = rlnorm(npeaks, 7, 0.3), y = rlnorm(npeaks, 1, 0.9),
	domain = c(0.9 * min(x), 1.1 * max(x)), size = 10000,
	sdx = 1e-5, sdy = sdymult * log1p(y), sdymult = 0.2,
	sdnoise = 0.1, resolution = 1000, fmax = 0.5,
	baseline = 0, decay = 10, units = "relative")
{
	if ( length(x) != length(y) )
		stop("length of 'x' and 'y' must match")
	if ( length(domain) == 2L ) {
		xout <- seq(from=domain[1L], to=domain[2L], length.out=size)
	} else {
		xout <- domain
	}
	if ( missing(x) ) {
		from <- min(domain)
		to <- max(domain)
		x <- (x - min(x)) / max(x - min(x))
		x <- (from + 0.1 * (to - from)) + (0.8 * (to - from)) * x
	}
	i <- order(x)
	x <- x[i]
	y <- y[i]
	if ( n > 1L ) {
		yout <- replicate(n, simspec(x=x, y=y,
			domain=domain, size=size, sdx=sdx, sdy=sdy, sdymult=sdymult,
			sdnoise=sdnoise, resolution=resolution, fmax=fmax,
			baseline=baseline, decay=decay))
	} else {
		# calculate peak widths
		dx <- x / resolution
		peakwidths <- qnorm(1 - fmax / 2) * dx
		# calculate peak variance
		sdy <- rep_len(sdy, npeaks)
		yerr <- rlnorm(npeaks, sdlog=sdy)
		yerr <- yerr - exp(sdy^2 / 2)
		y2 <- y + yerr
		# calculate x error
		errtype <- pmatch(units, c("relative", "absolute"), nomatch=1L)
		xerr <- rnorm(1L) * c(x * sdx, sdx)[errtype]
		x2 <- x + xerr
		# calculate baseline
		b <- baseline * exp(-(decay/max(xout)) * (xout - min(xout)))
		# simulate the signal
		yout <- simspec1(x2, y2, xout=xout,
			peakwidths=peakwidths, sdnoise=sdnoise)
		yout <- yout + b
	}
	structure(yout, domain=xout, peaks=x)
}

simspec1 <- function(x, y, xout, peakwidths = NA_real_,
	sdnoise = 0, resolution = 1000, fmax = 0.5)
{
	yout <- numeric(length(xout))
	if ( length(x) != length(y) )
		y <- rep_len(y, length(x))
	if ( length(x) != length(peakwidths) )
		peakwidths <- rep_len(peakwidths, length(x))
	if ( anyNA(peakwidths) ) {
		dx <- x / resolution
		reswidth <- qnorm(1 - fmax / 2) * dx
		na <- is.na(peakwidths)
		peakwidths[na] <- reswidth[na]
	}
	xr <- range(xout)
	for ( i in seq_along(x) ) {
		if ( y[i] <= 0 || x[i] < xr[1L] || x[i] > xr[2L] )
			next
		yi <- y[i]
		wi <- peakwidths[i]
		peak <- which(x[i] - 6 * wi < xout & xout < x[i] + 6 * wi)
		if ( length(peak) > 0L ) {
			di <- dnorm(xout[peak], mean=x[i], sd=wi)
			yout[peak] <- yout[peak] + yi * (di / max(di))
		} else {
			peak <- bsearch(x[i], xout, nearest=TRUE)
			yout[peak] <- yi
		}
	}
	if ( sdnoise > 0 ) {
		noise <- rlnorm(length(xout), sdlog=sdnoise)
		noise <- noise - exp(sdnoise^2 / 2)
		yout <- yout + noise
	}
	pmax(yout, 0)
}
