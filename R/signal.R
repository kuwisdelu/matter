
#### Filtering and Smoothing ####
## -----------------------------

filt1_ma <- function(x, width = 5L, weights = rep_len(1L, width))
{
	if ( width %% 2L != 1L )
		width <- 1 + 2 * (width %/% 2)
	if ( length(weights) %% 2L != 1L )
		warning("length of 'weights' isn't odd")
	.Call(C_linearFilter, x, weights, PACKAGE="matter")
}

filt1_gauss <- function(x, width = 5L, sd = (width %/% 2) / 2)
{
	if ( width %% 2L != 1L )
		width <- 1L + 2L * as.integer(width %/% 2)
	radius <- width %/% 2
	i <- seq(from=-radius, to=radius, by=1L)
	weights <- dnorm(i, sd=sd)
	.Call(C_linearFilter, x, weights, PACKAGE="matter")
}

filt1_bi <- function(x, width = 5L, scale = 5L,
	sddist = NA_real_, sdrange = NA_real_)
{
	if ( width %% 2L != 1L )
		width <- 1L + 2L * as.integer(width %/% 2)
	.Call(C_bilateralFilter, x, width, as.double(scale),
		as.double(sddist), as.double(sdrange), PACKAGE="matter")
}

#### Binning and resampling ####
## -----------------------------

binvec <- function(x, lower, upper, stat = "sum")
{
	if ( missing(upper) ) {
		upper <- lower[-1] - 1L
		lower <- lower[-length(lower)]
	}
	.Call(C_binVector, x, as.integer(lower - 1L), as.integer(upper - 1L),
		as_binstat(stat), PACKAGE="matter")
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
	method = c("dynamic", "lttb", "ltob"))
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

resample1 <- function(x, y, xi, halfwidth = 2, interp = "linear")
{
	if ( is.integer(x) && is.double(xi) )
		x <- as.double(x)
	if ( is.double(x) && is.integer(xi) )
		xi <- as.double(xi)
	if ( is.unsorted(x) ) {
		ord <- order(x)
		x <- x[ord]
		y <- y[ord]
	}
	asearch_int(as.double(xi), as.double(x), as.double(y),
		tol=halfwidth, tol.ref=as_tol_ref("abs"), nomatch=NA_real_,
		interp=as_interp(interp))
}

#### Signal alignment and warping ####
## -----------------------------------

warp_loc <- function(x, t, landmarks,
	interp = c("linear", "loess", "spline"), span = 3/4,
	tol = 2.5, tol.ref = "abs", maxima = TRUE)
{
	if ( missing(t) )
		t <- seq_along(x)
	if ( maxima ) {
		locs <- t[which(locmax(x))]
	} else {
		locs <- t[which(locmin(x))]
	}
	i <- bsearch(locs, landmarks, tol=tol, tol.ref=tol.ref)
	matched <- !is.na(i)
	if ( sum(matched) >= 1 ) {
		locs <- locs[matched]
		landmarks <- landmarks[i[matched]]
		dt <- landmarks - locs
		dt <- c(dt[1L], dt, dt[length(dt)])
		locs <- c(t[1L], locs, t[length(t)])
		interp <- match.arg(interp)
		if ( interp == "loess" ) {
			shift <- loess(dt ~ locs, span=span)
			shift <- predict(shift, t)
		} else if ( interp == "spline" ) {
			shift <- spline(locs, dt, xout=t)$y
		} else {
			shift <- approx(locs, dt, xout=t)$y
		}
		tout <- t + shift
	} else {
		warning("no landmarks matched")
		tout <- t
	}
	attr(tout, "x") <- spline(tout, x, xout=t)$y
	tout
}

#### Continuum estimation ####
## ----------------------------

estbase_loc <- function(x,
	interp = c("linear", "loess", "spline"),
	span = 1/10, spar = NULL, upper = FALSE)
{
	if ( upper ) {
		locs <- which(locmax(x))
	} else {
		locs <- which(locmin(x))
	}
	if ( length(locs) >= 2 ) {
		interp <- match.arg(interp)
		locs <- c(1L, locs, length(x))
		if ( interp == "loess" ) {
			y <- lowess(locs, x[locs], f=span)$y
		} else if ( interp == "spline" ) {
			y <- smooth.spline(locs, x[locs], spar=spar)$y
		} else {
			y <- x[locs]
		}
		y <- approx(locs, y, xout=seq_along(x))$y
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
	.Call(C_smoothSNIP, x, as.integer(width),
		isTRUE(decreasing), PACKAGE="matter")
}

estbase_med <- function(x, width = 100L)
{
	runmed(x, k = 1L + 2L * (width %/% 2L))
}

#### Noise estimation ####
## -----------------------

estnoise_sd <- function(x, nbins = 1L, dynamic = TRUE)
{
	if ( nbins > 1L ) {
		xb <- findbins(x, nbins=nbins, dynamic=dynamic)
		fun <- function(xi) rep.int(sd(xi, na.rm=TRUE), length(xi))
		noise <- unlist(lapply(xb, fun))
		noise <- lowess(noise)$y
	} else {
		noise <- sd(x, na.rm=TRUE)
		noise <- rep.int(noise, length(x))
	}
	noise
}

estnoise_mad <- function(x, nbins = 1L, dynamic = TRUE)
{
	if ( nbins > 1L ) {
		xb <- findbins(x, nbins=nbins, dynamic=dynamic)
		fun <- function(xi) rep.int(mad(xi, na.rm=TRUE), length(xi))
		noise <- unlist(lapply(xb, fun))
		noise <- lowess(noise)$y
	} else {
		noise <- mad(x, na.rm=TRUE)
		noise <- rep.int(noise, length(x))
	}
	noise
}

estnoise_diff <- function(x, nbins = 1L, dynamic = TRUE)
{
	if ( nbins > 1L ) {
		xb <- findbins(x, nbins=nbins, dynamic=dynamic)
		fun <- function(xi) {
			dxi <- mean(diff(xi), na.rm=TRUE)
			nsi <- mean(abs(xi - dxi), na.rm=TRUE)
			rep.int(nsi, length(xi))
		}
		noise <- unlist(lapply(xb, fun))
		noise <- lowess(noise)$y
	} else {
		dx <- mean(diff(x), na.rm=TRUE)
		noise <- mean(abs(x - dx), na.rm=TRUE)
		noise <- rep.int(noise, length(x))
	}
	noise
}

estnoise_smooth <- function(x, span = 2/3)
{
	lowess(x, f=span)$y
}

estnoise_filt <- function(x, snr = 2, nbins = 1L,
	threshold = 0.5, centroided = FALSE)
{
	if ( nbins > 1L ) {
		# Gallia et al (2013) but with lowess
		xb <- findbins(x, nbins=nbins, dynamic=FALSE)
		noise <- lapply(xb, estnoise_filt, snr=snr,
			threshold=threshold, centroided=centroided)
		noise <- unlist(noise)
		noise <- lowess(noise)$y
	} else {
		# Xu and Freitas (2010) dynamic noise level
		if ( !centroided ) {
			y <- sort(x[findpeaks(x)])
		} else {
			y <- sort(x[x > 0])
		}
		if ( length(y) <= 1L )
			return(y)
		noise <- (1 + threshold) * y[1L]
		i <- 2L
		snr_i <- y[2L] / noise
		fit <- lr(1L:i, y[1L:i])
		while ( snr_i < snr )
		{
			i <- i + 1L
			noise <- lr_predict(fit, i)
			snr_i <- y[i] / noise
			fit <- lr_update(fit, i, y[i])
		}
		noise <- rep.int(noise, length(x))
	}
	noise
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

findpeaks <- function(x, prominence = NULL, bounds = TRUE)
{
	peaks <- which(locmax(x))
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
	if ( length(ann) > 0L )
		attributes(peaks) <- ann
	peaks
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
	tol = NA_real_, tol.ref = "abs", merge = TRUE, na.drop = TRUE)
{
	if ( any(lengths(peaklist) != lengths(xlist)) )
		stop("lengths of 'peaklist' and 'xlist' must match")
	if ( is.na(tol) ) {
		# guess tolerance
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
	tol = 2.5, tol.ref = "abs", na.drop = TRUE)
{
	if ( length(peaks) != length(x) )
		stop("length of 'peaks' and 'x' must match")
	if ( is.unsorted(peaks) )
		stop("'peaks' must be sorted")
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
	peaks
}

#### Simulation ####
## -----------------

simspectra <- function(n = 1L, peaks = 50L,
	x = rlnorm(peaks, 7, 0.3), y = rlnorm(peaks, 1, 0.9),
	domain = c(0.9 * min(x), 1.1 * max(x)), size = 10000,
	sdpeaks = sdpeakmult * log1p(y), sdpeakmult = 0.2,
	sdnoise = 0.1, sdx = 1e-5, resolution = 1000, fmax = 0.5,
	baseline = 0, decay = 10, units = "relative", ...)
{
	if ( length(x) != length(y) )
		stop("length of 'x' and 'y' must match")
	if ( missing(x) && !missing(domain) ) {
		to <- domain[2L]
		from <- domain[1L]
		x <- (x - min(x)) / max(x - min(x))
		x <- (from + 0.1 * (to - from)) + (0.8 * (to - from)) * x
	}
	xout <- seq(from=domain[1L], to=domain[2L], length.out=size)
	i <- order(x)
	x <- x[i]
	y <- y[i]
	if ( n > 1L ) {
		yout <- replicate(n, simspectra(x=x, y=y,
			domain=domain, size=size, sdpeaks=sdpeaks, sdpeakmult=sdpeakmult,
			sdnoise=sdnoise, sdx=sdx, resolution=resolution, fmax=fmax,
			baseline=baseline, decay=decay))
	} else {
		dx <- x / resolution
		sdwidth <- qnorm(1 - fmax / 2) * dx
		sdpeaks <- rep_len(sdpeaks, peaks)
		errtype <- pmatch(units, c("relative", "absolute"), nomatch=1L)
		xerr <- rnorm(1) * c(x * sdx, sdx)[errtype]
		x2 <- x + xerr
		b <- baseline * exp(-(decay/max(xout)) * (xout - min(xout)))
		yout <- simspectrum(x2, y, xout=xout,
			peakwidth=sdwidth, sdpeaks=sdpeaks, sdnoise=sdnoise)
		yout <- yout + b
	}
	structure(yout, domain=xout, peaks=x)
}

simspectrum <- function(x, y, xout, peakwidth, sdpeaks, sdnoise)
{
	yout <- numeric(length(xout))
	xrange <- range(xout)
	for ( i in seq_along(x) ) {
		if ( y[i] <= 0 || x[i] < xrange[1L] || x[i] > xrange[2L] )
			next
		peak <- which(x[i] - 6 * peakwidth[i] < xout & xout < x[i] + 6 * peakwidth[i])
		di <- dnorm(xout[peak], mean=x[i], sd=peakwidth[i])
		yerr <- rlnorm(1, sdlog=sdpeaks[i])
		yerr <- yerr - exp(sdpeaks[i]^2 / 2)
		yi <- y[i] + yerr
		yout[peak] <- yout[peak] + yi * (di / max(di))
	}
	noise <- rlnorm(length(yout), sdlog=sdnoise)
	noise <- noise - exp(sdnoise^2 / 2)
	pmax(yout + noise, 0)
}
