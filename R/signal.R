
#### Filtering and Smoothing ####
## -----------------------------

filt1 <- function(x, width = 5L, weights = rep_len(1L, width))
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

findbins <- function(x, nbins, niter = NA)
{
	n <- floor(nbins)
	bw <- ceiling(length(x) / n)
	breaks <- floor(seq(from=bw, to=length(x) - bw, length.out=n - 1L))
	lower <- c(1L, breaks + 1L)
	upper <- c(breaks, length(x))
	if ( is.na(niter) ) {
		niter <- floor(10 * length(x) / n)
		check_converge <- TRUE
	} else {
		check_converge <- FALSE
	}
	if ( niter >= 0 ) {
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
		structure(data.frame(lower=lower, upper=upper,
			size=upper - lower + 1L, sse=sse), trace=trace)
	} else {
		data.frame(lower=lower, upper=upper, size=upper - lower + 1L)
	}
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
		buckets <- findbins(x, n - 2L, niter=NA)
	} else {
		buckets <- findbins(x, n - 2L, niter=-1)
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
	asearch_int(as.double(xi), as.double(x), as.double(y), tol=halfwidth,
		tol.ref=as_tol_ref("abs"), nomatch=NA_real_,
		interp=as_interp(interp))
}

#### Continuum estimation ####
## ----------------------------

estbase <- function(x, interp = c("linear", "loess", "spline"),
	span = 1/10, spar = NULL, upper = FALSE)
{
	if ( upper ) {
		bases <- which(locmax(x))
	} else {
		bases <- which(locmin(x))
	}
	if ( length(bases) >= 2 ) {
		interp <- match.arg(interp)
		bases <- c(1L, bases, length(x))
		if ( interp == "loess" ) {
			y <- lowess(bases, x[bases], f=span)$y
		} else if ( interp == "spline" ) {
			y <- smooth.spline(bases, x[bases], spar=spar)$y
		} else {
			y <- x[bases]
		}
		y <- approx(bases, y, xout=seq_along(x))$y
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

estbase_med <- function(x, width = 100L, decreasing = TRUE)
{
	runmed(x, k = 1L + 2L * (width %/% 2L))
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

findpeaks <- function(x, prominence = NULL)
{
	peaks <- which(locmax(x))
	# find peak boundaries (nearest local maxima)
	bounds <- .Call(C_peakBoundaries, x,
		as.integer(peaks - 1L), PACKAGE="matter")
	ann <- data.frame(row.names=seq_along(peaks))
	ann$left_bounds <- bounds[[1L]] + 1L
	ann$right_bounds <- bounds[[2L]] + 1L
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
	attributes(peaks) <- ann
	peaks
}

peakwidths <- function(x, peaks, domain = NULL,
	fmax = 0.5, ref = c("height", "prominence"))
{
	ref <- match.arg(ref)
	if ( is.null(domain) )
		domain <- seq_along(x)
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
	yout + noise
}
