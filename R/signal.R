
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
		sse <- binvec(x, lower, upper, stat="sse")
		trace <-  numeric(niter + 1L)
		trace[1L] <- sum(sse)
		for ( i in seq_len(niter) )
		{
			merge <- which.min(sse[-n] + sse[-1])
			lower_new <- lower[-(merge + 1L)]
			upper_new <- upper[-merge]
			split <- which.max(sse[-merge])
			at <- ceiling((lower_new[split] + upper_new[split]) / 2)
			lower_split <- c(lower_new[split], at)
			upper_split <- c(at - 1L, upper_new[split])
			if ( split == 1L ) {
				lower_new <- c(lower_split, lower_new[-1L])
				upper_new <- c(upper_split, upper_new[-1L])
			} else if ( split == n ) {
				lower_new <- c(lower_new[-n], lower_split)
				upper_new <- c(upper_new[-n], upper_split)
			} else {
				lower_new <- c(lower_new[1:(split - 1L)], lower_split,
					lower_new[(split + 1L):(n - 1L)])
				upper_new <- c(upper_new[1:(split - 1L)], upper_split,
					upper_new[(split + 1L):(n - 1L)])
			}
			sse <- binvec(x, lower_new, upper_new, stat="sse")
			score <- sum(sse)
			if ( score < trace[i] || !check_converge ) {
				trace[i + 1L] <- score
				lower <- lower_new
				upper <- upper_new
			} else {
				trace <- trace[!is.na(trace)]
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
	.Call(C_sampleLTOB, x, t, as.integer(lower - 1L), as.integer(upper - 1L),
		PACKAGE="matter")
}

lttb <- function(x, t, lower, upper)
{
	.Call(C_sampleLTTB, x, t, as.integer(lower - 1L), as.integer(upper - 1L),
		PACKAGE="matter")
}

downsample <- function(x, t, n = length(x) / 10L,
	method = c("dynamic", "lttb", "ltob"))
{
	if ( missing(t) || is.null(t) )
		t <- as.numeric(seq_along(x))
	method <- match.arg(method)
	if ( method == "dynamic" ) {
		buckets <- findbins(x, n - 2L, niter=NA)
	} else {
		buckets <- findbins(x, n - 2L, niter=-1)
	}
	if ( method == "ltob" ) {
		samples <- ltob(x, t, buckets$lower, buckets$upper)
	} else {
		samples <- lttb(x, t, buckets$lower, buckets$upper)
	}
	samples <- c(1L, samples, length(x))
	x[samples]
}

#### Peak detection ####
## ---------------------

locmax <- function(x, window = 5L)
{
	.Call(C_localMaxima, x, as.integer(window), PACKAGE="matter")
}

findpeaks <- function(x, prominence = NULL)
{
	peaks <- which(locmax(x))
	bounds <- .Call(C_peakBoundaries, x,
		as.integer(peaks - 1L), PACKAGE="matter")
	ann <- data.frame(row.names=seq_along(peaks))
	ann$left_bounds <- bounds[[1L]] + 1L
	ann$right_bounds <- bounds[[2L]] + 1L
	if ( isTRUE(prominence) || is.numeric(prominence) )
	{
		bases <- .Call(C_peakBases, x,
			as.integer(peaks - 1L), PACKAGE="matter")
		ann$left_bases <- bases[[1L]] + 1L
		ann$right_bases <- bases[[2L]] + 1L
		contour <- pmax(x[ann$left_bases], x[ann$right_bases])
		ann$prominences <- x[peaks] - contour
		if ( is.numeric(prominence) )
		{
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
	heights <- x[peaks] - (1 - fmax) * p
	thresholds <- .Call(C_peakWidths, x, as.integer(peaks - 1L),
		as.double(domain), as.integer(left_end - 1L), as.integer(right_end - 1L),
		as.double(heights), PACKAGE="matter")
	ann <- data.frame(row.names=seq_along(peaks))
	ann$width_heights <- heights
	ann$left_points <- thresholds[[1L]]
	ann$right_points <- thresholds[[2L]]
	widths <- ann$right_points - ann$left_points
	attributes(widths) <- ann
	widths
}

peakareas <- function(x, peaks, domain = NULL)
{
	if ( is.null(domain) )
		domain <- seq_along(x)
	left_end <- as.integer(attr(peaks, "left_bounds") - 1L)
	right_end <- as.integer(attr(peaks, "right_bounds") - 1L)
	if ( is.null(left_end) || is.null(right_end) )
	{
		bounds <- .Call(C_peakBoundaries, x,
			as.integer(peaks - 1L), PACKAGE="matter")
		left_end <- bounds[[1L]]
		right_end <- bounds[[2L]]
	}
	.Call(C_peakAreas, x, as.integer(peaks - 1L),
		domain, left_end, right_end, PACKAGE="matter")
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
