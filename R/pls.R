
#### Partial least squares regression ####
## ---------------------------------------

# NIPALS
pls_nipals <- function(x, k = 3L, center = TRUE, scale. = FALSE,
	transpose = FALSE, niter = 100L, tol = 1e-5, verbose = NA, ...)
{
	k <- min(k, dim(x))
	if ( transpose ) {
		x <- rowscale(x, center=center, scale=scale.)
		center <- attr(x, "row-scaled:center")
		scale <- attr(x, "row-scaled:scale")
	} else {
		x <- colscale(x, center=center, scale=scale.)
		center <- attr(x, "col-scaled:center")
		scale <- attr(x, "col-scaled:scale")
	}
	j <- seq_len(k)

	# TODO

	if ( is.null(center) ) {
		ans$center <- FALSE
	} else {
		ans$center <- center
	}
	if ( is.null(scale) ) {
		ans$scale <- FALSE
	} else {
		ans$scale <- scale
	}
	class(ans) <- "pls"
	ans
}
