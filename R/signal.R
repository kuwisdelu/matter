
#### Peak detection ####
## ---------------------

locmax <- function(x, width = 5)
{
	.Call(C_localMaxima, x, width, PACKAGE="matter")
}

findpeaks <- function(x, width = 5)
{
	peaks <- .Call(C_localMaxima, x, width, PACKAGE="matter")
	peaks <- which(peaks)
	bounds <- .Call(C_peakBoundaries, x, width, as.integer(peaks - 1L))
	attr(peaks, "left_bounds") <- as.integer(bounds[[1L]] + 1L)
	attr(peaks, "right_bounds") <- as.integer(bounds[[2L]] + 1L)
	peaks
}

#### Binning ####
## ---------------

binvec <- function(x, u, v, method = "sum")
{
	if ( missing(v) ) {
		v <- u[-1] - 1L
		u <- u[-length(u)]
	}
	.Call(C_binVector, x, as.integer(u), as.integer(v),
		as_binfun(method), PACKAGE="matter")
}

