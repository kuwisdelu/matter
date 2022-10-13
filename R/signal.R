
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

#### Peak detection ####
## ---------------------

locmax <- function(x, width = 5)
{
	.Call(C_localMaxima, x, width, PACKAGE="matter")
}

