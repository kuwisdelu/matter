
#### Scaling for matter matrices ####
## ----------------------------------

setMethod("scale", "matter_mat", function(x, center=TRUE, scale=TRUE)
{
	.Deprecated("colscale")
	colscale(x, center=center, scale=scale)
})

setMethod("rowscale", "ANY",
	function(x, center=TRUE, scale=TRUE, group = NULL, ..., BPPARAM = bpparam())
{
	rowscale_int(x, center=center, scale=scale, group=group, ..., BPPARAM=BPPARAM)
})

setMethod("colscale", "ANY",
	function(x, center=TRUE, scale=TRUE, group = NULL, ..., BPPARAM = bpparam())
{
	colscale_int(x, center=center, scale=scale, group=group, ..., BPPARAM=BPPARAM)
})

rowscale_int <- function(x, center, scale, group, ..., BPPARAM = NULL)
{
	nr <- nrow(x)
	if ( isTRUE(center) )
		center <- rowStats(x, "mean", group=group, ..., BPPARAM=BPPARAM)
	if ( !is.logical(center) ) {
		if ( NROW(center) != nr )
			stop("length of 'center' must be equal to nrow of x")
		x <- rowsweep(x, center, "-", group=group)
	}
	if ( isTRUE(scale) ) {
		scale <- rowStats(x^2, "sum", group=group, simplify=FALSE, ..., BPPARAM=BPPARAM)
		dof <- pmax(1, unlist(lapply(scale, nobs)) - 1L)
		scale <- sqrt(drop(simplify2array(scale)) / dof)
	}
	if ( !is.logical(scale) ) {
		if ( NROW(scale) != nr )
			stop("length of 'scale' must be equal to nrow of x")
		x <- rowsweep(x, scale, "/", group=group)
	}
	if ( is.numeric(center) ) 
        attr(x, "row-scaled:center") <- center
    if ( is.numeric(scale) ) 
        attr(x, "row-scaled:scale") <- scale
    if ( !is.null(group) )
    	attr(x, "row-scaled:group") <- group
    x
}

colscale_int <- function(x, center, scale, group, ..., BPPARAM = NULL)
{
	nc <- ncol(x)
	if ( isTRUE(center) )
		center <- colStats(x, "mean", group=group, ..., BPPARAM=BPPARAM)
	if ( !is.logical(center) ) {
		if ( NROW(center) != nc )
			stop("length of 'center' must be equal to ncol of x")
		x <- colsweep(x, center, "-", group=group)
	}
	if ( isTRUE(scale) ) {
		scale <- colStats(x^2, "sum", group=group, simplify=FALSE, ..., BPPARAM=BPPARAM)
		dof <- pmax(1, unlist(lapply(scale, nobs)) - 1L)
		scale <- sqrt(drop(simplify2array(scale)) / dof)
	}
	if ( !is.logical(scale) ) {
		if ( NROW(scale) != nc )
			stop("length of 'scale' must be equal to ncol of x")
		x <- colsweep(x, scale, "/", group=group)
	}
	if ( is.numeric(center) ) 
        attr(x, "col-scaled:center") <- center
    if ( is.numeric(scale) )
        attr(x, "col-scaled:scale") <- scale
    if ( !is.null(group) )
    	attr(x, "col-scaled:group") <- group
    x
}

setMethod("rowsweep", "ANY",
	function(x, STATS, FUN = "-", group = NULL, ...)
		rowsweep_matrix(x, STATS, FUN, group, ...))

setMethod("colsweep", "ANY",
	function(x, STATS, FUN = "-", group = NULL, ...)
		colsweep_matrix(x, STATS, FUN, group, ...))

setMethod("rowsweep", "matter_mat",
	function(x, STATS, FUN = "-", group = NULL, ...)
		rowsweep_deferred(x, STATS, FUN, group, ...))

setMethod("colsweep", "matter_mat",
	function(x, STATS, FUN = "-", group = NULL, ...)
		colsweep_deferred(x, STATS, FUN, group, ...))

setMethod("rowsweep", "sparse_mat",
	function(x, STATS, FUN = "-", group = NULL, ...)
		rowsweep_deferred(x, STATS, FUN, group, ...))

setMethod("colsweep", "sparse_mat",
	function(x, STATS, FUN = "-", group = NULL, ...)
		colsweep_deferred(x, STATS, FUN, group, ...))

rowsweep_matrix <- function(x, STATS, FUN = "-", group = NULL, ...)
{
	x <- as.matrix(x)
	if ( is.null(group) )
		group <- 1L
	if ( nrow(x) %% NROW(STATS) != 0L )
		stop("STATS does not recycle exactly across rows")
	if ( ncol(x) %% length(group) != 0L )
		stop("group does not recycle exactly across columns")
	ugroup <- unique(group[!is.na(group)])
	group <- rep_len(group, ncol(x))
	STATS <- matrix(STATS, nrow=nrow(x), ncol=length(ugroup))
	FUN <- match.fun(FUN)
	for ( i in seq_len(ncol(x)))
		x[,i] <- FUN(x[,i], STATS[,group[i]])
	x
}

colsweep_matrix <- function(x, STATS, FUN = "-", group = NULL, ...)
{
	x <- as.matrix(x)
	if ( is.null(group) )
		group <- 1L
	if ( ncol(x) %% NROW(STATS) != 0L )
		stop("STATS does not recycle exactly across columns")
	if ( nrow(x) %% length(group) != 0L )
		stop("group does not recycle exactly across rows")
	ugroup <- unique(group[!is.na(group)])
	group <- rep_len(group, nrow(x))
	STATS <- matrix(STATS, nrow=ncol(x), ncol=length(ugroup))
	FUN <- match.fun(FUN)
	for ( i in seq_len(nrow(x)))
		x[i,] <- FUN(x[i,], STATS[,group[i]])
	x
}

rowsweep_deferred <- function(x, STATS, FUN = "-", group = NULL, ...)
{
	if ( is.null(group) )
		group <- 1L
	if ( nrow(x) %% NROW(STATS) != 0L )
		stop("STATS does not recycle exactly across rows")
	if ( ncol(x) %% length(group) != 0L )
		stop("group does not recycle exactly across columns")
	ugroup <- unique(group[!is.na(group)])
	group <- rep_len(group, ncol(x))
	STATS <- matrix(STATS, nrow=nrow(x), ncol=length(ugroup))
	FUN <- as_Ops(FUN)
	if ( length(ugroup) > 1L ) {
		x <- register_group_op(x, FUN, STATS, group, FALSE, c(1L, 2L))
	} else {
		x <- register_op(x, FUN, STATS, FALSE)
	}
	x
}

colsweep_deferred <- function(x, STATS, FUN = "-", group = NULL, ...)
{
	if ( is.null(group) )
		group <- 1L
	if ( ncol(x) %% NROW(STATS) != 0L )
		stop("STATS does not recycle exactly across columns")
	if ( nrow(x) %% length(group) != 0L )
		stop("group does not recycle exactly across rows")
	ugroup <- unique(group[!is.na(group)])
	group <- rep_len(group, nrow(x))
	STATS <- matrix(STATS, nrow=ncol(x), ncol=length(ugroup))
	FUN <- as_Ops(FUN)
	if ( length(ugroup) > 1L ) {
		x <- register_group_op(x, FUN, STATS, group, FALSE, c(2L, 1L))
	} else {
		x <- register_op(x, FUN, t(STATS), FALSE)
	}
	x
}
