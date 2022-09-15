
#### Scaling for matter matrices ####
## ----------------------------------

setMethod("scale", "matter_mat", function(x, center=TRUE, scale=TRUE)
{
	stop("not implemented yet")
	if ( is.numeric(center) )
		attr(x, "scaled:center") <- center
	if ( is.numeric(scale) )
		attr(x, "scaled:scale") <- scale
	x
})

setMethod("rowsweep", "ANY",
	function(x, group, STATS, FUN = "-", ...)
		rowsweep_matrix(x, group, STATS, FUN, ...))

setMethod("colsweep", "ANY",
	function(x, group, STATS, FUN = "-", ...)
		colsweep_matrix(x, group, STATS, FUN, ...))

setMethod("rowsweep", "matter_mat",
	function(x, group, STATS, FUN = "-", ...)
		rowsweep_matter(x, group, STATS, FUN, ...))

setMethod("colsweep", "matter_mat",
	function(x, group, STATS, FUN = "-", ...)
		colsweep_matter(x, group, STATS, FUN, ...))

rowsweep_matter <- function(x, group, STATS, FUN = "-", ...)
{
	if ( nrow(x) %% NROW(STATS) != 0L )
		stop("STATS does not recycle exactly across rows")
	if ( ncol(x) %% length(group) != 0L )
		stop("group does not recycle exactly across columns")
	ugroup <- unique(group)
	group <- rep_len(group, ncol(x))
	STATS <- matrix(STATS, nrow=nrow(x), ncol=length(ugroup))
	FUN <- as_Ops(FUN)
	if ( length(ugroup) > 1L ) {
		register_group_op(x, FUN, STATS, group, FALSE, c(1L, 2L))
	} else {
		register_op(x, FUN, STATS, FALSE)
	}
}

colsweep_matter <- function(x, group, STATS, FUN = "-", ...)
{
	if ( ncol(x) %% NROW(STATS) != 0L )
		stop("STATS does not recycle exactly across columns")
	if ( nrow(x) %% length(group) != 0L )
		stop("group does not recycle exactly across rows")
	ugroup <- unique(group)
	group <- rep_len(group, nrow(x))
	STATS <- matrix(STATS, nrow=ncol(x), ncol=length(ugroup))
	FUN <- as_Ops(FUN)
	if ( length(ugroup) > 1L ) {
		register_group_op(x, FUN, STATS, group, FALSE, c(2L, 1L))
	} else {
		register_op(x, FUN, t(STATS), FALSE)
	}
}

rowsweep_matrix <- function(x, group, STATS, FUN = "-", ...)
{
	x <- as.matrix(x)
	if ( nrow(x) %% NROW(STATS) != 0L )
		stop("STATS does not recycle exactly across rows")
	if ( ncol(x) %% length(group) != 0L )
		stop("group does not recycle exactly across columns")
	ugroup <- unique(group)
	group <- rep_len(group, ncol(x))
	STATS <- matrix(STATS, nrow=nrow(x), ncol=length(ugroup))
	FUN <- match.fun(FUN)
	vapply(seq_len(ncol(x)), ..., FUN=function(i)
		FUN(x[,i], STATS[,group[i]]), FUN.VALUE=numeric(nrow(x)))
}

colsweep_matrix <- function(x, group, STATS, FUN = "-", ...)
{
	x <- as.matrix(x)
	if ( ncol(x) %% NROW(STATS) != 0L )
		stop("STATS does not recycle exactly across columns")
	if ( nrow(x) %% length(group) != 0L )
		stop("group does not recycle exactly across rows")
	ugroup <- unique(group)
	group <- rep_len(group, nrow(x))
	STATS <- matrix(STATS, nrow=ncol(x), ncol=length(ugroup))
	FUN <- match.fun(FUN)
	t(vapply(seq_len(nrow(x)), ..., FUN=function(i)
		FUN(x[i,], STATS[,group[i]]), FUN.VALUE=numeric(ncol(x))))
}

