
getGroupStats <- function(x, stat, group, na.rm = FALSE)
{
	along <- match.arg(along)
	if ( attr(x, "iter.dim") != along )
		groups <- groups[attr(x, "idx")]
	ret <- lapply(levels(groups), function(g) {
		i <- which(groups == g)
		xg <- switch(along,
			"rows"=x[,i,drop=FALSE],
			"cols"=x[i,,drop=FALSE])
		if ( !is.null(tform) )
			xg <- transformChunk(xg, attributes(x), g, i, along, tform)
		getChunkStats(xg, stat=stat, along=along,
			na.rm=na.rm, tform=NULL, ...)
	})
	names(ret) <- levels(groups)
	ret
}

get_row_stats <- function(x, stat, group, na.rm = FALSE)
{
	ans <- lapply(stat, function(sx)
		s_rowstats(x, stat=sx, na.rm=na.rm))
	set_names(ans, stat)
}

get_row_stats <- function(x, stat, na.rm = FALSE)
{
	ans <- lapply(stat, function(sx)
		s_rowstats(x, stat=sx, na.rm=na.rm))
	set_names(ans, stat)
}

get_col_stats <- function(x, stat, na.rm = FALSE)
{
	ans <- lapply(stat, function(sx)
		s_rowstats(x, stat=sx, na.rm=na.rm))
	set_names(ans, stat)
}
