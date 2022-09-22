
setMethod("rowStats", "ANY",
	function(x, stat, group = NULL, na.rm = FALSE,
		drop = TRUE, BPPARAM = bpparam(), ...)
	{
		BIND <- if(is.null(group)) "c" else "rbind"
		ans <- lapply(stat, function(s)
			chunk_rowapply(x, s_rowstats, stat=s, group=group,
				na.rm=na.rm, simplify=BIND, BPPARAM=BPPARAM))
		names(ans) <- stat
		if ( drop && length(ans) == 1L )
			ans <- ans[[1L]]
		ans
	})

setMethod("colStats", "ANY",
	function(x, stat, group = NULL, na.rm = FALSE,
		drop = TRUE, BPPARAM = bpparam(), ...)
	{
		BIND <- if(is.null(group)) "c" else "cbind"
		ans <- lapply(stat, function(s)
			chunk_colapply(x, s_colstats, stat=s, group=group,
				na.rm=na.rm, simplify=BIND, BPPARAM=BPPARAM))
		names(ans) <- stat
		if ( drop && length(ans) == 1L )
			ans <- ans[[1L]]
		ans
	})

