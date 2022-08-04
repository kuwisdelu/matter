


setMethod("rowStats", "ANY", function(x, stat, groups,
								na.rm = FALSE, tform = identity,
								col.center = NULL, col.scale = NULL,
								row.center = NULL, row.scale = NULL,
								drop = TRUE, BPPARAM = bpparam(), ...)
	{
		getStats(x, stat=stat, groups=groups,
			na.rm=na.rm, tform=tform, along = "rows",
			col.center=col.center, col.scale=col.scale,
			row.center=row.center, row.scale=row.scale,
			drop=drop, BPPARAM=BPPARAM, ...)
	})

setMethod("colStats", "ANY", function(x, stat, groups,
								na.rm = FALSE, tform = identity,
								col.center = NULL, col.scale = NULL,
								row.center = NULL, row.scale = NULL,
								drop = TRUE, BPPARAM = bpparam(), ...)
	{
		getStats(x, stat=stat, groups=groups,
			na.rm=na.rm, tform=tform, along = "cols",
			col.center=col.center, col.scale=col.scale,
			row.center=row.center, row.scale=row.scale,
			drop=drop, BPPARAM=BPPARAM, ...)
	})

setMethod("rowStats", "matter_matc", function(x, stat, groups,
								na.rm = FALSE, tform = identity,
								col.center = NULL, col.scale = NULL,
								row.center = NULL, row.scale = NULL,
								drop = TRUE, BPPARAM = bpparam(), ...)
	{
		getStats(x, stat=stat, groups=groups,
			na.rm=na.rm, tform=tform, along = "rows",
			col.center=col.center, col.scale=col.scale,
			row.center=row.center, row.scale=row.scale,
			drop=drop, iter.dim="cols", BPPARAM=BPPARAM, ...)
	})

setMethod("colStats", "matter_matr", function(x, stat, groups,
								na.rm = FALSE, tform = identity,
								col.center = NULL, col.scale = NULL,
								row.center = NULL, row.scale = NULL,
								drop = TRUE, BPPARAM = bpparam(), ...)
	{
		getStats(x, stat=stat, groups=groups,
			na.rm=na.rm, tform=tform, along = "cols",
			col.center=col.center, col.scale=col.scale,
			row.center=row.center, row.scale=row.scale,
			drop=drop, iter.dim="rows", BPPARAM=BPPARAM, ...)
	})

setMethod("rowStats", "sparse_old_matc", function(x, stat, groups,
								na.rm = FALSE, tform = identity,
								col.center = NULL, col.scale = NULL,
								row.center = NULL, row.scale = NULL,
								drop = TRUE, BPPARAM = bpparam(), ...)
	{
		getStats(x, stat=stat, groups=groups,
			na.rm=na.rm, tform=tform, along = "rows",
			col.center=col.center, col.scale=col.scale,
			row.center=row.center, row.scale=row.scale,
			drop=drop, iter.dim="cols", BPPARAM=BPPARAM, ...)
	})

setMethod("colStats", "sparse_old_matr", function(x, stat, groups,
								na.rm = FALSE, tform = identity,
								col.center = NULL, col.scale = NULL,
								row.center = NULL, row.scale = NULL,
								drop = TRUE, BPPARAM = bpparam(), ...)
	{
		getStats(x, stat=stat, groups=groups,
			na.rm=na.rm, tform=tform, along = "cols",
			col.center=col.center, col.scale=col.scale,
			row.center=row.center, row.scale=row.scale,
			drop=drop, iter.dim="rows", BPPARAM=BPPARAM, ...)
	})

setMethod("rowStats", "virtual_matc", function(x, stat, groups,
								na.rm = FALSE, tform = identity,
								col.center = NULL, col.scale = NULL,
								row.center = NULL, row.scale = NULL,
								drop = TRUE, BPPARAM = bpparam(), ...)
	{
		getStats(x, stat=stat, groups=groups,
			na.rm=na.rm, tform=tform, along = "rows",
			col.center=col.center, col.scale=col.scale,
			row.center=row.center, row.scale=row.scale,
			drop=drop, iter.dim="cols", BPPARAM=BPPARAM, ...)
	})

setMethod("colStats", "virtual_matr", function(x, stat, groups,
								na.rm = FALSE, tform = identity,
								col.center = NULL, col.scale = NULL,
								row.center = NULL, row.scale = NULL,
								drop = TRUE, BPPARAM = bpparam(), ...)
	{
		getStats(x, stat=stat, groups=groups,
			na.rm=na.rm, tform=tform, along = "cols",
			col.center=col.center, col.scale=col.scale,
			row.center=row.center, row.scale=row.scale,
			drop=drop, iter.dim="rows", BPPARAM=BPPARAM, ...)
	})

getStats <- function(x, stat, groups, along = c("rows", "cols"),
						na.rm = FALSE, tform = identity,
						col.center = NULL, col.scale = NULL,
						row.center = NULL, row.scale = NULL,
						chunks = NA, iter.dim = along,
						drop = TRUE, BPPARAM = bpparam(), ...)
{
	along <- match.arg(along)
	d1 <- switch(along, "rows"=dim(x)[1L], "cols"=dim(x)[2L])
	d2 <- switch(along, "rows"=dim(x)[2L], "cols"=dim(x)[1L])
	iter.dim <- match.arg(iter.dim, c("rows", "cols"))
	if ( !missing(groups) && !is.null(groups) ) {
		groups <- as.factor(rep_len(groups, d2))
	} else {
		groups <- NULL
	}
	attr <- list()
	if ( !is.null(col.center) )  {
		if ( is.matrix(col.center) && (along == "rows" || is.null(groups)) )
			stop("col.center must be a vector, not a matrix")
		if ( !is.null(groups) ) {
			col.center <- matrix(col.center,
				nrow=ncol(x), ncol=nlevels(groups))
			colnames(col.center) <- levels(groups)
		}
		attr[["col.center"]] <- col.center
	}
	if ( !is.null(col.scale) )  {
		if ( is.matrix(col.scale) && (along == "rows" || is.null(groups)) )
			stop("col.scale must be a vector, not a matrix")
		if ( !is.null(groups) ) {
			col.scale <- matrix(col.scale,
				nrow=ncol(x), ncol=nlevels(groups))
			colnames(col.scale) <- levels(groups)
		}
		attr[["col.scale"]] <- col.scale
	}
	if ( !is.null(row.center) )  {
		if ( is.matrix(row.center) && (along == "cols" || is.null(groups)) )
			stop("row.center must be a vector, not a matrix")
		if ( !is.null(groups) ) {
			row.center <- matrix(row.center,
				nrow=nrow(x), ncol=nlevels(groups))
			colnames(row.center) <- levels(groups)
		}
		attr[["row.center"]] <- row.center
	}
	if ( !is.null(row.scale) )  {
		if ( is.matrix(row.scale) && (along == "cols" || is.null(groups)) )
			stop("row.scale must be a vector, not a matrix")
		if ( !is.null(groups) ) {
			row.scale <- matrix(row.scale,
				nrow=nrow(x), ncol=nlevels(groups))
			colnames(row.scale) <- levels(groups)
		}
		attr[["row.scale"]] <- row.scale
	}
	if ( length(attr) > 0L || !is.null(groups) )
		attr[["iter.dim"]] <- iter.dim
	alist <- list()
	if ( iter.dim == along ) {
		idx <- seq_len(d1)
		margin <- switch(along, "rows"=1L, "cols"=2L)
		if ( is.null(groups) ) {
			if ( length(attr) > 0L )
				alist[["idx"]] <- idx
			ans <- chunk_apply(x, getChunkStats, margin, view="chunk",
				stat=stat, along=along, na.rm=na.rm, tform=tform,
				chunks=chunks, attr=attr, alist=alist,
				simplify=combine_list, BPPARAM=BPPARAM, ...)
			ans <- collect_by_key(ans, c)
		} else {
			alist[["idx"]] <- idx
			ans <- chunk_apply(x, getGroupStats, margin, view="chunk",
				stat=stat, along=along, na.rm=na.rm, tform=tform,
				chunks=chunks, attr=attr, alist=alist,
				simplify=combine_list, groups=groups,
				BPPARAM=BPPARAM, ...)
			ans <- collect_by_key(ans, c)
			ans <- lapply(ans, collect_by_key, c)
		}
	} else {
		idx <- seq_len(d2)
		margin <- switch(along, "rows"=2L, "cols"=1L)
		if ( is.null(groups) ) {
			if ( length(attr) > 0L )
				alist[["idx"]] <- idx
			ans <- chunk_apply(x, getChunkStats, margin, view="chunk",
				stat=stat, along=along, na.rm=na.rm, tform=tform,
				chunks=chunks, attr=attr, alist=alist,
				simplify=combine_list, BPPARAM=BPPARAM, ...)
			ans <- collect_by_key(ans, stat_c)
		} else {
			alist[["idx"]] <- idx
			ans <- chunk_apply(x, getGroupStats, margin, view="chunk",
				stat=stat, along=along, na.rm=na.rm, tform=tform,
				chunks=chunks, attr=attr, alist=alist,
				simplify=combine_list, groups=groups,
				BPPARAM=BPPARAM, ...)
			ans <- collect_by_key(ans, c)
			ans <- lapply(ans, collect_by_key, stat_c)
		}
	}
	if ( !is.null(groups) ) {
		ans <- lapply(stat, function(sx) {
			xx <- do.call(cbind, lapply(ans, "[[", sx))
			colnames(xx) <- levels(groups)
			xx
		})
	} else {
		ans <- lapply(ans, drop_attr)
	}
	names(ans) <- stat
	if ( drop && length(ans) == 1 )
		ans <- ans[[1]]
	ans
}

getGroupStats <- function(x, stat, groups, along = c("rows", "cols"),
							na.rm = FALSE, tform = NULL, ...)
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

getChunkStats <- function(x, stat, along = c("rows", "cols"),
							na.rm = FALSE, tform = NULL, ...)
{
	if ( missing(stat) )
		stop("missing argument 'stat'")
	along <- match.arg(along)
	if ( !is.null(tform) )
		x <- transformChunk(x, attributes(x), NULL, NULL, along, tform)
	if ( along == "rows" ) {
		ret <- lapply(stat, function(sx)
			rowstreamStats(x, stat=sx, na.rm=na.rm, ...))
	} else {
		ret <- lapply(stat, function(sx)
			colstreamStats(x, stat=sx, na.rm=na.rm, ...))
	}
	names(ret) <- stat
	ret
}

transformChunk <- function(x, attr, group, group_idx, along, tform) {
	dms <- dim(x)
	nms <- c("col.center", "col.scale", "row.center", "row.scale")
	if ( any(nms %in% names(attr)) && prod(dim(x)) > 0L ) {
		if ( !is.null(attr$col.center) ) {
			if ( along == "rows" || is.null(group) ) {
				col.center <- attr$col.center
			} else {
				col.center <- attr$col.center[,group]
			}
			if ( along == "rows" && !is.null(group_idx) ) {
				col.center <- col.center[group_idx]
			} else if ( attr$iter.dim == "cols" ) {
				col.center <- col.center[attr$idx]
			}
		} else {
			col.center <- rep_len(0, ncol(x))
		}
		if ( !is.null(attr$col.scale) )	{
			if ( is.null(group) ) {
				col.scale <- attr$col.scale
			} else {
				col.scale <- attr$col.scale[,group]
			}
			if ( along == "rows" && !is.null(group_idx) ) {
				col.scale <- col.scale[group_idx]
			} else if ( attr$iter.dim == "cols" ) {
				col.scale <- col.scale[attr$idx]
			}
		} else {
			col.scale <- rep_len(1, ncol(x))
		}
		x <- scale(x, center=col.center, scale=col.scale)
		if ( !is.null(attr$row.center) ) {
			if ( is.null(group) ) {
				row.center <- attr$row.center
			} else {
				row.center <- attr$row.center[,group]
			}
			if ( along == "cols" && !is.null(group_idx) ) {
				row.center <- row.center[group_idx]
			} else if ( attr$iter.dim == "rows" ) {
				row.center <- row.center[attr$idx]
			}
		} else {
			row.center <- rep_len(0, nrow(x))
		}
		if ( !is.null(attr$row.scale) )	{
			if ( is.null(group) ) {
				row.scale <- attr$row.scale
			} else {
				row.scale <- attr$row.scale[,group]
			}
			if ( along == "cols" && !is.null(group_idx) ) {
				row.scale <- row.scale[group_idx]
			} else if ( attr$iter.dim == "rows" ) {
				row.scale <- row.scale[attr$idx]
			}
		} else {
			row.scale <- rep_len(1, nrow(x))
		}
		x <- (x - row.center) / row.scale
	}
	tform(x)
}

