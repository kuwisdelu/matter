
setMethod("rowStats", "ANY",
	function(x, ..., BPPARAM = bpparam()) {
		rowStats_int(x, ..., BPPARAM = bpparam())
	})

setMethod("colStats", "ANY",
	function(x, ..., BPPARAM = bpparam()) {
		colStats_int(x, ..., BPPARAM = bpparam())
	})

setMethod("rowStats", "matter_mat",
	function(x, ..., BPPARAM = bpparam()) {
		if ( x@transpose ) {
			rowStats_int(x, ..., iter.dim=1L, BPPARAM = bpparam())
		} else {
			rowStats_int(x, ..., iter.dim=2L, BPPARAM = bpparam())
		}
	})

setMethod("colStats", "matter_mat",
	function(x, ..., BPPARAM = bpparam()) {
		if ( x@transpose ) {
			colStats_int(x, ..., iter.dim=1L, BPPARAM = bpparam())
		} else {
			colStats_int(x, ..., iter.dim=2L, BPPARAM = bpparam())
		}
	})

setMethod("rowStats", "sparse_mat",
	function(x, ..., BPPARAM = bpparam()) {
		if ( x@transpose ) {
			rowStats_int(x, ..., iter.dim=1L, BPPARAM = bpparam())
		} else {
			rowStats_int(x, ..., iter.dim=2L, BPPARAM = bpparam())
		}
	})

setMethod("colStats", "sparse_mat",
	function(x, ..., BPPARAM = bpparam()) {
		if ( x@transpose ) {
			colStats_int(x, ..., iter.dim=1L, BPPARAM = bpparam())
		} else {
			colStats_int(x, ..., iter.dim=2L, BPPARAM = bpparam())
		}
	})

rowStats_int <- function(x, stat, group = NULL,
	na.rm = FALSE, simplify = TRUE, drop = TRUE,
	iter.dim = 1L, BPPARAM = bpparam(), ...)
{
	if ( !iter.dim %in% c(1L, 2L) )
		stop("iter.dim must be 1 or 2")
	if ( !is.null(group) ) {
		if ( length(group) %% ncol(x) != 0 )
			stop(paste0("length of groups [", length(group), "] ",
				"is not a multiple of column extent [", ncol(x), "]"))
		group <- as.factor(rep_len(group, ncol(x)))
	}
	if ( iter.dim == 1L ) {
		BIND <- if(is.null(group)) "c" else "rbind"
		ans <- lapply(stat, function(s)
			chunk_rowapply(x, s_rowstats, stat=s,
				group=group, na.rm=na.rm, simplify=BIND,
					BPPARAM=BPPARAM, ...))
	} else {
		ans <- lapply(stat, function(s)
			chunk_colapply(x,
				function(xi) {
					g <- group[attr(xi, "index")]
					s_rowstats(xi, stat=s, group=g, na.rm=na.rm)
				}, simplify=stat_c, BPPARAM=BPPARAM, ...))
	}
	ans <- lapply(ans,
		function(s) {
			if ( is.array(s) )
				rownames(s) <- rownames(x)
			if ( simplify )
				s <- drop_attr(s)
			s
		})
	names(ans) <- stat
	if ( simplify )
		ans <- simplify2array(ans)
	if ( drop && !is.list(ans) )
		ans <- drop(ans)
	ans
}

.rowStats <- rowStats_int

colStats_int <- function(x, stat, group = NULL,
	na.rm = FALSE, simplify = TRUE, drop = TRUE,
	iter.dim = 2L, BPPARAM = bpparam(), ...)
{
	if ( !iter.dim %in% c(1L, 2L) )
		stop("iter.dim must be 1 or 2")
	if ( !is.null(group) ) {
		if ( length(group) %% nrow(x) != 0 )
			stop(paste0("length of groups [", length(group), "] ",
				"is not a multiple of row extent [", nrow(x), "]"))
		group <- as.factor(rep_len(group, nrow(x)))
	}
	if ( iter.dim == 1L ) {
		ans <- lapply(stat, function(s)
			chunk_rowapply(x,
				function(xi) {
					g <- group[attr(xi, "index")]
					s_colstats(xi, stat=s, group=g, na.rm=na.rm)
				}, simplify=stat_c, BPPARAM=BPPARAM, ...))
	} else {
		BIND <- if(is.null(group)) "c" else "rbind"
		ans <- lapply(stat, function(s)
			chunk_colapply(x, s_colstats, stat=s,
				group=group, na.rm=na.rm, simplify=BIND,
					BPPARAM=BPPARAM, ...))
	}
	ans <- lapply(ans,
		function(s) {
			if ( is.array(s) )
				rownames(s) <- colnames(x)
			if ( simplify )
				s <- drop_attr(s)
			s
		})
	names(ans) <- stat
	if ( simplify )
		ans <- simplify2array(ans)
	if ( drop && !is.list(ans) )
		ans <- drop(ans)
	ans
}

.colStats <- colStats_int

