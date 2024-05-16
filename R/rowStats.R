
#### Statistics for matter matrices ####
## -------------------------------------

setMethod("rowStats", "ANY",
	function(x, stat, ..., BPPARAM = bpparam()) {
		rowStats_int(x, stat=stat, ..., BPPARAM = BPPARAM)
	})

setMethod("colStats", "ANY",
	function(x, stat, ..., BPPARAM = bpparam()) {
		colStats_int(x, stat=stat, ..., BPPARAM = BPPARAM)
	})

# matter matrices
setMethod("rowStats", "matter_mat",
	function(x, stat, ..., BPPARAM = bpparam()) {
		if ( rowMaj(x) ) {
			rowStats_int(x, stat=stat, ..., iter.dim=1L, BPPARAM = BPPARAM)
		} else {
			rowStats_int(x, stat=stat, ..., iter.dim=2L, BPPARAM = BPPARAM)
		}
	})

setMethod("colStats", "matter_mat",
	function(x, stat, ..., BPPARAM = bpparam()) {
		if ( rowMaj(x) ) {
			colStats_int(x, stat=stat, ..., iter.dim=1L, BPPARAM = BPPARAM)
		} else {
			colStats_int(x, stat=stat, ..., iter.dim=2L, BPPARAM = BPPARAM)
		}
	})

# sparse matrices
setMethod("rowStats", "sparse_mat",
	function(x, stat, ..., BPPARAM = bpparam()) {
		if ( rowMaj(x) ) {
			rowStats_int(x, stat=stat, ..., iter.dim=1L, BPPARAM = BPPARAM)
		} else {
			rowStats_int(x, stat=stat, ..., iter.dim=2L, BPPARAM = BPPARAM)
		}
	})

setMethod("colStats", "sparse_mat",
	function(x, stat, ..., BPPARAM = bpparam()) {
		if ( rowMaj(x) ) {
			colStats_int(x, stat=stat, ..., iter.dim=1L, BPPARAM = BPPARAM)
		} else {
			colStats_int(x, stat=stat, ..., iter.dim=2L, BPPARAM = BPPARAM)
		}
	})

rowStats_fun <- function(iter.dim)
{
	switch(iter.dim,
		`1` = s_rowstats,
		`2` = 
		function(x, stat, group, na.rm)
		{
			g <- group[attr(x, "index")]
			s_rowstats(x, stat=stat,
				group=g, na.rm=na.rm)
		})
}

rowStats_int <- function(x, stat, group = NULL,
	na.rm = FALSE, simplify = TRUE, drop = TRUE,
	iter.dim = 1L, BPPARAM = bpparam(), ...)
{
	if ( !iter.dim %in% c(1L, 2L) )
		stop("iter.dim must be 1 or 2")
	if ( !is.null(group) ) {
		if ( length(group) %% ncol(x) != 0 )
			stop("length of groups [", length(group), "] ",
				"is not a multiple of column extent [", ncol(x), "]")
		group <- as.factor(rep_len(group, ncol(x)))
	}
	FUN <- rowStats_fun(iter.dim)
	if ( iter.dim == 1L ) {
		BIND <- if(is.null(group)) "c" else "rbind"
		ans <- lapply(stat, function(s)
			chunk_rowapply(x, FUN, stat=s,
				group=group, na.rm=na.rm, simplify=BIND,
					BPPARAM=BPPARAM, ...))
	} else {
		ans <- lapply(stat, function(s)
			chunk_colapply(x, FUN, stat=s,
				group=group, na.rm=na.rm, simplify=stat_c,
				BPPARAM=BPPARAM, ...))
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

colStats_fun <- function(iter.dim)
{
	switch(iter.dim,
		`1` = 
		function(x, stat, group, na.rm)
		{
			g <- group[attr(x, "index")]
			s_colstats(x, stat=stat,
				group=g, na.rm=na.rm)
		},
		`2` = s_colstats)
}

colStats_int <- function(x, stat, group = NULL,
	na.rm = FALSE, simplify = TRUE, drop = TRUE,
	iter.dim = 2L, BPPARAM = bpparam(), ...)
{
	if ( !iter.dim %in% c(1L, 2L) )
		stop("iter.dim must be 1 or 2")
	if ( !is.null(group) ) {
		if ( length(group) %% nrow(x) != 0 )
			stop("length of groups [", length(group), "] ",
				"is not a multiple of row extent [", nrow(x), "]")
		group <- as.factor(rep_len(group, nrow(x)))
	}
	FUN <- colStats_fun(iter.dim)
	if ( iter.dim == 1L ) {
		ans <- lapply(stat, function(s)
			chunk_rowapply(x, FUN, stat=s,
				group=group, na.rm=na.rm, simplify=stat_c,
				BPPARAM=BPPARAM, ...))
	} else {
		BIND <- if(is.null(group)) "c" else "rbind"
		ans <- lapply(stat, function(s)
			chunk_colapply(x, FUN, stat=s,
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

