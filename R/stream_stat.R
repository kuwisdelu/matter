
# streaming statistical summaries

s_range <- function(x, ..., na.rm = FALSE) {
	if ( length(list(...)) > 0L ) {
		x <- s_range(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(range(x, na.rm=na.rm),
			class=c("stream_range", "stream_stat"),
			na.rm=na.rm,
			nobs=na_length(x, na.rm))
	} else {
		x
	}
}

s_min <- function(x, ..., na.rm = FALSE) {
	if ( length(list(...)) > 0L ) {
		x <- s_min(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(min(x, na.rm=na.rm),
			class=c("stream_min", "stream_stat"),
			na.rm=na.rm,
			nobs=na_length(x, na.rm))
	} else {
		x
	}
}

s_max <- function(x, ..., na.rm = FALSE) {
	if ( length(list(...)) > 0L ) {
		x <- s_max(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(max(x, na.rm=na.rm),
			class=c("stream_max", "stream_stat"),
			na.rm=na.rm,
			nobs=na_length(x, na.rm))
	} else {
		x
	}
}

s_prod <- function(x, ..., na.rm = FALSE) {
	if ( length(list(...)) > 0L ) {
		x <- s_prod(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(prod(x, na.rm=na.rm),
			class=c("stream_prod", "stream_stat"),
			na.rm=na.rm,
			nobs=na_length(x, na.rm))
	} else {
		x
	}
}

s_sum <- function(x, ..., na.rm = FALSE) {
	if ( length(list(...)) > 0L ) {
		x <- s_sum(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(sum(x, na.rm=na.rm),
			class=c("stream_sum", "stream_stat"),
			na.rm=na.rm,
			nobs=na_length(x, na.rm))
	} else {
		x
	}
}

s_mean <- function(x, ..., na.rm = FALSE) {
	if ( length(list(...)) > 0L ) {
		x <- s_mean(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(mean(x, na.rm=na.rm),
			class=c("stream_mean", "stream_stat"),
			na.rm=na.rm,
			nobs=na_length(x, na.rm))
	} else {
		x
	}
}

s_var <- function(x, ..., na.rm = FALSE) {
	if ( length(list(...)) > 0L ) {
		x <- s_var(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(var(x, na.rm=na.rm),
			class=c("stream_var", "stream_stat"),
			mean=mean(x, na.rm=na.rm),
			na.rm=na.rm,
			nobs=na_length(x, na.rm))
	} else {
		x
	}
}

s_sd <- function(x, ..., na.rm = FALSE) {
	if ( length(list(...)) > 0L ) {
		x <- s_sd(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(sd(x, na.rm=na.rm),
			class=c("stream_sd", "stream_stat"),
			mean=mean(x, na.rm=na.rm),
			na.rm=na.rm,
			nobs=na_length(x, na.rm))
	} else {
		x
	}
}

s_any <- function(x, ..., na.rm = FALSE) {
	if ( length(list(...)) > 0L ) {
		x <- s_any(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(any(x, na.rm=na.rm),
			class=c("stream_any", "stream_stat"),
			na.rm=na.rm,
			nobs=na_length(x, na.rm))
	} else {
		x
	}
}

s_all <- function(x, ..., na.rm = FALSE) {
	if ( length(list(...)) > 0L ) {
		x <- s_all(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(all(x, na.rm=na.rm),
			class=c("stream_all", "stream_stat"),
			na.rm=na.rm,
			nobs=na_length(x, na.rm))
	} else {
		x
	}
}

# length function

na_length <- function(x, na.rm = FALSE) {
	if ( na.rm ) {
		sum(!is.na(x))
	} else {
		length(x)
	}
}

# register for S4 methods

setOldClass(c("stream_range", "stream_stat"))

setOldClass(c("stream_min", "stream_stat"))

setOldClass(c("stream_max", "stream_stat"))

setOldClass(c("stream_prod", "stream_stat"))

setOldClass(c("stream_sum", "stream_stat"))

setOldClass(c("stream_mean", "stream_stat"))

setOldClass(c("stream_var", "stream_stat"))

setOldClass(c("stream_sd", "stream_stat"))

setOldClass(c("stream_any", "stream_stat"))

setOldClass(c("stream_all", "stream_stat"))

# streaming statistics methods

drop_attr <- function(x, keep.names = TRUE) {
	y <- as.vector(x)
	if ( keep.names )
		names(y) <- names(x)
	y
}

is.stream_stat <- function(x) is(x, "stream_stat")

print.stream_stat <- function(x, ...) {
	cat(class(x)[1L], "with n =", paste_head(nobs(x)), "\n")
	print(drop_attr(x), ...)
	cat("na.rm = ", na_rm(x), "\n")
}

nobs.stream_stat <- function(object, ...) {
	attr(object, "nobs")
}

setMethod("nobs", "stream_stat", nobs.stream_stat)

na_rm <- function(object, ...) UseMethod("na_rm")

na_rm.default <- function(object, ...) attr(object, "na.rm")

stat_c <- function(x, y, ...) {
	if ( length(list(...)) > 0 ) {
		stat_c(x, do.call(stat_c, list(y, ...)))
	} else {
		UseMethod("stat_c")
	}
}

setMethod("combine", "stream_stat",
	function(x, y, ...) {
		if ( class(x)[1L] != class(y)[1L] )
			return(c(drop_attr(x), drop_attr(y)))
		structure(c(drop_attr(x), drop_attr(y)),
			class=class(x),
			na.rm=all(na_rm(x) & na_rm(y)),
			nobs=c(nobs(x), nobs(y)))
	})

c.stream_stat <- function(x, ...) {
	dots <- list(...)
	if ( length(dots) > 0 ) {
		combine(x, ...)
	} else {
		x
	}
}

`[.stream_stat` <- function(x, i, j, ..., drop = TRUE) {
	structure(drop_attr(x)[i],
			class=class(x),
			na.rm=na_rm(x),
			nobs=nobs(x)[i])
}

`[[.stream_stat` <- function(x, i, exact = TRUE) {
	structure(drop_attr(x)[[i]],
			class=class(x),
			na.rm=na_rm(x),
			nobs=nobs(x)[[i]])
}

# create new stream_stat w/ inherited attributes

stream_stat_attr <- function(value, x, y) {
	if ( na_rm(x) != na_rm(y) )
		warning("combining statistics with differing na.rm")
	structure(value,
		class=class(x), names=names(x),
		na.rm=all(na_rm(x) & na_rm(y)),
		nobs=nobs(x) + nobs(y))
}

# combine statistics

stat_c.default <- function(x, y, ...) {
	if ( is.stream_stat(y) ) {
		stat_c(y, x)
	} else {
		c(x, y)
	}
}

stat_c.stream_range <- function(x, y, ...) {
	if ( !inherits(y, class(x)) )
		y <- s_range(y, na.rm=na_rm(x))
	xx <- drop_attr(x)
	yy <- drop_attr(y)
	xmin <- xx[c(TRUE, FALSE)]
	xmax <- xx[c(FALSE, TRUE)]
	ymin <- yy[c(TRUE, FALSE)]
	ymax <- yy[c(FALSE, TRUE)]
	val1 <- pmin(xmin, ymin, na.rm=na_rm(x) && na_rm(y))
	val2 <- pmax(xmax, ymax, na.rm=na_rm(x) && na_rm(y))
	val <- as.vector(matrix(c(val1, val2), nrow=2, byrow=TRUE))
	stream_stat_attr(val, x, y)
}

stat_c.stream_min <- function(x, y, ...) {
	if ( !inherits(y, class(x)) )
		y <- s_min(y, na.rm=na_rm(x))
	val <- pmin(x, y, na.rm=na_rm(x) && na_rm(y))
	stream_stat_attr(val, x, y)
}

stat_c.stream_max <- function(x, y, ...) {
	if ( !inherits(y, class(x)) )
		y <- s_max(y, na.rm=na_rm(x))
	val <- pmax(x, y, na.rm=na_rm(x) && na_rm(y))
	stream_stat_attr(val, x, y)
}

stat_c.stream_prod <- function(x, y, ...) {
	if ( !inherits(y, class(x)) )
		y <- s_prod(y, na.rm=na_rm(x))
	if ( na_rm(x) && na_rm(y) ) {
		xx <- ifelse(is.na(x), 1, x)
		yy <- ifelse(is.na(y), 1, y)
	} else {
		xx <- x
		yy <- y
	}
	val <- xx * yy
	stream_stat_attr(val, x, y)
}

stat_c.stream_sum <- function(x, y, ...) {
	if ( !inherits(y, class(x)) )
		y <- s_sum(y, na.rm=na_rm(x))
	if ( na_rm(x) && na_rm(y) ) {
		xx <- ifelse(is.na(x), 0, x)
		yy <- ifelse(is.na(y), 0, y)
	} else {
		xx <- x
		yy <- y
	}
	val <- xx + yy
	stream_stat_attr(val, x, y)
}

stat_c.stream_mean <- function(x, y, ...) {
	if ( !inherits(y, class(x)) )
		y <- s_mean(y, na.rm=na_rm(x))
	nx <- nobs(x)
	ny <- nobs(y)
	if ( na_rm(x) && na_rm(y) ) {
		xx <- ifelse(is.na(x), 0, x)
		yy <- ifelse(is.na(y), 0, y)
	} else {
		xx <- ifelse(nx == 0, 0, x)
		yy <- ifelse(ny == 0, 0, y)
	}
	val <- (nx * xx + ny * yy) / (nx + ny)
	stream_stat_attr(val, x, y)
}

stat_c.stream_var <- function(x, y, ...) {
	if ( !inherits(y, class(x)) )
		y <- s_var(y, na.rm=na_rm(x))
	nx <- nobs(x)
	ny <- nobs(y)
	if ( all(nx == 0) )
		return(y)
	if ( all(ny == 0) )
		return(x)
	mx <- attr(x, "mean")
	my <- attr(y, "mean")
	m <- (nx * mx + ny * my) / (nx + ny)
	if ( na_rm(x) && na_rm(y) ) {
		mx <- ifelse(is.na(mx), 0, mx)
		my <- ifelse(is.na(my), 0, my)
		m <- ifelse(is.na(m), 0, m)
	} else {
		mx <- ifelse(nx == 0, 0, mx)
		my <- ifelse(ny == 0, 0, my)
	}
	nn1 <- nx <= 1 | ny <= 1
	nnN <- nx > 1 & ny > 1
	if ( any(nn1) ) {
		if ( any(nx > 1) ) {
			ss1 <- (nx - 1) * x
			ss1 <- ifelse(is.na(ss1), 0, ss1)
			ss2 <- ss1 + (my - mx) * (my - m)
			val_1 <- ss2 / (nx + ny - 1)
		} else {
			ss1 <- (ny - 1) * y
			ss1 <- ifelse(is.na(ss1), 0, ss1)
			ss2 <- ss1 + (mx - my) * (mx - m)
			val_1 <- ss2 / (nx + ny - 1)
		}
	} else {
		val_1 <- rep(NA_real_, length(x))
	}
	if ( any(nnN) ) {
		num1 <- ((nx - 1) * x) + ((ny - 1) * y)
		num2 <- (nx * ny / (nx + ny)) * (mx - my)^2
		val_N <- (num1 + num2) / (nx + ny - 1)
	} else {
		val_N <- rep(NA_real_, length(x))
	}
	val <- ifelse(nn1, val_1, val_N)
	ret <- stream_stat_attr(val, x, y)
	attr(ret, "mean") <- m
	ret
}

stat_c.stream_sd <- function(x, y, ...) {
	if ( !inherits(y, class(x)) )
		y <- s_sd(y, na.rm=na_rm(x))
	nx <- nobs(x)
	ny <- nobs(y)
	if ( all(nx == 0) )
		return(y)
	if ( all(ny == 0) )
		return(x)
	mx <- attr(x, "mean")
	my <- attr(y, "mean")
	m <- (nx * mx + ny * my) / (nx + ny)
	if ( na_rm(x) && na_rm(y) ) {
		mx <- ifelse(is.na(mx), 0, mx)
		my <- ifelse(is.na(my), 0, my)
		m <- ifelse(is.na(m), 0, m)
	} else {
		mx <- ifelse(nx == 0, 0, mx)
		my <- ifelse(ny == 0, 0, my)
	}
	nn1 <- nx <= 1 | ny <= 1
	nnN <- nx > 1 & ny > 1
	if ( any(nn1) ) {
		if ( any(nx > 1) ) {
			ss1 <- (nx - 1) * x^2
			ss1 <- ifelse(is.na(ss1), 0, ss1)
			ss2 <- ss1 + (my - mx) * (my - m)
			val_1 <- sqrt(ss2 / (nx + ny - 1))
		} else {
			ss1 <- (ny - 1) * y^2
			ss1 <- ifelse(is.na(ss1), 0, ss1)
			ss2 <- ss1 + (mx - my) * (mx - m)
			val_1 <- sqrt(ss2 / (nx + ny - 1))
		}
	} else {
		val_1 <- rep(NA_real_, length(m))
	}
	if ( any(nnN) ) {
		num1 <- ((nx - 1) * x^2) + ((ny - 1) * y^2)
		num2 <- (nx * ny / (nx + ny)) * (mx - my)^2
		val_N <- sqrt((num1 + num2) / (nx + ny - 1))
	} else {
		val_N <- rep(NA_real_, length(m))
	}
	val <- ifelse(nn1, val_1, val_N)
	ret <- stream_stat_attr(val, x, y)
	attr(ret, "mean") <- m
	ret
}

stat_c.stream_any <- function(x, y, ...) {
	if ( !inherits(y, class(x)) )
		y <- s_any(y, na.rm=na_rm(x))
	if ( na_rm(x) && na_rm(y) ) {
		xx <- ifelse(is.na(x), FALSE, x)
		yy <- ifelse(is.na(y), FALSE, y)
	} else {
		xx <- x
		yy <- y
	}
	val <- xx | yy
	stream_stat_attr(val, x, y)
}

stat_c.stream_all <- function(x, y, ...) {
	if ( !inherits(y, class(x)) )
		y <- s_all(y, na.rm=na_rm(x))
	if ( na_rm(x) && na_rm(y) ) {
		xx <- ifelse(is.na(x), TRUE, x)
		yy <- ifelse(is.na(y), TRUE, y)
	} else {
		xx <- x
		yy <- y
	}
	val <- xx & yy
	stream_stat_attr(val, x, y)
}


# streaming matrix stats

stream_stat_fun <- function(name) {
	f <- list(
		range=base::range,
		min=base::min,
		max=base::max,
		prod=base::prod,
		sum=base::sum,
		mean=base::mean.default,
		var=stats::var,
		sd=stats::sd,
		any=base::any,
		all=base::all)
	f[[name, exact=TRUE]]
}

stream_stat_class <- function(name) {
	f <- list(
		range="stream_range",
		min="stream_min",
		max="stream_max",
		prod="stream_prod",
		sum="stream_sum",
		mean="stream_mean",
		var="stream_var",
		sd="stream_sd",
		any="stream_any",
		all="stream_all")
	c(f[[name, exact=TRUE]], "stream_stat")
}

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
			BPPARAM=BPPARAM, ...)
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

setMethod("rowStats", "sparse_matc", function(x, stat, groups,
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

setMethod("colStats", "sparse_matr", function(x, stat, groups,
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
	if ( !missing(groups) )
		groups <- as.factor(rep_len(groups, d2))
	attr <- list()
	if ( !is.null(col.center) )  {
		if ( !missing(groups) ) {
			col.center <- matrix(col.center,
				nrow=ncol(x), ncol=nlevels(groups))
			colnames(col.center) <- levels(groups)
		}
		attr[["col.center"]] <- col.center
	}
	if ( !is.null(col.scale) )  {
		if ( !missing(groups) ) {
			col.scale <- matrix(col.scale,
				nrow=ncol(x), ncol=nlevels(groups))
			colnames(col.scale) <- levels(groups)
		}
		attr[["col.scale"]] <- col.scale
	}
	if ( !is.null(row.center) )  {
		if ( !missing(groups) ) {
			row.center <- matrix(row.center,
				nrow=nrow(x), ncol=nlevels(groups))
			colnames(row.center) <- levels(groups)
		}
		attr[["row.center"]] <- row.center
	}
	if ( !is.null(row.scale) )  {
		if ( !missing(groups) ) {
			row.scale <- matrix(row.scale,
				nrow=nrow(x), ncol=nlevels(groups))
			colnames(row.scale) <- levels(groups)
		}
		attr[["row.scale"]] <- row.scale
	}
	if ( length(attr) > 0L || !missing(groups) )
		attr[["iter.dim"]] <- iter.dim
	alist <- list()
	if ( iter.dim == along ) {
		idx <- seq_len(d1)
		margin <- switch(along, "rows"=1L, "cols"=2L)
		if ( missing(groups) ) {
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
		if ( missing(groups) ) {
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
	if ( !missing(groups) ) {
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

rowstreamStats <- function(x, stat, na.rm = FALSE, ...) {
	fun <- stream_stat_fun(stat)
	template <- switch(stat, range=numeric(2),
		any=logical(1), all=logical(1), numeric(1))
	val <- apply_int(x, 1, fun, template, na.rm=na.rm)
	nobs <- apply_int(x, 1, na_length, numeric(1), na.rm=na.rm)
	if ( stat %in% c("var", "sd") ) {
		means <- rowMeans(x, na.rm=na.rm)
		ans <- structure(val, class=stream_stat_class(stat),
			na.rm=na.rm, nobs=nobs, mean=means)
	} else {
		ans <- structure(val, class=stream_stat_class(stat),
			na.rm=na.rm, nobs=nobs)
	}
	nms <- rownames(x)
	if ( stat %in% "range" ) {
		names(ans) <- rep(nms, each=2)
	} else {
		names(ans) <- nms
	}
	ans
}

colstreamStats <- function(x, stat, na.rm = FALSE, ...) {
	fun <- stream_stat_fun(stat)
	template <- switch(stat, range=numeric(2),
		any=logical(1), all=logical(1), numeric(1))
	val <- apply_int(x, 2, fun, template, na.rm=na.rm)
	nobs <- apply_int(x, 2, na_length, numeric(1), na.rm=na.rm)
	if ( stat %in% c("var", "sd") ) {
		means <- colMeans(x, na.rm=na.rm)
		ans <- structure(val, class=stream_stat_class(stat),
			na.rm=na.rm, nobs=nobs, mean=means)
	} else {
		ans <- structure(val, class=stream_stat_class(stat),
			na.rm=na.rm, nobs=nobs)
	}
	nms <- colnames(x)
	if ( stat %in% "range" ) {
		names(ans) <- rep(nms, each=2)
	} else {
		names(ans) <- nms
	}
	ans
}

transformChunk <- function(x, attr, group, group_idx, along, tform) {
	dms <- dim(x)
	nms <- c("col.center", "col.scale", "row.center", "row.scale")
	if ( any(nms %in% names(attr)) && prod(dim(x)) > 0L ) {
		if ( !is.null(attr$col.center) ) {
			if ( is.null(group) ) {
				col.center <- attr$col.center
			} else {
				col.center <- attr$col.center[,group]
			}
			if ( attr$iter.dim == "cols" ) {
				col.center <- col.center[attr$idx]
			} else if ( !is.null(group_idx) ) {
				col.center <- col.center[group_idx]
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
			if ( attr$iter.dim == "cols" ) {
				col.scale <- col.scale[attr$idx]
			} else if ( !is.null(group_idx) ) {
				col.scale <- col.scale[group_idx]
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
			if ( attr$iter.dim == "rows" ) {
				row.center <- row.center[attr$idx]
			} else if ( !is.null(group_idx) ) {
				row.center <- row.center[group_idx]
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
			if ( attr$iter.dim == "rows" ) {
				row.scale <- row.scale[attr$idx]
			} else if ( !is.null(group_idx) ) {
				row.scale <- row.scale[group_idx]
			}
		} else {
			row.scale <- rep_len(1, nrow(x))
		}
		x <- (x - row.center) / row.scale
	}
	tform(x)
}

