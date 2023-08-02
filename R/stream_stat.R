
# streaming statistical summaries

s_range <- function(x, ..., na.rm = FALSE) {
	if ( ...length() > 0L ) {
		x <- s_range(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(range(x, na.rm=na.rm),
			class=c("stream_range", "stream_stat"),
			na.rm=na.rm,
			nobs=rep.int(na_length(x, na.rm), 2L))
	} else {
		x
	}
}

s_min <- function(x, ..., na.rm = FALSE) {
	if ( ...length() > 0L ) {
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
	if ( ...length() > 0L ) {
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
	if ( ...length() > 0L ) {
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
	if ( ...length() > 0L ) {
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
	if ( ...length() > 0L ) {
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
	if ( ...length() > 0L ) {
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
	if ( ...length() > 0L ) {
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
	if ( ...length() > 0L ) {
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
	if ( ...length() > 0L ) {
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

s_nnzero <- function(x, ..., na.rm = FALSE) {
	if ( ...length() > 0L ) {
		x <- s_nnzero(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(nnzero_na_rm(x, na.rm=na.rm),
			class=c("stream_nnzero", "stream_stat"),
			na.rm=na.rm,
			nobs=na_length(x, na.rm))
	} else {
		x
	}
}

# nnzero

nnzero_na_rm <- function(x, na.rm = FALSE) {
	if ( na.rm ) {
		sum(x != 0 & !is.na(x))
	} else {
		sum(x != 0)
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
setOldClass(c("stream_nnzero", "stream_stat"))

# streaming statistics methods

is.stream_stat <- function(x) is(x, "stream_stat")

print.stream_stat <- function(x, n = getOption("matter.show.head.n"), ...) {
	dn <- if(is.null(dim(x))) length(x) else dim(x)
	dn <- paste0("[", paste0(dn, collapse=" x "), "]")
	cat(class(x)[1L], dn, "with n =", paste_head(nobs(x)), "\n")
	rank <- length(dim(x))
	if ( rank <= 1L ) {
		preview_vector(x, n=n)
	} else if ( rank == 2L ) {
		preview_matrix(x, n=n)
	} else {
		preview_Nd_array(x, n=n)
	}
	cat("na.rm =", na_rm(x), "\n")
}

nobs.stream_stat <- function(object, ...) {
	attr(object, "nobs")
}

setMethod("nobs", "stream_stat", nobs.stream_stat)

na_rm <- function(object, ...) UseMethod("na_rm")

na_rm.default <- function(object, ...) attr(object, "na.rm")

stat_c <- function(x, y, ...) {
	if ( missing(y) )
		return(x)
	if ( ...length() > 0L ) {
		stat_c(x, do.call(stat_c, list(y, ...)))
	} else {
		UseMethod("stat_c")
	}
}

setMethod("combine", "stream_stat",
	function(x, y, ...) {
		if ( !isa(y, class(x)) )
			return(c(drop_attr(x), drop_attr(y)))
		structure(c(drop_attr(x), drop_attr(y)),
			class=class(x),
			na.rm=all(na_rm(x) & na_rm(y)),
			nobs=c(nobs(x), nobs(y)),
			mean=c(attr(x, "mean"), attr(y, "mean")))
	})

c.stream_stat <- function(x, ...) {
	if ( ...length() > 0L ) {
		combine(x, ...)
	} else {
		x
	}
}

cbind.stream_stat <- function(..., deparse.level = 1) {
	x <- ...elt(1L)
	more <- list(...)[-1L]
	if ( length(more) > 1L ) {
		y <- do.call(cbind, more)
	} else {
		y <- more[[1L]]
	}
	if ( !isa(y, class(x)) )
		return(cbind(drop_attr(x), drop_attr(y)))
	nx <- nobs(x)
	ny <- nobs(y)
	dim(nx) <- dim(x)
	dim(ny) <- dim(y)
	ux <- attr(x, "mean")
	uy <- attr(y, "mean")
	if ( !is.null(ux) )
		dim(ux) <- dim(x)
	if ( !is.null(uy) )
		dim(uy) <- dim(y)
	structure(cbind(drop_attr(x), drop_attr(y)),
		class=class(x),
		na.rm=all(na_rm(x) & na_rm(y)),
		nobs=as.vector(cbind(nx, ny)),
		mean=as.vector(cbind(ux, uy)))
}

rbind.stream_stat <- function(..., deparse.level = 1) {
	x <- ...elt(1L)
	more <- list(...)[-1L]
	if ( length(more) > 1L ) {
		y <- do.call(rbind, more)
	} else {
		y <- more[[1L]]
	}
	if ( !isa(y, class(x)) )
		return(rbind(drop_attr(x), drop_attr(y)))
	nx <- nobs(x)
	ny <- nobs(y)
	dim(nx) <- dim(x)
	dim(ny) <- dim(y)
	ux <- attr(x, "mean")
	uy <- attr(y, "mean")
	if ( !is.null(ux) )
		dim(ux) <- dim(x)
	if ( !is.null(uy) )
		dim(uy) <- dim(y)
	structure(rbind(drop_attr(x), drop_attr(y)),
		class=class(x),
		na.rm=all(na_rm(x) & na_rm(y)),
		nobs=as.vector(rbind(nx, ny)),
		mean=as.vector(rbind(ux, uy)))
}

`[.stream_stat` <- function(x, i, j, ..., drop = TRUE) {
	narg <- nargs() - 1L - !missing(drop)
	if ( (narg == 1L && !missing(i)) || is.null(dim(x)) ) {
		i <- as_subscripts(i, x)
		if (is.null(i)) i <- seq_along(x)
		structure(drop_attr(x)[i],
			class=class(x),
			na.rm=na_rm(x),
			nobs=nobs(x)[i],
			mean=attr(x, "mean")[i])
	} else {
		i <- as_row_subscripts(i, x)
		j <- as_col_subscripts(j, x)
		if (is.null(i)) i <- seq_len(nrow(x))
		if (is.null(j)) j <- seq_len(ncol(x))
		n <- nobs(x)
		dim(n) <- dim(x)
		u <- attr(x, "mean")
		if ( !is.null(u) )
			dim(u) <- dim(x)
		structure(drop_attr(x)[i, j, ..., drop=drop],
			class=class(x),
			na.rm=na_rm(x),
			nobs=n[i, j, ..., drop=drop],
			mean=u[i, j, ..., drop=drop])
	}
}

`[[.stream_stat` <- function(x, i, exact = TRUE) {
	i <- as_subscripts(i, x)
	if (is.null(i)) i <- seq_along(x)
	structure(drop_attr(x)[[i]],
			class=class(x),
			na.rm=na_rm(x),
			nobs=nobs(x)[[i]],
			mean=attr(x, "mean")[[i]])
}

as.data.frame.stream_stat <- function(x,
	row.names = NULL, optional = FALSE, ...)
{
	if ( is.null(row.names) ) {
		if ( is.null(names(x)) ) {
			row.names <- as.character(seq_along(x))
		} else {
			row.names <- names(x)
		}
	}
	ans <- list(x)
	if ( !optional )
		names(ans) <- strsplit(class(x), "_")[[1L]][2L]
	structure(ans, class="data.frame", row.names=row.names)
}

setMethod("as.data.frame", "stream_stat", as.data.frame.stream_stat)

# create new stream_stat w/ inherited attributes

stream_stat_attr <- function(value, x, y) {
	if ( na_rm(x) != na_rm(y) )
		warning("combining statistics with differing na.rm")
	structure(value,
		class=class(x),
		names=names(x),
		dim=dim(x), dimnames=dimnames(x),
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
	if ( !isa(y, class(x)) )
		y <- s_range(y, na.rm=na_rm(x))
	xx <- drop_attr(x)
	yy <- drop_attr(y)
	xmin <- xx[1L:(length(x)/2L)]
	xmax <- xx[(1L + length(x)/2L):length(x)]
	ymin <- yy[1L:(length(x)/2L)]
	ymax <- yy[(1L + length(x)/2L):length(x)]
	val1 <- pmin(xmin, ymin, na.rm=na_rm(x) && na_rm(y))
	val2 <- pmax(xmax, ymax, na.rm=na_rm(x) && na_rm(y))
	val <- c(val1, val2)
	stream_stat_attr(val, x, y)
}

stat_c.stream_min <- function(x, y, ...) {
	if ( !isa(y, class(x)) )
		y <- s_min(y, na.rm=na_rm(x))
	val <- pmin(x, y, na.rm=na_rm(x) && na_rm(y))
	stream_stat_attr(val, x, y)
}

stat_c.stream_max <- function(x, y, ...) {
	if ( !isa(y, class(x)) )
		y <- s_max(y, na.rm=na_rm(x))
	val <- pmax(x, y, na.rm=na_rm(x) && na_rm(y))
	stream_stat_attr(val, x, y)
}

stat_c.stream_prod <- function(x, y, ...) {
	if ( !isa(y, class(x)) )
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
	if ( !isa(y, class(x)) )
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
	if ( !isa(y, class(x)) )
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
	nxy <- nx + ny
	nxy <- ifelse(nxy == 0, NA_real_, nxy)
	val <- (nx * xx + ny * yy) / nxy
	stream_stat_attr(val, x, y)
}

stat_c.stream_var <- function(x, y, ...) {
	if ( !isa(y, class(x)) )
		y <- s_var(y, na.rm=na_rm(x))
	nx <- nobs(x)
	ny <- nobs(y)
	if ( all(nx == 0) )
		return(y)
	if ( all(ny == 0) )
		return(x)
	ux <- attr(x, "mean")
	uy <- attr(y, "mean")
	m <- (nx * ux + ny * uy) / (nx + ny)
	if ( na_rm(x) && na_rm(y) ) {
		ux <- ifelse(is.na(ux), 0, ux)
		uy <- ifelse(is.na(uy), 0, uy)
	} else {
		ux <- ifelse(nx == 0, 0, ux)
		uy <- ifelse(ny == 0, 0, uy)
	}
	m <- ifelse(is.na(m), 0, m)
	nle1 <- nx <= 1 | ny <= 1
	val_1 <- rep(NA_real_, length(x))
	# handle single observations using Welford (1962)
	if ( any(nle1) ) {
		if ( any(n11 <- nx == 1 & ny == 1) ) {
			v1 <- ((ux - uy) * (ux - m)) / (nx + ny - 1)
			val_1[n11] <- v1[n11]
		}
		if ( any(nx > 1) ) {
			ss1 <- (nx - 1) * x
			ss1 <- ifelse(is.na(ss1), 0, ss1)
			ss2 <- ss1 + (uy - ux) * (uy - m)
			vx <- ss2 / (nx + ny - 1)
			val_1[nx > 1] <- vx[nx > 1]
		}
		if ( any(ny > 1) ) {
			ss1 <- (ny - 1) * y
			ss1 <- ifelse(is.na(ss1), 0, ss1)
			ss2 <- ss1 + (ux - uy) * (ux - m)
			vy <- ss2 / (nx + ny - 1)
			val_1[ny > 1] <- vy[ny > 1]
		}
	}
	val_N <- rep(NA_real_, length(x))
	# combine variances for the n > 1 samples
	if ( any(nx > 1 & ny > 1) ) {
		num1 <- ((nx - 1) * x) + ((ny - 1) * y)
		num2 <- (nx * ny / (nx + ny)) * (ux - uy)^2
		val_N <- (num1 + num2) / (nx + ny - 1)
	}
	val <- ifelse(nle1, val_1, val_N)
	ret <- stream_stat_attr(val, x, y)
	attr(ret, "mean") <- m
	ret
}

stat_c.stream_sd <- function(x, y, ...) {
	if ( !isa(y, class(x)) )
		y <- s_sd(y, na.rm=na_rm(x))
	nx <- nobs(x)
	ny <- nobs(y)
	if ( all(nx == 0) )
		return(y)
	if ( all(ny == 0) )
		return(x)
	ux <- attr(x, "mean")
	uy <- attr(y, "mean")
	m <- (nx * ux + ny * uy) / (nx + ny)
	if ( na_rm(x) && na_rm(y) ) {
		ux <- ifelse(is.na(ux), 0, ux)
		uy <- ifelse(is.na(uy), 0, uy)
	} else {
		ux <- ifelse(nx == 0, 0, ux)
		uy <- ifelse(ny == 0, 0, uy)
	}
	m <- ifelse(is.na(m), 0, m)
	nle1 <- nx <= 1 | ny <= 1
	val_1 <- rep(NA_real_, length(m))
	# handle single observations using Welford (1962)
	if ( any(nle1) ) {
		if ( any(n11 <- nx == 1 & ny == 1) ) {
			v1 <- sqrt(((ux - uy) * (ux - m)) / (nx + ny - 1))
			val_1[n11] <- v1[n11]
		}
		if ( any(nx > 1) ) {
			ss1 <- (nx - 1) * x^2
			ss1 <- ifelse(is.na(ss1), 0, ss1)
			ss2 <- ss1 + (uy - ux) * (uy - m)
			vx <- sqrt(ss2 / (nx + ny - 1))
			val_1[nx > 1] <- vx[nx > 1]
		}
		if ( any(ny > 1) ) {
			ss1 <- (ny - 1) * y^2
			ss1 <- ifelse(is.na(ss1), 0, ss1)
			ss2 <- ss1 + (ux - uy) * (ux - m)
			vy <- sqrt(ss2 / (nx + ny - 1))
			val_1[ny > 1] <- vy[ny > 1]
		}
	}
	val_N <- rep(NA_real_, length(m))
	# combine standard deviations for the n > 1 samples
	if ( any(nx > 1 & ny > 1) ) {
		num1 <- ((nx - 1) * x^2) + ((ny - 1) * y^2)
		num2 <- (nx * ny / (nx + ny)) * (ux - uy)^2
		val_N <- sqrt((num1 + num2) / (nx + ny - 1))
	}
	val <- ifelse(nle1, val_1, val_N)
	ret <- stream_stat_attr(val, x, y)
	attr(ret, "mean") <- m
	ret
}

stat_c.stream_any <- function(x, y, ...) {
	if ( !isa(y, class(x)) )
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
	if ( !isa(y, class(x)) )
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

stat_c.stream_nnzero <- function(x, y, ...) {
	if ( !isa(y, class(x)) )
		y <- s_nnzero(y, na.rm=na_rm(x))
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
		all=base::all,
		nnzero=nnzero_na_rm)
	if ( !is.character(name) )
		stop("stat must be a string")
	if ( !name %in% names(f) )
		stop("stat = ", sQuote(name), " not supported")
	f[[name, exact=TRUE]]
}

stream_stat_class <- function(name) {
	cls <- list(
		range="stream_range",
		min="stream_min",
		max="stream_max",
		prod="stream_prod",
		sum="stream_sum",
		mean="stream_mean",
		var="stream_var",
		sd="stream_sd",
		any="stream_any",
		all="stream_all",
		nnzero="stream_nnzero")
	c(cls[[name, exact=TRUE]], "stream_stat")
}

s_rowstats <- function(x, stat, group = NULL, na.rm = FALSE, ...) {
	if ( !is.character(stat) )
		stop("stat must be a string")
	if ( is.null(group) ) {
		ans <- s_rowstats_int(x, stat, na.rm)
		if ( stat %in% "range" ) {
			names(ans) <- c(rownames(x), rownames(x))
		} else {
			names(ans) <- rownames(x)
		}
	} else {
		if ( stat %in% "range" )
			stop("'range' stat not allowed with non-NULL group")
		if ( length(group) %% ncol(x) != 0 )
			stop("length of groups [", length(group), "] ",
				"is not a multiple of column extent [", ncol(x), "]")
		group <- as.factor(rep_len(group, ncol(x)))
		ans <- lapply(levels(group), function(g) {
				xi <- x[,which(group == g),drop=FALSE]
				s_rowstats_int(xi, stat, na.rm)
			})
		ans <- do.call(cbind, ans)
		colnames(ans) <- levels(group)
		rownames(ans) <- names(x)
	}
	ans
}

s_colstats <- function(x, stat, group = NULL, na.rm = FALSE, ...) {
	if ( !is.character(stat) )
		stop("stat must be a string")
	if ( is.null(group) ) {
		ans <- s_colstats_int(x, stat, na.rm)
		if ( stat %in% "range" ) {
			names(ans) <- c(colnames(x), colnames(x))
		} else {
			names(ans) <- colnames(x)
		}
	} else {
		if ( stat %in% "range" )
			stop("'range' stat not allowed with non-NULL group")
		if ( length(group) %% nrow(x) != 0 )
			stop("length of groups [", length(group), "] ",
				"is not a multiple of row extent [", nrow(x), "]")
		group <- as.factor(rep_len(group, nrow(x)))
		ans <- lapply(levels(group), function(g) {
				xi <- x[which(group == g),,drop=FALSE]
				s_colstats_int(xi, stat, na.rm)
			})
		ans <- do.call(cbind, ans)
		colnames(ans) <- levels(group)
		rownames(ans) <- names(x)
	}
	ans
}

s_rowstats_int <- function(x, stat, na.rm) {
	fun <- stream_stat_fun(stat)
	template <- switch(stat, range=numeric(2L),
		any=, all=logical(1L), numeric(1L))
	val <- apply_int(x, 1L, fun, template, na.rm=na.rm)
	nobs <- apply_int(x, 1L, na_length, numeric(1L), na.rm=na.rm)
	if ( stat %in% "range" ) {
		nobs <- rep.int(nobs, 2L)
		val <- matrix(val, ncol=2L, byrow=TRUE)
	}
	if ( stat %in% c("var", "sd") ) {
		means <- rowMeans(x, na.rm=na.rm)
		structure(val, class=stream_stat_class(stat),
			na.rm=na.rm, nobs=nobs, mean=means)
	} else {
		structure(val, class=stream_stat_class(stat),
			na.rm=na.rm, nobs=nobs)
	}
}

s_colstats_int <- function(x, stat, na.rm) {
	fun <- stream_stat_fun(stat)
	template <- switch(stat, range=numeric(2L),
		any=, all=logical(1L), numeric(1L))
	val <- apply_int(x, 2L, fun, template, na.rm=na.rm)
	nobs <- apply_int(x, 2L, na_length, numeric(1L), na.rm=na.rm)
	if ( stat %in% "range" ) {
		nobs <- rep.int(nobs, 2L)
		val <- matrix(val, ncol=2L, byrow=TRUE)
	}
	if ( stat %in% c("sd", "var") ) {
		means <- colMeans(x, na.rm=na.rm)
		structure(val, class=stream_stat_class(stat),
			na.rm=na.rm, nobs=nobs, mean=means)
	} else {
		structure(val, class=stream_stat_class(stat),
			na.rm=na.rm, nobs=nobs)
	}
}
