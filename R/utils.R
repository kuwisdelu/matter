
#### Set up matter options ####
## ----------------------------

.onLoad <- function(libname, pkgname) {
	options(
		matter.cast.warning = TRUE,
		matter.compress.atoms = 3,
		matter.default.nchunks = 20L,
		matter.default.chunksize = 1000000L,
		matter.show.head = TRUE,
		matter.show.head.n = 6L,
		matter.coerce.altrep = FALSE,
		matter.coerce.altrep.list = FALSE,
		matter.wrap.altrep = FALSE,
		matter.dump.dir = tempdir(),
		matter.vizi.par=par_style_light(),
		matter.vizi.panelgrid=NULL,
		matter.vizi.style="light")
}

#### Normalize subscripts ####
## ----------------------------

as_subscripts <- function(i, x, exact = TRUE) {
	if ( missing(i) )
		return(NULL)
	if ( is.logical(i) )
		i <- which(rep_len(i, length(x)))
	if ( !is.numeric(i) )
		if ( exact ) {
			i <- match(i, names(x))
		} else {
			i <- pmatch(i, names(x))
		}
	i
}

as_array_subscripts <- function(i, x, margin, exact = TRUE) {
	if ( missing(i) )
		return(NULL)
	if ( is.list(i) || missing(margin) ) {
		i <- rep_len(i, length(dim(x)))
		return(lapply(seq_along(i), function(j)
			as_array_subscripts(i[[j]], x, j, exact)))
	}
	if ( is.null(i) ) {
		i <- seq_len(dim(x)[margin])
	} else if ( is.logical(i) ) {
		i <- which(rep_len(i, dim(x)[[margin]]))
	} else if ( !is.numeric(i) ) {
		if ( exact ) {
			i <- match(i, dimnames(x)[[margin]])
		} else {
			i <- pmatch(i, dimnames(x)[[margin]])
		}
	}
	i
}

as_row_subscripts <- function(i, x, exact = TRUE) {
	as_array_subscripts(i, x, 1L, exact)
}

as_col_subscripts <- function(i, x, exact = TRUE) {
	as_array_subscripts(i, x, 2L, exact)
}

# export
linear_ind <- function(index, dim, rowMaj = FALSE) {
	if ( is.null(index) )
		return(seq_len(prod(dim)))
	if ( is.list(index) ) {
		for ( j in seq_along(dim) ) {
			if ( is.null(index[[j]]) )
				index[[j]] <- seq_len(dim[j])
		}
		ans.dim <- lengths(index)
		if ( rowMaj ) {
			index <- as.matrix(rev(expand.grid(rev(index))))
		} else {
			index <- as.matrix(expand.grid(index))
		}
	} else {
		ans.dim <- NULL
	}
	index <- as.matrix(index)
	for ( j in seq_along(dim) ) {
		i <- index[,j]
		if ( any(!is.na(i) & (i <= 0 | i > dim[j])) )
			stop("subscript out of bounds")
	}
	if ( rowMaj ) {
		strides <- c(rev(cumprod(rev(dim[-1L]))), 1L)
	} else {
		strides <- c(1L, cumprod(dim[-length(dim)]))
	}
	index <- ((index - 1L) %*% strides) + 1L
	if ( !is.null(ans.dim) ) {
		if ( rowMaj ) {
			ord <- aperm(array(seq_along(index), rev(ans.dim)))
		} else {
			ord <- array(seq_along(index), ans.dim)
		}
		storage.mode(ord) <- typeof(index)
		attr(index, "order") <- ord
	}
	index
}

# export
array_ind <- function(i, dim, rowMaj = FALSE) {
	i <- i - 1L
	if ( rowMaj ) {
		strides <- c(rev(cumprod(rev(dim[-1L]))), 1L)
	} else {
		strides <- c(1L, cumprod(dim[-length(dim)]))
	}
	index <- matrix(nrow=length(i), ncol=length(dim))
	for ( j in seq_along(dim) ) {
		nextind <- i %/% strides[j]
		index[,j] <- nextind %% dim[j]
	}
	index + 1L
}

#### Data type codes and type conversions ####
## ---------------------------------------------

make_code <- function(codes, x, nomatch = NA_integer_) {
	if ( missing(x) )
		x <- character()
	if ( is.factor(x) && setequal(levels(x), codes) )
		return(x)
	if ( !is.numeric(x) )
		x <- pmatch(tolower(x), codes, nomatch, TRUE)
	factor(x, levels=seq_along(codes), labels=codes)
}

# export
get_Rtypes <- function() {
	codes <- c("raw", "logical", "integer",
		"double", "character", "list")
}

# export
get_Ctypes <- function() {
	codes <- c("char", "uchar", "short", "ushort",
		"int", "uint", "long", "ulong",
		"float", "double")
}

# export
as_Rtype <- function(x) {
	if ( !missing(x) ) {
		if ( is_Rtype(x, strict=TRUE) )
			return(x)
		if ( is_Ctype(x) && !is_Rtype(x) )
			return(to_Rtype(as_Ctype(x)))
	}
	if ( !missing(x) && is.character(x) ) {
		# allow 'numeric' as synonym for 'double'
		i <- pmatch(x, "numeric", 0L, TRUE)
		i <- which(as.logical(i))
		if ( length(i) > 0 )
			x[i] <- "double"
	}
	make_code(get_Rtypes(), x)
}

# export
as_Ctype <- function(x) {
	if ( !missing(x) ) {
		if ( is_Ctype(x, strict=TRUE) )
			return(x)
		if ( is_Rtype(x) && !is_Ctype(x) )
			return(to_Ctype(as_Rtype(x)))
	}
	
	make_code(get_Ctypes(), x)
}

is_Rtype <- function(x, strict = FALSE) {
	codes <- get_Rtypes()
	valid <- is.factor(x) && setequal(levels(x), codes)
	valid || (all(x %in% codes) && !strict)
}

is_Ctype <- function(x, strict = FALSE) {
	codes <- get_Ctypes()
	valid <- is.factor(x) && setequal(levels(x), codes)
	valid || (all(x %in% codes) && !strict)
}

to_Rtype <- function(x) {
	codes <- c(char = "character", uchar = "raw",
		short = "integer", ushort = "integer",
		int = "integer", uint = "integer",
		long = "double", ulong = "double",
		float = "double", double = "double")
	as_Rtype(codes[as.integer(as_Ctype(x))])
}

to_Ctype <- function(x) {
	codes <- c(raw = "uchar", logical = "int",
		integer = "int", numeric = "double",
		character = "char", list = NA_character_)
	as_Ctype(codes[as.integer(as_Rtype(x))])
}

# export
sizeof <- function(x) {
	sizes <- c(char = 1L, uchar = 1L, short = 2L, ushort = 2L,
		int = 4L, uint = 4L, long = 8L, ulong = 8L,
		float = 4L, double = 8L)
	sizes[as.integer(as_Ctype(x))]
}

topmode_Rtype <- function(x) {
	x <- as_Rtype(x)
	codes <- levels(x)
	as_Rtype(codes[max(as.integer(x))])
}

#### Codes for C-level switch statements ####
## ------------------------------------------

as_Ops <- function(x) {
	codes <- c(
		# Arith (1-7)
		"+", "-", "*", "^", "%%", "%/%", "/",
		# Compare (8-13)
		"==", ">", "<", "!=", "<=", ">=",
		# Logic (14-16)
		"&", "|", "!",
		# Math (17+)
		"log", "log10", "log2", "log1p", "exp")
	make_code(codes, x)
}

as_Summary <- function(x) {
	codes <- c(
		# Summary (1-7)
		"max", "min", "range", "prod", "sum", "any", "all",
		# Statistics (8-11)
		"mean", "var", "sd", "nnzero")
	make_code(codes, x)
}

# export
as_tol <- function(x) {
	tol <- x[1L]
	codes <- c("absolute", "relative")
	if ( !is.null(names(tol)) ) {
		tol_type <- pmatch(names(tol), codes, nomatch=1L)
	} else {
		tol_type <- 1L
	}
	tol_type <- factor(tol_type, levels=seq_len(2L), labels=codes)
	structure(as.double(tol), tol_type=tol_type)
}

as_tol_ref <- function(x) {
	codes <- c("abs", "x", "y")
	make_code(codes, x[1L], nomatch=1L)
}

# export
as_interp <- function(x) {
	codes <- c(
		# simple interp (1-5)
		"none", "mean", "sum", "max", "min",
		# peak-based (6)
		"area",
		# spline interp (7-8)
		"linear", "cubic",
		# kernel interp (9-10)
		"gaussian", "lanczos")
	make_code(codes, x)
}

as_binstat <- function(x) {
	codes <- c(
		# location (1-4)
		"sum", "mean", "max", "min",
		# spread (5-7)
		"sd", "var", "sse")
	make_code(codes, x[1L], nomatch=1L)
}

#### Miscellaneous utility functions ####
## --------------------------------------

is_nil <- function(x) is.na(x) || is.null(x)

is_discrete <- function(x) {
	is.factor(x) || is.character(x) || is.logical(x)
}

set_attr <- function(x, attr) {
	for ( nm in names(attr) )
		attr(x, nm) <- attr[[nm]]
	x
}

drop_attr <- function(x, keep.names = TRUE) {
	y <- as.vector(x)
	dim(y) <- dim(x)
	if ( keep.names ) {
		dimnames(y) <- dimnames(x)
		names(y) <- names(x)
	}
	y
}

set_names <- function(x, nm, i) {
	if ( !missing(i) && !is.null(i) )
		nm <- nm[i]
	names(x) <- nm
	x
}

combine_names <- function(x1, x2) {
	nm1 <- names(x1)
	nm2 <- names(x2)
	if ( is.null(nm1) && is.null(nm2) )
		return(names(x1))
	nm1 <- if (is.null(nm1)) character(length(x1)) else nm1
	nm2 <- if (is.null(nm2)) character(length(x2)) else nm2
	c(nm1, nm2)
}

combine_any <- function(x, ...)
{
	if ( ...length() > 0 ) {
		do.call(combine, list(x, ...))
	} else {
		x
	}
}

set_dimnames <- function(x, dnm, index) {
	if ( !missing(index) && !is.null(index) )
		dnm <- subset_dimnames(dnm, index)
	dimnames(x) <- dnm
	x
}

subset_dimnames <- function(dnm, index) {
	for ( i in seq_along(index) ) {
		j <- index[[i]]
		if ( !is.null(dnm[[i]]) && !is.null(j) )
			dnm[[i]] <- dnm[[i]][j]
	}
	dnm
}

cbind_dimnames <- function(x1, x2) {
	c1 <- colnames(x1)
	c2 <- colnames(x2)
	if ( is.null(c1) && is.null(c2) )
		return(dimnames(x1))
	c1 <- if (is.null(c1)) character(ncol(x1)) else c1
	c2 <- if (is.null(c2)) character(ncol(x2)) else c2
	list(rownames(x1), c(c1, c2))
}

rbind_dimnames <- function(x1, x2) {
	r1 <- rownames(x1)
	r2 <- rownames(x2)
	if ( is.null(r1) && is.null(r2) )
		return(dimnames(x1))
	r1 <- if (is.null(r1)) character(nrow(x1)) else r1
	r2 <- if (is.null(r2)) character(nrow(x2)) else r2
	list(c(r1, r2), colnames(x1))
}

cbind_any <- function(..., deparse.level = 1)
{
	if ( ...length() == 1L )
		return(...elt(1))
	x <- ...elt(1)
	y <- ...elt(2)
	if ( ...length() > 2L )	{
		more <- list(...)[-c(1L, 2L)]
		cbind2(x, do.call(cbind2, c(list(y), more)))
	} else {
		cbind2(x, y)
	}
}

rbind_any <- function(..., deparse.level = 1)
{
	if ( ...length() == 1L )
		return(...elt(1))
	x <- ...elt(1)
	y <- ...elt(2)
	if ( ...length() > 2L )	{
		more <- list(...)[-c(1L, 2L)]
		rbind2(x, do.call(rbind2, c(list(y), more)))
	} else {
		rbind2(x, y)
	}
}

check_comformable_lengths <- function(x, y) {
	if ( is.vector(x) ) {
		return(check_comformable_lengths(y, x))
	} else if ( length(y) != 1 && length(y) != length(x) ) {
		return("argument length is non-conformable with array length")
	}
	TRUE
}

check_comformable_dims <- function(x, y, margin = 1L) {
	if ( is.vector(x) ) {
		return(check_comformable_dims(y, x))
	} else if ( length(y) != 1 && length(y) != dim(x)[margin] ) {
		return("argument length is non-conformable with array dimensions")
	}
	TRUE
}

normalize_lengths <- function(list)
{
	n <- lengths(list)
	if ( length(unique(n)) != 1L )
		list <- lapply(list, rep_len, length.out=max(n))
	list
}

#### Show utility functions ####
## -----------------------------

show_matter_mem <- function(x) {
	rmem <- size_bytes(object.size(x))
	rmem <- format(rmem, units="auto")
	if ( is.matter(x) ) {
		vmem <- format(vm_used(x), units="auto")
		cat("(", rmem, " real", " | ", vmem, " virtual)\n", sep="")
	} else {
		cat("(", rmem, " real)\n", sep="")
	}
}

paste_head <- function(x, n=getOption("matter.show.head.n"), collapse=" ") {
	if ( length(x) > n ) {
		paste0(paste0(head(x, n=n), collapse=collapse), " ...")
	} else {
		paste0(x, collapse=collapse)
	}
}

preview_vector_data <- function(x, n = getOption("matter.show.head.n")) {
	hdr <- head(x, n=n)
	out <- format.default(hdr)
	more <- length(x) > length(hdr)
	if ( !is.null(names(hdr)) ) {
		nms <- names(hdr)
	} else {
		nms <- paste0("[", seq_along(hdr), "]")
	}
	if ( more ) {
		out <- c(out, "...")
		nms <- c(nms, "...")
	}
	matrix(out, nrow=1, dimnames=list("", nms))
}

preview_vector <- function(x, n = getOption("matter.show.head.n")) {
	print(preview_vector_data(x, n), quote=FALSE, right=TRUE)
}

preview_matrix_data <- function(x, n = getOption("matter.show.head.n")) {
	more_i <- nrow(x) > n
	more_j <- ncol(x) > n
	if ( more_i ) {
		i <- 1:n
	} else {
		i <- 1:nrow(x)
	}
	if ( more_j ) {
		j <- 1:n
	} else {
		j <- 1:ncol(x)
	}
	hdr <- x[i,j,drop=FALSE]
	out <- matrix(format.default(hdr), nrow=nrow(hdr), ncol=ncol(hdr))
	if ( !is.null(rownames(x)) ) {
		rnm <- rownames(x)[i]
	} else {
		rnm <- paste0("[", seq_along(i), ",]")
	}
	if ( !is.null(colnames(x)) ) {
		cnm <- colnames(x)[j]
	} else {
		cnm <- paste0("[,", seq_along(j), "]")
	}
	if ( more_i ) {
		out <- rbind(out, "...")
		rnm <- c(rnm, "...")
	}
	if ( more_j ) {
		out <- cbind(out, "...")
		cnm <- c(cnm, "...")
	}
	dimnames(out) <- list(rnm, cnm)
	out
}

preview_matrix <- function(x, n = getOption("matter.show.head.n")) {
	print(preview_matrix_data(x, n), quote=FALSE, right=TRUE)
}

preview_Nd_array <- function(x, n = getOption("matter.show.head.n")) {
	more_i <- nrow(x) > n
	more_j <- ncol(x) > n
	if ( more_i ) {
		i <- 1:n
	} else {
		i <- 1:nrow(x)
	}
	if ( more_j ) {
		j <- 1:n
	} else {
		j <- 1:ncol(x)
	}
	extra <- rep(1L, length(dim(x)) - 2L)
	inds <- c(list(i, j), as.list(extra))
	hdr <- do.call("[", c(list(x), inds, list(drop=FALSE)))
	out <- matrix(format.default(hdr), nrow=nrow(hdr), ncol=ncol(hdr))
	if ( !is.null(rownames(x)) ) {
		rnm <- rownames(x)[i]
	} else {
		rnm <- paste0("[", seq_along(i), ",]")
	}
	if ( !is.null(colnames(x)) ) {
		cnm <- colnames(x)[j]
	} else {
		cnm <- paste0("[,", seq_along(j), "]")
	}
	if ( more_i ) {
		out <- rbind(out, "...")
		rnm <- c(rnm, "...")
	}
	if ( more_j ) {
		out <- cbind(out, "...")
		cnm <- c(cnm, "...")
	}
	dimnames(out) <- list(rnm, cnm)
	cat(paste0(c("", "", extra), collapse=", "), "\n")
	print(out, quote=FALSE, right=TRUE)
	if ( prod(dim(x)[-c(1,2)]) > 1L ) {
		dots <- ifelse(dim(x)[-c(1,2)] > 1L, "...", "")
		cat(paste0(c("", "", dots), collapse=", "), "\n")
	}
}

preview_list <- function(x, n = getOption("matter.show.head.n")) {
	n1 <- min(n, length(x))
	for ( i in 1:n1 ) {
		fmt <- preview_vector_data(x[[i]], n)
		if ( !is.null(names(x)) ) {
			rownames(fmt) <- paste0("$", names(x)[i])
		} else {
			rownames(fmt) <- paste0("[[", i, "]]")
		}
		print(fmt, quote=FALSE, right=TRUE)
	}
	if ( length(x) > n1 )
		cat("...\n")
}

preview_table <- function(x, n = getOption("matter.show.head.n"), classinfo = NULL) {
	more_i <- nrow(x) > n
	more_j <- ncol(x) > n
	if ( more_i ) {
		i <- 1:n
	} else {
		i <- 1:nrow(x)
	}
	if ( more_j ) {
		j <- 1:n
	} else {
		j <- 1:ncol(x)
	}
	hdr <- x[i,j,drop=FALSE]
	out <- as.matrix(hdr)
	if ( is.null(classinfo) )
		classinfo <- sapply(hdr, function(y) class(y)[1])
	classinfo <- classinfo[j]
	classinfo <- paste0("<", classinfo, ">")
	if ( !is.null(rownames(x)) ) {
		rnm <- rownames(x)[i]
	} else {
		rnm <- paste0("[", seq_along(i), ",]")
	}
	if ( !is.null(colnames(x)) ) {
		cnm <- colnames(x)[j]
	} else {
		cnm <- paste0("[,", seq_along(j), "]")
	}
	if ( more_i ) {
		out <- rbind(out, "...")
		rnm <- c(rnm, "...")
	}
	if ( more_j ) {
		out <- cbind(out, "...")
		cnm <- c(cnm, "...")
		classinfo <- c(classinfo, "")
	}
	out <- rbind(classinfo, out)
	rnm <- c("", rnm)
	dimnames(out) <- list(rnm, cnm)
	print(out, quote=FALSE, right=TRUE)
}

#### Miscellaneous internal functions ####
## --------------------------------------

apply_int <- function(X, MARGIN, FUN, FUN.VALUE, ...) {
	FUN <- match.fun(FUN)
	if ( !MARGIN %in% c(1L, 2L) )
		stop("MARGIN must be 1 or 2")
	switch(MARGIN,
		vapply(seq_len(nrow(X)), function(i)
			FUN(X[i,,drop=TRUE], ...), FUN.VALUE),
		vapply(seq_len(ncol(X)), function(j)
			FUN(X[,j,drop=TRUE], ...), FUN.VALUE))
}

bplapply_int <- function(X, FUN, ..., BPPARAM = NULL) {
	if ( !is.null(BPPARAM) ) {
		bplapply(X, FUN, ..., BPPARAM=BPPARAM)
	} else {
		lapply(X, FUN, ...)
	}
}

roll <- function(x, width = 3L, na.drop = FALSE, fill = NA) {
	r <- floor(width / 2)
	x <- lapply(seq_along(x),
		function(i) {
			j <- (i - r):(i + r)
			j[j < 1L | j > length(x)] <- NA
			ifelse(!is.na(j), x[j], fill)
		})
	if ( na.drop )
		x <- lapply(x, function(xi) xi[!is.na(xi)])
	x
}

rmatmul <- function(x, y, useOuter = FALSE) {
	ans <- matrix(0, nrow=nrow(x), ncol=ncol(y))
	if ( useOuter ) {
		for ( i in 1:ncol(x) )
			ans <- ans + outer(x[,i], y[i,])
	} else {
		for ( i in 1:nrow(x) )
			ans[i,] <- x[i,,drop=FALSE] %*% y
	}
	ans
}

lmatmul <- function(x, y, useOuter = FALSE) {
	ans <- matrix(0, nrow=nrow(x), ncol=ncol(y))
	if ( useOuter ) {
		for ( i in 1:nrow(y) )
			ans <- ans + outer(x[,i], y[i,])
	} else {
		for ( i in 1:ncol(y) )
			ans[,i] <- x %*% y[,i,drop=FALSE]
	}
	ans
}

lr <- function(x, y) {
	ux <- mean(x)
	uy <- mean(y)
	sxx <- var(x)
	sxy <- cov(x, y)
	b <- sxy / sxx
	a <- uy - b * ux
	list(coef=c(a, b), n=length(x),
		x=ux, y=uy, xx=sxx, xy=sxy)
}

lr_update <- function(fit, x, y) {
	n <- fit$n + length(x)
	ux <- (fit$n * fit$x + sum(x)) / n
	uy <- (fit$n * fit$y + sum(y)) / n
	qx <- sqrt(fit$n) * fit$x + sqrt(n) * ux
	qx <- qx / (sqrt(fit$n) + sqrt(n))
	qy <- sqrt(fit$n) * fit$y + sqrt(n) * uy
	qy <- qy / (sqrt(fit$n) + sqrt(n))
	sxx <- (fit$n - 1) * fit$xx + sum((x - qx) * (x - qx))
	sxx <- sxx / (n - 1)
	sxy <- (fit$n - 1) * fit$xy + sum((x - qx) * (y - qy))
	sxy <- sxy / (n - 1)
	b <- sxy / sxx
	a <- uy - b * ux
	list(coef=c(a, b), n=n,
		x=ux, y=uy, xx=sxx, xy=sxy)
}

lr_predict <- function(fit, xi) {
	sum(fit$coef * c(1, xi))
}

# A sequence with half-bin-widths in relative units
# x = bin center, y = half-width, d = relative diff
# y[n] = d * x[n]
# y[n+1] = d * (x[n] - y[n])) / (1 - d)
# x[n+1] = x[n] + y[n] + y[n+1]
# => x[n] ((1 + d) / (1 - d))^n * x[0]
# log x[n] = n log {(1 + d) / (1 - d)} + log x[0]
# => n = (log x[n] - log x[0]) / log {(1 + d) / (1 - d)}
seq_rel <- function(from, to, by) {
	half <- by / 2
	length.out <- (log(to) - log(from)) / log((1 + half) / (1 - half))
	length.out <- floor(1 + length.out)
	i <- seq_len(length.out)
	from * ((1 + half) / (1 - half))^(i - 1)
}

# Cleveland style shingles (i.e., overlapping intervals)
shingles <- function(x, breaks, overlap = 0.5, labels = NULL)
{
	if ( !is.matrix(breaks) )
		breaks <- co.intervals(x, number=breaks, overlap=overlap)
	y <- apply(breaks, 1L, function(b) I(which(b[1L] <= x & x <= b[2L])))
	binner <- function(i) which(breaks[i,1L] <= x & x <= breaks[i,2L])
	y <- lapply(seq_len(nrow(breaks)), binner)
	if ( is.null(labels) ) {
		labeller <- function(b) paste0("[", b[1L], ",", b[2L], "]")
		labels <- apply(breaks, 1L, labeller)
	} else if ( length(labels) != length(y) ) {
		stop("length of labels not equal to length of breaks")
	}
	attr(y, "breaks") <- breaks
	attr(y, "counts") <- lengths(y)
	attr(y, "mids") <- rowMeans(breaks)
	names(y) <- labels
	y
}

#### Utilities for raw bytes and memory ####
## -----------------------------------------

# convert between 'raw' and 'character'

raw2char <- function(x, multiple = FALSE, encoding = "unknown") {
	y <- rawToChar(as.raw(x), multiple=multiple)
	Encoding(y) <- encoding
	y
}

char2raw <- function(x) {
	charToRaw(as.character(x))
}

# convert between 'raw' and hexadecimal strings

hex2raw <- function(x) {
	x <- tolower(gsub("[^[:alnum:] ]", "", x))
	sst <- strsplit(x, "")[[1]]
	hex <- paste0(sst[c(TRUE, FALSE)], sst[c(FALSE, TRUE)])
	codes <- factor(hex, levels=as.character(as.raw(0:255)))
	as.raw(as.integer(codes) - 1L)
}

raw2hex <- function(x, uppercase = FALSE) {
	hex <- paste0(as.character(x), collapse="")
	if ( uppercase ) {
		toupper(hex)
	} else {
		hex
	}
}

# create a uuid

uuid <- function(uppercase = FALSE) {
	hex <- as.raw(0:255)
	version <- hex[65:80] # 0100 xxxx (version 4)
	variant <- hex[129:192] # 10xx xxxx (variant 1)
	time_low <- sample(hex, 4)
	time_hi <- sample(hex, 2)
	time_hi_and_version <- c(sample(version, 1), sample(hex, 1))
	clock_seq_hi_and_res <- sample(variant, 1)
	clock_seq_low <- sample(hex, 1)
	node <- sample(hex, 6)
	bytes <- c(time_low, time_hi, time_hi_and_version,
		clock_seq_hi_and_res, clock_seq_low, node)
	string <- c(
		paste0(as.character(time_low), collapse=""),
		paste0(as.character(time_hi), collapse=""),
		paste0(as.character(time_hi_and_version), collapse=""),
		paste0(as.character(clock_seq_hi_and_res),
			as.character(clock_seq_low), collapse=""),
		paste0(as.character(node), collapse=""))
	if ( uppercase ) {
		string <- toupper(paste0(string, collapse="-"))
	} else {
		string <- paste0(string, collapse="-")
	}
	list(string=string, bytes=bytes)
}

# creates internal S3 class 'size_bytes'
# (similar to 'object_size' but works w/ vectors)
size_bytes <- function(x) {
	class(x) <- "size_bytes"
	x
}

# based on utils:::format.object_size
format.size_bytes <- function(x, units = "auto", ...)
{
	units <- match.arg(units, c("auto",
		"B", "KB", "MB", "GB", "TB", "PB"))
	mx <- min(x, na.rm=TRUE)
	if ( units == "auto" )
		units <- if ( is.na(mx) )
			" "
		else if ( mx >= 1000^4 )
			"TB"
		else if ( mx >= 1000^3 )
			"GB"
		else if ( mx >= 1000^2 )
			"MB"
		else if ( mx >= 1000 )
			"KB"
		else "B"
	sizes <- switch(units,
		" " = , "B" = x,
		"KB" = round(x/1000, 2L),
		"MB" = round(x/1000^2, 2L),
		"GB" = round(x/1000^3, 2L),
		"TB" = round(x/1000^4, 2L),
		"PB" = round(x/1000^5, 2L))
	label <- switch(units, "B"="bytes", units)
	set_names(paste(sizes, label), names(x))
}

print.size_bytes <- function(x, units = "auto",
	quote = FALSE, right = TRUE, ...) 
{
	print.default(format(x, units=units),
		quote=quote, right=right, ...)
}

`[.size_bytes` <- function(x, i, j, ..., drop=FALSE)
{
	structure(NextMethod(), class="size_bytes")
}

# based on pryr::mem_used and pryr::mem_change
mem <- function(x, reset = FALSE)
{
	if ( !missing(x) ) {
		rmem <- as.numeric(object.size(x))
		vmem <- as.numeric(vm_used(x))
		mem <- c("real"=rmem, "virtual"=vmem)
	} else {
		cell.size <- c(Ncells=56, Vcells=8)
		gc.result <- gc(reset=reset)
		gc.cols <- c(1L, 3L, ncol(gc.result) - 1L)
		mem <- colSums(gc.result[,gc.cols] * cell.size)
		names(mem) <- c("used", "gc", "max")
	}
	size_bytes(mem)
}

profmem <- function(expr)
{
	start <- mem(reset = TRUE)
	t.start <- proc.time()
	expr <- substitute(expr)
	eval(expr, parent.frame())
	rm(expr)
	t.end <- proc.time()
	end <- mem(reset = FALSE)
	mem <- c(start[1], end[1], end[3])
	mem <- c(format(size_bytes(mem)),
		format(size_bytes(end[3] - end[1])),
		paste0(round(t.end[3] - t.start[3], 4L), " sec"))
	names(mem) <- c("start", "finish",
		"max", "overhead", "time")
	print.default(mem, quote=FALSE, right=TRUE)
}

#### Formula parsing ####
## ----------------------

parse_formula <- function(formula, envir = NULL, eval = TRUE)
{
	e <- environment(formula)
	if ( length(formula) == 2L ) {
		rhs <- formula[[2L]]
		lhs <- NULL
	} else if ( length(formula) == 3L ) {
		rhs <- formula[[3L]]
		lhs <- formula[[2L]]
	}
	if ( length(rhs) == 1L ) {
		# single-term rhs that doesn't include |
		g <- NULL
		rhs <- rhs
	} else if ( length(rhs) == 3L && deparse1(rhs[[1L]]) != "|" ) {
		# rhs includes multiple terms but not |
		g <- NULL
		rhs <- rhs
	} else if ( length(rhs) == 3L && deparse1(rhs[[1L]]) == "|" ) {
		# rhs includes | so add condition
		g <- rhs[[3]]
		rhs <- rhs[[2]]
	} else {
		# failsafe
		g <- NULL
	}
	# parse lhs
	if ( !is.null(lhs) )
		lhs <- parse_side(lhs)
	if ( eval )
		for ( i in seq_along(lhs) )
			lhs[[i]] <- eval(lhs[[i]], envir=envir, enclos=e)
	# parse rhs
	if ( !is.null(rhs) )
		rhs <- parse_side(rhs)
	if ( eval )
		for ( i in seq_along(rhs) )
			rhs[[i]] <- eval(rhs[[i]], envir=envir, enclos=e)
	# parse condition
	if ( !is.null(g) )
		g <- parse_side(g)
	if ( eval )
		for ( i in seq_along(g) )
			g[[i]] <- eval(g[[i]], envir=envir, enclos=e)
	list(lhs=lhs, rhs=rhs, g=g)
}

parse_side <- function(formula, envir = NULL, eval = FALSE)
{
	enclos <- environment(formula)
	if ( length(formula) != 1L ) {
		if ( deparse1(formula[[1L]]) %in% c("~", "*", "+", ":") ) {
			side <- lapply(as.list(formula)[-1L], parse_side)
		} else if ( deparse1(formula[[1L]]) == "I" ) {
			side <- list(formula[[2L]])
		} else {
			side <- list(formula)
		}
	} else {
		side <- list(formula)
	}
	if ( is.list(side) ) {
		side <- unlist(side, recursive=TRUE)
		names(side) <- sapply(side, deparse1)
	}
	if ( eval ) {
		for ( i in seq_along(side) )
			side[[i]] <- eval(side[[i]], envir=envir, enclos=enclos)
	}
	side
}

