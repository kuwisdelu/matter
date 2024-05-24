
#### Set up matter options ####
## ----------------------------

.onLoad <- function(libname, pkgname) {
	options(
		matter.compress.atoms = 3,
		matter.default.nchunks = 20L,
		matter.default.verbose = FALSE,
		matter.matmul.bpparam = NULL,
		matter.show.head = TRUE,
		matter.show.head.n = 6L,
		matter.coerce.altrep = FALSE,
		matter.wrap.altrep = FALSE,
		matter.dump.dir = tempdir(),
		matter.vizi.par = par_style_new(),
		matter.vizi.panelgrid = NULL,
		matter.vizi.trans3d = NULL,
		matter.vizi.engine = "base",
		matter.vizi.style = "light",
		matter.vizi.dpal = "Tableau 10",
		matter.vizi.cpal = "Viridis")
}

#### Parallel RNG ####
## --------------------

RNGStreams <- function(n = length(size), size = 1L) {
	if ( length(size) != n )
		size <- rep_len(size, n)
	s <- getRNGStream()
	seeds <- vector("list", n)
	for ( i in seq_len(n) )
	{
		seeds[[i]] <- s
		if ( s$kind == "L'Ecuyer-CMRG" )
		{
			if ( size[i] > 1L ) {
				for ( j in seq_len(size[i]) )
					s$seed <- nextRNGSubStream(s$seed)
			} else {
				s$seed <- nextRNGStream(s$seed)
			}
		}
	}
	seeds
}

getRNGStream <- function() {
	if ( exists(".Random.seed", envir=globalenv()) ) {
		seed <- get(".Random.seed", envir=globalenv())
	} else {
		seed <- NULL
	}
	list(seed=seed, kind=RNGkind()[1L])
}

setRNGStream <- function(seed = NULL, kind = NULL) {
	if ( is.list(seed) ) {
		kind <- seed$kind
		seed <- seed$seed
	}
	RNGkind(kind)
	if ( is.null(seed) ) {
		if ( exists(".Random.seed", envir=globalenv()))
			rm(".Random.seed", envir=globalenv())
	} else {
		assign(".Random.seed", seed, envir=globalenv())
	}
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

subset_list <- function(x, i) {
	f <- function(y) {
		if ( length(y) > 1L ) {
			y[i]
		} else {
			y
		}
	}
	lapply(x, f)
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

get_Rtypes <- function() {
	c("raw", "logical", "integer",
		"double", "character", "list")
}

get_Ctypes <- function() {
	c("char", "uchar", "int16", "uint16",
		"int32", "uint32", "int64", "uint64",
		"float32", "float64")
}

get_Caliases <- function() {
	c(short="int16", ushort="uint16",
		`16-bit integer`="int16",
		`16-bit unsigned integer`="uint16",
		int="int32", uint="uint32",
		`32-bit integer`="int32",
		`32-bit unsigned integer`="uint32",
		long="int64", ulong="uint64",
		`64-bit integer`="int64",
		`64-bit unsigned integer`="uint64",
		float="float32", double="float64",
		`32-bit float`="float32",
		`64-bit float`="float64")
}

as_Rtype <- function(x) {
	if ( !missing(x) ) {
		if ( is_Rtype(x, strict=TRUE) )
			return(x)
		if ( !is_Rtype(x) ) {
			if ( is_Ctype(x) )
				return(to_Rtype(as_Ctype(x)))
			if ( is_Calias(x) )
				return(to_Rtype(unalias_Ctype(x)))
		}
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

as_Ctype <- function(x) {
	if ( !missing(x) ) {
		if ( is_Ctype(x, strict=TRUE) )
			return(x)
		if ( !is_Ctype(x) ) {
			if ( is_Rtype(x) )
				return(to_Ctype(as_Rtype(x)))
			if ( is_Calias(x) )
				x <- unalias_Ctype(x)
		}
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

is_Calias <- function(x) {
	aliases <- names(get_Caliases())
	any(x %in% aliases)
}

unalias_Ctype <- function(x) {
	aliases <- get_Caliases()
	ifelse(x %in% names(aliases), aliases[x], x)
}

to_Rtype <- function(x) {
	codes <- c(char = "character", uchar = "raw",
		int16 = "integer", uint16 = "integer",
		int32 = "integer", uint32 = "integer",
		int64 = "double", uint64 = "double",
		float32 = "double", float64 = "double")
	as_Rtype(codes[as.integer(as_Ctype(x))])
}

to_Ctype <- function(x) {
	codes <- c(raw = "uchar", logical = "int32",
		integer = "int32", numeric = "float64",
		character = "char", list = NA_character_)
	as_Ctype(codes[as.integer(as_Rtype(x))])
}

sizeof <- function(x) {
	sizes <- c(char = 1L, uchar = 1L, int16 = 2L, uint16 = 2L,
		int32 = 4L, uint32 = 4L, int64 = 8L, uint64 = 8L,
		float32 = 4L, float64 = 8L)
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

as_run_type <- function(x) {
	codes <- c("drle", "rle", "seq")
	make_code(codes, x[1L], nomatch=1L)
}

as_tol <- function(x) {
	if ( !is.null(attr(x, "tol_type")) )
		return(x)
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

as_interp <- function(x) {
	codes <- c(
		# simple interp (1-5)
		"none", "sum", "mean", "max", "min",
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
		"sd", "var", "mad",
		# other (8-9)
		"quantile", "sse")
	make_code(codes, x[1L], nomatch=1L)
}

as_dist <- function(x) {
	codes <- c(
		"euclidean", "maximum",
		"manhattan", "minkowski")
	make_code(codes, x[1L], nomatch=1L)
}

as_weights <- function(x) {
	codes <- c("gaussian", "adaptive")
	make_code(codes, x[1L], nomatch=1L)
}

#### Data structure utility functions ####
## ---------------------------------------

as_real_memory_matrix <- function(x) {
	if ( is.matrix(x) || is(x, "Matrix") ) {
		x
	} else {
		warning("coercing input to a local matrix")
		as.matrix(x)
	}
}

I <- function(x) {
	if ( is.null(x) ) {
		NULL
	} else {
		base::I(x)
	}
}

non_null <- function(x) !vapply(x, is.null, logical(1L))

is_null_or_na <- function(x) is.null(x) || is.na(x)

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

normalize_lengths <- function(list) {
	if ( length(list) > 0L ) {
		ns <- lengths(list)
		nmax <- max(ns)
		if ( any(ns != nmax) )
			list <- lapply(list, rep_len, length.out=nmax)
	}
	list
}

normalize_lengths2 <- function(list1, list2) {
	n1 <- length(list1)
	n2 <- length(list1)
	nmax <- max(n1, n2)
	if ( length(list1) != nmax )
		list1 <- rep_len(list1, nmax)
	if ( length(list2) != nmax )
		list2 <- rep_len(list2, nmax)
	ns1 <- lengths(list1)
	ns2 <- lengths(list2)
	nsmax <- pmax(ns1, ns2)
	if ( any(ns1 != nsmax) )
		list1 <- Map(rep_len, list1, nsmax)
	if ( any(ns2 != nsmax) )
		list2 <- Map(rep_len, list2, nsmax)
	list(list1, list2)
}

nlines <- function(x) {
	xsub <- gsub("\n", "", x, fixed=TRUE)
	nchar(x) - nchar(xsub) + 1L
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

preview_vector_data <- function(x, n = getOption("matter.show.head.n"), ...) {
	hdr <- head(x, n=n)
	out <- format(hdr, ...)
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
	if ( length(x) > 0L ) {
		matrix(out, nrow=1, dimnames=list("", nms))
	} else {
		matrix("", nrow=1, dimnames=list("", "[0]"))
	}
}

preview_vector <- function(x, n = getOption("matter.show.head.n"), ...) {
	print(preview_vector_data(x, n, ...), quote=FALSE, right=TRUE)
}

preview_matrix_data <- function(x, n = getOption("matter.show.head.n"), ...) {
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
	out <- matrix(format(hdr, ...), nrow=nrow(hdr), ncol=ncol(hdr))
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

preview_matrix <- function(x, n = getOption("matter.show.head.n"), ...) {
	print(preview_matrix_data(x, n, ...), quote=FALSE, right=TRUE)
}

preview_Nd_array <- function(x, n = getOption("matter.show.head.n"), ...) {
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
	out <- matrix(format(hdr, ...), nrow=nrow(hdr), ncol=ncol(hdr))
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

preview_list <- function(x, n = getOption("matter.show.head.n"), ...) {
	n1 <- min(n, length(x))
	for ( i in 1:n1 ) {
		xi <- x[[i]]
		if ( is.null(dim(xi)) ) {
			fmt <- preview_vector_data(x[[i]], n, ...)
		} else {
			fmt <- preview_matrix_data(x[[i]], n, ...)
		}
		if ( !is.null(names(x)) ) {
			nm <- paste0("$", names(x)[i])
		} else {
			nm <- paste0("[[", i, "]]")
		}
		if ( nrow(fmt) > 1L )
			nm <- c(nm, character(nrow(fmt) - 1L))
		rownames(fmt) <- nm
		print(fmt, quote=FALSE, right=TRUE)
	}
	if ( length(x) > n1 )
		cat("...\n")
}

preview_table <- function(x, n = getOption("matter.show.head.n"), cls = NULL, ...) {
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
	out <- matrix(format(out, ...), nrow=nrow(out), ncol=ncol(out))
	if ( is.null(cls) ) {
		cls <- vapply(hdr, function(xj) class(xj)[1L], character(1L))
	} else {
		cls <- cls[j]
	}
	cls <- paste0("<", cls, ">")
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
		cls <- c(cls, "")
	}
	out <- rbind(cls, out)
	rnm <- c("", rnm)
	dimnames(out) <- list(rnm, cnm)
	print(out, quote=FALSE, right=TRUE, ...)
}

#### Miscellaneous internal functions ####
## ---------------------------------------

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
	if ( is.null(BPPARAM) ) {
		ans <- vector("list", length=length(X))
		for ( i in seq_along(X) )
			ans[[i]] <- FUN(X[[i]], ...)
		set_names(ans, names(X))
	} else {
		bplapply(X, FUN, ..., BPPARAM=BPPARAM)
	}
}

roll <- function(x, width, na.drop = FALSE, fill = NA) {
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

# calculate mean/median/mode
avg <- function(x, center = mean)
{
	x <- x[!is.na(x)]
	if ( is.numeric(x) ) {
		y <- center(x)
	} else {
		ux <- unique(x)
		y <- ux[which.max(tabulate(match(x, ux)))]
	}
	unname(y)
}

# encode a dummy (one-hot) variable
encode_dummy <- function(x, drop = TRUE) {
	x <- as.factor(x)
	if ( drop )
		x <- droplevels(x)
	v <- levels(x)
	d <- matrix(0L, nrow=length(x), ncol=length(v))
	for ( i in seq_along(v) )
		d[x == v[i],i] <- 1L
	dimnames(d) <- list(names(x), v)
	d
}

n_unique <- function(x, na.rm = TRUE) {
	x <- unique(as.vector(x))
	if ( is.atomic(x) && na.rm ) {
		length(x[!is.na(x)])
	} else {
		length(x)
	}
}

# soft thresholding
soft <- function(x, t) sign(x) * pmax(abs(x) - t, 0)

# get predicted classes from scores
predict_class <- function(scores) {
	i <- seq_len(ncol(scores))
	if ( is.null(colnames(scores)) ) {
		labs <- i
	} else {
		labs <- colnames(scores)
	}
	cls <- apply(scores, 1L, which.max)
	factor(cls, levels=i, labels=labs)
}

# matrix pseudoinverse based on MASS::ginv
pinv <- function (x, tol = sqrt(.Machine$double.eps)) 
{
	x <- as.matrix(x)
	sv <- svd(x)
	pos <- sv$d > max(tol * sv$d[1L], 0)
	if ( all(pos) ) {
		sv$v %*% (1 / sv$d * t(sv$u))
	} else if ( !any(pos) ) {
		array(0, dim(x)[2L:1L])
	} else {
		sv$v[,pos,drop=FALSE] %*% (t(sv$u[,pos,drop=FALSE]) / sv$d[pos])
	}
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
	mx <- max(x, na.rm=TRUE)
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

mtime <- function(expr)
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

memtime <- function(expr)
{
	.Deprecated("mtime")
	mtime(expr)
}

profmem <- function(expr)
{
	.Defunct("mtime")
}

#### Formula parsing ####
## ----------------------

parse_formula <- function(formula, envir = NULL, eval = !missing(envir))
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
		names(side) <- vapply(side, deparse1, character(1L))
	}
	if ( eval ) {
		for ( i in seq_along(side) )
			side[[i]] <- eval(side[[i]], envir=envir, enclos=enclos)
	}
	side
}

eval_exprs <- function(exprs, data, i1 = NULL, i2 = NULL,
	recursive = NA, margin = 0L, reduce = "+")
{
	if ( is.na(recursive) )
		recursive <- !is.null(i1) || !is.null(i2)
	ans <- vector("list", length=length(exprs))
	for ( i in seq_along(exprs) )
	{
		ans[[i]] <- eval_expr(exprs[[i]], data=data, i1=i1, i2=i2,
			recursive=recursive, margin=margin, reduce=reduce)
	}
	attr(ans, "recursive") <- recursive
	names(ans) <- names(exprs)
	ans
}

eval_expr <- function(expr, data, i1 = NULL, i2 = NULL,
	recursive = NA, margin = 0L, reduce = "+")
{
	if ( is.na(recursive) )
		recursive <- !is.null(i1) || !is.null(i2)
	vars <- all.vars(expr)
	data <- lapply(vars,
		function(nm) {
			v <- data[[nm]]
			if ( !is.null(i1) ) {
				v <- switch(margin + 1L,
					v[i1],
					v[i1,,drop=FALSE],
					v[,i1,drop=FALSE])
			}
			if ( recursive ) {
				v <- switch(margin + 1L,
					as.vector(v),
					apply(v, 1L, identity, simplify=FALSE),
					apply(v, 2L, identity, simplify=FALSE))
			}
			if ( !is.null(names(i1)) ) {
				if ( anyDuplicated(names(i1)) ) {
					FUN <- match.fun(reduce)
					group <- factor(names(i1), levels=unique(names(i1)))
					v <- lapply(levels(group),
						function(g) Reduce(FUN, v[group %in% g]))
					if ( !recursive )
						v <- unlist(v)
					names(v) <- levels(group)
				} else {
					names(v) <- names(i1)
				}
			}
			v
		})
	names(data) <- vars
	if ( recursive ) {
		EVAL <- function(...)
		{
			datalist <- list(...)
			names(datalist) <- vars
			eval(expr, envir=datalist)
		}
		x <- do.call(Map, c(list(EVAL), data))
	} else {
		x <- eval(expr, envir=data)
	}
	if ( !is.null(i2) ) {
		if ( recursive ) {
			x <- subset_list(x, i2)
		} else {
			x <- x[i2]
		}
	}
	x
}

