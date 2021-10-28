
#### Set up matter options ####
## ----------------------------

.onLoad <- function(libname, pkgname) {
	options(matter.cast.warning = TRUE)
	options(matter.default.chunksize = 1000000L)
	options(matter.show.head = TRUE)
	options(matter.show.head.n = 6L)
	options(matter.coerce.altrep = FALSE)
	options(matter.coerce.altrep.list = FALSE)
	options(matter.wrap.altrep = FALSE)
	options(matter.dump.dir = tempdir())
}

#### Binary search that allows near-matches ####
## ----------------------------------------------

# exported version with argument checking (safer, easier)

bsearch <- function(x, table, tol = 0, tol.ref = "abs",
					nomatch = NA_integer_, nearest = FALSE)
{
	if ( is.integer(x) && is.double(table) )
		x <- as.double(x)
	if ( is.double(x) && is.integer(table) )
		table <- as.double(table)
	if ( is.unsorted(table) )
		stop("'table' must be sorted")
	tol.ref <- pmatch(tol.ref, c("abs", "x", "y"), nomatch=1L)
	.Call("C_binarySearch", x, table, tol,
		tol.ref, nomatch, nearest, PACKAGE="matter")
}

# faster internal version with no argument checking

bsearch_int <- function(x, table, tol = 0, tol.ref = 1L,
					nomatch = NA_integer_, nearest = FALSE)
{
	.Call("C_binarySearch", x, table, tol,
		tol.ref, nomatch, nearest, PACKAGE="matter")
}

# linear search (for sanity check)

lsearch <- function(x, table, tol = 0, tol.ref = "abs",
					nomatch = NA_integer_, nearest = FALSE)
{
	if ( is.integer(x) && is.double(table) )
		x <- as.double(x)
	if ( is.double(x) && is.integer(table) )
		table <- as.double(table)
	if ( is.unsorted(table) )
		stop("'table' must be sorted")
	tol.ref <- pmatch(tol.ref, c("abs", "x", "y"), nomatch=1L)
	.Call("C_linearSearch", x, table, tol,
		tol.ref, nomatch, nearest, PACKAGE="matter")
}

# relative difference

reldiff <- function(x, y, ref = "abs")
{
	if ( is.integer(x) && is.double(y) )
		x <- as.double(x)
	if ( is.double(x) && is.integer(y) )
		y <- as.double(y)
	ref <- pmatch(ref, c("abs", "x", "y"), nomatch=1L)
	n <- max(length(x), length(y))
	x <- rep_len(x, n)
	y <- rep_len(y, n)
	fun <- function(a, b) .Call("C_relativeDiff", a, b, ref, PACKAGE="matter")
	mapply(fun, x, y, USE.NAMES=FALSE)
}

is.sorted <- function(x, ...) !is.unsorted(x, ...)

#### Find local maxima and local minima ####
## -----------------------------------------

locmax <- function(x, halfWindow = 2, findLimits = FALSE)
{
	if ( halfWindow <= 0 )
		stop("'halfWindow' must be a positive integer")
	halfWindow <- as.integer(halfWindow)
	ans <- .Call("C_localMaxima", x, halfWindow, PACKAGE="matter")
	ans <- which(ans)
	if ( findLimits ) {
		lims <- .Call("C_regionMaxima", x, ans, halfWindow, PACKAGE="matter")
		attr(ans, "lower") <- lims[[1]]
		attr(ans, "upper") <- lims[[2]]
	}
	ans
}

#### Binning ####
## ---------------

binvec <- function(x, u, v, method = "sum")
{
	if ( length(u) == length(x) ) {
		u <- as.factor(u)
		group <- as.integer(u)
		ngroup <- nlevels(u)
		if ( is.function(method) ) {
			groupCombiner(method)(x, group, ngroup, NA)
		} else {
			method <- pmatch(method, c("sum", "mean", "min", "max"))
			switch(method,
				.Call("C_groupSums", x, group, ngroup, NA, PACKAGE="matter"),
				.Call("C_groupMeans", x, group, ngroup, NA, PACKAGE="matter"),
				.Call("C_groupMins", x, group, ngroup, NA, PACKAGE="matter"),
				.Call("C_groupMaxs", x, group, ngroup, NA, PACKAGE="matter"))
		}
	} else {
		if ( missing(v) ) {
			v <- u[-1] - 1L
			u <- u[-length(u)]
		}
		if ( length(u) != length(v) )
			stop("'u' and 'v' must be the same length")
		u <- as.integer(u)
		v <- as.integer(v)
		if ( is.function(method) ) {
			binFun(method)(x, u, v)
		} else {
			method <- pmatch(method, c("sum", "mean", "min", "max"))
			switch(method,
				.Call("C_binSums", x, u, v, PACKAGE="matter"),
				.Call("C_binMeans", x, u, v, PACKAGE="matter"),
				.Call("C_binMins", x, u, v, PACKAGE="matter"),
				.Call("C_binMaxs", x, u, v, PACKAGE="matter"))
		}
	}
}

#### Summarise vectors based on bin intervals ####
## -----------------------------------------------

binMeans <- function(x, lower, upper) {
	.Call("C_binMeans", x, lower, upper, PACKAGE="matter")
}

binSums <- function(x, lower, upper) {
	.Call("C_binSums", x, lower, upper, PACKAGE="matter")
}

binMins <- function(x, lower, upper) {
	.Call("C_binMins", x, lower, upper, PACKAGE="matter")
}

binMaxs <- function(x, lower, upper) {
	.Call("C_binMaxs", x, lower, upper, PACKAGE="matter")
}

binFun <- function(fun) {
	function(x, u, v) {
		mapply(function(i, j) fun(x[i:j]), u, v, SIMPLIFY=TRUE)
	}
}

#### Summarise vectors based on integer groupings ####
## --------------------------------------------------

groupMeans <- function(x, group, ngroup, default=NA) {
	.Call("C_groupMeans", x, group, ngroup, default, PACKAGE="matter")
}

groupSums <- function(x, group, ngroup, default=NA) {
	.Call("C_groupSums", x, group, ngroup, default, PACKAGE="matter")
}

groupMins <- function(x, group, ngroup, default=NA) {
	.Call("C_groupMins", x, group, ngroup, default, PACKAGE="matter")
}

groupMaxs <- function(x, group, ngroup, default=NA) {
	.Call("C_groupMaxs", x, group, ngroup, default, PACKAGE="matter")
}

groupIds <- function(x, group, ngroup, default=NA) {
	vals <- vector(mode=typeof(x), length=ngroup)
	if ( anyDuplicated(group, incomparables=NA) > 0 )
		stop("duplicate key matches, can't resolve collision")
	vals[] <- default
	na_rm <- !is.na(group)
	vals[group[na_rm]] <- x[na_rm]
	vals
}

groupCombiner <- function(fun) {
	function(x, group, ngroup, default=NA) {
		group <- factor(group, levels=seq_len(ngroup))
		as.vector(tapply(x, group, fun, default=default))
	}
}

#### Show utility functions ####
## -----------------------------

show_matter_memory_and_storage <- function(object) {
	object.memory <- object.size(object)
	class(object.memory) <- "num_bytes"
	if ( is.matter(object) ) {
		rmem <- format(object.memory, units="auto")
		vmem <- format(vm_used(object), units="auto")
		cat("(", rmem, " real", " | ", vmem, " virtual)\n", sep="")
	} else {
		cat("(", rmem, " real)\n", sep="")
	}
}

preview_vector_data <- function(x, n = getOption("matter.show.head.n")) {
	hdr <- head(x, n=n)
	out <- as.character(hdr)
	more <- length(x) - length(hdr) > 0
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
	out <- matrix(as.character(hdr), nrow=nrow(hdr), ncol=ncol(hdr))
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
	out <- matrix(as.character(hdr), nrow=nrow(hdr), ncol=ncol(hdr))
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
		y <- subListElementAsVector(x, i)
		fmt <- preview_vector_data(y, n)
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

#### Miscellaneous utility functions ####
## --------------------------------------

apply_int <- function(x, margin, fun, fun.value, ...) {
	fun <- match.fun(fun)
	if ( margin == 1L ) {
		vapply(seq_len(nrow(x)), function(i) fun(x[i,,drop=TRUE], ...), fun.value)
	} else if ( margin == 2L ) {
		vapply(seq_len(ncol(x)), function(j) fun(x[,j,drop=TRUE], ...), fun.value)
	} else {
		stop("internal 'apply' error")
	}
}

check_compatible_classes <- function(x, y) {
	if ( !inherits(y, class(x)) ) {
		stop("incompatible classes: ",
			class(x)[1L], ", ", class(y)[1L])
	} else {
		TRUE
	}
}

stringsToFactors <- function(x) {
	if ( is.character(x) ) {
		as.factor(x)
	} else {
		x
	}
}

returnWithWarning <- function(x, ...) {
	warning(...)
	x
}

paste_head <- function(x, n=getOption("matter.show.head.n"), collapse=" ") {
	if ( length(x) > n ) {
		paste0(paste0(head(x, n=n), collapse=collapse), " ...")
	} else {
		paste0(x, collapse=collapse)
	}
}

collect_by_key <- function(x, reduce) {
	keys <- unique(names(x))
	ans <- lapply(keys, function(k) {
		Reduce(reduce, unname(x[names(x) %in% k]))
	})
	names(ans) <- keys
	ans
}

logical2index <- function(x, i, margin) {
	if ( missing(margin) ) {
		len <- length(x)
	} else {
		len <- dim(x)[margin]
	}
	as.numeric(which(rep(i, length.out=len)))
}

names2index <- function(x, i, exact) {
	if ( missing(exact) )
		exact <- TRUE
	if ( exact ) {
		as.numeric(match(i, names(x)))
	} else {
		as.numeric(pmatch(i, names(x)))
	}
}

dimnames2index <- function(x, i, margin, exact) {
	if ( missing(exact) )
		exact <- TRUE
	if ( exact ) {
		as.numeric(match(i, dimnames(x)[[margin]]))
	} else {
		as.numeric(pmatch(i, dimnames(x)[[margin]]))
	}
}

allIndices <- function(x, i, margin) {
	if ( missing(margin) ) {
		all(i == seq_len(length(x)))
	} else {
		all(i == seq_len(dim(x)[margin]))
	}
}

sizeof <- function(type) {
	type <- make_datamode(type, type="C")
	vapply(as.character(type), switch, numeric(1),
		char = 1,
		uchar = 1,
		short = 2,
		ushort = 2,
		int = 4,
		uint = 4,
		long = 8,
		ulong = 8,
		float = 4,
		double = 8)
}

do_recursive <- function(fun, args) {
	if ( length(args) > 2 ) {
		fun(args[[1]], do_recursive(fun, args[-1]))
	} else {
		fun(args[[1]], args[[2]])
	}
}

bind_elements <- function(x, y) {
	if ( !identical(names(x), intersect(names(x), names(y))) )
		stop("names do not match previous names")
	y <- y[names(x)]
	try_c <- function(e1, e2, label) {
		tryCatch(c(e1, e2), error=function(e)
			stop("couldn't combine element '", label, "'"))
	}
	mapply(try_c, x, y, names(x))
}

combine_list <- function(list) {
	Reduce(match.fun("c"), list)
}

combine_colnames <- function(x, y, ...) {
	if ( length(list(...)) > 0 )
		y <- combine_colnames(y, ...)
	if ( is.null(dimnames(x)[[2]]) && is.null(dimnames(y)[[2]]) ) {
		colnames <- NULL
	} else if ( is.null(dimnames(x)[[2]]) ) {
		colnames <- c(character(dim(x)[2]), dimnames(y)[[2]])
	} else if ( is.null(dimnames(y)[[2]]) ) {
		colnames <- c(dimnames(x)[[2]], character(dim(y)[2]))
	} else {
		colnames <- c(dimnames(x)[[2]], dimnames(y)[[2]])
	}
	if ( !is.null(dimnames(x)[[1]]) ) {
		rownames <- dimnames(x)[[1]]
	} else {
		rownames <- dimnames(y)[[1]]
	}
	if ( is.null(rownames) && is.null(colnames) ) {
		NULL
	} else {
		list(rownames, colnames)
	}
}

combine_rownames <- function(x, y, ...) {
	if ( length(list(...)) > 0 )
		y <- combine_rownames(y, ...)
	if ( is.null(dimnames(x)[[1]]) && is.null(dimnames(y)[[1]]) ) {
		rownames <- NULL
	} else if ( is.null(dimnames(x)[[1]]) ) {
		rownames <- c(character(dim(x)[1]), dimnames(y)[[1]])
	} else if ( is.null(dimnames(y)[[1]]) ) {
		rownames <- c(dimnames(x)[[1]], character(dim(y)[1]))
	} else {
		rownames <- c(dimnames(x)[[1]], dimnames(y)[[1]])
	}
	if ( !is.null(dimnames(x)[[2]]) ) {
		colnames <- dimnames(x)[[2]]
	} else {
		colnames <- dimnames(y)[[2]]
	}
	if ( is.null(rownames) && is.null(colnames) ) {
		NULL
	} else {
		list(rownames, colnames)
	}
}

# convert array indices to linear indices
linearInd <- function(ind, .dim) {
	if ( is.list(ind) ) {
		ind <- expand.grid(ind)
		apply(ind, 1, linearInd, .dim=.dim)
	} else {
		if ( any(ind <= 0 | ind > .dim) )
			stop("subscript out of bounds")
		mult <- c(1, cumprod(.dim[-length(.dim)]))
		sum((ind - 1) * mult) + 1
	}
}

# convert linear indices to matrix indices
matrixInd <- function(ind, .dim) {
	i <- as.integer(ind - 1L) %% .dim[1]
	j <- as.integer(ind - 1L) %/% .dim[1]	
	list(i=i + 1L, j=j + 1L)
}

# convert linear indices to row-major indices
rowMajInd <- function(ind, .dim) {
	ind <- matrixInd(ind, .dim)
	.dim[2] * (ind$i - 1L) + ind$j
}

#### Define allowed delayed operation types ####
## -----------------------------------------------

make_op <- function(x) {
	levels <- c(
		"+",
		"-",
		"*",
		"/",
		"^",
		"%%",
		"%/%",
		"==",
		"!=",
		">",
		"<",
		">=",
		"<=",
		"&",
		"|",
		"log")
	if ( missing(x) )
		return(factor(levels=levels))
	if ( is.numeric(x) ) {
		x <- as.integer(x)
		factor(x, levels=seq_along(levels), labels=levels)
	} else if ( is.character(x) ) {
		x <- tolower(x)
		if ( any(!x %in% levels) )
			stop("unsupported delayed operation type")
		factor(x, levels=levels)
	} else {
		as.factor(x)
	}
}

register_op <- function(x, lhs, rhs, op,
	where = c("by_group", "by_each_group"))
{
	op <- make_op(op)
	where <- match.arg(where)
	where <- switch(where,
		by_group=1L, by_each_group=2L)
	op <- list(lhs=lhs, rhs=rhs, op=op, where=where)
	x@ops <- c(x@ops, list(op))
	x
}

#### File read/write mode functions and utilities ####
## ---------------------------------------------------

make_filemode <- function(x) {
	levels <- c("r", "w", "rw")
	if ( missing(x) )
		return(factor(NA_character_, levels=levels))
	if ( !is.numeric(x) )
		x <- as.character(x)
	switch(x,
		"r" = factor("r", levels=levels),
		"w" = factor("w", levels=levels),
		"rw" = factor("rw", levels=levels),
		stop("unsupported I/O mode"))
}

common_filemode <- function(x, y) {
	if ( x == "rw" && y == "rw" ) {
		make_filemode("rw")
	} else if ( x %in% c("w", "rw") && y %in% c("w", "rw") ) {
		make_filemode("w")
	} else {
		make_filemode("r")
	}
}

#### Define data types and utility functions for them ####
## -------------------------------------------------------

# --> R , --> C

make_datamode <- function(x, type=c("C", "R")) {
	type <- match.arg(type)
	levels <- switch(type,
		C = c(
			"char",
			"uchar",
			"short",
			"ushort",
			"int",
			"uint",
			"long",
			"ulong",
			"float",
			"double"),
		R = c(
			"raw",
			"logical",
			"integer",
			"numeric",
			"character",
			"list",
			"virtual"))
	if ( missing(x) )
		return(factor(levels=levels))
	if ( is.numeric(x) ) {
		x <- as.integer(x)
		factor(x, levels=seq_along(levels), labels=levels)
	} else if ( is.character(x) ) {
		x <- tolower(x)
		if ( any(!x %in% levels) ) {
			if ( type == "C" ) {
				if ( all(x %in% levels(make_datamode(type="R"))) ) {
					x <- convert_datamode(x, to="C")
				} else {
					stop("unsupported data type")
				}
			} else if ( type == "R" ) {
				if ( all(x %in% levels(make_datamode(type="C"))) ) {
					x <- convert_datamode(x, to="R")
				} else {
					stop("unsupported data type")
				}
			} else {
				stop("unsupported data type")
			}
		}
		factor(x, levels=levels)
	} else {
		as.factor(x)
	}
}

# R <--> C

convert_datamode <- function(x, to=c("C", "R")) {
	if ( to == "R" ) {
		x <- vapply(as.character(x), switch, character(1),
			char = "raw",
			uchar = "raw",
			short = "integer",
			ushort = "integer",
			int = "integer",
			uint = "integer",
			long = "numeric",
			ulong = "numeric",
			float = "numeric",
			double = "numeric")
	} else {
		x <- vapply(as.character(x), switch, character(1),
			raw = "uchar",
			logical = "int",
			integer = "int",
			numeric = "double",
			character = "uchar",
			list = stop("cannot convert data mode 'list' to C data type"),
			virtual = stop("cannot convert data mode 'virtual' to C data type"))
	}
	names(x) <- NULL
	x
}

# --> R

widest_datamode <- function(x) {
	if ( is.drle(x) )
		x <- x[]
	if ( is.integer(x) )
		x <- make_datamode(x, type="C")
	x <- as.character(unique(x))
	if ( all(x %in% levels(make_datamode(type="R"))) ) {
		x <- max(as.integer(make_datamode(x, type="R")))
		make_datamode(switch(x,
			raw = "raw",
			logical = "logical",
			integer = "integer",
			numeric = "numeric",
			character = stop("data mode 'character' not expected here"),
			list = stop("data mode 'list' not expected here"),
			virtual = stop("data mode 'virtual' not expected here")),
		type="R")
	} else if ( all(x %in% levels(make_datamode(type="C"))) ) {
		x <- max(as.integer(make_datamode(x, type="C")))
		make_datamode(switch(x,
			char = "raw",
			uchar = "raw",
			short = "integer",
			ushort = "integer",
			int = "integer",
			uint = "integer",
			long = "numeric",
			ulong = "numeric",
			float = "numeric",
			double = "numeric"), type="R")
	} else {
		stop("unsupported data type")
	}
}

#### Utilities for working with raw bytes and memory ####
## ------------------------------------------------------

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

# creates internal S3 class 'num_bytes'

num_bytes <- function(x) {
	class(x) <- "num_bytes"
	x
}

# calculates vm used by a matter object

vm_used <- function(x) {
	if ( is(x, "atoms") ) {
		size <- sum(x@extent[] * sizeof(datamode(x)[]))
	} else if ( is.matter(x) ) {
		if ( is(adata(x), "atoms") ) {
			size <- vm_used(adata(x))
		} else {
			size <- sum(vapply(adata(x), vm_used, numeric(1)))
		}
	} else {
		size <- 0
	}
	num_bytes(size)
}

# based on utils::format.object_size

fmt_num_bytes <- function(x, units = "auto") {
	units <- match.arg(units, c("auto",
				"B", "KB", "MB", "GB", "TB", "PB"))
    if (units == "auto")
        units <- if (x >= 1000^4) 
            "TB"
        else if (x >= 1000^3) 
            "GB"
        else if (x >= 1000^2) 
            "MB"
        else if (x >= 1000) 
            "KB"
        else "B"
    switch(units,
    	B = c("bytes"=x),
    	KB = c("KB"=round(x/1000, 1L)),
    	MB = c("MB"=round(x/1000^2, 1L)), 
        GB = c("GB"=round(x/1000^3, 1L)),
        TB = c("TB"=round(x/1000^4, 1L)),
        PB = c("PB"=round(x/1000^5, 1L)))
}

print.num_bytes <- function (x, units = "auto", ...)  {
	print(fmt_num_bytes(x, units=units))
}

format.num_bytes <- function(x, units = "auto", ...) {
	x <- fmt_num_bytes(x, units=units)
	paste(x, names(x))
}

# based on pryr::mem_used and pryr::mem_change

mem <- function(x, reset = FALSE) {
	if ( !missing(x) ) {
		mem <- num_bytes(as.numeric(object.size(x)))
	} else {
		cell.size <- c(Ncells=56, Vcells=8)
		mem <- round(colSums(gc(reset=reset)[,c(1,3,6)] * cell.size) / 1000^2, 1)
		names(mem) <- c("used (MB)", "gc trigger (MB)", "max used (MB)")
	}
	mem
}

profmem <- function(expr) {
	start <- mem(reset = TRUE)
	t.start <- proc.time()
	expr <- substitute(expr)
	eval(expr, parent.frame())
	rm(expr)
	t.end <- proc.time()
	end <- mem(reset = FALSE)
	mem <- c(start[1], end[1], end[3], end[3] - end[1], t.end[3] - t.start[3])
	names(mem) <- c("start (MB)", "finish (MB)",
		"max used (MB)", "overhead (MB)", "time (sec)")
	mem
}
