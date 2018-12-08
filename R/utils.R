
#### Set up matter options ####
## ----------------------------

.onLoad <- function(libname, pkgname) {
	options(matter.cast.warning = TRUE)
}

#### Binary search that allows near-matches ####
## ----------------------------------------------

# exported version with argument checking (safer, easier)

bsearch <- function(key, values, tol = 0, tol.ref = "none",
					nomatch = NA_integer_, nearest = FALSE)
{
	if ( is.integer(key) && is.double(values) )
		key <- as.double(key)
	if ( is.double(key) && is.integer(values) )
		values <- as.double(values)
	tol.ref <- pmatch(tol.ref, c("none", "key", "values"), nomatch=1L)
	.Call("C_binarySearch", key, values, tol,
		tol.ref, nomatch, nearest, PACKAGE="matter")
}

# faster internal version with no argument checking

bsearch_int <- function(key, values, tol = 0, tol.ref = 1L,
					nomatch = NA_integer_, nearest = FALSE)
{
	.Call("C_binarySearch", key, values, tol,
		tol.ref, nomatch, nearest, PACKAGE="matter")
}

is.sorted <- function(x, ...) !is.unsorted(x, ...)

#### Summarise vectors based on intger groupings ####
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

#### Miscellaneous utility functions ####
## --------------------------------------

returnWithWarning <- function(x, ...) {
	warning(...)
	x
}

paste_head <- function(x, n=6L, collapse=" ") {
	if ( length(x) > n ) {
		paste0(paste0(head(x, n=n), collapse=collapse),
			" [and ", length(x) - n, " more]")
	} else {
		paste0(x, collapse=collapse)
	}
}

logical2index <- function(x, i, margin) {
	if ( missing(margin) ) {
		len <- length(x)
	} else {
		len <- dim(x)[margin]
	}
	as.numeric(which(rep(i, length.out=len)))
}

names2index <- function(x, i)
	as.numeric(match(i, names(x)))

dimnames2index <- function(x, i, margin)
	as.numeric(match(i, dimnames(x)[[margin]]))

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

combine_colnames <- function(x, y, ...) {
	if ( length(list(...)) > 0 )
		y <- combine_colnames(y, ...)
	if ( is.null(dimnames(x)[[2]]) && is.null(dimnames(y)[[2]]) ) {
		colnames <- NULL
	} else if ( is.null(dimnames(x)[[2]]) ) {
		colnames <- c(character(dim(x)[2]), dimnames(y)[[2]])
	} else if ( is.null(dimnames(y)[[2]]) ) {
		colnames <- c(dimnames(x)[[2]], character(dim(x)[2]))
	} else {
		colnames <- c(dimnames(x)[[2]], dimnames(x)[[2]])
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
		rownames <- c(dimnames(x)[[1]], character(dim(x)[1]))
	} else {
		rownames <- c(dimnames(x)[[1]], dimnames(x)[[1]])
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

linearInd <- function(ind, .dim) {
	if ( is.list(ind) ) {
		ind <- expand.grid(ind)
		apply(ind, 1, linearInd, .dim=.dim)
	} else {
		if ( any(ind <= 0 || ind > .dim) )
			stop("subscript out of bounds")
		mult <- c(1, cumprod(.dim[-length(.dim)]))
		sum((ind - 1) * mult) + 1
	}
}

is_ragged_array <- function(x) {
	is(x, "matter_arr") && is.null(dim(x))
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
		by_group=1, by_each_group=2)
	op <- list(lhs=lhs, rhs=rhs, op=op, where=where)
	x@ops <- c(x@ops, list(op))
	x
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
			virtual = stop("data mode 'virtual' not expected here")), type="R")
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
	y <- rawToChar(x, multiple=multiple)
	Encoding(y) <- encoding
	y
}

char2raw <- function(x) {
	charToRaw(x)
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
		mem <- print(mem)
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
