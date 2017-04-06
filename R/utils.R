
#### Miscellaneous utility functions ####
## --------------------------------------

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

combine_colnames <- function(x, y) {
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

combine_rownames <- function(x, y) {
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

#### Define allowed delayed operation types ####
## -----------------------------------------------

make_op <- function(x) {
	levels <- c("+", "-", "*", "/", "^", "log")
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

register_op <- function(x, lhs, rhs, op, where) {
	if ( is.null(lhs) && is.null(rhs) ) {
		where <- "scalar"
	} else if ( !is.null(lhs) && length(lhs) == 1 ) {
		where <- "scalar"
	} else if ( !is.null(rhs) && length(rhs) == 1 ) {
		where <- "scalar"
	} else if ( missing(where) ) {
		stop("non-scalar arguments not supported")
	}
	op <- make_op(op)
	where <- switch(where, scalar=0, by_major_dim=1, by_minor_dim=2)
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
		C = c("char", "uchar", "short", "ushort", "int", "uint",
			"long", "ulong", "float", "double"),
		R = c("raw", "integer", "numeric"))
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
		vapply(as.character(x), switch, character(1),
			char = "integer",
			uchar = "integer",
			short = "integer",
			ushort = "integer",
			int = "integer",
			uint = "integer",
			long = "numeric",
			ulong = "numeric",
			float = "numeric",
			double = "numeric")
	} else {
		vapply(as.character(x), switch, character(1),
			raw = "uchar",
			integer = "int",
			numeric = "double")
	}
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
			raw="raw",
			integer="integer",
			numeric="numeric"), type="R")
	} else if ( all(x %in% levels(make_datamode(type="C"))) ) {
		x <- max(as.integer(make_datamode(x, type="C")))
		make_datamode(switch(x,
			char = "integer",
			uchar = "integer",
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

# creates internal S3 class 'bytes'

disk_used <- function(x) {
	if ( is.list(x) ) {
		bytes <- sum(vapply(x, disk_used, numeric(1)))
	} else {
		bytes <- sum(x@extent[] * sizeof(datamode(x)[]))
	}
	class(bytes) <- "bytes"
	bytes
}

# based on utils::format.object_size

print.bytes <- function (x, ..., units = "auto")  {
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

format.bytes <- function(x, units = "auto", ...) {
	bytes <- print(x, units=units)
	paste(bytes, names(bytes))
}

# based on pryr::mem_used and pryr::mem_change

mem <- function(x, reset = FALSE) {
	if ( !missing(x) ) {
		mem <- structure(as.numeric(object.size(x)), class="bytes")
		mem <- print(mem)
	} else {
		cell.size <- c(Ncells=56, Vcells=8)
		mem <- round(colSums(gc(reset=reset)[,c(1,3,5)] * cell.size) / 1000^2, 1)
		names(mem) <- c("used (MB)", "gc trigger (MB)", "max used (MB)")
	}
	mem
}

profile <- function(expr, reset = FALSE) {
	start <- mem(reset = TRUE)
	t.start <- proc.time()
	expr <- substitute(expr)
	eval(expr, parent.frame())
	rm(expr)
	t.end <- proc.time()
	end <- mem()
	mem <- c(start[1], end[1], end[3], end[3] - end[1], t.end[3] - t.start[3])
	names(mem) <- c("start (MB)", "finish (MB)",
		"max used (MB)", "overhead (MB)", "time (sec)")
	mem
}
