
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

collapse_Rtype <- function(x) {
	sizes <- sizeof(x)
	x <- to_Rtype(x)
	x[which.max(sizes)]
}

#### Codes for C-level switch statements ####
## ------------------------------------------

as_Op <- function(x) {
	codes <- c(
		# Arith (1-7)
		"+", "-", "*", "^", "%%", "%/%", "/",
		# Compare (8-13)
		"==", ">", "<", "!=", "<=", ">=",
		# Logic (14-15)
		"&", "|")
	make_code(codes, x)
}

as_Summary <- function(x) {
	codes <- c(
		# Summary (1-7)
		"max", "min", "range", "prod", "sum", "any", "all",
		# Statistics (8-11)
		"mean", "sd", "var", "nnzero")
	make_code(codes, x)
}

as_Math <- function(x) {
	# FIXME: add support for more later
	codes <- c("exp", "log", "log10", "log2", "log1p")
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
	tol_type <- factor(type, levels=c(1L, 2L), labels=codes)
	structure(as.integer(tol), tol_type=tol_type)
}

as_tol_ref <- function(x) {
	codes <- c("abs", "x", "y")
	make_code(codes, x[1L], nomatch=1L)
}

# export
as_kernel <- function(x) {
	codes <- c(
		# simple interp (1-5)
		"none", "mean", "sum", "max", "min",
		# peak-based (6)
		"auc",
		# spline interp (7-8)
		"linear", "cubic",
		# kernel interp (9-10)
		"gaussian", "lanczos")
	make_code(codes, x)
}

#### Miscellaneous utility functions ####
## --------------------------------------

is_nil <- function(x) is.na(x) || is.null(x)

set_names <- function(x, nm, i) {
	if ( !missing(i) && !is.null(i) )
		nm <- nm[i]
	names(x) <- nm
	x
}

set_dimnames <- function(x, dnm, index) {
	if ( !missing(index) && !is.null(index) )
		for ( i in seq_along(index) ) {
			j <- index[[i]]
			if ( !is.null(dnm[[i]]) && !is.null(j) )
				dnm[[i]] <- dnm[[i]][j]
		}
	dimnames(x) <- dnm
	x
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

