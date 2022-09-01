
#### Codes for C-level functions ####
## -----------------------------------

make_code <- function(codes, x) {
	if ( missing(x) )
		x <- character()
	if ( is.factor(x) && all(levels(x) == codes) )
		return(x)
	if ( !is.numeric(x) )
		x <- pmatch(tolower(x), codes)
	factor(x, levels=seq_along(codes), labels=codes)
}

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
	codes <- c("log", "log10", "log2", "log1p")
	make_code(codes, x)
}

as_interp <- function(x) {
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

as_tol <- function(x) {
	tol <- x[1L]
	codes <- c("absolute", "relative")
	if ( !is.null(names(tol)) ) {
		tol_type <- pmatch(names(tol), codes, nomatch=1L)
	} else {
		tol_type <- 1L
	}
	tol_type <- factor(type, levels=c(1, 2), labels=codes)
	structure(as.vector(tol), tol_type=tol_type)
}

#### Miscellaneous utility functions ####
## --------------------------------------

is_nil <- function(x) is.na(x) || is.null(x)

check_comformable_dims <- function(x, y, margin = 1) {
	if ( is.vector(x) ) {
		return(check_comformable_dims(y, x))
	} else if ( length(y) != 1 && length(y) != dim(x)[margin] ) {
		return("argument length is non-conformable with array dimensions")
	}
	TRUE
}

check_comformable_lengths <- function(x, y, margin = 1) {
	if ( is.vector(x) ) {
		return(check_comformable_dims(y, x))
	} else if ( length(y) != 1 && length(x) != length(y) ) {
		return("argument length is non-conformable with array length")
	}
	TRUE
}


