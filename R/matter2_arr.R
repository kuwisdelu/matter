
#### 'matter_arr' class for file-based arrays ####
## ----------------------------------------------

setClass("matter2_arr",
	slots = c(
		ops = "list_OR_NULL",
		transpose = "logical"),
	contains = "matter2_",
	validity = function(object) {
		errors <- NULL
		if ( is.null(object@dim) )
			errors <- c(errors, "array must have non-NULL 'dim'")
		if ( length(object@transpose) != 1L )
			errors <- c(errors, "'transpose' must be a scalar (length 1)")
		if ( is.null(errors) ) TRUE else errors
	})

matter2_arr <- function(data, type = "double", path = NULL,
	dim = 0, dimnames = NULL, offset = 0, extent = prod(dim),
	readonly = file.exists(path), rowMaj = FALSE, ...)
{
	if ( !missing(data) ) {
		data <- as.array(data)
		if ( missing(type) )
			type <- typeof(data)
		if ( missing(dim) )
			dim <- dim(data)
	}
	if ( is.null(path) && all(dim == 0) )
		return(new("matter2_arr"))
	if ( is.null(path) )
		path <- tempfile(tmpdir=getOption("matter.dump.dir"), fileext=".bin")
	path <- normalizePath(path, mustWork=FALSE)
	exists <- file.exists(path)
	readonly <- force(readonly)
	if ( all(exists) ) {
		if ( missing(data) ) {
			if ( missing(dim) && missing(extent) ) {
				# attempt to infer data size from file(s)
				sizes <- file.size(path)
				if ( length(type) == 1L ) {
					dim <- sum(sizes - offset) %/% sizeof(type)
				} else {
					dim <- sum((sizes - offset) %/% sizeof(type))
				}
			}
		} else if ( !readonly ) {
			warning("data may overwrite existing file(s): ", sQuote(path[exists]))
		}
	} else {
		if ( missing(data) ) {
			data <- vector(as.character(collapse_Rtype(type)), length=1L)
		} else if ( any(exists) && !readonly ) {
			warning("data may overwrite existing file(s): ", sQuote(path[exists]))
		}
		# create files if they don't exist
		success <- file.create(path)
		if ( !all(success) )
			stop("error creating file(s): ", sQuote(path[!success]))
	}
	x <- new("matter2_arr",
		data=atoms2(
			source=path,
			type=type,
			offset=offset,
			extent=extent,
			group=0L,
			readonly=readonly),
		type=collapse_Rtype(type),
		dim=dim,
		names=NULL,
		dimnames=dimnames,
		ops=NULL,
		transpose=rowMaj)
	if ( !missing(data) )
		x[] <- data
	x
}

setMethod("describe_for_display", "matter2_arr", function(x) {
	desc1 <- paste0("<", paste0(dim(x), collapse=" x "), " dim> ", class(x))
	desc2 <- paste0("out-of-memory ", type(x), " array")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "matter2_arr", function(x) {
	if ( length(dim(x)) < 2L ) {
		preview_vector(x)
	} else if ( length(dim(x)) == 2L ) {
		preview_matrix(x)
	} else {
		preview_Nd_array(x)
	}
})

get_matter_arr_elts <- function(x, i = NULL) {
	.Call("C_getMatterArray", x, i)
}

set_matter_arr_elts <- function(x, i = NULL, value = 0L) {
	.Call("C_setMatterArray", x, i, value)
}

setMethod("[", c(x="matter2_arr"),
	function(x, i, j, ..., drop = TRUE) {
		narg <- nargs() - 1L - !missing(drop)
		if ( missing(i) )
			i <- NULL
		if ( missing(j) )
			j <- NULL
		if ( narg == 1L ) {
			get_matter_arr_elts(x, i)
		} else {
			index <- list(i, j, ...)
			index <- as_array_subscripts(index, x)
			k <- linear_ind(index, dim(x))
			y <- get_matter_arr_elts(x, k)
			dim(y) <- attr(index, "dim")
			y
		}
	})

setReplaceMethod("[", c(x="matter2_arr"),
	function(x, i, j, ..., value) {
		narg <- nargs() - 2L
		if ( missing(i) )
			i <- NULL
		if ( missing(j) )
			j <- NULL
		if ( narg == 1L ) {
			set_matter_arr_elts(x, i, value)
		} else {
			index <- list(i, j, ...)
			index <- as_array_subscripts(index, x)
			k <- linear_ind(index, dim(x))
			set_matter_arr_elts(x, k, value)
		}
	})
