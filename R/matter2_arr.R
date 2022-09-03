
#### 'matter_arr' class for file-based arrays ####
## ----------------------------------------------

setClass("matter2_arr",
	slots = c(
		ops = "list_OR_NULL",
		tranpose = "logical"),
	contains = "matter2_",
	validity = function(object) {
		errors <- NULL
		if ( is.null(object@dim) )
			errors <- c(errors, "array must have non-NULL 'dim'")
		if ( length(object@tranpose) != 1L )
			errors <- c(errors, "'tranpose' must be a scalar (length 1)")
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
			data <- vector(collapse_Rtype(type), length=1L)
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
	# if ( !missing(data) )
	# 	x[] <- data
	x
}

setMethod("describe_for_display", "matter2_arr", function(x) {
	desc1 <- paste0("<", paste0(x@dim, collapse=" x "), " dim> ", class(x))
	desc2 <- paste0("out-of-memory ", x@datamode, " array")
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

set_matter_arr_elts <- function(x, i = NULL) {
	.Call("C_setMatterArray", x, i)
}

# setMethod("[", c(x="matter2_arr"),
# 	function(x, i, j, ..., drop = TRUE) {
# 		if ( ...length() > 0 )
# 			stop("incorrect number of dimensions")
# 		i <- as_subscripts(i, x)
# 		j <- as_subscripts(j, x)
# 		if ( nargs() - 1L == 1L ) {
# 			subset_atoms_1d(x, i)
# 		} else {
# 			subset_atoms(x, i, j)
# 		}
# 	})

