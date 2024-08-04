
#### VIRTUAL 'matter' class ####
## ------------------------------

setClass("matter",
	slots = c(
		data = "ANY",
		type = "factor",
		dim = "numeric_OR_NULL",
		names = "character_OR_NULL",
		dimnames = "list_OR_NULL"),
	contains = "VIRTUAL",
	validity = function(object) {
		errors <- NULL
		if ( anyNA(object@type) )
			errors <- c(errors, "'type' must not contain missing values")
		if ( !is.null(object@names) && length(object@names) != length(object) )
			errors <- c(errors, paste0("names [length ", length(object@names), "] ",
				"do not match length of object [", length(object), "]"))
		if ( !is.null(dimnames) && is.null(dim) )
			errors <- c(errors, "'dimnames' applied to non-array")
		if ( !is.null (object@dimnames) )
		{
			if ( is.null(object@dim) )
				errors <- c(errors, "'dimnames' applied to non-array")
			if ( length(object@dimnames) != length(object@dim) )
				errors <- c(errors, paste0("length of 'dimnames' [", length(object@dimnames), "] ",
					"must match that of 'dims' [", length(object@dim), "]"))
			for ( i in seq_along(object@dimnames) )
			{
				dmn <- object@dimnames[[i]]
				if ( !is.null(dmn) && length(dmn) != object@dim[i] )
					errors <- c(errors, paste0("length of 'dimnames' [", i, "] ",
						"not equal to array extent"))
			}
		}
		if ( is.null(errors) ) TRUE else errors
	})

setMethod("show", "matter", function(object) {
	cat(describe_for_display(object), "\n", sep="")
	if ( getOption("matter.show.head") )
		try(preview_for_display(object), silent=TRUE)
})

matter <- function(...) {
	nm <- ...names()
	if ( nargs() == 1L ) {
		data <- ...elt(1)
	} else if ( "data" %in% nm ) {
		data <- list(...)$data
	} else {
		data <- NULL
	}
	if ( nargs() == 1L && !is.null(data) )
		return(as.matter(data))
	arg_arr <- c("dim", "dimnames")
	arg_mat <- c("nrow", "ncol")
	arg_vec <- c("length", "names")
	arg_fct <- c("levels", "labels")
	arg_list <- c("lengths")
	arg_str <- c("nchar")
	known_args <- c(arg_arr, arg_mat, arg_vec,
		arg_fct, arg_list, arg_str)
	if ( any(nm %in% known_args) ) {
		if ( any(arg_arr %in% nm) ) {
			matter_arr(...)
		} else if ( any(arg_mat %in% nm) ) {
			matter_mat(...)
		} else if ( any(arg_vec %in% nm) ) {
			matter_vec(...)
		} else if ( any(arg_fct %in% nm) ) {
			matter_fct(...)
		} else if ( any(arg_list %in% nm) ) {
			matter_list(...)
		} else if ( any(arg_str %in% nm) ) {
			matter_str(...)
		} else {
			matter_error("couldn't guess data structure, use 'matter_*' functions")
		}
	} else if ( !is.null(data) ) {
		if ( is.array(data) ) {
			matter_arr(...)
		} else if ( is.matrix(data) ) {
			matter_mat(...)
		} else if ( is.atomic(data) ) {
			matter_vec(...)
		} else if ( is.factor(data) ) {
			matter_fct(...)
		} else if ( is.list(data) ) {
			matter_list(...)
		} else if ( is.character(data) ) {
			matter_str(...)
		} else {
			matter_error("couldn't guess data structure, use 'matter_*' functions")
		}
	} else {
		matter_error("couldn't guess data structure, use 'matter_*' functions")
	}
}

is.matter <- function(x) {
	is(x, "matter")
}

as.matter <- function(x) {
	if ( is.matter(x) )
		return(x)
	if ( is.array(x) ) {
		matter_arr(x)
	} else if ( is.matrix(x) ) {
		matter_mat(x)
	} else if ( is.atomic(x) ) {
		matter_vec(x)
	} else if ( is.factor(x) ) {
		matter_fct(x)
	} else if ( is.list(x) ) {
		matter_list(x)
	} else if ( is.character(x) ) {
		matter_str(x)
	} else {
		matter_error("cannot coerce object of class ",
			sQuote(class(x)), " to a 'matter' object")
	}
}

setMethod("adata", "matter",
	function(object, ...) atomdata(object, ...))

setMethod("atomdata", "matter",
	function(object, ...) object@data)

setReplaceMethod("atomdata", "matter", function(object, ..., value) {
	object@data <- value
	if ( validObject(object) )
		object
})

setMethod("type", "matter", function(x) x@type)

setReplaceMethod("type", "matter", function(x, value) {
	x@type <- as_Rtype(value)
	if ( validObject(x) )
		x
})

setMethod("length", "matter", function(x) prod(x@dim))

setMethod("dim", "matter", function(x) x@dim)

setReplaceMethod("dim", "matter", function(x, value) {
	if ( prod(x@dim) != prod(value) )
		matter_error("dims [product ", prod(value), "] do not match ",
			"the length of object [", prod(x@dim), "]")
	x@dim <- value
	x@dimnames <- NULL
	if ( validObject(x) )
		x
})

setMethod("names", "matter", function(x) x@names)

setReplaceMethod("names", "matter", function(x, value) {
	if ( !is.null(value) )
		value <- as.character(value)
	x@names <- value
	if ( validObject(x) )
		x
})

setMethod("dimnames", "matter", function(x) x@dimnames)

setReplaceMethod("dimnames", "matter", function(x, value) {
	if ( !is.null(value) )
		value <- lapply(value, function(v) if(!is.null(v)) as.character(v))
	x@dimnames <- value
	if ( validObject(x) )
		x
})

#### VIRTUAL 'matter_' class for file-based data structures ####
## -----------------------------------------------------------

setClass("matter_",
	slots = c(
		data = "atoms",
		type = "factor",
		dim = "numeric_OR_NULL",
		names = "character_OR_NULL",
		dimnames = "list_OR_NULL"),
	contains = c("VIRTUAL", "matter"))

setMethod("describe_for_display", "ANY", function(x) class(x))

setMethod("preview_for_display", "ANY", function(x) head(x))

setMethod("shm_used", "matter_", function(x) shm_used(atomdata(x)))

setMethod("vm_used", "matter_", function(x) vm_used(atomdata(x)))

setMethod("show", "matter_", function(object) {
	callNextMethod()
	show_matter_mem(object)
})

setMethod("path", "matter_", function(object, ...) path(object@data))

setReplaceMethod("path", "matter_",
	function(object, ..., value) {
		path(object@data) <- value
		object
	})

setMethod("readonly", "matter_", function(x) readonly(x@data))

setReplaceMethod("readonly", "matter_",
	function(x, value) {
		readonly(x@data) <- value
		x
	})

setMethod("checksum", "matter_",
	function(x, algo = "sha1", ...) {
		checksum(path(x), algo=algo, ...)
	})

