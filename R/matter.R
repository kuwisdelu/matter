
#### VIRTUAL 'matter' class ####
## ------------------------------

setClassUnion("numeric_OR_NULL", c("numeric", "NULL"))
setClassUnion("character_OR_NULL", c("character", "NULL"))
setClassUnion("list_OR_NULL", c("list", "NULL"))

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

is.matter <- function(x) {
	is(x, "matter")
}

setMethod("adata", "matter", function(object) atomdata(object))

setMethod("atomdata", "matter", function(object) object@data)

setReplaceMethod("atomdata", "matter", function(object, value) {
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
		stop("dims [product ", prod(value), "] do not match ",
			"the length of object [", prod(x@dim), "]")
	x@dim <- value
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
		value <- lapply(value, as.character)
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

setMethod("vm_used", "ANY", function(x) {
	size_bytes(NA_real_)
})

setMethod("vm_used", "matter_", function(x) {
	vm_used(atomdata(x))
})

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
	function(x, algo="sha1", ...) {
		checksum(x@data, algo=algo, ...)
	})

