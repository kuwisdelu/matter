
#### Define sparse VIRTUAL class ####
## ----------------------------------

setClassUnion("matter_OR_numeric", c("matter_vec", "integer", "numeric"))
setClassUnion("matter_OR_list", c("matter_list", "list"))
setClassUnion("matter_OR_numeric_OR_list", c("matter_OR_numeric", "matter_OR_list"))

setClass("sparse_",
	slots = c(
		index = "ANY",
		offset = "integer",
		domain = "numeric_OR_NULL",
		pointers = "numeric_OR_NULL",
		tolerance = "numeric",
		sampler = "factor"),
	contains = c("matter", "VIRTUAL"),
	validity = function(object) {
		errors <- NULL
		if ( length(object@offset) != 1L )
			errors <- c(errors, "'offset' must be scalar (length 1)")
		if ( !is.null(object@domain) && anyNA(object@domain) )
			errors <- c(errors, "'domain' can't have missing values")
		if ( is.null(errors) ) TRUE else errors
	})

setMethod("aindex", "sparse_", function(object) atomindex(object))

setMethod("atomindex", "sparse_", function(object) object@index)

setReplaceMethod("atomindex", "sparse_", function(object, value) {
	object@index <- value
	if ( validObject(object) )
		object
})

setMethod("domain", "sparse_", function(object) object@domain)

setReplaceMethod("domain", "sparse_", function(object, value) {
	object@domain <- value
	if ( validObject(object) )
		object
})

setMethod("tolerance", "sparse_", function(object) object@tolerance)

setReplaceMethod("tolerance", "sparse_", function(object, value) {
	object@tolerance <- as_tol(value)
	object
})

setMethod("sampler", "sparse_", function(object) object@sampler)

setReplaceMethod("sampler", "sparse_", function(object, value) {
	object@sampler <- as_kern(value)
	object
})
