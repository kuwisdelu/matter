
#### Define sparse VIRTUAL class ####
## ----------------------------------

setClass("sparse_",
	slots = c(
		index = "ANY",
		domain = "ANY",
		tolerance = "numeric",
		combiner = "factor"),
	contains = c("matter", "VIRTUAL"),
	validity = function(object) {
		errors <- NULL
		if ( !is.null(object@domain) ) {
			if ( is.matter(object@index) ) {
				if ( typeof(object@domain) == "integer" && any(datamode(object@index) != "integer") )
					errors <- c(errors, paste0("data type of 'domain' [",
						typeof(object@domain), "], and 'index' [",
						datamode(object@index), "] must match"))
				if ( typeof(object@domain) == "double" && any(datamode(object@index) != "numeric") )
					errors <- c(errors, paste0("data type of 'domain' [",
						typeof(object@domain), "], and 'index' [",
						datamode(object@index), "] must match"))
			} else if ( is.atomic(object@index) ) {
				if ( typeof(object@domain) != typeof(object@index) )
					errors <- c(errors, paste0("data type of 'domain' [",
						typeof(object@domain), "], and 'index' [",
						typeof(object@index), "] must match"))
			} else {
				index_types <- vapply(object@index, typeof, character(1))
				if ( !all(typeof(object@domain) == index_types) )
					errors <- c(errors, paste0("data type of 'domain' [",
						typeof(object@domain), "] and 'index' types must match"))
			}
		}
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
	object@tolerance <- make_tolerance(value)
	object
})

setMethod("combiner", "sparse_", function(object) object@combiner)

setReplaceMethod("combiner", "sparse_", function(object, value) {
	object@combiner <- make_combiner(value)
	object
})
