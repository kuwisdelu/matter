
#### Define sparse VIRTUAL class ####
## ----------------------------------

setClass("sparse_",
	slots = c(
		index = "ANY",
		keys = "ANY",
		tolerance = "numeric",
		combiner = "factor"),
	contains = c("matter", "VIRTUAL"),
	validity = function(object) {
		errors <- NULL
		if ( !is.null(object@keys) ) {
			if ( is.matter(object@index) ) {
				if ( typeof(object@keys) == "integer" && any(datamode(object@index) != "integer") )
					errors <- c(errors, paste0("data type of 'keys' [",
						typeof(object@keys), "], and 'index' [",
						datamode(object@index), "] must match"))
				if ( typeof(object@keys) == "double" && any(datamode(object@index) != "numeric") )
					errors <- c(errors, paste0("data type of 'keys' [",
						typeof(object@keys), "], and 'index' [",
						datamode(object@index), "] must match"))
			} else if ( is.atomic(object@index) ) {
				if ( typeof(object@keys) != typeof(object@index) )
					errors <- c(errors, paste0("data type of 'keys' [",
						typeof(object@keys), "], and 'index' [",
						typeof(object@index), "] must match"))
			} else {
				index_types <- vapply(object@index, typeof, character(1))
				if ( !all(typeof(object@keys) == index_types) )
					errors <- c(errors, paste0("data type of 'keys' [",
						typeof(object@keys), "] and 'index' types must match"))
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

setMethod("keys", "sparse_", function(object) object@keys)

setReplaceMethod("keys", "sparse_", function(object, value) {
	object@keys <- value
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
