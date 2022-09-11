
#### 'deferred_ops' for deferred operations ####
## ----------------------------------------------

setClass("deferred_ops",
	slots = c(
		ops = "factor",
		args = "list",
		groups = "integer"),
	validity = function(object) {
		errors <- NULL
		if ( is.null(object@dim) )
			errors <- c(errors, "array must have non-NULL 'dim'")
			if ( length(object@type) != 1L )
			errors <- c(errors, "'type' must be a scalar (length 1)")
		if ( length(object@transpose) != 1L )
			errors <- c(errors, "'transpose' must be a scalar (length 1)")
		if ( is.null(errors) ) TRUE else errors
	})


