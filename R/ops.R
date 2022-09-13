
#### 'deferred_ops' for deferred operations ####
## ----------------------------------------------

setClass("deferred_ops",
	slots = c(
		ops = "factor",  # deferred op codes (Arith/Math)
		arg = "list",
		rhs = "logical", # is the original object rhs?
		margin = "integer",
		group = "list"),
	validity = function(object) {
		errors <- NULL
		lens <- c(
			ops=length(object@ops),
			arg=length(object@arg),
			rhs=length(object@rhs),
			margin=length(object@margin),
			group=length(object@group))
		if ( length(unique(lens)) != 1 )
			errors <- c(errors, paste0("lengths of ",
				"'ops' [", lens["ops"], "], ",
				"'arg' [", lens["arg"], "], ",
				"'rhs' [", lens["rhs"], "], ",
				"'margin' [", lens["margin"], "], ",
				"and 'group' [", lens["group"], "] ",
				"must all be equal"))
		if ( anyNA(object@ops) )
			errors <- c(errors, "'ops' can't have missing values")
		for ( i in seq_along(object@ops) ) {
			if ( !is.null(object@group[[i]]) ) {
				ngroups <- length(unique(object@group[[i]]))
				if ( ngroups != ncol(object@arg[[i]]) )
					errors <- c(errors,
						paste0("number of groups [", ngroups, "] ",
						"does not match ncol of 'arg'",
						" [", ncol(object@arg[[i]]), "]"))
			}
			if ( !is.null(object@arg[[i]]) ) {
				if ( is.na(object@rhs[i]) )
					errors <- c(errors,
						"'rhs' can't be missing for non-NULL 'arg'")
				if ( is.na(object@margin[i]) && length(object@arg[[i]]) != 1L )
					errors <- c(errors,
						"'margin' can't be missing for non-scalar 'arg'")
			}
		}
		if ( is.null(errors) ) TRUE else errors
	})

append_op <- function(object, op, arg = NULL,
	rhs = FALSE, margin = NA_integer_, group = NULL)
{
	if ( !is.null(group) ) {
		group <- as.integer(group)
		group <- group - min(group)
	}
	if ( is.null(object) ) {
		new("deferred_ops", ops=op, arg=list(arg),
			rhs=rhs, margin=margin, group=list(group))
	} else {
		object@ops <- c(object@ops, op)
		object@arg <- c(object@arg, list(arg))
		object@rhs <- c(object@rhs, rhs)
		object@margin <- c(object@margin, margin)
		object@group <- c(object@group, list(group))
		if ( validObject(object) )
			object
	}
}

register_op <- function(x, op, arg = NULL, rhs = FALSE)
{
	op <- as_Ops(op)
	if ( is.null(arg) ) {
		margin <- NA_integer_
	} else if ( is.null(dim(x)) || is.null(dim(arg)) ) {
		margin <- 1L
	} else {
		if ( rhs ) {
			rdim <- dim(x)
			ldim <- dim(arg)
		} else {
			rdim <- dim(arg)
			ldim <- dim(x)
		}
		if ( length(ldim) != length(rdim) )
			stop(paste0("number of dimensions are not equal for ",
				"lhs [", length(ldim), "] and rhs [", length(rdim), "]"))
		margin <- which(dim(arg) != 1L)
	}
	if ( length(margin) < 1L )
		margin <- 1L
	if ( length(margin) != 1L )
		stop("only a single dim of argument may be unequal to 1")
	arg <- as.vector(arg)
	xlen <- if (is.null(dim(x))) length(x) else dim(x)[margin]
	if ( !is.na(margin) && length(arg) != 1L ) {
		if ( rhs ) {
			rext <- xlen
			lext <- length(arg)
		} else {
			rext <- length(arg)
			lext <- xlen
		}
		if ( lext != rext )
			stop(paste0("extent of array is not equal for ",
				"lhs [", lext, "] and rhs [", rext, "]"))
	}
	x@ops <- append_op(x@ops, op=op, arg=arg, rhs=rhs, margin=margin)
	x@type <- as_Rtype("double")
	if ( validObject(x) )
		x
}

register_group_op <- function(x, op, group, arg = NULL,
	rhs = FALSE, margin = NA_integer_)
{
	op <- as_Ops(op)
	if ( is.na(margin) || length(margin) != 1L )
		stop("margin must be a non-missing scalar (length-1)")
	xlen <- if (is.null(dim(x))) length(x) else dim(x)[margin]
	if ( rhs ) {
		rext <- xlen
		lext <- nrow(arg)
	} else {
		rext <- nrow(arg)
		lext <- xlen
	}
	if ( lext != rext )
		stop(paste0("extent of array is not equal for ",
			"lhs [", lext, "] and rhs [", rext, "]"))
	x@ops <- append_op(x@ops, op=op, arg=arg,
		rhs=rhs, margin=margin, group=group)
	x@type <- as_Rtype("double")
	if ( validObject(x) )
		x
}
 