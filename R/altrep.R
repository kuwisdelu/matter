
# create matter-backed ALTREP objects

as_ALTREP <- function(x) {
	y <- .Call("C_makeMatterAltrep", x)
	copy_mostattributes(x, y)
}

# coercion to ALTREP objects

setMethod("as.altrep", "matter_vec", function(x, ...) as_ALTREP(x))

setMethod("as.altrep", "matter_mat", function(x, ...) as_ALTREP(as(x, "matter_vec")))

setMethod("as.altrep", "matter_arr", function(x, ...) as_ALTREP(as(x, "matter_vec")))


