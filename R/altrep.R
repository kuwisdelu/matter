
# create matter-backed ALTREP objects

as_ALTREP <- function(x) {
	.Call("C_makeMatterAltrep", x)
}

# coercion to ALTREP objects

setMethod("as.altrep", "matter_vec", function(x, ...) {
		y <- as_ALTREP(x)
		copy_mostattributes(x, y)
	})

setMethod("as.altrep", "matter_mat", function(x, ...) {
		y <- as_ALTREP(as(x, "matter_vec"))
		copy_mostattributes(x, y)
	})

setMethod("as.altrep", "matter_arr", function(x, ...) {
		y <- as_ALTREP(as(x, "matter_vec"))
		copy_mostattributes(x, y)
	})


