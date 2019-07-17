
# create matter-backed ALTREP objects

as_ALTREP <- function(x) {
	.Call("C_makeMatterAltrep", x)
}

copy_primitives <- function(x, y) {
	if ( !is.null(names(x)) )
		names(y) <- names(x)
	if ( !is.null(dim(x)) )
		dim(y) <- dim(x)
	if ( !is.null(dimnames(x)) )
		dimnames(y) <- dimnames(x)
	y
}

wrapper <- function(x, srt = 0, nna = 0) {
	.Internal(wrap_meta(x, srt, nna))
}

wrap_structure <- function(.Data, ...) {
	structure(wrapper(.Data), ...)
}

# coercion to ALTREP objects

setMethod("as.altrep", "matter_vec", function(x, ...)
{
	y <- as_ALTREP(x)
	copy_primitives(x, y)
})

setMethod("as.altrep", "matter_mat", function(x, ...)
{
	y <- as_ALTREP(as(x, "matter_vec"))
	copy_primitives(x, y)
})

setMethod("as.altrep", "matter_arr", function(x, ...)
{
	y <- as_ALTREP(as(x, "matter_vec"))
	copy_primitives(x, y)
})

setMethod("as.altrep", "matter_fc", function(x, ...)
{
	y <- as_ALTREP(as(x, "matter_vec"))
	wrap_structure(y, class="factor", levels=levels(x))
})
