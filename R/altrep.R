
# create matter-backed ALTREP objects

as_ALTREP <- function(x, attr = list(), wrap = getOption("matter.wrap.altrep")) {
	if ( !is.matter(x) )
		stop("'x' must be a matter object")
	if ( !is.list(attr) )
		stop("'attr' must be a list")
	nm <- dm <- dnm <- NULL
	if ( !is.na(inm <- match("names", names(attr))) ) {
		nm <- attr[[inm]]
		attr <- attr[-inm]
	}
	if ( !is.na(idm <- match("dim", names(attr))) ) {
		dm <- attr[[idm]]
		attr <- attr[-idm]
	}
	if ( !is.na(idnm <- match("dimnames", names(attr))) ) {
		dnm <- attr[[idnm]]
		attr <- attr[-idnm]
	}
	.Call("C_makeMatterAltrep", x, attr, nm, dm, dnm, wrap, PACKAGE="matter")
}

# coercion to ALTREP objects

setMethod("as.altrep", "matter_vec",
	function(x, ...) 
	{
		attr <- list(names=names(x))
		as_ALTREP(x, attr=attr)
	})

setMethod("as.altrep", "matter_mat",
	function(x, ...)
	{
		attr <- list(names=names(x), dim=dim(x), dimnames=dimnames(x))
		as_ALTREP(as(x, "matter_vec"), attr=attr)
	})

setMethod("as.altrep", "matter_arr",
	function(x, ...)
	{
		attr <- list(names=names(x), dim=dim(x), dimnames=dimnames(x))
		as_ALTREP(as(x, "matter_vec"), attr=attr)
	})

setMethod("as.altrep", "matter_str",
	function(x, ...) as_ALTREP(x))

setMethod("as.altrep", "matter_fc",
	function(x, ...)
	{
		attr <- list(names=names(x), class="factor", levels=levels(x))
		as_ALTREP(as(x, "matter_vec"), attr=attr)
	})

