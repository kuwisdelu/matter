
# create matter-backed ALTREP objects

new_matter_ALTREP <- function(x, attr = list(),
	wrap = getOption("matter.wrap.altrep"))
{
	if ( !is.matter(x) && !is.atomic(x) && !is.list(x) )
		stop("'x' must be a matter object, atomic vector, or a list")
	if ( !is.list(attr) )
		stop("'attr' must be a list")
	wrap <- isTRUE(wrap)
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
	.Call(C_newMatterAltrep, x, attr, nm, dm, dnm, wrap, PACKAGE="matter")
}

# coercion to ALTREP objects

setMethod("as.altrep", "matter_arr",
	function(x, ...)
	{
		attr <- list(names=names(x), dim=dim(x), dimnames=dimnames(x))
		new_matter_ALTREP(x, attr=attr)
	})

setMethod("as.altrep", "matter_fct",
	function(x, ...)
	{
		attr <- list(names=names(x), class="factor", levels=levels(x))
		new_matter_ALTREP(as(x, "matter_vec"), attr=attr)
	})

setMethod("as.altrep", "matter_str",
	function(x, ...)
	{
		attr <- list(names=names(x))
		new_matter_ALTREP(x, attr=attr)
	})

setMethod("as.altrep", "matter_list",
	function(x, ...)
	{
		stop("ALTREP for matter list not implemented yet") # TODO
	})
