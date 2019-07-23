
# create matter-backed ALTREP objects

as_ALTREP <- function(x, attr = list(), wrap = getOption("matter.wrap.altrep")) {
	if ( !is.matter(x) && !is.atomic && !is.list(x) )
		stop("'x' must be a matter object, atomic vector, or a list")
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
	.Call("C_makeAltrep", x, attr, nm, dm, dnm, wrap, PACKAGE="matter")
}

# coercion to ALTREP objects

setMethod("as.altrep", "ANY",
	function(x, ...) 
	{
		if ( !is.matter(x) ) {
			x <- as.matter(x)
		} else {
			stop("don't know how to ALTREP-ify object of class '", class(x), "'")
		}
		as.altrep(x)
	})

setMethod("as.altrep", "list",
	function(x, ...)
	{
		lapply(x, as.altrep)
	})

setMethod("as.altrep", "data.frame",
	function(x, ...) 
	{
		as.altrep(as.matter(x))
	})

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
	function(x, ...)
	{
		attr <- list(names=names(x))
		as_ALTREP(x, attr=attr)
	})

setMethod("as.altrep", "matter_fc",
	function(x, ...)
	{
		attr <- list(names=names(x), class="factor", levels=levels(x))
		as_ALTREP(as(x, "matter_vec"), attr=attr)
	})

setMethod("as.altrep", "matter_list",
	function(x, ...)
	{
		lapply(seq_along(x), function(i) as.altrep(subListElementAsVector(x, i)))
	})

setMethod("as.altrep", "matter_df",
	function(x, ...)
	{
		if ( is.null(rownames(x)) ) {
			row.names <- as.character(seq_len(nrow(x)))
		} else {
			row.names <- rownames(x)
		}
		attr <- list(class="data.frame", row.names=row.names)
		.Call("C_makeAltrep", as.altrep(atomdata(x)),
			attr, names(x), NULL, NULL, TRUE, PACKAGE="matter")
	})

setMethod("as.altrep", "virtual_df",
	function(x, ...)
	{
		as.altrep(as(x, "matter_df"))
	})


