
#### Define new generics from base ####
## -------------------------------------

setGeneric("as.vector")
setGeneric("as.array")
setGeneric("as.matrix")
setGeneric("as.factor")

setGeneric("levels")
setGeneric("levels<-")
setGeneric("droplevels")

setGeneric("Encoding")
setGeneric("Encoding<-")

setGeneric("crossprod")
setGeneric("tcrossprod")

#### Define new generics from stats ####
## -------------------------------------

setGeneric("prcomp")
setGeneric("preplot")

#### Define new generics for summary stats ####
## --------------------------------------------

setGeneric("rowsweep", signature=c("x"),
	function(x, STATS, FUN = "-", ...)
		standardGeneric("rowsweep"))

setGeneric("colsweep", signature=c("x"),
	function(x, STATS, FUN = "-", ...)
		standardGeneric("colsweep"))

setGeneric("rowscale", signature=c("x"),
	function(x, center = TRUE, scale = TRUE, ...)
		standardGeneric("rowscale"))

setGeneric("colscale", signature=c("x"),
	function(x, center = TRUE, scale = TRUE, ...)
		standardGeneric("colscale"))

setGeneric("rowStats", function(x, ...) standardGeneric("rowStats"))
setGeneric("colStats", function(x, ...) standardGeneric("colStats"))

setGeneric("rowDists", function(x, y, ...) standardGeneric("rowDists"))
setGeneric("colDists", function(x, y, ...) standardGeneric("colDists"))

#### Basic accessor, setter, and manipulation ####
## -----------------------------------------------

setGeneric("adata", function(object, ...) standardGeneric("adata"))
setGeneric("atomdata", function(object, ...) standardGeneric("atomdata"))
setGeneric("atomdata<-", function(object, ..., value) standardGeneric("atomdata<-"))
setGeneric("readonly", function(x) standardGeneric("readonly"))
setGeneric("readonly<-", function(x, value) standardGeneric("readonly<-"))

setGeneric("aindex", function(object, ...) standardGeneric("aindex"))
setGeneric("atomindex", function(object, ...) standardGeneric("atomindex"))
setGeneric("atomindex<-", function(object, ..., value) standardGeneric("atomindex<-"))
setGeneric("pointers", function(object) standardGeneric("pointers"))
setGeneric("pointers<-", function(object, value) standardGeneric("pointers<-"))
setGeneric("domain", function(x) standardGeneric("domain"))
setGeneric("domain<-", function(x, value) standardGeneric("domain<-"))

setMethod("domain", "vector", function(x) attr(x, "domain"))
setReplaceMethod("domain", "vector",
	function(x, value) {
		attr(x, "domain") <- value
		x
	})
setMethod("domain", "array", function(x) attr(x, "domain"))
setReplaceMethod("domain", "array",
	function(x, value) {
		attr(x, "domain") <- value
		x
	})

# setGeneric("tolerance", function(object, ...) standardGeneric("tolerance")) # use ProtGenerics
setGeneric("tolerance<-", function(object, ..., value) standardGeneric("tolerance<-"))
setGeneric("sampler", function(object, ...) standardGeneric("sampler"))
setGeneric("sampler<-", function(object, ..., value) standardGeneric("sampler<-"))

#### Additional generic functions ####
## ------------------------------------

setGeneric("as.altrep", function(x, ...) standardGeneric("as.altrep"))
setGeneric("checksum", function(x, ...) standardGeneric("checksum"))

setGeneric("rowMaj", function(x) standardGeneric("rowMaj"))
setMethod("rowMaj", "matrix", function(x) TRUE)
setMethod("rowMaj", "Matrix", function(x) {
	if ( is(x, "RsparseMatrix") ) {
		FALSE
	} else if ( is(x, "generalMatrix") ) {
		TRUE
	} else {
		NA
	}
})

#### Internal generic functions ####
## ----------------------------------

setGeneric("describe_for_display", function(x) standardGeneric("describe_for_display"))
setGeneric("preview_for_display", function(x) standardGeneric("preview_for_display"))
setGeneric("vm_used", function(x) standardGeneric("vm_used"))
setMethod("vm_used", "ANY", function(x) size_bytes(NA_real_))
setMethod("vm_used", "array", function(x) size_bytes(0))
setMethod("vm_used", "vector", function(x) {
	if ( is.atomic(x) ) {
		size_bytes(0)
	} else {
		size_bytes(NA_real_)
	}
})
setGeneric("vm_realized", function(x) standardGeneric("vm_realized"))
setMethod("vm_realized", "ANY", function(x) {
	size_bytes(sum(mem(x), na.rm=TRUE))
})

#### Implement basic generics from BiocGenerics ####
## ---------------------------------------------------

setMethod("type", "vector", function(x) typeof(x))
setMethod("type", "array", function(x) typeof(x))
setMethod("combine", c("ANY", "ANY"),
	function(x, y, ...) c(x, y))

