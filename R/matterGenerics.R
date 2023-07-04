
#### Define new generics from base ####
## -------------------------------------

setGeneric("as.matrix")
setGeneric("as.array")
setGeneric("as.factor")

setGeneric("apply", signature="X")
setGeneric("scale", signature="x")

setGeneric("levels")
setGeneric("levels<-")

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

#### Basic accessor, setter, and manipulation ####
## -----------------------------------------------

setGeneric("adata", function(object, ...) standardGeneric("adata"))
setGeneric("atomdata", function(object, ...) standardGeneric("atomdata"))
setGeneric("atomdata<-", function(object, ..., value) standardGeneric("atomdata<-"))
setGeneric("readonly", function(x) standardGeneric("readonly"))
setGeneric("readonly<-", function(x, value) standardGeneric("readonly<-"))
setGeneric("chunksize", function(x) standardGeneric("chunksize"))
setGeneric("chunksize<-", function(x, value) standardGeneric("chunksize<-"))

setGeneric("aindex", function(object, ...) standardGeneric("aindex"))
setGeneric("atomindex", function(object, ...) standardGeneric("atomindex"))
setGeneric("atomindex<-", function(object, ..., value) standardGeneric("atomindex<-"))
setGeneric("pointers", function(object) standardGeneric("pointers"))
setGeneric("pointers<-", function(object, value) standardGeneric("pointers<-"))
setGeneric("domain", function(x) standardGeneric("domain"))
setGeneric("domain<-", function(x, value) standardGeneric("domain<-"))

# setGeneric("tolerance", function(object, ...) standardGeneric("tolerance")) # use ProtGenerics
setGeneric("tolerance<-", function(object, ..., value) standardGeneric("tolerance<-"))
setGeneric("sampler", function(object, ...) standardGeneric("sampler"))
setGeneric("sampler<-", function(object, ..., value) standardGeneric("sampler<-"))
setGeneric("keys", function(object) standardGeneric("keys"))
setGeneric("keys<-", function(object, value) standardGeneric("keys<-"))

#### Additional generic functions ####
## ------------------------------------

setGeneric("as.altrep", function(x, ...) standardGeneric("as.altrep"))
setGeneric("checksum", function(x, ...) standardGeneric("checksum"))

#### Internal generic functions ####
## ----------------------------------

setGeneric("describe_for_display", function(x) standardGeneric("describe_for_display"))
setGeneric("preview_for_display", function(x) standardGeneric("preview_for_display"))
setGeneric("vm_used", function(x) standardGeneric("vm_used"))

#### Implement basic generics from BiocGenerics ####
## ---------------------------------------------------

setMethod("type", "vector", function(x) typeof(x))
setMethod("type", "array", function(x) typeof(x))
setMethod("combine", c("ANY", "ANY"),
	function(x, y, ...) c(x, y))

#### Deprecated generics ####
## ---------------------------

setGeneric("paths", function(x) standardGeneric("paths")) # use BiocGenerics 'path()'
setMethod("paths", "ANY",
	function(x) {
		.Defunct("path")
		path(x)
	})

setGeneric("paths<-", function(x, value) standardGeneric("paths<-")) # use BiocGenerics 'path()'
setReplaceMethod("paths", "ANY",
	function(x, value) {
		.Defunct("path<-")
		path(x) <- value
		x
	})

setGeneric("datamode", function(x) standardGeneric("datamode"))
setMethod("datamode", "ANY",
	function(x) {
		.Defunct("type")
		type(x)
	})

setGeneric("datamode<-", function(x, value) standardGeneric("datamode<-"))
setReplaceMethod("datamode", "ANY",
	function(x, value) {
		.Defunct("datamode<-")
		type(x) <- value
		x
	})

setGeneric("filemode", function(x) standardGeneric("filemode"))
setMethod("filemode", "ANY",
	function(x) {
		.Defunct("readonly")
		if (readonly(x)) "r" else "rw"
	})

setGeneric("filemode<-", function(x, value) standardGeneric("filemode<-"))
setReplaceMethod("filemode", "ANY",
	function(x, value) {
		.Defunct("readonly<-")
		readonly(x) <- switch(value, "r"=TRUE, "rw"=FALSE)
		x
	})

setGeneric("combiner", function(object) standardGeneric("combiner"))
setMethod("combiner", "ANY",
	function(object) {
		.Defunct("sampler")
		sampler(object)
	})

setGeneric("combiner<-", function(object, value) standardGeneric("combiner<-"))
setReplaceMethod("combiner", "ANY",
	function(object, value) {
		.Defunct("sampler<-")
		sampler(object) <- value
		object
	})

setGeneric("rowVars", function(x, ...) standardGeneric("rowVars"))
setMethod("rowVars", "ANY",
	function(x, ..., na.rm = FALSE)
	{
		.Defunct("colStats")
		rowStats(x, stat="var", ..., na.rm=na.rm)
	})

setGeneric("rowSds", function(x, ...) standardGeneric("rowSds"))
setMethod("rowSds", "ANY",
	function(x, ..., na.rm = FALSE)
	{
		.Defunct("colStats")
		rowStats(x, stat="sd", ..., na.rm=na.rm)
	})

setGeneric("colVars", function(x, ...) standardGeneric("colVars"))
setMethod("colVars", "ANY",
	function(x, ..., na.rm = FALSE)
	{
		.Defunct("colStats")
		colStats(x, stat="var", ..., na.rm=na.rm)
	})

setGeneric("colSds", function(x, ...) standardGeneric("colSds"))
setMethod("colSds", "ANY",
	function(x, ..., na.rm = FALSE)
	{
		.Defunct("colStats")
		colStats(x, stat="sd", ..., na.rm=na.rm)
	})

