
#### Define new generics from base R ####
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

setGeneric("crossprod", signature=c("x", "y"))
setGeneric("tcrossprod", signature=c("x", "y"))

#### Define new generics from stats ####
## -------------------------------------

setGeneric("prcomp")

#### Define new generics from biglm ####
## -------------------------------------

setGeneric("biglm", signature=c("formula", "data"))

#### Define new generics for summary stats ####
## --------------------------------------------

setGeneric("colVars", function(x, ...) standardGeneric("colVars"))
setGeneric("rowVars", function(x, ...) standardGeneric("rowVars"))
setGeneric("colSds", function(x, ...) standardGeneric("colSds"))
setGeneric("rowSds", function(x, ...) standardGeneric("rowSds"))

#### Basic accessor, setter, and manipulation ####
## -----------------------------------------------

setGeneric("adata", function(object) standardGeneric("adata"))
setGeneric("atomdata", function(object) standardGeneric("atomdata"))
setGeneric("atomdata<-", function(object, value) standardGeneric("atomdata<-"))
setGeneric("datamode", function(x) standardGeneric("datamode"))
setGeneric("datamode<-", function(x, value) standardGeneric("datamode<-"))
setGeneric("paths", function(x) standardGeneric("paths")) # BiocGenerics 'path()' ?
setGeneric("paths<-", function(x, value) standardGeneric("paths<-")) # BiocGenerics 'path()' ?
setGeneric("filemode", function(x) standardGeneric("filemode"))
setGeneric("filemode<-", function(x, value) standardGeneric("filemode<-"))
setGeneric("readonly", function(x) standardGeneric("readonly"))
setGeneric("readonly<-", function(x, value) standardGeneric("readonly<-"))
setGeneric("chunksize", function(x) standardGeneric("chunksize"))
setGeneric("chunksize<-", function(x, value) standardGeneric("chunksize<-"))

#### Additional generic functions for matter ####
## ----------------------------------------------

setGeneric("as.altrep", function(x, ...) standardGeneric("as.altrep"))
setGeneric("checksum", function(x, ...) standardGeneric("checksum"))

#### Additional generic functions for subclasses ####
## --------------------------------------------------

setGeneric("keys", function(object) standardGeneric("keys"))
setGeneric("keys<-", function(object, value) standardGeneric("keys<-"))
# setGeneric("tolerance", function(object, ...) standardGeneric("tolerance")) # use ProtGenerics
setGeneric("tolerance<-", function(object, ..., value) standardGeneric("tolerance<-"))
setGeneric("combiner", function(object) standardGeneric("combiner"))
setGeneric("combiner<-", function(object, value) standardGeneric("combiner<-"))

#### Internal generic functions for matter ####
## ----------------------------------------------

setGeneric("describe_for_display", function(x) standardGeneric("describe_for_display"))

setGeneric("preview_for_display", function(x) standardGeneric("preview_for_display"))

setGeneric("combine_by_cols", function(x, y, ...) {
	if ( length(list(...)) > 0L ) {
		combine_by_cols(x, do.call(combine_by_cols, list(y, ...)))
	} else {
		standardGeneric("combine_by_cols")
	}
})

setGeneric("combine_by_rows", function(x, y, ...) {
	if ( length(list(...)) > 0L ) {
		combine_by_rows(x, do.call(combine_by_rows, list(y, ...)))
	} else {
		standardGeneric("combine_by_rows")
	}
})

#### Implement basic generics from BiocGenerics ####
## ---------------------------------------------------

setMethod("combine", c("ANY", "ANY"),
	function(x, y, ...) c(x, y))

