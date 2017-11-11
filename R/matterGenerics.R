
#### Define new generics from base R ####
## -------------------------------------

setGeneric("as.matrix")
setGeneric("as.array")

setGeneric("apply", signature="X")
setGeneric("scale", signature="x")

setGeneric("levels")
setGeneric("levels<-")

setGeneric("Encoding")
setGeneric("Encoding<-")

t.matter <- function(x) t(x)

mean.matter <- function(x, ...) mean(x, ...)

scale.matter <- function(x, center = TRUE, scale = TRUE) {
	scale(x, center = center, scale = scale)
}

setGeneric("crossprod", signature=c("x", "y"))
setGeneric("tcrossprod", signature=c("x", "y"))

#### Define new generics from stats ####
## -------------------------------------

setGeneric("prcomp")

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
setGeneric("paths", function(x) standardGeneric("paths"))
setGeneric("paths<-", function(x, value) standardGeneric("paths<-"))
setGeneric("filemode", function(x) standardGeneric("filemode"))
setGeneric("filemode<-", function(x, value) standardGeneric("filemode<-"))
setGeneric("readonly", function(x) standardGeneric("readonly"))
setGeneric("readonly<-", function(x, value) standardGeneric("readonly<-"))
setGeneric("chunksize", function(x) standardGeneric("chunksize"))
setGeneric("chunksize<-", function(x, value) standardGeneric("chunksize<-"))

#### Additional generic functions for matter ####
## ----------------------------------------------

setGeneric("checksum", function(x, ...) standardGeneric("checksum"))
setGeneric("chunker", function(x, ...) standardGeneric("chunker"))

#### Additional generic functions for subclasses ####
## --------------------------------------------------

setGeneric("keys", function(object) standardGeneric("keys"))
setGeneric("keys<-", function(object, value) standardGeneric("keys<-"))
setGeneric("tolerance", function(object) standardGeneric("tolerance"))
setGeneric("tolerance<-", function(object, value) standardGeneric("tolerance<-"))

#### Internal generic functions for matter ####
## ----------------------------------------------

setGeneric("describe_for_display", function(x) standardGeneric("describe_for_display"))
setGeneric("combine_by_cols", function(x, y, ...) standardGeneric("combine_by_cols"))
setGeneric("combine_by_rows", function(x, y, ...) standardGeneric("combine_by_rows"))

#### Implement basic generics from BiocGenerics ####
## ---------------------------------------------------

setMethod("combine", c("numeric", "numeric"),
	function(x, y, ...) c(x, y))

