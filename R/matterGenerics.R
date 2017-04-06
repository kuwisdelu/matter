
#### Define new generics from base R ####
## -------------------------------------

setGeneric("t")
setGeneric("sum")
setGeneric("mean")
# setGeneric("var", signature="x") # Use BiocGenerics
# setGeneric("sd", signature="x") # Use BiocGenerics
# setGeneric("colSums", signature="x") # Use BiocGenerics
# setGeneric("rowSums", signature="x") # Use BiocGenerics
# setGeneric("colMeans", signature="x") # Use BiocGenerics
# setGeneric("rowMeans", signature="x") # Use BiocGenerics
setGeneric("apply", signature="X")
setGeneric("scale", signature="x")

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

# Do these conditionally in case user has generics from matrixStats package

if ( !isGeneric("colVars") )
	setGeneric("colVars", function(x, na.rm = FALSE) standardGeneric("colVars"))
if ( !isGeneric("rowVars") )
	setGeneric("rowVars", function(x, na.rm = FALSE) standardGeneric("rowVars"))
if ( !isGeneric("colSds") )
	setGeneric("colSds", function(x, na.rm = FALSE) standardGeneric("colSds"))
if ( !isGeneric("rowSds") )
	setGeneric("rowSds", function(x, na.rm = FALSE) standardGeneric("rowSds"))

#### Basic accessor, setter, and manipulation ####
## -----------------------------------------------	

setGeneric("adata", function(object) standardGeneric("adata"))
setGeneric("atomdata", function(object) standardGeneric("atomdata"))
setGeneric("datamode", function(x) standardGeneric("datamode"))
setGeneric("datamode<-", function(x, value) standardGeneric("datamode<-"))
setGeneric("paths", function(x) standardGeneric("paths"))
setGeneric("paths<-", function(x, value) standardGeneric("paths<-"))
setGeneric("filemode", function(x) standardGeneric("filemode"))
setGeneric("filemode<-", function(x, value) standardGeneric("filemode<-"))
setGeneric("chunksize", function(x) standardGeneric("chunksize"))
setGeneric("chunksize<-", function(x, value) standardGeneric("chunksize<-"))

#### Implement basic generics from BiocGenerics ####
## ---------------------------------------------------

setMethod("combine", c("numeric", "numeric"),
	function(x, y, ...) c(x, y))

