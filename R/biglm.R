
#### Linear regression for matter matrices and data frames ####
## -----------------------------------------------------------

setMethod("bigglm", c("formula", "matter_mat"),
	function(formula, data, ..., chunksize = NULL, fc = NULL)
{
	bigglm_int(formula, data, ..., chunksize=chunksize, fc=fc)
})
		

setMethod("bigglm", c("formula", "sparse_mat"),
	function(formula, data, ..., chunksize = NULL, fc = NULL)
{
	bigglm_int(formula, data, ..., chunksize=chunksize, fc=fc)
})

bigglm_int <- function(formula, data, ..., chunksize = NULL, fc = NULL)
{
	if ( !requireNamespace("biglm") )
		stop("failed to load required package 'biglm'")
	if ( is.null(chunksize) )
		chunksize <- getOption("matter.default.chunksize")
	getNextDataChunk <- matrix_chunker(formula, data, chunksize, fc)
	bigglm(formula, getNextDataChunk, ...)
}

# based on code from package:biglm and package:biganalytics

matrix_chunker <- function(formula, data, chunksize, fc) {
	n <- nrow(data)
	vars <- unique(c(all.vars(formula), fc))
	current <- 1
	if ( !is.null(fc) ) {
		fclevels <- lapply(fc, function(x) sort(unique(x[,fc])))
		names(fclevels) <- fc
	}
	function(reset = FALSE) {
		if ( reset ) {
			current <<- 1
			return(NULL)
		}
		if ( current > n )
			return(NULL)
		chunkrange <- current:(current + min(chunksize, n - current))
		chunk <- sapply(vars, function(v) data[chunkrange,v,drop=FALSE], simplify=FALSE)
		chunk <- as.data.frame(chunk)
		if ( !is.null(fc) ) {
			for ( name in names(fclevels) )
				chunk[,name] <- factor(chunk[,name], levels=fclevels[[name]])
		}
		current <<- max(chunkrange) + 1
		chunk
	}
}
