
#### Linear regression for matter matrices and data frames ####
## -----------------------------------------------------------

setMethod("bigglm", c("formula", "matter_mat"),
	function(formula, data, ..., chunksize = NULL, fc = NULL) {
		bigglm_matter_mat(formula, data, ..., chunksize=chunksize, fc=fc)
})

setMethod("bigglm", c("formula", "matter_df"),
	function(formula, data, ..., chunksize = NULL) {
		bigglm_matter_df(formula, data, ..., chunksize=chunksize)
})

# based on code from package:biglm and package:biganalytics

bigglm_matter_mat <- function(formula, data, ..., chunksize, fc) {
	n <- nrow(data)
	vars <- unique(c(all.vars(formula), fc))
	p <- length(vars)
	if ( is.null(chunksize) )
		chunksize <- chunksize(data) %/% p
	if ( !is.null(fc) ) {
		flevels <- lapply(fc, function(x) sort(unique(x[,fc])))
		names(flevels) <- fc
	}
	current <- 1
	getNextDataChunk <- function(reset = FALSE) {
		if ( reset ) {
			current <<- 1
			return(NULL)
		}
		if ( current > n )
			return(NULL)
		chunkrange <- current:(current + min(chunksize, n - current))
		chunk <- sapply(vars, function(v) data[chunkrange,v], simplify=FALSE)
		chunk <- as.data.frame(chunk)
		if ( !is.null(fc) ) {
			for ( name in names(flevels) )
				chunk[,name] <- factor(chunk[,name], levels=flevels[name])
		}
		current <<- max(chunkrange) + 1
		chunk
	}
	bigglm(formula, getNextDataChunk, ...)
}

bigglm_matter_df <- function(formula, data, ..., chunksize) {
	n <- nrow(data)
	vars <- all.vars(formula)
	p <- length(vars)
	if ( is.null(chunksize) )
		chunksize <- chunksize(data) %/% p
	current <- 1
	getNextDataChunk <- function(reset = FALSE) {
		if ( reset ) {
			current <<- 1
			return(NULL)
		}
		if ( current > n )
			return(NULL)
		chunkrange <- current:(current + min(chunksize, n - current))
		chunk <- sapply(vars, function(v) data[chunkrange,v], simplify=FALSE)
		chunk <- as.data.frame(chunk)
		current <<- max(chunkrange) + 1
		chunk
	}
	bigglm(formula, getNextDataChunk, ...)
}

