
#### Linear regression for matter matrices and data frames ####
## -----------------------------------------------------------

setMethod("biglm", c("formula", "matter_df"),
	function(formula, data, weights = NULL, sandwich = FALSE)
{
		n <- nrow(data)
		vars <- all.vars(formula)
		p <- length(vars)
		chunksize <- chunksize(data) %/% p
		current <- 1
		getNextDataChunk <- matter_df_chunker(formula, data, chunksize)
		data <- getNextDataChunk(FALSE)
		blm <- biglm(formula=formula, data=data,
			weights=weights, sandwich=sandwich)
		data <- getNextDataChunk(FALSE)
		while ( !is.null(data) ) {
			blm <- update(blm, data)
			data <- getNextDataChunk(FALSE)
		}
		blm
})

setMethod("bigglm", c("formula", "matter_df"),
	function(formula, data, ..., chunksize = NULL)
{
		n <- nrow(data)
		vars <- all.vars(formula)
		p <- length(vars)
		if ( is.null(chunksize) )
			chunksize <- chunksize(data) %/% p
		getNextDataChunk <- matter_df_chunker(formula, data, chunksize)
		bigglm(formula, getNextDataChunk, ...)
})

setMethod("bigglm", c("formula", "matter_mat"),
	function(formula, data, ..., chunksize = NULL, fc = NULL)
{
		n <- nrow(data)
		vars <- unique(c(all.vars(formula), fc))
		p <- length(vars)
		if ( is.null(chunksize) )
			chunksize <- chunksize(data) %/% p
		getNextDataChunk <- matter_mat_chunker(formula, data, chunksize, fc)
		bigglm(formula, getNextDataChunk, ...)
})

# based on code from package:biglm and package:biganalytics

matter_mat_chunker <- function(formula, data, chunksize, fc) {
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
		chunk <- sapply(vars, function(v) data[chunkrange,v], simplify=FALSE)
		chunk <- as.data.frame(chunk)
		if ( !is.null(fc) ) {
			for ( name in names(fclevels) )
				chunk[,name] <- factor(chunk[,name], levels=fclevels[[name]])
		}
		current <<- max(chunkrange) + 1
		chunk
	}
}

matter_df_chunker <- function(formula, data, chunksize) {
	n <- nrow(data)
	vars <- all.vars(formula)
	current <- 1
	function(reset = FALSE) {
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
}
