
#### Linear regression for matter matrices ####
## --------------------------------------------

setMethod("bigglm", c("formula", "matter_mat"),
	function(formula, data, ..., chunksize = NULL)
{
	bigglm_int(formula, data, ..., chunksize=chunksize)
})
		

setMethod("bigglm", c("formula", "sparse_mat"),
	function(formula, data, ..., chunksize = NULL)
{
	bigglm_int(formula, data, ..., chunksize=chunksize)
})

bigglm_int <- function(formula, data, ..., chunksize = NULL)
{
	if ( !requireNamespace("biglm") )
		stop("failed to load required package 'biglm'")
	if ( is.null(chunksize) )
		chunksize <- getOption("matter.default.chunksize")
	n <- nrow(data)
	vars <- all.vars(formula)
	current <- 0L
	datafun <- function(reset = FALSE) {
		if ( reset ) {
			current <<- 0L
			return(NULL)
		}
		if ( current >= n )
			return(NULL)
		start <- current + 1L
		current <<- current + min(chunksize, n - current)
		chunk <- data[start:current,vars,drop=FALSE]
		as.data.frame(chunk)
	}
	bigglm(formula=formula, data=datafun, ...)
}
