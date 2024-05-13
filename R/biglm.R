
#### Linear regression for matter matrices ####
## --------------------------------------------

setMethod("bigglm", c("formula", "matter_mat"),
	function(formula, data, ..., nchunks = NA, verbose = NA)
{
	bigglm_int(formula, data, ..., nchunks=nchunks, verbose=verbose)
})
		

setMethod("bigglm", c("formula", "sparse_mat"),
	function(formula, data, ..., nchunks = NA, verbose = NA)
{
	bigglm_int(formula, data, ..., nchunks=nchunks, verbose=verbose)
})

bigglm_int <- function(formula, data, ...,
	nchunks = NA, verbose = FALSE)
{
	if ( !requireNamespace("biglm") )
		stop("failed to load required package 'biglm'")
	if ( is.na(nchunks) )
		nchunks <- getOption("matter.default.nchunks")
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	vars <- all.vars(formula)
	INDEX <-  chunkify(seq_len(nrow(data)), nchunks)
	current <- 1L
	npass <- 0L
	datafun <- function(reset = FALSE) {
		if ( reset ) {
			npass <<- npass + 1L
			if ( verbose )
				message("pass ", npass)
			current <<- 1L
			return(NULL)
		}
		if ( current > length(INDEX) )
			return(NULL)
		i <- INDEX[[current]]
		if ( verbose )
			print_chunk_progress(i)
		chunk <- as.matrix(data[i,vars,drop=FALSE])
		current <<- current + 1L
		as.data.frame(chunk)
	}
	bigglm(formula=formula, data=datafun, ...)
}
