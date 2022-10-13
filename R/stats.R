
#### Summarize arrays ####
## -----------------------

setMethod("range", "matter_arr",
	function(x, ..., na.rm = FALSE)
	{
		drop_attr(chunk_lapply(x, FUN=s_range,
			na.rm=na.rm, simplify=stat_c, ...))
	})

setMethod("min", "matter_arr",
	function(x, ..., na.rm = FALSE)
	{
		drop_attr(chunk_lapply(x, FUN=s_min,
			na.rm=na.rm, simplify=stat_c, ...))
	})

setMethod("max", "matter_arr",
	function(x, ..., na.rm = FALSE)
	{
		drop_attr(chunk_lapply(x, FUN=s_max,
			na.rm=na.rm, simplify=stat_c, ...))
	})

setMethod("prod", "matter_arr",
	function(x, ..., na.rm = FALSE)
	{
		drop_attr(chunk_lapply(x, FUN=s_prod,
			na.rm=na.rm, simplify=stat_c, ...))
	})

setMethod("sum", "matter_arr",
	function(x, ..., na.rm = FALSE)
	{
		drop_attr(chunk_lapply(x, FUN=s_sum,
			na.rm=na.rm, simplify=stat_c, ...))
	})

setMethod("mean", "matter_arr",
	function(x, ..., na.rm = FALSE)
	{
		drop_attr(chunk_lapply(x, FUN=s_mean,
			na.rm=na.rm, simplify=stat_c, ...))
	})

setMethod("var", "matter_arr",
	function(x, na.rm = FALSE)
	{
		drop_attr(chunk_lapply(x, FUN=s_var,
			na.rm=na.rm, simplify=stat_c))
	})

setMethod("sd", "matter_arr",
	function(x, na.rm = FALSE)
	{
		drop_attr(chunk_lapply(x, FUN=s_sd,
			na.rm=na.rm, simplify=stat_c))
	})

setMethod("any", "matter_arr",
	function(x, ..., na.rm = FALSE)
	{
		drop_attr(chunk_lapply(x, FUN=s_any,
			na.rm=na.rm, simplify=stat_c, ...))
	})

setMethod("all", "matter_arr",
	function(x, ..., na.rm = FALSE)
	{
		drop_attr(chunk_lapply(x, FUN=s_all,
			na.rm=na.rm, simplify=stat_c, ...))
	})

#### Summarize matrix rows ####
## ----------------------------

setMethod("rowSums", "matter_mat",
	function(x, na.rm = FALSE, dims = 1, ...)
	{
		rowStats(x, stat="sum", ..., na.rm=na.rm)
	})

setMethod("rowMeans", "matter_mat",
	function(x, na.rm = FALSE, dims = 1, ...)
	{
		rowStats(x, stat="mean", ..., na.rm=na.rm)
	})

setMethod("rowSums", "sparse_mat",
	function(x, na.rm = FALSE, dims = 1, ...)
	{
		rowStats(x, stat="sum", ..., na.rm=na.rm)
	})

setMethod("rowMeans", "sparse_mat",
	function(x, na.rm = FALSE, dims = 1, ...)
	{
		rowStats(x, stat="mean", ..., na.rm=na.rm)
	})

#### Summarize matrix columns ####
## -------------------------------

setMethod("colSums", "matter_mat",
	function(x, na.rm = FALSE, dims = 1, ...)
	{
		colStats(x, stat="sum", ..., na.rm=na.rm)
	})

setMethod("colMeans", "matter_mat",
	function(x, na.rm = FALSE, dims = 1, ...)
	{
		colStats(x, stat="mean", ..., na.rm=na.rm)
	})

setMethod("colSums", "sparse_mat",
	function(x, na.rm = FALSE, dims = 1, ...)
	{
		colStats(x, stat="sum", ..., na.rm=na.rm)
	})

setMethod("colMeans", "sparse_mat",
	function(x, na.rm = FALSE, dims = 1, ...)
	{
		colStats(x, stat="mean", ..., na.rm=na.rm)
	})
