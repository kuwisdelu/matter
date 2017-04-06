
setMethod("sum", "matter", function(x, na.rm = FALSE) {
	ret <- .Call("C_getSum", x, na.rm)
	names(ret) <- names(x)
	ret
})

setMethod("mean", "matter", function(x, na.rm = FALSE) {
	ret <- .Call("C_getMean", x, na.rm)
	names(ret) <- names(x)
	ret
})

setMethod("var", "matter", function(x, na.rm = FALSE) {
	ret <- .Call("C_getVar", x, na.rm)
	names(ret) <- names(x)
	ret
})

setMethod("sd", "matter", function(x, na.rm = FALSE) {
	ret <- sqrt(.Call("C_getVar", x, na.rm))
	names(ret) <- names(x)
	ret
})

setMethod("colSums", "matter_mat", function(x, na.rm = FALSE) {
	ret <- .Call("C_getColSums", x, na.rm)
	names(ret) <- colnames(x)
	ret
})

setMethod("colMeans", "matter_mat", function(x, na.rm = FALSE) {
	ret <- .Call("C_getColMeans", x, na.rm)
	names(ret) <- colnames(x)
	ret	
})

setMethod("colVars", "matter_mat", function(x, na.rm = FALSE) {
	ret <- .Call("C_getColVars", x, na.rm)
	names(ret) <- colnames(x)
	ret
})

setMethod("colSds", "matter_mat", function(x, na.rm = FALSE) {
	ret <- sqrt(.Call("C_getColVars", x, na.rm))
	names(ret) <- colnames(x)
	ret
})

setMethod("rowSums", "matter_mat", function(x, na.rm = FALSE) {
	ret <- .Call("C_getRowSums", x, na.rm)
	names(ret) <- rownames(x)
	ret
})

setMethod("rowMeans", "matter_mat", function(x, na.rm = FALSE) {
	ret <- .Call("C_getRowMeans", x, na.rm)
	names(ret) <- rownames(x)
	ret
})

setMethod("rowVars", "matter_mat", function(x, na.rm = FALSE) {
	ret <- .Call("C_getRowVars", x, na.rm)
	names(ret) <- rownames(x)
	ret
})

setMethod("rowSds", "matter_mat", function(x, na.rm = FALSE) {
	ret <- sqrt(.Call("C_getRowVars", x, na.rm))
	names(ret) <- rownames(x)
	ret
})
