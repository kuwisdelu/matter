
# statistical summaries

setMethod("range", "matter", function(x, na.rm = FALSE) {
	ret <- .Call("C_getRange", x, na.rm, PACKAGE="matter")
	ret
})

setMethod("min", "matter", function(x, na.rm = FALSE) {
	ret <- .Call("C_getRange", x, na.rm, PACKAGE="matter")
	ret <- ret[1]
	ret
})

setMethod("max", "matter", function(x, na.rm = FALSE) {
	ret <- .Call("C_getRange", x, na.rm, PACKAGE="matter")
	ret <- ret[2]
	ret
})

setMethod("prod", "matter", function(x, na.rm = FALSE) {
	ret <- .Call("C_getProd", x, na.rm, PACKAGE="matter")
	ret
})

setMethod("sum", "matter", function(x, na.rm = FALSE) {
	ret <- .Call("C_getSum", x, na.rm, PACKAGE="matter")
	ret
})

setMethod("mean", "matter", function(x, na.rm = FALSE) {
	ret <- .Call("C_getMean", x, na.rm, PACKAGE="matter")
	ret
})

setMethod("var", "matter", function(x, na.rm = FALSE) {
	ret <- .Call("C_getVar", x, na.rm, PACKAGE="matter")
	ret
})

setMethod("sd", "matter", function(x, na.rm = FALSE) {
	ret <- sqrt(.Call("C_getVar", x, na.rm, PACKAGE="matter"))
	ret
})

setMethod("any", "matter", function(x, na.rm = FALSE) {
	ret <- .Call("C_getAny", x, na.rm, PACKAGE="matter")
	ret
})

setMethod("all", "matter", function(x, na.rm = FALSE) {
	ret <- .Call("C_getAll", x, na.rm, PACKAGE="matter")
	ret
})

setMethod("colSums", "matter_mat", function(x, na.rm = FALSE) {
	ret <- .Call("C_getColSums", x, na.rm, PACKAGE="matter")
	names(ret) <- colnames(x)
	ret
})

setMethod("colMeans", "matter_mat", function(x, na.rm = FALSE) {
	ret <- .Call("C_getColMeans", x, na.rm, PACKAGE="matter")
	names(ret) <- colnames(x)
	ret	
})

setMethod("colVars", "matter_mat", function(x, na.rm = FALSE) {
	ret <- .Call("C_getColVars", x, na.rm, PACKAGE="matter")
	names(ret) <- colnames(x)
	ret
})

setMethod("colSds", "matter_mat", function(x, na.rm = FALSE) {
	ret <- sqrt(.Call("C_getColVars", x, na.rm, PACKAGE="matter"))
	names(ret) <- colnames(x)
	ret
})

setMethod("rowSums", "matter_mat", function(x, na.rm = FALSE) {
	ret <- .Call("C_getRowSums", x, na.rm, PACKAGE="matter")
	names(ret) <- rownames(x)
	ret
})

setMethod("rowMeans", "matter_mat", function(x, na.rm = FALSE) {
	ret <- .Call("C_getRowMeans", x, na.rm, PACKAGE="matter")
	names(ret) <- rownames(x)
	ret
})

setMethod("rowVars", "matter_mat", function(x, na.rm = FALSE) {
	ret <- .Call("C_getRowVars", x, na.rm, PACKAGE="matter")
	names(ret) <- rownames(x)
	ret
})

setMethod("rowSds", "matter_mat", function(x, na.rm = FALSE) {
	ret <- sqrt(.Call("C_getRowVars", x, na.rm, PACKAGE="matter"))
	names(ret) <- rownames(x)
	ret
})
