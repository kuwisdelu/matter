\name{chunk_apply}
\docType{methods}

\alias{chunk_apply}
\alias{chunk_mapply}

\title{Apply Functions Over Chunks of a List, Vector, or Matrix}

\description{
    Perform equivalents of \code{apply}, \code{lapply}, and \code{mapply}, but over parallelized chunks of the data. This is most useful if accessing the data is potentially time-consuming, such as for file-based \code{matter} objects. Operating on chunks reduces the number of I/O operations.
}

\usage{
chunk_apply(X, FUN, MARGIN, \dots, simplify = FALSE,
    chunks = NA, view = c("element", "chunk"),
    attr = list(), alist = list(), pattern = NULL,
    outfile = NULL, verbose = FALSE,
    BPREDO = list(), BPPARAM = bpparam())

chunk_mapply(FUN, \dots, MoreArgs = NULL, simplify = FALSE,
    chunks = NA, view = c("element", "chunk"),
    attr = list(), alist = list(), pattern = NULL,
    outfile = NULL, verbose = FALSE,
    BPREDO = list(), BPPARAM = bpparam())
}

\arguments{
    \item{X}{A list, vector, or matrix for \code{chunk_apply()}. These may be any class that implements suitable methods for \code{[}, \code{[[}, \code{dim}, and \code{length()}. Only lists are supported for \code{chunk_mapply()}.}
    
    \item{FUN}{The function to be applied.}

    \item{MARGIN}{If the object is matrix-like, which dimension to iterate over. Must be 1 or 2, where 1 indicates rows and 2 indicates columns. The dimension names can also be used if \code{X} has \code{dimnames} set.}

    \item{MoreArgs}{A list of other arguments to \code{FUN}.}

    \item{\dots}{Additional arguments to be passed to \code{FUN}.}

    \item{simplify}{Should the result be simplified into a vector, matrix, or higher dimensional array?}

    \item{chunks}{The number of chunks to use. If \code{NA} (the default), this is inferred from \code{chunksize(X)} for \code{matter} objects, or from \code{getOption("matter.default.chunksize")} for non-\code{matter} classes. For IO-bound operations, using fewer chunks will often be faster, but use more memory.}

    \item{view}{What should be passed as the argment to \code{FUN}: "element" means the vector element, row, or column are passed (same as the behavior of \code{lapply} and \code{apply}), and "chunk" means to pass the entire chunk.}

    \item{attr}{A named list of attributes that will be attached to the argument passed to \code{FUN} as-is.}

    \item{alist}{A named list of vector-like attributes that will be attached to the argument passed to \code{FUN}, subsetted to the current elements. Typically, each attribute should be as long as \code{X}, unless \code{pattern} is specified, in which case each attribute should be as long as \code{pattern}.}

    \item{pattern}{A list of indices giving a pattern over which to apply \code{FUN} to \code{X}. Each element of \code{pattern} should give a vector of indices which can be used subscript \code{X}. For time and space efficiency, no attempt is made to verify these indices are valid.}

    \item{outfile}{If non-NULL, a file path where the results should be written as they are processed. If specified, \code{FUN} must return a 'raw', 'logical', 'integer', or 'numeric' vector. The result will be returned as a \code{matter} object.}

    \item{verbose}{Should user messages be printed with the current chunk being processed?}

    \item{BPREDO}{See documentation for \code{\link{bplapply}}.}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}
}

\details{
    When \code{view = "element"}:

    For vectors and lists, the vector is broken into some number of chunks according to \code{chunks}. The individual elements of the chunk are then passed to \code{FUN}.

    For matrices, the matrix is chunked along rows or columns, based on the number of \code{chunks}. The individual rows or columns of the chunk are then passed to \code{FUN}.

    In this way, the first argument of \code{FUN} is analogous to using the base \code{apply} and \code{lapply} functions.

    However, when \code{view = "chunk"}:

    In this situation, the entire chunk is passed to \code{FUN}, and \code{FUN} is responsible for knowing how to handle a sub-vector or sub-matrix of the original object. This may be useful if \code{FUN} is already a function that could be applied to the whole object such as \code{rowSums} or \code{colSums}.

    When this is the case, it may be useful to provide a custom \code{simplify} function. Otherwise, the result will be returned as a list with length equal to the number of chunks, which must be post-processed to get into a desirable form.

    For convenience to the programmer, several attributes are made available when \code{view = "chunk"}.

    \itemize{

        \item{"chunk_id":}{The index of the chunk currently being processed by \code{FUN}.}

        \item{"chunk_elt":}{The indices of the elements of the chunk, as rows/columns/elements in the original matrix/vector.}

        \item{"pattern_id" (optional):}{The indices of the patterns that compose the current chunk.}

        \item{"pattern_elt" (optional):}{The indices of the elements of the patterns, as rows/columns/elements in the original matrix/vector, that compose the current chunk.}

    }

    The \code{pattern} argument can be used to iterate over dependent elements of a vector, or dependent rows/columns of a matrix. This can be useful if the calculation for a particular row/column/element depends on the values of others.

    When \code{pattern} is provided, multiple rows/columns/elements will be passed to \code{FUN}, even when \code{view="element"}. Each element of the \code{pattern} list should be a vector giving the indices that should be passed to \code{FUN}.

    This can be used to implement a rolling apply function.
}

\value{
	Typically, a list if \code{simplify=FALSE}. Otherwise, the results may be coerced to a vector or array.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link[base]{apply}},
    \code{\link[base]{lapply}},
    \code{\link[base]{mapply}},
}

\examples{
register(SerialParam())

set.seed(1)
x <- matrix(rnorm(1000^2), nrow=1000, ncol=1000)

out <- chunk_apply(x, mean, 1, chunks=20, verbose=TRUE)
}

\keyword{methods}
