\name{chunk_apply}
\docType{methods}

\alias{chunk_apply}
\alias{chunk_mapply}

\title{Apply Functions Over Chunks of a Vector or Matrix}

\description{
    Perform equivalents of \code{apply}, \code{lapply}, and \code{mapply}, but over parallelized chunks of the data. This is most useful if accessing the data is potentially time-consuming, such as for file-based \code{matter} objects. Operating on chunks reduces the number of I/O operations.
}

\usage{
chunk_apply(X, FUN, MARGIN, \dots, simplify = FALSE,
    chunks = NA, view = c("element", "chunk"),
    attr = list(), alist = list(), outfile = NULL,
    verbose = FALSE, BPREDO = list(), BPPARAM = bpparam())

chunk_mapply(FUN, \dots, MoreArgs = NULL, simplify = FALSE,
    chunks = NA, view = c("element", "chunk"),
    attr = list(), alist = list(), outfile = NULL,
    verbose = FALSE, BPREDO = list(), BPPARAM = bpparam())
}

\arguments{
    \item{X}{A list, vector, or matrix. These may be any class that implements suitable methods for \code{[}, \code{[[}, \code{dim}, and \code{length()}.}
    
    \item{FUN}{The function to be applied.}

    \item{MARGIN}{If the object is matrix-like, which dimension to iterate over. Must be 1 or 2, where 1 indicates rows and 2 indicates columns. The dimension names can also be used if \code{X} has \code{dimnames} set.}

    \item{MoreArgs}{A list of other arguments to \code{FUN}.}

    \item{\dots}{Additional arguments to be passed to \code{FUN}.}

    \item{simplify}{Should the result be simplified into a vector, matrix, or higher dimensional array?}

    \item{chunks}{The number of chunks to use. If \code{NA} (the default), this is inferred from \code{chunksize(X)} for \code{matter} objects, or from \code{getOption("matter.default.chunksize")} for non-\code{matter} classes. For IO-bound operations, using fewer chunks will often be faster, but use more memory.}

    \item{view}{What should be passed as the argment to \code{FUN}: "element" means the vector element, row, or column are passed (same as the behavior of \code{lapply} and \code{apply}), and "chunk" means to pass the entire chunk.}

    \item{attr}{A list of attributes that will be attached to the argument passed to \code{FUN} as-is.}

    \item{alist}{A list of vector-like attributes that will be attached to the argument passed to \code{FUN}, subsetted to the current elements.}

    \item{outfile}{If non-NULL, a file path where the results should be written as they are processed. Not implemented yet.}

    \item{verbose}{Should user messages be printed with the current chunk being processed?}

    \item{BPREDO}{See documentation for \code{\link{bplapply}}.}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}
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
