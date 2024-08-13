\name{chunkApply}
\docType{methods}

\alias{chunkApply}
\alias{chunkLapply}
\alias{chunkMapply}

\alias{chunk_apply}
\alias{chunk_rowapply}
\alias{chunk_colapply}
\alias{chunk_lapply}
\alias{chunk_mapply}

\alias{chunk_writer}

\title{Apply Functions Over Chunks of a List, Vector, or Matrix}

\description{
    Perform equivalents of \code{apply}, \code{lapply}, and \code{mapply}, but over parallelized chunks of data. This is most useful if accessing the data is potentially time-consuming, such as for file-based \code{matter} objects. Operating on chunks reduces the number of I/O operations.
}

\usage{
## Operate on elements/rows/columns
chunkApply(X, MARGIN, FUN, \dots,
    simplify = FALSE, outpath = NULL,
    verbose = NA, BPPARAM = bpparam())

chunkLapply(X, FUN, \dots,
    simplify = FALSE, outpath = NULL,
    verbose = NA, BPPARAM = bpparam())

chunkMapply(FUN, \dots,
    simplify = FALSE, outpath = NULL,
    verbose = NA, BPPARAM = bpparam())


## Operate on complete chunks
chunk_rowapply(X, FUN, \dots,
    simplify = "c", depends = NULL, permute = FALSE,
    RNG = FALSE, verbose = NA, chunkopts = list(),
    BPPARAM = bpparam())

chunk_colapply(X, FUN, \dots,
    simplify = "c", depends = NULL, permute = FALSE,
    RNG = FALSE, verbose = NA, chunkopts = list(),
    BPPARAM = bpparam())

chunk_lapply(X, FUN, \dots,
    simplify = "c", depends = NULL, permute = FALSE,
    RNG = FALSE, verbose = NA, chunkopts = list(),
    BPPARAM = bpparam())

chunk_mapply(FUN, \dots, MoreArgs = NULL,
    simplify = "c", depends = NULL, permute = FALSE,
    RNG = FALSE, verbose = NA, chunkopts = list(),
    BPPARAM = bpparam())
}

\arguments{
    \item{X}{A matrix for \code{chunkApply()}, a list or vector for \code{chunkLapply()}, or lists for \code{chunkMapply()}. These may be any class that implements suitable methods for \code{[}, \code{[[}, \code{dim}, and \code{length()}.}

    \item{MARGIN}{If the object is matrix-like, which dimension to iterate over. Must be 1 or 2, where 1 indicates rows and 2 indicates columns. The dimension names can also be used if \code{X} has \code{dimnames} set.}
    
    \item{FUN}{The function to be applied.}

    \item{MoreArgs}{A list of other arguments to \code{FUN}.}

    \item{\dots}{Additional arguments to be passed to \code{FUN}.}

    \item{simplify}{Should the result be simplified into a vector, matrix, or higher dimensional array?}

    \item{outpath}{If non-NULL, a file path where the results should be written as they are processed. If specified, \code{FUN} must return a 'raw', 'logical', 'integer', or 'numeric' vector. The result will be returned as a \code{matter} object.}

    \item{verbose}{Should user messages be printed with the current chunk being processed? If \code{NA} (the default), this is taken from \code{getOption("matter.default.verbose")}.}

    \item{chunkopts}{An (optional) list of chunk options including \code{nchunks}, \code{chunksize}, and \code{serialize}. See "Details".}

    \item{depends}{A list with length equal to the extent of \code{X}. Each element of \code{depends} should give a vector of indices which correspond to other elements of \code{X} on which each computation depends. These elements are passed to \code{FUN}. For time  efficiency, no attempt is made to verify these indices are valid.}

    \item{permute}{Should the order of items be randomized? This may be useful for iterating over random subsets. No attempt is made to re-order the results.}

    \item{RNG}{Should the local random seed (as set by \code{set.seed}) be forwarded to the worker processes? If \code{RNGkind} is set to "L'Ecuyer-CMRG", then the random seed will be set to appropriate substreams for each chunk or for each element/row/column. Note that forwarding the local random seed incurs additional overhead.}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}
}

\details{
    For \code{chunkApply()}, \code{chunkLapply()}, and \code{chunkMapply()}:

    For vectors and lists, the vector is broken into some number of chunks according to \code{chunks}. The individual elements of the chunk are then passed to \code{FUN}.

    For matrices, the matrix is chunked along rows or columns, based on the number of \code{chunks}. The individual rows or columns of the chunk are then passed to \code{FUN}.

    In this way, the first argument of \code{FUN} is analogous to using the base \code{apply}, \code{lapply}, and \code{mapply} functions.

    For \code{chunk_rowapply()}, \code{chunk_colapply()}, \code{chunk_lapply()}, and \code{chunk_mapply()}:

    In this situation, the entire chunk is passed to \code{FUN}, and \code{FUN} is responsible for knowing how to handle a sub-vector or sub-matrix of the original object. This may be useful if \code{FUN} is already a function that could be applied to the whole object such as \code{rowSums} or \code{colSums}.

    When this is the case, it may be useful to provide a custom \code{simplify} function.

    For convenience to the programmer, several attributes are made available when operating on a chunk.

    \itemize{

        \item{"chunkid": The index of the chunk currently being processed by \code{FUN}.}

        \item{"chunklen": The number of elements in the chunk that should be processed.}

        \item{"index": The indices of the elements of the chunk, as elements/rows/columns in the original matrix/vector.}

        \item{"depends" (optional): If \code{depends} is given, then this is a list of indices within the chunk. The length of the list is equal to the number of elements/rows/columns in the chunk. Each list element is either \code{NULL} or a vector of indices giving the elements/rows/columns of the chunk that should be processed for that index. The indices that should be processed will be non-\code{NULL}, and indices that should be ignored will be \code{NULL}.}
    
    }

    The \code{depends} argument can be used to iterate over dependent elements of a vector, or dependent rows/columns of a matrix. This can be useful if the calculation for a particular row/column/element depends on the values of others.

    When \code{depends} is provided, multiple rows/columns/elements will be passed to \code{FUN}. Each element of the \code{depends} list should be a vector giving the indices that should be passed to \code{FUN}.

    For example, this can be used to implement a rolling apply function.

    Several options are supported by \code{chunkopts} to override the global options:

    \itemize{

        \item{nchunks: The number of chunks to use. If omitted, this is taken from \code{getOption("matter.default.nchunks")}. For IO-bound operations, using fewer chunks will often be faster, but use more memory.}

        \item{chunksize: The approximate chunk size in bytes. If omitted, this is taken from \code{getOption("matter.default.chunksize")}. For IO-bound operations, using larger chunks will often be faster, but use more memory. If set to \code{NA_real_}, then the chunk size is determined by the number of chunks.}

        \item{serialize: Whether data in virtual memory should be realized on the manager and serialized to the workers (\code{TRUE}), passed to the workers in virtual memory as-is (\code{FALSE}), or if \code{matter} should decide the behavior based on the cluster configuration (\code{NA}). If omitted, this is taken from \code{getOption("matter.default.serialize")}. If all workers have access to the same virtual memory resources (whether file storage or shared memory), then it can be significantly faster to avoid serializing the data.}
    }
}

\value{
	Typically, a list if \code{simplify=FALSE}. Otherwise, the results may be coerced to a vector or array.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link[base]{apply}},
    \code{\link[base]{lapply}},
    \code{\link[base]{mapply}},
    \code{\link[base]{RNGkind}},
    \code{\link{RNGStreams}},
    \code{\link{SnowfastParam}}
}

\examples{
register(SerialParam())

set.seed(1)
x <- matrix(rnorm(1000^2), nrow=1000, ncol=1000)

out <- chunkApply(x, 1L, mean, chunkopts=list(nchunks=10))
head(out)
}

\keyword{iteration}
