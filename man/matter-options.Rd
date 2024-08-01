\name{matter-options}

\alias{matter-options}
\alias{matter_defaults}

\title{Options for ``matter'' Objects}

\description{
	Set global parameters for \code{matter}.
}

\usage{
## Set defaults for common arguments
matter_defaults(nchunks = 20L, chunksize = NA_real_,
    serialize = TRUE, verbose = FALSE)
}

\arguments{
	\item{nchunks}{The number of chunks to use. This sets \code{getOption("matter.default.nchunks")}. For IO-bound operations, using fewer chunks will often be faster, but use more memory.}

	\item{chunksize}{The approximate chunk size in bytes. This sets \code{getOption("matter.default.chunksize")}. For IO-bound operations, using larger chunks will often be faster, but use more memory.}

	\item{serialize}{Whether \code{matter} chunks should be realized in memory on the manager and the data serialized to the workers (\code{TRUE}), or the realization should be performed on the workers (\code{FALSE}). This sets \code{getOption("matter.default.serialize")}. If all workers are on the same machine, then it can be significantly faster to avoid serializing the realized data.}

	\item{verbose}{Whether progress messages should be printed. This sets \code{getOption("matter.default.verbose")}.}
}

\details{
	The \code{matter} package provides the following options:

	\itemize{
		\item{\code{options(matter.compress.atoms=3)}: The compression ratio threshold to be used to determine when to compress atoms in a matter object. Setting to 0 or \code{FALSE} means that atoms are never compressed.}

		\item{\code{options(matter.default.nchunks=20L)}: The default number of chunks to use when iterating over matter objects.}

		\item{\code{options(matter.default.chunksize=NA_real_)}: The default chunk size in bytes to use when iterating over matter objects.}

		\item{\code{options(matter.default.serialize=TRUE)}: Whether \code{matter} chunks should be realized in memory on the manager (\code{TRUE}) or on the workers \code{FALSE}.}

		\item{\code{options(matter.default.verbose=FALSE)}: The default verbosity for printing progress messages.}

		\item{\code{options(matter.matmul.bpparam=NULL)}: An optional \code{BiocParallelParam} passed to \code{\link{bplapply}} when performing matrix multiplication with \code{matter_mat} and \code{sparse_mat} objects.}

		\item{\code{options(matter.show.head=TRUE)}: Should a preview of the beginning of the data be displayed when the object is printed?}

		\item{\code{options(matter.show.head.n=6)}: The number of elements, rows, and/or columns to be displayed by the object preview.}

		\item{\code{options(matter.coerce.altrep=FALSE)}: When coercing \code{matter} objects to native R objects (such as \code{matrix}), should a \code{matter}-backed ALTREP object be returned instead? The initial coercion will be cheap, and the result will look like a native R object. This does not guarantee that the full data is never read into memory. Not all functions are ALTREP-aware at the C-level, so some operations may still trigger the full data to be read into memory. This should only ever happen once, as long as the object is not duplicated, though.}

		\item{\code{options(matter.wrap.altrep=FALSE)}: When coercing to a \code{matter}-backed ALTREP object, should the object be wrapped in an ALTREP wrapper? (This is always done in cases where the coercion preserves existing attributes.) This allows setting of attributes without triggering a (potentially expensive) duplication of the object when safe to do so.}

		\item{\code{options(matter.dump.dir=tempdir())}: Temporary directory where \code{matter} object files should be dumped when created without user-specified file paths.}

		\item{\code{options(matter.logger=simple_logger())}: The logger used by \code{matter}. Must be a \code{simple_logger} object.}
	}
}

\keyword{misc}
