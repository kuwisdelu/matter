\name{matter-options}

\alias{matter-options}

\title{Options for ``matter'' Objects}

\description{
	The \code{matter} package provides the following options:

	\itemize{
	    \item{\code{options(matter.cast.warning=TRUE)}:}{Should a warning be emitted when casting between data types results in a loss of precision?}

	    \item{\code{options(matter.compress.atoms=3)}:}{The compression ratio threshold to be used to determine when to compress atoms in a matter object. Setting to 0 or \code{FALSE} means that atoms are never compressed.}

	    \item{\code{options(matter.default.nchunks=20L)}:}{The default number of chunks to use when iterating over matter objects.}

	    \item{\code{options(matter.default.chunksize=1000000L)}:}{The default chunksize for new \code{matter} objects. This is the (suggested) maximum number of elements which should be accessed at once by summary functions and linear algebra. Ignored when explicitly subsetting the dataset. Must be an integer.}

	    \item{\code{options(matter.show.head=TRUE)}:}{Should a preview of the beginning of the data be displayed when the object is printed?}

	    \item{\code{options(matter.show.head.n=6)}:}{The number of elements, rows, and/or columns to be displayed by the object preview.}

	    \item{\code{options(matter.coerce.altrep=FALSE)}:}{When coercing \code{matter} objects to native R objects (such as \code{matrix}), should a \code{matter}-backed ALTREP object be returned instead? The initial coercion will be cheap, and the result will look like a native R object. This does not guarantee that the full data is never read into memory. Not all functions are ALTREP-aware at the C-level, so some operations may still trigger the full data to be read into memory. This should only ever happen once, as long as the object is not duplicated, though.}

	    \item{\code{options(matter.coerce.altrep.list=FALSE)}:}{Should a \code{matter}-backed ALTREP list be returned when coercing \code{matter_list} lists to native R lists? Lists are treated differently, because the coercion is more costly, as the metadata for each list element must be uncompressed and converted to separate ALTREP representations. (Note that this does not affect \code{matter_df} data frames, which do not compress metadata about the columns, because the columns are regular \code{matter} vectors.)}

	    \item{\code{options(matter.wrap.altrep=FALSE)}:}{When coercing to a \code{matter}-backed ALTREP object, should the object be wrapped in an ALTREP wrapper? (This is always done in cases where the coercion preserves existing attributes.) This allows setting of attributes without triggering a (potentially expensive) duplication of the object when safe to do so.}

	    \item{\code{options(matter.dump.dir=tempdir())}:}{Temporary directory where \code{matter} object files should be dumped when created without user-specified file paths.}
    }
}

\keyword{misc}
