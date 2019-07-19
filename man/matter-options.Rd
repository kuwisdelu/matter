\name{matter-options}

\alias{matter-options}

\title{Options for ``matter'' Objects}

\description{
	The \code{matter} package provides the following options:

	\itemize{
	    \item{\code{options(matter.cast.warning=TRUE)}:}{Should a warning be emitted when casting between data types results in a loss of precision?}

	    \item{\code{options(matter.show.head=TRUE)}:}{Should a preview of the beginning of the data be displayed when the object is printed?}

	    \item{\code{options(matter.show.head.n=6)}:}{The number of elements, rows, and/or columns to be displayed by the object preview.}

	    \item{\code{options(matter.coerce.altrep=TRUE)}:}{When coercing \code{matter} objects to native R objects (such as \code{matrix}), should a \code{matter}-backed ALTREP object be returned instead? The initial coercion will be cheap, and the result will look like a native R object. This does not guarantee that the full data is never read into memory. Not all functions are ALTREP-aware at the C-level, so some operations may still trigger the full data to be read into memory. This should only ever happen once, as long as the object is not duplicated, though.}

	    \item{\code{options(matter.wrap.altrep=TRUE)}:}{When coercing to a \code{matter}-backed ALTREP object, should the object be wrapped in an ALTREP wrapper? (This is always done in cases where the coercion preserves existing attributes.) This allows setting of attributes without triggering a (potentially expensive) duplication of the object when safe to do so.}
    }
}

\keyword{misc}
