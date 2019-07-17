\name{matter-datatypes}

\alias{matter-datatypes}
\alias{datatypes}

\title{Data Types for ``matter'' Objects}

\description{
	The \code{matter} package defines a number of data types for translating between data elements stored in virtual memory and data elements loaded into R. These are typically set and stored via the \code{datamode} argument and slot.

	At the R level, \code{matter} objects may be any of the following data modes:

	\itemize{
	    \item{\code{raw}:}{\code{matter} objects of this mode are typically vectors of raw bytes.}

	    \item{\code{logical}:}{Any \code{matter} object that represents a logical vector or has had any Compare or Logic delayed operations applied to it will be of this type.}

	    \item{\code{integer}:}{\code{matter} objects represented as integers in R.}

	    \item{\code{numeric}:}{\code{matter} objects represented as doubles in R.}

	    \item{\code{character}:}{\code{matter} objects representated as character vectors in R.}

	    \item{\code{virtual}:}{A number of \code{matter} objects do not necessarily represent out-of-memory data, or may include a number of components mixed between virtual memory and real memory; these will use this data mode.}
    }

    In virtual memory, \code{matter} objects may be composed of atomic units of the following data types:

	\itemize{
	    \item{\code{char}:}{8-bit signed integer; defined as \code{char}.}

	    \item{\code{uchar}:}{8-bit unsigned integer; used for `Rbyte' or `raw'; defined as \code{unsigned char}.}

	    \item{\code{short}:}{16-bit signed integer; defined as \code{int16_t}.}

	    \item{\code{ushort}:}{16-bit unsigned integer; defined as \code{uint16_t}.}

	    \item{\code{int}:}{32-bit signed integer; defined as \code{int32_t}.}

	    \item{\code{uint}:}{32-bit unsigned integer; defined as \code{uint32_t}.}

	    \item{\code{long}:}{64-bit signed integer; defined as \code{int64_t}.}

	    \item{\code{ulong}:}{64-bit unsigned integer; defined as \code{uint64_t}.}

	    \item{\code{float}:}{Platform dependent, but usually a 32-bit float; defined as \code{float}.}

	    \item{\code{double}:}{Platform dependent, but usually a 64-bit float; defined as \code{double}.}
	}

	While a substantial effort is made to coerce data elements properly between data types, sometimes this cannot be done losslessly. This will generate a warning (typically \emph{many} such warnings) that can be silenced by setting \code{options(matter.cast.warning=FALSE)}.

	Note that the unsigned data types do not support \code{NA}; coercion to signed \code{short} and \code{long} attempts to preserve missingness. The special values \code{NaN}, \code{Inf}, and \code{-Inf} are only supported by the floating-point types, and will be set to \code{NA} for signed integral types, and to \code{0} for unsigned integral types.
}

\keyword{IO}
