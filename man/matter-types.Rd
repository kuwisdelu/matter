\name{matter-types}

\alias{matter-types}
\alias{types}

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
    }

    In virtual memory, \code{matter} objects may be composed of atomic units of the following data types:

	\itemize{
	    \item{\code{char}:}{8-bit signed integer; defined as \code{char}.}

	    \item{\code{uchar}:}{8-bit unsigned integer; used for `Rbyte' or `raw'; defined as \code{unsigned char}.}

	    \item{\code{int16}:}{16-bit signed integer; defined as \code{int16_t}. May be aliased as `short' and `16-bit integer'.}

	    \item{\code{uint16}:}{16-bit unsigned integer; defined as \code{uint16_t}. May be aliased as `ushort' and `16-bit unsigned integer'.}

	    \item{\code{int32}:}{32-bit signed integer; defined as \code{int32_t}. May be aliased as `int' and `32-bit integer'.}

	    \item{\code{uint32}:}{32-bit unsigned integer; defined as \code{uint32_t}. May be aliased as `uint' and `32-bit unsigned integer'.}

	    \item{\code{int64}:}{64-bit signed integer; defined as \code{int64_t}. May be aliased as `long' and `64-bit integer'.}

	    \item{\code{uint64}:}{64-bit unsigned integer; defined as \code{uint64_t}. May be aliased as `ulong' and `64-bit unsigned integer'.}

	    \item{\code{float32}:}{32-bit float; defined as \code{float}. May be aliased as `float' and `32-bit float'.}

	    \item{\code{float64}:}{64-bit float; defined as \code{double}. May be aliased as `double' and `64-bit float'.}
	}

	While a substantial effort is made to coerce data elements properly between data types, sometimes this cannot be done losslessly. Loss of precision is silent, while values outside of the representable range will generate a warning (sometimes \emph{many} such warnings) and will be set to \code{NA} if available or 0 otherwise.

	Note that the unsigned data types do not support \code{NA}; coercion between signed integer types attempts to preserve missingness. The special values \code{NaN}, \code{Inf}, and \code{-Inf} are only supported by the floating-point types, and will be set to \code{NA} for signed integral types, and to \code{0} for unsigned integral types.
}

\keyword{IO}
