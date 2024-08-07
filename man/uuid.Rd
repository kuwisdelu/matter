\name{uuid}

\alias{uuid}
\alias{tempmem}
\alias{raw2hex}
\alias{hex2raw}

\title{Universally Unique Identifiers}

\description{
    Generate a UUIDv4.
}

\usage{
uuid(uppercase = FALSE)

hex2raw(x)

raw2hex(x, uppercase = FALSE)
}

\arguments{
	\item{x}{A vector to convert between \code{raw} bytes and hexadecimal strings.}

	\item{uppercase}{Should the result be in uppercase?}
}

\details{
    \code{uuid} generates a random (version 4) universally unique identifier. A private RNG stream is used to avoid collisions due to \code{set.seed} and to avoid altering the global \code{.Random.seed}.

    \code{hex2raw} converts a hexadecimal string to a \code{raw} vector.

    \code{raw2hex} converts a \code{raw} vector to a hexadecimal string.
}

\value{
    For \code{uuid}, a list of length 2:

    \itemize{
        \item{\code{string}: A character vector giving the UUID.}
        \item{\code{bytes}: The \code{raw} bytes of the UUID.}
    }

    For \code{hex2raw}, a \code{raw} vector.

    For \code{raw2hex}, a character vector of length 1.
}

\author{Kylie A. Bemis}

\examples{
id <- uuid()
id
hex2raw(id$string)
raw2hex(id$bytes)
}

\keyword{utilities}
