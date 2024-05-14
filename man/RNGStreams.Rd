\name{RNGStreams}

\alias{RNGStreams}
\alias{getRNGStream}
\alias{setRNGStream}

\title{Parallel RNG Streams}

\description{
    These functions provide utilities for working with multiple streams of pseudo-random numbers.
}

\usage{
RNGStreams(n = length(size), size = 1L)

getRNGStream()

setRNGStream(seed = NULL, kind = NULL)
}

\arguments{
	\item{n}{The number of RNG streams to create.}

	\item{size}{If \code{RNGkind} is set to "L'Ecuyer-CMRG", then iterate this number of RNG substreams between each returned stream.}

	\item{seed}{A valid RNG stream to assign to \code{.Random.seed}, or a list with elements named \code{seed} and \code{kind}.}

	\item{kind}{The \code{RNGkind} to use when setting the RNG stream.}
}

\value{
    For \code{RNGStreams}, a list of length \code{n} with RNG streams (with elements named \code{seed} and \code{kind}) for use with \code{setRNGStream}.
}

\author{Kylie A. Bemis}

\seealso{
	\code{\link[base]{RNGkind}},
    \code{\link[parallel]{nextRNGStream}}
}

\examples{
# create parallel RNG streams
register(SerialParam())
RNGkind("L'Ecuyer-CMRG")
set.seed(1)
seeds <- RNGStreams(4)
seeds
}

\keyword{utilities}
