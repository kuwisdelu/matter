\name{RNGStreams}

\alias{RNGStreams}
\alias{getRNGStream}
\alias{setRNGStream}

\title{Parallel RNG Streams}

\description{
    These functions provide utilities for working with multiple streams of pseudo-random numbers.
}

\usage{
RNGStreams(n, parallel = FALSE)

getRNGStream(env = globalenv())

setRNGStream(seed = NULL, env = globalenv())
}

\arguments{
	\item{n}{The number of RNG streams to create.}

	\item{parallel}{Should the \code{RNGkind} be set to the parallel-safe "L'Ecuyer-CMRG" generator if it isn't already? Note that if the RNG is set to a different generator, this will reset the random seed.}

	\item{env}{The environment in which to get or set the random number generator. Defaults to the global environment.}

	\item{seed}{A valid RNG stream to assign to \code{.Random.seed}. For safety, should be a value returned by either \code{getRNGStream()} or \code{RNGStreams()}.}
}

\value{
    For \code{RNGStreams} a list of length \code{n} with RNG streams for \code{.Random.seed}.
}

\author{Kylie A. Bemis}

\seealso{
	\code{\link[base]{RNGkind}},
    \code{\link[parallel]{nextRNGStream}}
}

\examples{
# initialize parallel RNG
RNGStreams(parallel=TRUE)

# create and use RNG streams
register(SerialParam())
set.seed(1)
seeds <- RNGStreams(5)
chunkLapply(rep.int(10, 5), rnorm, seeds=seeds)
}

\keyword{utilities}
