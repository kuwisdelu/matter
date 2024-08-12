\name{seq_rel}

\alias{seq_rel}

\title{Relative Sequence Generation}

\description{
    Generate a sequence with spacing based on relative differences.
}

\usage{
seq_rel(from, to, by)
}

\arguments{
	\item{from}{The start of the sequence.}

	\item{to}{The end of the sequence.}

    \item{by}{The relative difference between consecutive elements.}
}

\details{
    Because the relative differences depend on whether we treat the lesser or greater term as the "reference", the function actually treats each element as the center of a bin of width \code{by} (relative to that element).
}

\value{
    A numeric vector of some (algorithmically-determined) length where consecutive elements are separated by consistent relative differences.
}

\author{Kyle Dahlin and Kylie A. Bemis}

\seealso{
    \code{\link{seq}}
}

\examples{
# create a sequence w/ 1e-4 relative diff spacing
x <- seq_rel(500, 505, by=1e-4)
x

# relative spacing is averaged between lesser/greater element
head(diff(x) / x[-1])
head(diff(x) / x[-length(x)])
dx <- 0.5 * ((diff(x) / x[-1]) + (diff(x) / x[-length(x)]))
dx
}

\keyword{utilities}
