\name{rowdist}

\alias{rowdist}
\alias{coldist}
\alias{rowdist_at}
\alias{coldist_at}

\title{Compute Distances between Rows or Columns of a Matrix}

\description{
    Compute and return the distances between specific rows or columns of matrices according to a specific distance metric.
}

\usage{
rowdist(x, y = x, metric = "euclidean", p = 2)

coldist(x, y = x, metric = "euclidean", p = 2)

rowdist_at(x, ix, y = x, iy = list(1L:nrow(y)),
	metric = "euclidean", p = 2)

coldist_at(x, ix, y = x, iy = list(1L:ncol(y)),
	metric = "euclidean", p = 2)
}

\arguments{
	\item{x, y}{Numeric matrices for which distances should be calculated according to rows or columns.}

	\item{metric}{Distance metric to use when finding the nearest neighbors. Supported metrics include "euclidean", "maximum", "manhattan", and "minkowski".}

	\item{p}{The power for the Minkowski distance.}

	\item{ix, iy}{A list of specific row or column indices for which to calculate the pairwise distances. Numeric vectors will be coerced to lists. Each list element should give a vector of indices to use for a distance computation. Elements of length 1 will be recycled to an appropriate length.}
}

\details{
    \code{rowdist()} and \code{coldist()} calculate straightforward distances between each row or column, respectively in \code{x} and \code{y}. If \code{y = x} (the default), then the output of \code{rowdist()} will match the output of \code{\link{dist}} (except it will be an ordinary matrix).

    \code{rowdist_at()} and \code{coldist_at()} allow passing a list of specific row or column indices for which to calculate the distances.
}

\value{
    For \code{rowdist()} and \code{coldist()}, a matrix with rows equal to the number of observations in \code{x} and columns equal to the number of observations in \code{y}.

    For \code{rowdist_at()} and \code{coldist_at()}, a list where each element gives the pairwise distances corresponding to the indices given by \code{ix} and \code{iy}.
}

\author{Kylie A. Bemis}

\seealso{
	\code{\link{dist}}
}

\examples{
set.seed(1)
x <- matrix(runif(25), nrow=5, ncol=5)
y <- matrix(runif(25), nrow=3, ncol=5)

rowdist(x) # same as as.matrix(dist(x))
rowdist(x, y)

# distances between:
# x[1,] vs x[,]
# x[5,] vs x[,]
rowdist_at(x, c(1,5))

# distances between:
# x[1,] vs x[1:3,]
# x[5,] vs x[3:5,]
rowdist_at(x, ix=c(1,5), iy=list(1:3, 3:5))
}

\keyword{array}
