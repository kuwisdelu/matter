\name{rowDists}

\alias{colDists}
\alias{rowDists}

\alias{colDists,ANY,missing-method}
\alias{colDists,matrix,matrix-method}
\alias{colDists,matter_mat,matrix-method}
\alias{colDists,matrix,matter_mat-method}
\alias{colDists,sparse_mat,matrix-method}
\alias{colDists,matrix,sparse_mat-method}

\alias{rowDists,ANY,missing-method}
\alias{rowDists,matrix,matrix-method}
\alias{rowDists,matter_mat,matrix-method}
\alias{rowDists,matrix,matter_mat-method}
\alias{rowDists,sparse_mat,matrix-method}
\alias{rowDists,matrix,sparse_mat-method}

\alias{rowdist}
\alias{coldist}
\alias{rowdist_at}
\alias{coldist_at}

\title{Compute Distances between Rows or Columns of a Matrix}

\description{
    Compute and return the distances between specific rows or columns of matrices according to a specific distance metric.
}

\usage{
\S4method{rowDists}{ANY,missing}(x, at, \dots, BPPARAM = bpparam())

\S4method{colDists}{ANY,missing}(x, at, \dots, BPPARAM = bpparam())

\S4method{rowDists}{matrix,matrix}(x, y, \dots, BPPARAM = bpparam())

\S4method{colDists}{matrix,matrix}(x, y, \dots, BPPARAM = bpparam())

\S4method{rowDists}{matter_mat,matrix}(x, y, \dots, BPPARAM = bpparam())

\S4method{colDists}{matrix,matter_mat}(x, y, \dots, BPPARAM = bpparam())

\S4method{rowDists}{sparse_mat,matrix}(x, y, \dots, BPPARAM = bpparam())

\S4method{colDists}{matrix,sparse_mat}(x, y, \dots, BPPARAM = bpparam())

rowdist(x, y = x, metric = "euclidean", p = 2, weights = NULL)

coldist(x, y = x, metric = "euclidean", p = 2, weights = NULL)

rowdist_at(x, ix, y = x, iy = list(1L:nrow(y)),
	metric = "euclidean", p = 2, weights = NULL)

coldist_at(x, ix, y = x, iy = list(1L:ncol(y)),
	metric = "euclidean", p = 2, weights = NULL)
}

\arguments{
	\item{x, y}{Numeric matrices for which distances should be calculated according to rows or columns.}

	\item{at}{A list of specific row or column indices for which to calculate the distances. Each row or column of \code{x} will be compared to the rows or columns indicated by the corresponding element of \code{at}.}

	\item{metric}{Distance metric to use when finding the nearest neighbors. Supported metrics include "euclidean", "maximum", "manhattan", and "minkowski".}

	\item{p}{The power for the Minkowski distance.}

	\item{weights}{A numeric vector of weights for the distance components if calculating weighted distances. For example, the weighted Euclidean distance is \code{sqrt(sum(w * (x - y)^2))}.}

	\item{ix, iy}{A list of specific row or column indices for which to calculate the pairwise distances. Numeric vectors will be coerced to lists. Each list element should give a vector of indices to use for a distance computation. Elements of length 1 will be recycled to an appropriate length.}

	\item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{\dots}{Additional arguments passed to \code{rowdist()} or \code{coldist()}.}
}

\details{
    \code{rowdist()} and \code{coldist()} calculate straightforward distances between each row or column, respectively in \code{x} and \code{y}. If \code{y = x} (the default), then the output of \code{rowdist()} will match the output of \code{\link{dist}} (except it will be an ordinary matrix).

    \code{rowdist_at()} and \code{coldist_at()} allow passing a list of specific row or column indices for which to calculate the distances.

    \code{rowDists()} and \code{colDists()} are S4 generics. The current methods provide (optionally parallelized) versions of \code{rowdist()} and \code{coldist()} for \code{\linkS4class{matter_mat}} and \code{\linkS4class{sparse_mat}} matrices.
}

\value{
    For \code{rowdist()} and \code{coldist()}, a matrix with rows equal to the number of observations in \code{x} and columns equal to the number of observations in \code{y}.

    For \code{rowdist_at()} and \code{coldist_at()}, a list where each element gives the pairwise distances corresponding to the indices given by \code{ix} and \code{iy}.

    \code{rowDists()} and \code{colDists()} have corresponding return values depending on whether \code{at} has been specified.
}

\author{Kylie A. Bemis}

\seealso{
	\code{\link{dist}}
}

\examples{
register(SerialParam())

set.seed(1)

x <- matrix(runif(25), nrow=5, ncol=5)
y <- matrix(runif(25), nrow=3, ncol=5)

rowDists(x) # same as as.matrix(dist(x))
rowDists(x, y)

# distances between:
# x[1,] vs x[,]
# x[5,] vs x[,]
rowdist_at(x, c(1,5))

# distances between:
# x[1,] vs x[1:3,]
# x[5,] vs x[3:5,]
rowdist_at(x, ix=c(1,5), iy=list(1:3, 3:5))

# distances between:
# x[i,] vs x[(i-1):(i+1),]
rowDists(x, at=roll(1:5, width=3))
}

\keyword{array}
