\name{fastmap}

\alias{fastmap}
\alias{predict.fastmap}

\title{FastMap Projection}

\description{
    The FastMap algorithm performs approximate multidimensional scaling (MDS) based on any distance function. It is faster and more efficient than traditional MDS algorithms, scaling as O(n) rather than O(n^2). FastMap accomplishes this by finding two distant pivot objects on some hyperplane for each projected dimension, and then projecting all other objects onto the line between these pivots.
}

\usage{
# FastMap projection
fastmap(x, k = 3L, group = NULL, distfun = NULL,
	transpose = FALSE, pivots = 10L, niter = 10L, verbose = NA, \dots)

\S3method{predict}{fastmap}(object, newdata, \dots)
}

\arguments{
	\item{x}{A numeric matrix-like object.}

	\item{k}{The number of FastMap components to project.}

	\item{group}{Grouping variable if pivots should be guaranteed to come from different groups.}

	\item{distfun}{A distance function with the same usage (i.e., supports the same arguments and return values) as \code{\link{rowDists}} or \code{\link{colDists}}.}

	\item{transpose}{A logical value indicating whether \code{x} should be considered transposed or not. This only used internally to indicate whether the input matrix is (P x N) or (N x P), and therefore extract the number of objects and their names.}

	\item{pivots}{The number of pivot candidates to attempt each iteration. Using more pivot candidates can help improve the quality of the pivot selection. Using fewer can help speed up computation.}

	\item{niter}{The maximum number of iterations for selecting the pivots.}

	\item{\dots}{Additional options passed to \code{distfun}.}

	\item{object}{An object inheriting from \code{fastmap}.}

	\item{newdata}{An optional data matrix to use for the prediction.}

	\item{verbose}{Should progress be printed for each pivot iteration and FastMap component projection?}
}

\details{
	The pivots are initialized randomly for each new dimension, so the selection of pivots (and therefore the resulting projection) can be sensitive to the random seed for some datasets.

    A custom distance function can be passed via \code{distfun}. If not provided, then this defaults to \code{\link{rowDists}} if \code{transpose=FALSE} or \code{\link{colDists}} if \code{transpose=TRUE}.

    If a custom function is passed, it must support the same arguments and return values as \code{\link{rowDists}} and \code{\link{colDists}}.
}

\value{
    An object of class \code{fastmap}, with the following components:

    \itemize{
        \item{\code{x}: The projected variable matrix.}
        \item{\code{sdev}: The standard deviations of each column of the projected matrix \code{x}.}
        \item{\code{pivots}: A matrix giving the indices of the  pivots and the distances between them.}
        \item{\code{pivot.array}: A subset of the original data matrix containing only the pivots.}
        \item{\code{distfun}: The function used to generate the distance function.}
    }
}

\author{Kylie A. Bemis}

\references{
	C. Faloutsos, and D. Lin. ``FastMap: A Fast Algorithm for Indexing, Data-Mining and Visualization of Traditional and Multimedia Datasets.'' Proceedings of the 1995 ACM SIGMOD international conference on Management of data, pp. 163 - 174, June 1995.
}

\seealso{
	\code{\link{rowDists}},
	\code{\link{colDists}},
	\code{\link{cmdscale}},
	\code{\link{prcomp}}
}

\examples{
register(SerialParam())
set.seed(1)

a <- matrix(sort(runif(500)), nrow=50, ncol=10)
b <- matrix(rev(sort(runif(500))), nrow=50, ncol=10)
x <- cbind(a, b)

fm <- fastmap(x, k=2)
}

\keyword{multivariate}
