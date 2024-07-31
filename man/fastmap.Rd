\name{fastmap}

\alias{fastmap}
\alias{predict.fastmap}
\alias{rowDistFun}
\alias{colDistFun}

\title{FastMap Projection}

\description{
    The FastMap algorithm performs approximate multidimensional scaling (MDS) based on any distance function. It is faster and more efficient than traditional MDS algorithms, scaling as O(n) rather than O(n^2). FastMap accomplishes this by finding two distant pivot objects on some hyperplane for each projected dimension, and then projecting all other objects onto the line between these pivots.
}

\usage{
# FastMap projection
fastmap(x, k = 3L, distfun = NULL,
	transpose = FALSE, niter = 3L, verbose = NA,
	BPPARAM = bpparam(), \dots)

\S3method{predict}{fastmap}(object, newdata, \dots)

# Distance functionals
rowDistFun(x, y, metric = "euclidean", p = 2, weights = NULL,
	verbose = NA, chunkopts = list(),
	BPPARAM = bpparam(), \dots)
colDistFun(x, y, metric = "euclidean", p = 2, weights = NULL,
	verbose = NA, chunkopts = list(),
	BPPARAM = bpparam(), \dots)
}

\arguments{
	\item{x, y}{A numeric matrix-like object.}

	\item{k}{The number of FastMap components to project.}

	\item{distfun}{The function of the form \code{function(x, y, \dots)} used to generate a distance function of the form \code{function(i)} giving the distances between the \code{i}th object(s) in \code{x} and \emph{all} objects in \code{y}.}

	\item{transpose}{A logical value indicating whether \code{x} should be considered transposed or not. This only used internally to indicate whether the input matrix is (P x N) or (N x P), and therefore extract the number of objects and their names.}

	\item{niter}{The maximum number of iterations for finding the pivots.}

	\item{\dots}{Additional options passed to \code{distfun}.}

	\item{object}{An object inheriting from \code{fastmap}.}

	\item{newdata}{An optional data matrix to use for the prediction.}

	\item{metric}{Distance metric to use when finding the nearest neighbors. Supported metrics include "euclidean", "maximum", "manhattan", and "minkowski".}

	\item{p}{The power for the Minkowski distance.}

	\item{weights}{A numeric vector of weights for the distance components if calculating weighted distances. For example, the weighted Euclidean distance is \code{sqrt(sum(w * (x - y)^2))}.}

	\item{verbose}{Should progress be printed for each iteration? \emph{Passed} to the distance function returned by \code{distfun}.}

	\item{chunkopts}{See \code{chunkApply} for details. \emph{Passed} to the distance function returned by \code{distfun}.}

	\item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}. \emph{Passed} to the distance function returned by \code{distfun}}
}

\details{
	The pivots are initialized randomly for each new dimension, so the selection of pivots (and therefore the resulting projection) can be sensitive to the random seed for some datasets.

    A custom distance function can be passed via \code{distfun}. If not provided, then this defaults to \code{rowDistFun()} if \code{transpose=FALSE} or \code{colDistFun()} if \code{transpose=TRUE}.

    If a custom function is passed, it should take the form \code{function(x, y, \dots)}, and it must \emph{return} a function of the form \code{function(i)}. The returned function should return the distances between the \code{i}th object(s) in \code{x} and \emph{all} objects in \code{y}. \code{rowDistFun()} and \code{colDistFun()} are examples of functions that satisfy these properties.
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
