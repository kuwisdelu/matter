\name{kdsearch}

\alias{kdsearch}
\alias{kdtree}

\title{K-Dimensional Search within a Neighborhood}

\description{
    Search a matrix of K-dimensional data points and return the indices of all data points within a specified tolerance in each dimension.
}

\usage{
kdsearch(x, data, tol = 0, tol.ref = "abs")

kdtree(data)
}

\arguments{
	\item{x}{A numeric matrix coordinates to be matched. Each column should be different dimension.}

	\item{data}{Either a \code{kdtree} object returned by \code{kdtree()}, or a numeric matrix of coordinates to search, where each column is a different dimension.}

	\item{tol}{The tolerance for finding neighboring points in each dimension. May be a vector with the same length as the number of dimensions. Must be positive.}

	\item{tol.ref}{One of 'abs', 'x', or 'y'. If 'abs', then comparison is done by taking the absolute difference. If either 'x' or 'y', then relative differences are used, and this specifies which to use as the reference (target) value.}
}

\details{
    The algorithm is implemented in C and works by building a kd-tree to perform the search. If multiple calls to \code{kdsearch()} are expected on the same data, it can be much faster to build the tree once with \code{kdtree()}.

    A kd-tree is essentially a multidimensional generalization of a binary search tree. Building the search tree is O(n * log n) and searching for a single data point is O(log n).
}

\value{
    A list with length equal to the number of rows of \code{x}, where each list element is a vector of indexes of the matches in \code{data}.
}

\author{Kylie A. Bemis}

\seealso{
	\code{\link{asearch}},
	\code{\link{bsearch}},
	\code{\link{approx2}},
}

\examples{
d <- expand.grid(x=1:10, y=1:10)
x <- rbind(c(1.11, 2.22), c(3.33, 4.44))

kdsearch(x, d, tol=1)
}

\keyword{tree}
\keyword{spatial}
\keyword{utilities}
