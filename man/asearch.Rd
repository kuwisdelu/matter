\name{asearch}

\alias{asearch}

\title{Approximate Search with Interpolation}

\description{
    Search a set of values indexed by a sorted (non-decreasing) vector of keys. Finds the values corresponding to matches between the elements of the first argument and the keys. Approximate matching is allowed within a specified tolerance. Interpolation can be performed for key collisions and/or non-exact matches.
}

\usage{
asearch(x, keys, values, tol = 0, tol.ref = "abs",
		nomatch = NA_integer_, interp = "none")
}

\arguments{
	\item{x}{A vector of values to be matched. Only integer, numeric, and character vectors are supported.}

	\item{keys}{A sorted (non-decreasing) vector of keys to match against. Only integer, numeric, and character vectors are supported.}

	\item{values}{A vector of values corresponding to the keys. Only numeric types are supported.}

	\item{tol}{The tolerance for matching doubles. Must be nonnegative.}

	\item{tol.ref}{One of 'abs', 'x', or 'y'. If 'abs', then comparison is done by taking the absolute difference. If either 'x' or 'y', then relative differences are used, and this specifies which to use as the reference (target) value.}

	\item{nomatch}{The value to be returned in the case when no match is found, coerced to an integer. (Ignored if \code{nearest = TRUE}.)}

	\item{interp}{Interpolation scheme for non-exact matches or key collisions. One of 'none', 'mean', 'sum', 'max', 'min', 'area', 'linear', 'cubic', 'gaussian', or 'lanczos'.}
}

\details{
    The algorithm is implemented in C and relies on binary search when the keys are sorted. See implementation details for \code{bsearch} for matching behavior when keys are sorted. For unsorted keys, a fallback to linear search is used.
}

\value{
    A vector of the same length as \code{x}, giving the \code{values} corresponding to matching \code{keys}.
}

\author{Kylie A. Bemis}

\seealso{
	\code{\link{bsearch}}
}

\examples{
keys <- c(1.11, 2.22, 3.33, 5.0, 5.1)
values <- keys^1.11

asearch(2.22, keys, values) # 2.42359
asearch(3.0, keys, values) # NA
asearch(3.0, keys, values, tol=0.1, tol.ref="y") # 3.801133
}

\keyword{utilities}
