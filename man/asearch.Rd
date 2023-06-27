\name{asearch}

\alias{asearch}

\title{Approximate Key-Value Search}

\description{
    Search a set of values indexed by a sorted (non-decreasing) vector of keys. Finds the values corresponding to matches between the elements of the first argument and the keys. Approximate matching is allowed within a specified tolerance.
}

\usage{
asearch(x, keys, values, tol = 0, tol.ref = "abs",
		nomatch = NA_integer_)
}

\arguments{
	\item{x}{A vector of values to be matched. Only integer, numeric, and character vectors are supported.}

	\item{keys}{A sorted (non-decreasing) vector of keys to match against. Only integer, numeric, and character vectors are supported.}

	\item{values}{A vector of values corresponding to the keys. Only numeric types are supported.}

	\item{tol}{The tolerance for matching. Must be nonnegative.}

	\item{tol.ref}{If 'abs', then comparison is done by taking the absolute difference. If 'x', then relative differences are used.}

	\item{nomatch}{The value to be returned in the case when no match is found.}
}

\details{
    The algorithm is implemented in C and relies on binary search when the keys are sorted. The keys are sorted internally if necessary. See details for \code{bsearch} for matching behavior.
}

\value{
    A vector of the same length as \code{x}, giving the \code{values} corresponding to matching \code{keys}.
}

\author{Kylie A. Bemis}

\seealso{
	\code{\link{bsearch}},
	\code{\link{approx1}}
}

\examples{
keys <- c(1.11, 2.22, 3.33, 5.0, 5.1)
values <- keys^1.11

asearch(2.22, keys, values) # 2.42359
asearch(3.0, keys, values) # NA
asearch(3.0, keys, values, tol=0.2, tol.ref="x") # 3.801133
}

\keyword{utilities}
