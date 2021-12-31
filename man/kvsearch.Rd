\name{kvsearch}

\alias{kvsearch}

\title{Key-Value Search with Approximate Matching}

\description{
    Given a set of keys and a sorted (non-decreasing) vector of values, find the values corresponding to matches between the elements of the first argument and the reference keys. Approximate matching is allowed within a specified tolerance.
}

\usage{
kvsearch(key, values, tol = 0, tol.ref = "abs",
		nomatch = NA_integer_, dups = "top")
}

\arguments{
	\item{key}{A vector of keys to match.}

	\item{values}{A sorted (non-decreasing) vector of values to be matched.}

	\item{tol}{The tolerance for matching doubles. Must be >= 0.}

	\item{tol.ref}{One of 'abs', 'x', or 'y'. If 'abs', then comparison is done by taking the absolute difference. If either 'x' or 'y', then relative differences are used, and this specifies which to use as the reference (target) value.}

	\item{nomatch}{The value to be returned in the case when no match is found, coerced to an integer. (Ignored if \code{nearest = TRUE}.)}

	\item{dups}{How to resolve matches to items with duplicate keys. One of 'top', 'sum', or 'max'. For 'top', choose the first matched key-value pair. For \code{tol > 0}, this will still select the closest match. For other options, summarize over all matches within the given tolerance.}
}

\details{
    The algorithm is implemented in C and relies on binary search when the keys are sorted. See implementation details for \code{bsearch} for mathcing behavior when keys are sorted. For unsorted keys, a fallback to linear search is used.
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

kvsearch(2.22, keys, values) # 2.42359
kvsearch(3.0, keys, values) # NA
kvsearch(3.0, keys, values, tol=0.1, tol.ref="y") # 3.801133
}

\keyword{utilities}
