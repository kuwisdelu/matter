\name{bsearch}

\alias{bsearch}
\alias{reldiff}

\title{Binary Search with Approximate Matching}

\description{
    Use a binary search to find approximate matches for the elements of its first argument among those in its second. This implementation allows for returning the index of the nearest match if there are no exact matches. It also allows specifying a tolerance for the comparison.
}

\usage{
bsearch(x, table, tol = 0, tol.ref = "abs",
		nomatch = NA_integer_, nearest = FALSE)

reldiff(x, y, ref = "y")
}

\arguments{
	\item{x}{A vector of values to be matched. Only integer, numeric, and character vectors are supported.}

	\item{y, table}{A sorted (non-decreasing) vector of values to be matched against. Only integer, numeric, and character vectors are supported.}

	\item{tol}{The tolerance for matching doubles. Must be >= 0.}

	\item{ref, tol.ref}{One of 'abs', 'x', or 'y'. If 'abs', then comparison is done by taking the absolute difference. If either 'x' or 'y', then relative differences are used, and this specifies which to use as the reference (target) value. For strings, this uses the Hamming distance (number of errors), normalized by the length of the reference string for relative differences.}

	\item{nomatch}{The value to be returned in the case when no match is found, coerced to an integer. (Ignored if \code{nearest = TRUE}.)}

	\item{nearest}{Should the index of the closest match be returned if no exact matches are found?}
}

\details{
    The algorithm is implemented in C and currently only works for 'integer', 'numeric', and 'character' vectors. If there are multiple matches, then the first match that is found will be returned, with no guarantees. If a nonzero tolerance is provided, the closest match will be returned.

    The "nearest" match for strings when there are no exact matches is decided by the match with the most initial matching characters. Tolerance is ignored for strings and integers. Behavior is undefined and results may be unexpected if \code{values} includes NAs.
}

\value{
    A vector of the same length as \code{x}, giving the indexes of the matches in \code{table}.
}

\author{Kylie A. Bemis}

\seealso{
	\code{\link{asearch}},
    \code{\link{match}},
    \code{\link{pmatch}},
    \code{\link{findInterval}}
}

\examples{
a <- c(1.11, 2.22, 3.33, 5.0, 5.1)

bsearch(2.22, a) # 2
bsearch(3.0, a) # NA
bsearch(3.0, a, nearest=TRUE) # 3
bsearch(3.0, a, tol=0.1, tol.ref="values") # 3

b <- c("hello", "world!")
bsearch("world!", b) # 2
bsearch("worl", b) # NA
bsearch("worl", b, nearest=TRUE) # 2
}

\keyword{utilities}
