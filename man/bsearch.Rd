\name{bsearch}

\alias{bsearch}

\title{Binary Search with Approximate Matching}

\description{
    Given a set of keys and a sorted (non-decreasing) vector of values, use a binary search to find the indexes in \code{values} that match the values of \code{key}. This implementation allows for returning the index of the nearest match if there are no exact matches. It also allows specifying a tolerance for comparison of doubles.
}

\usage{
bsearch(key, values, tol = 0, tol.ref = "none",
		nomatch = NA_integer_, nearest = FALSE)
}

\arguments{
	\item{key}{A vector of keys to match.}

	\item{values}{A sorted (non-decreasing) vector of values to be matched.}

	\item{tol}{The tolerance for matching doubles. Must be >= 0.}

	\item{tol.ref}{One of 'none', 'key', or 'values'. If 'none', then comparison of doubles is done by taking the absolute difference. If either 'key' or 'values', then relative differences are used, and this specifies which to use as the reference (target) value.}

	\item{nomatch}{The value to be returned in the case when no match is found, coerced to an integer. (Ignored if \code{nearest = TRUE}.)}

	\item{nearest}{Should the index of the closest match be returned if no exact matches are found?}
}

\details{
    The algorithm is implemented in C and currently only works for 'integer', 'numeric', and 'character' vectors. If there are multiple matches, the first match that is found will be returned, but there are no guarantees about \emph{which} match is found.

    The "nearest" match for strings when there are no exact matches is decided by the match with the most initial matching characters. Tolerance is ignored for strings and integers. Behavior is undefined and results may be unexpected if \code{values} includes NAs.
}

\value{
    A vector of the same length as \code{key}, giving the indexes of the matches in \code{values}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link[base]{match}},
    \code{\link[base]{pmatch}},
    \code{\link[base]{findInterval}}
}

\examples{
x <- c(1.11, 2.22, 3.33, 5.0, 5.1)

bsearch(2.22, x) # 2
bsearch(3.0, x) # NA
bsearch(3.0, x, nearest=TRUE) # 3
bsearch(3.0, x, tol=0.1, tol.ref="values") # 3

y <- c("hello", "world!")
bsearch("world!", y) # 2
bsearch("worl", y) # NA
bsearch("worl", y, nearest=TRUE) # 2
}

\keyword{utilities}
