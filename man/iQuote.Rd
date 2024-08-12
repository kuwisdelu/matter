\name{iQuote}

\alias{iQuote}

\title{Quote Identifiers}

\description{
    Quote strings representing variable identifiers with backticks for use in formulas.
}

\usage{
iQuote(x, q = "`")
}

\arguments{
	\item{x}{An object coercible to a character vector.}

	\item{q}{The kind of quotes to be used.}
}

\value{
    A character vector of the same length as \code{x}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link{sQuote}},
    \code{\link{dQuote}}
}

\examples{
x1 <- "This is a non-syntactic variable name"
x2 <- "This is another variable name"
fm <- paste0(iQuote(x1), "~", iQuote(x2))
as.formula(fm)
}

\keyword{utilities}
