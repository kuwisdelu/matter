\name{tolerance}
\docType{methods}

\alias{tolerance}
\alias{tolerance<-}

\title{Get or Set Tolerance for an Object}

\description{
    This is a generic function for getting or setting 'tolerance' for an object which tests floating point equality.
}

\usage{
tolerance(object, \dots)

tolerance(object, \dots) <- value
}

\arguments{
    \item{object}{An object with tolerance.}

    \item{\dots}{Additional arguments.}
    
    \item{value}{The value to set the tolerance.}
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{sparse_mat}}
}

\examples{
x <- sparse_mat(diag(10), keys=rnorm(10))
tolerance(x)
tolerance(x) <- c(absolute=0.1)
x[]
}

\keyword{utilities}
