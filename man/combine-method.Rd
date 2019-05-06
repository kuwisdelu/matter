\name{combine}
\docType{methods}

\alias{combine}
\alias{combine,ANY,ANY-method}

\title{Combine Out-of-Memory Objects}

\description{
    This is a generic function for combining \code{matter} objects. A default fallback method to \code{c()} is provided as well.

    This generic is internally used to implement \code{c()}, \code{cbind()}, and \code{rbind()} for \code{matter} objects.
}

\usage{
combine(x, y, \dots)
}

\arguments{
    \item{x}{One of the objects.}
    
    \item{y}{A second object.}

    \item{\dots}{Any other objects of the same class as \code{x} and \code{y}.}
}

\author{Kylie A. Bemis}

\examples{
x <- 1:5
y <- 6:10

combine(x, y)
}

\keyword{utilities}
