\name{keys}
\docType{methods}

\alias{keys}
\alias{keys<-}

\title{Get or Set Keys for an Object}

\description{
    This is a generic function for getting or setting 'keys' for an object with key-value pairs such as a map data structure.
}

\usage{
keys(object)

keys(object) <- value
}

\arguments{
    \item{object}{An object with keys.}
    
    \item{value}{The value to set the keys.}
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{sparse_mat}}
}

\examples{
x <- sparse_mat(diag(10))
keys(x)
keys(x) <- 1:10
x[]
}

\keyword{utilities}
