\name{fetch}
\docType{methods}

\alias{fetch}
\alias{flash}

\title{Move Data Between Shared Memory and File Storage}

\description{
    These are generic functions for moving data in R objects between shared memory and file storage for sharing with other R processes.
}

\usage{
fetch(object, \dots)

flash(object, \dots)
}

\arguments{
    \item{object}{An object with data to move.}
    
    \item{\dots}{Additional arguments to the \code{\linkS4class{matter}} constructor or \code{\link{chunk_lapply}}.}
}

\details{
    The \code{fetch} methods for \code{\linkS4class{matter}} objects return new \code{matter} objects that use shared memory storage.

    The \code{flash} methods for \code{\linkS4class{matter}} objects return new \code{matter} objects that use file storage.
}

\value{
	A new object (typically of the same class but not necessarily) with data in the specified storage format.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{matter}}
}

\examples{
set.seed(1)
x <- as.matter(runif(10))
path(x)

# copy x into shared memory
y <- fetch(x)
path(y)

# copy y into file storage
z <- flash(y)
path(z)
}

\keyword{utilities}
\keyword{IO}
