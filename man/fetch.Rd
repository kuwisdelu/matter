\name{fetch}
\docType{methods}

\alias{fetch}
\alias{flash}

\alias{fetch,raw-method}
\alias{fetch,logical-method}
\alias{fetch,numeric-method}
\alias{fetch,character-method}
\alias{fetch,factor-method}
\alias{fetch,array-method}
\alias{fetch,matrix-method}
\alias{fetch,list-method}

\alias{flash,raw-method}
\alias{flash,logical-method}
\alias{flash,numeric-method}
\alias{flash,character-method}
\alias{flash,factor-method}
\alias{flash,array-method}
\alias{flash,matrix-method}
\alias{flash,list-method}

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
    
    \item{\dots}{Additional arguments to the \code{\linkS4class{matter}} constructor, \code{\link{chunk_colapply}}, \code{\link{chunk_rowapply}}, or \code{\link{chunk_lapply}}.}
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
