\name{fetch}
\docType{methods}

\alias{fetch}
\alias{fetch,matter_arr-method}
\alias{fetch,matter_mat-method}
\alias{fetch,matter_vec-method}
\alias{fetch,matter_fct-method}
\alias{fetch,matter_list-method}
\alias{fetch,matter_str-method}

\alias{flash}
\alias{flash,matter_arr-method}
\alias{flash,matter_mat-method}
\alias{flash,matter_vec-method}
\alias{flash,matter_fct-method}
\alias{flash,matter_list-method}
\alias{flash,matter_str-method}

\title{Move Data Between Shared Memory and File Storage}

\description{
    These are generic functions for moving data in R objects between shared memory and file storage for sharing with other R processes.
}

\usage{
fetch(object, \dots)

flash(object, \dots)

\S4method{fetch}{matter_arr}(object, \dots, BPPARAM = bpparam())
\S4method{fetch}{matter_mat}(object, \dots, BPPARAM = bpparam())
\S4method{fetch}{matter_vec}(object, \dots, BPPARAM = bpparam())
\S4method{fetch}{matter_fct}(object, \dots, BPPARAM = bpparam())
\S4method{fetch}{matter_list}(object, \dots, BPPARAM = bpparam())
\S4method{fetch}{matter_str}(object, \dots, BPPARAM = bpparam())

\S4method{flash}{matter_arr}(object, \dots, BPPARAM = bpparam())
\S4method{flash}{matter_mat}(object, \dots, BPPARAM = bpparam())
\S4method{flash}{matter_vec}(object, \dots, BPPARAM = bpparam())
\S4method{flash}{matter_fct}(object, \dots, BPPARAM = bpparam())
\S4method{flash}{matter_list}(object, \dots, BPPARAM = bpparam())
\S4method{flash}{matter_str}(object, \dots, BPPARAM = bpparam())
}

\arguments{
    \item{object}{An object with data to move.}
    
    \item{\dots}{Additional arguments to the \code{\linkS4class{matter}} constructor or \code{\link{chunk_lapply}}.}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}
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
