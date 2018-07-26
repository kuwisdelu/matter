\name{apply}
\docType{methods}

\alias{apply}
\alias{apply,matter_mat-method}
\alias{apply,sparse_mat-method}
\alias{apply,virtual_mat-method}

\title{Apply Functions Over ``matter'' Matrices}

\description{
    An implementation of \code{\link[base]{apply}} for \code{\linkS4class{matter_mat}}, \code{\linkS4class{sparse_mat}} and \code{\linkS4class{virtual_mat}} matrices.
}

\usage{
\S4method{apply}{matter_mat}(X, MARGIN, FUN, \dots)

\S4method{apply}{sparse_mat}(X, MARGIN, FUN, \dots)

\S4method{apply}{virtual_mat}(X, MARGIN, FUN, \dots)
}

\arguments{
    \item{X}{A \code{\linkS4class{matter_mat}} object.}
    
    \item{MARGIN}{Must be 1 or 2 for \code{\linkS4class{matter_mat}} matrices, where `1' indicates rows and `2' indicates columns. The dimension names can also be used if \code{X} has \code{dimnames} set.}

    \item{FUN}{The function to be applied.}

    \item{\dots}{Additional arguments to be passed to \code{FUN}.}
}

\details{
	Because \code{FUN} must be executed by the interpreter in the appropriate R environment, the full row or column will be loaded into memory. The \code{chunksize} of \code{X} is ignored. For summary statistics, functions like \code{\link{colMeans}} and \code{\link{rowMeans}} offer greater control over memory pressure.
}

\section{Warning}{
    Applying a function over the rows of a column-major matrix (e.g., \code{\linkS4class{matter_matc}}) or over the columns of a row-major matrix (e.g., \code{\linkS4class{matter_matr}}) may be very slow.
}

\value{
	See \code{\link[base]{apply}} for details.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link[base]{apply}}
}

\examples{
x <- matter(1:100, nrow=10, ncol=10)

apply(x, 2, summary)
}

\keyword{methods}
