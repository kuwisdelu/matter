\name{apply}
\docType{methods}

\alias{apply,matter_mat-method}

\title{Apply Functions Over ``matter'' Matrices}

\description{
    An implementation of \code{\link[base]{apply}} for \code{\linkS4class{matter_mat}} matrices.
}

\usage{
\S4method{apply}{matter_mat}(X, MARGIN, FUN, \dots)
}

\arguments{
    \item{X}{A \code{\linkS4class{matter_mat}} object.}
    
    \item{MARGIN}{Must be 1 or 2 for \code{\linkS4class{matter_mat}} matrices, where `1' indicates rows and `2' indicates columns. The dimension names can also be used if \code{X} has \code{dimnames} set.}

    \item{FUN}{The function to be applied.}

    \item{\dots}{Additional arguments to be passed to \code{FUN}.}
}

\details{
	Because \code{FUN} is executed by in the appropriate R environment, the full row or column must be loaded into memory, and the \code{chunksize} of \code{X} is ignored. For summary statistics, functions like \code{\link[matter]{colMeans,matter-method}} and \code{\link[matter]{rowMeans,matter-method}} offer greater control over memory pressure.
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
