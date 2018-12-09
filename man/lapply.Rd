\name{lapply}
\docType{methods}

\alias{lapply}
\alias{sapply}
\alias{lapply,matter_list-method}
\alias{sapply,matter_list-method}

\title{Apply Functions Over ``matter'' Lists}

\description{
    An implementation of \code{\link[base]{lapply}} and \code{\link[base]{sapply}} for \code{\linkS4class{matter_list}} objects.
}

\usage{
\S4method{lapply}{matter_list}(X, FUN, \dots, BPPARAM = bpparam())

\S4method{sapply}{matter_list}(X, FUN, \dots, BPPARAM = bpparam(),
	simplify = TRUE, USE.NAMES = TRUE)
}

\arguments{
    \item{X}{A \code{\linkS4class{matter}} list-like object.}
    
    \item{FUN}{The function to be applied.}

    \item{\dots}{Additional arguments to be passed to \code{FUN}.}

    \item{simplify}{Should the result be simplified into a vector, matrix, or higher dimensional array?}

    \item{USE.NAMES}{Use \code{names(X)} for the names of the answer. If \code{X} is a character, use \code{X} as \code{names} unless it has names already.}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}
}

\details{
	Because \code{FUN} must be executed by the interpreter in the appropriate R environment, the full list element will be loaded into memory. The \code{chunksize} of \code{X} is ignored. When performed in parallel, the \code{matter} metadata is serialized to each R session, so all workers must be able to access the data via the same \code{paths()}.
}

\value{
	See \code{\link[base]{lapply}} and \code{\link[base]{sapply}} for details.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link[base]{lapply}},
    \code{\link[base]{sapply}}
}

\examples{
register(SerialParam())

x <- matter_list(list(1:10, b=11:20, 21:30), names=c("a", "b", "c"))

lapply(x, sum)

sapply(x, sum)
}

\keyword{methods}
