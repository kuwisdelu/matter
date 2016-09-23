\name{bigglm}
\docType{methods}

\alias{bigglm,formula,matter_mat-method}

\title{Using ``biglm'' with ``matter''}

\description{
    This method allows \code{\linkS4class{matter_mat}} matrices to be used with the \code{\link[biglm]{bigglm}} function from the 
}

\usage{
\S4method{bigglm}{formula,matter_mat}(formula, data, \dots, chunksize = NULL, fc = NULL)
}

\arguments{
    \item{formula}{A model \code{formula}.}

    \item{data}{A \code{\linkS4class{matter}} matrix with column names.}

    \item{chunksize}{An integer giving the maximum number of rows to process at a time. If left \code{NULL}, this will be calculated by dividing the \code{chunksize} of \code{data} by the number of variables in the formula.}

    \item{fc}{Either column indices or names of variables which are factors.}

    \item{\dots}{Additional options passed to \code{\link[biglm]{bigglm}}.}
}

\value{
	An object of class \code{\link[biglm]{bigglm}}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link[biglm]{bigglm}}
}

\examples{
\dontrun{
## Do something
}
}

\keyword{models}
\keyword{regression}
