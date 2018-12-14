\name{scale}
\docType{methods}

\alias{scale}
\alias{scale.matter}
\alias{scale,matter_mat-method}

\title{Scaling and Centering of ``matter'' Matrices}

\description{
    An implementation of \code{\link[base]{scale}} for \code{\linkS4class{matter_mat}} matrices.
}

\usage{
\S4method{scale}{matter_mat}(x, center = TRUE, scale = TRUE)
}

\arguments{
    \item{x}{A \code{\linkS4class{matter_mat}} object.}
    
    \item{center}{Either a logical value or a numeric vector of length equal to the number of columns of 'x'.}

    \item{scale}{Either a logical value or a numeric vector of length equal to the number of columns of 'x'.}
}

\details{
	See \code{\link[base]{scale}} for details.
}

\value{
	A \code{\linkS4class{matter_mat}} object with the appropriate `scaled:center' and `scaled:scale' attributes set. No data in virtual memory is changed, but the scaling will be applied any time the data is read. This includes but is not limited to loading data elements via subsetting, summary statistics methods, and matrix multiplication.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link[base]{scale}}
}

\examples{
x <- matter(1:100, nrow=10, ncol=10)

scale(x)
}

\keyword{methods}
