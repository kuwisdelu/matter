\name{pinv}

\alias{pinv}

\title{Pseudoinverse of a Matrix}

\description{
    Calculate the Moore-Penrose pseudoinverse of a matrix.
}

\usage{
pinv(x, tol = sqrt(.Machine$double.eps))
}

\arguments{
	\item{x}{A matrix.}

	\item{tol}{The relative tolerance for detecting non-zero singular values.}
}

\value{
    A matrix giving the pseudoinverse of \code{x}.
}

\examples{
set.seed(1)
x <- diag(10) + rnorm(100)

pinv(x)
}

\keyword{algebra}
