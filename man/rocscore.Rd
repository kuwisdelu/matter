\name{rocscore}

\alias{rocscore}

\title{Compute area under ROC curve}

\description{
    Calculate the area under the receiver operating characteristic curve (ROC AUC).
}

\usage{
rocscore(x, ref, n = 32L)
}

\arguments{
	\item{x}{The prediction scores.}

    \item{ref}{The (logical) binary response.}

    \item{n}{The number of points in the curve.}
}

\value{
    A single number between 0 and 1 giving the ROC AUC, with an attribute called \code{ROC} which is a data frame giving the full ROC curve.
}

\author{Kylie A. Bemis}

\examples{
set.seed(1)
x <- runif(100)
y <- ifelse(x > 0.5 & runif(100) > 0.2, TRUE, FALSE)
rocscore(x, y)
}

\keyword{utilities}
\keyword{classif}
