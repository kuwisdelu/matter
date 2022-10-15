\name{deferred-ops}
\docType{methods}

\alias{Arith}
\alias{Compare}
\alias{Logic}
\alias{Ops}

\alias{Arith,matter_arr,vector-method}
\alias{Arith,matter_arr,array-method}
\alias{Arith,vector,matter_arr-method}
\alias{Arith,array,matter_arr-method}

\title{Deferred Operations on ``matter'' Objects}

\description{
    Some arithmetic, comparison, and logical operations are available as delayed operations on \code{\linkS4class{matter_arr}} and \code{\linkS4class{sparse_arr}} objects.
}

\details{
    Currently the following delayed operations are supported:

    `Arith': `+', `-', `*', `/', `^', `%%', `%/%'

    `Compare': `==', `>', `<', `!=', `<=', `>='

    `Logic': `&', `|'

    `Ops': `Arith', `Compare', `Logic'

    `Math': `exp', `log', `log2', `log10'

    Arithmetic operations are applied in C++ layer immediately after the elements are read from virtual memory. This means that operations that are implemented in C and/or C++ for efficiency (such as summary statistics) will also reflect the execution of the deferred arithmetic operations.
}

\value{
    A new \code{\linkS4class{matter}} object with the registered deferred operation. Data in storage is not modified; only object metadata is changed.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link{Arith}},
    \code{\link{Compare}},
    \code{\link{Logic}},
    \code{\link{Ops}},
    \code{\link{Math}}
}

\examples{
x <- matter(1:100)
y <- 2 * x + 1

x[1:10]
y[1:10]

mean(x)
mean(y)
}

\keyword{methods}
\keyword{arith}
