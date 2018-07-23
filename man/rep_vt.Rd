\name{rep_vt-class}
\docType{class}

\alias{class:rep_vt}
\alias{rep_vt}
\alias{rep_vt-class}

\alias{[,rep_vt,missing,missing,missing-method}
\alias{[,rep_vt,ANY,missing,missing-method}
\alias{[[,rep_vt,ANY,ANY-method}
\alias{length,rep_vt-method}

\alias{as.vector,rep_vt-method}
\alias{as.list,rep_vt-method}

\title{Virtual Replication of Vectors}

\description{
    The \code{rep_vt} class simulates the behavior of the base function \code{\link[base]{rep}} without actually allocating memory for the duplication. Only the original vector and the expected length of the result are stored. All attributes of the original vector (including names) are dropped.
}

\usage{
## Instance creation
rep_vt(x, times, length.out = length(x) * times)

## Additional methods documented below
}

\arguments{
    \item{x}{A vector (of any mode).}

    \item{times}{The number of times to repeat the whole vector.}

    \item{length.out}{The desired length of the result.}
}

\section{Slots}{
    \describe{
        \item{\code{data}:}{The original vector.}

        \item{\code{length}:}{The expected length of the repeated virtual vector.}
    }
}

\section{Creating Objects}{
    \code{rep_vt} instances can be created through \code{rep_vt()}.
}

\section{Methods}{
    Standard generic methods:
    \describe{
        \item{\code{x[i]}:}{Get the elements of the uncompressed vector.}

        \item{\code{x[[i]]}:}{Get a single element of the uncompressed vector.}

        \item{\code{length(x)}:}{Get the length of the uncompressed vector.}
    }
}

\value{
    An object of class \code{\linkS4class{rep_vt}}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{[base]{rep}}
}

\examples{
## Create a rep_vt vector
init <- 1:3
x <- rep(init, length.out=100)
y <- rep_vt(init, length.out=100)

# Check that their elements are equal
x == y[]
}

\keyword{classes}
