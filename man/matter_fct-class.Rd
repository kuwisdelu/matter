\name{matter_fct-class}
\docType{class}

\alias{class:matter_fct}
\alias{matter_fct}
\alias{matter_fct-class}

\alias{[,matter_fct-method}
\alias{[,matter_fct,ANY,missing,ANY-method}
\alias{[,matter_fct,ANY,missing,NULL-method}
\alias{[<-,matter_fct-method}
\alias{[<-,matter_fct,ANY,missing,ANY-method}

\alias{combine,matter_fct,ANY-method}

\alias{levels,matter_fct-method}
\alias{levels<-,matter_fct-method}

\alias{as.factor,matter_fct-method}

\title{Out-of-Memory Factors}

\description{
    The \code{matter_fct} class implements out-of-memory factors.
}

\usage{
## Instance creation
matter_fct(data, levels, type = "integer", path = NULL,
    length = NA_integer_, names = NULL, offset = 0, extent = NA_real_,
    readonly = NA, labels = as.character(levels), \dots)

## Additional methods documented below
}

\arguments{
        \item{data}{An optional data vector which will be initially written to the data in virtual memory if provided.}

        \item{levels}{The levels of the factor.}

        \item{type}{Should be 'integer' type for factors.}

        \item{path}{A 'character' vector of the path(s) to the file(s) where the data are stored. If 'NULL', then a temporary file is created using \code{\link{tempfile}}.}

        \item{length}{The length of the factor.}

        \item{names}{The names of the data elements.}

        \item{offset}{A vector giving the offsets in number of bytes from the beginning of each file in 'path', specifying the start of the data to be accessed for each file.}

        \item{extent}{A vector giving the length of the data for each file in 'path', specifying the number of elements of size 'type' to be accessed from each file.}

        \item{readonly}{Whether the data and file(s) should be treated as read-only or read/write.}

        \item{labels}{An optional character vector of labels for the factor levels.}

        \item{\dots}{Additional arguments to be passed to constructor.}
}

\section{Slots}{
    \describe{
        \item{\code{data}:}{This slot stores any information necessary to access the data for the object (which may include the data itself and/or paths to file locations, etc.).}

        \item{\code{type}:}{The storage mode of the \emph{accessed} data when read into R. This is a 'factor' with levels 'raw', 'logical', 'integer', 'numeric', or 'character'.}

        \item{\code{dim}:}{Either 'NULL' for vectors, or an integer vector of length one of more giving the maximal indices in each dimension for matrices and arrays.}

        \item{\code{names}:}{The names of the data elements for vectors.}

        \item{\code{dimnames}:}{Either 'NULL' or the names for the dimensions. If not 'NULL', then this should be a list of character vectors of the length given by 'dim' for each dimension. This is always 'NULL' for vectors.}

        \item{\code{levels}:}{The levels of the factor.}

        \item{\code{labels}:}{The labels for the levels.}
    }
}

\section{Extends}{
   \code{\linkS4class{matter}},
   \code{\linkS4class{matter_vec}}
}

\section{Creating Objects}{
    \code{matter_fct} instances can be created through \code{matter_fct()} or \code{matter()}.
}

\section{Methods}{
    Standard generic methods:
    \describe{
        \item{\code{length(x), length(x) <- value}:}{Get or set length.}

        \item{\code{names(x), names(x) <- value}:}{Get or set 'names'.}

        \item{\code{x[i], x[i] <- value}:}{Get or set the elements of the factor.}

        \item{\code{levels(x), levels(x) <- value}:}{Get or set the levels of the factor.}
    }
}

\value{
    An object of class \code{\linkS4class{matter_fct}}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{matter}},
    \code{\linkS4class{matter_vec}}
}

\examples{
x <- matter_fct(rep(c("a", "a", "b"), 5), levels=c("a", "b", "c"))
x
}

\keyword{classes}
\keyword{array}
\keyword{IO}
