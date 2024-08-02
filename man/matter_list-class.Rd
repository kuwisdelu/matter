\name{matter_list-class}
\docType{class}

\alias{class:matter_list}
\alias{matter_list}
\alias{matter_list-class}

\alias{dim,matter_list-method}
\alias{length,matter_list-method}
\alias{lengths,matter_list-method}

\alias{[,matter_list-method}
\alias{[,matter_list,ANY,ANY,ANY-method}
\alias{[<-,matter_list-method}
\alias{[<-,matter_list,ANY,ANY,ANY-method}

\alias{[[,matter_list-method}
\alias{[[,matter_list,ANY,ANY-method}
\alias{[[<-,matter_list-method}
\alias{[[<-,matter_list,ANY,ANY-method}

\alias{$,matter_list-method}
\alias{$<-,matter_list-method}

\alias{combine,matter_list,ANY-method}
\alias{c,matter_list-method}

\alias{as.list,matter_list-method}
\alias{as.vector,matter_list-method}

\title{Out-of-Memory Lists of Vectors}

\description{
    The \code{matter_list} class implements out-of-memory lists.
}

\usage{
## Instance creation
matter_list(data, type = "double", path = NULL,
    lengths = NA_integer_, names = NULL, offset = 0, extent = NA_real_,
    readonly = NA, append = FALSE, \dots)

## Additional methods documented below
}

\arguments{
        \item{data}{An optional data vector which will be initially written to virtual memory if provided.}

        \item{type}{A 'character' vector giving the storage mode of the data in virtual memory. See \code{?"matter-types"} for possible values.}

        \item{path}{A 'character' vector of the path(s) to the file(s) where the data are stored. If \code{NULL}, then a temporary file is created using \code{\link{tempfile}}, which will be managed according the \code{getOption("matter.temp.gc")}.}

        \item{lengths}{The lengths of the list elements.}

        \item{names}{The names of the list elements.}

        \item{offset}{A vector giving the offsets in number of bytes from the beginning of each file in 'path', specifying the start of the data to be accessed for each file.}

        \item{extent}{A vector giving the length of the data for each file in 'path', specifying the number of elements of size 'type' to be accessed from each file.}

        \item{readonly}{Whether the data and file(s) should be treated as read-only or read/write.}

        \item{append}{If \code{TRUE}, then all offsets will be adjusted to be from the \emph{end-of-file} (for all files in \code{path}), and \code{readonly} will be set to \code{FALSE}.}

        \item{\dots}{Additional arguments to be passed to constructor.}
}

\section{Slots}{
    \describe{
        \item{\code{data}:}{This slot stores any information necessary to access the data for the object (which may include the data itself and/or paths to file locations, etc.).}

        \item{\code{type}:}{The storage mode of the \emph{accessed} data when read into R. This is a 'factor' with levels 'raw', 'logical', 'integer', 'numeric', or 'character'.}

        \item{\code{dim}:}{Either 'NULL' for vectors, or an integer vector of length one of more giving the maximal indices in each dimension for matrices and arrays.}

        \item{\code{names}:}{The names of the data elements for vectors.}

        \item{\code{dimnames}:}{Either 'NULL' or the names for the dimensions. If not 'NULL', then this should be a list of character vectors of the length given by 'dim' for each dimension. This is always 'NULL' for vectors.}
    }
}

\section{Extends}{
   \code{\linkS4class{matter}}
}

\section{Creating Objects}{
    \code{matter_list} instances can be created through \code{matter_list()} or \code{matter()}.
}

\section{Methods}{
    Standard generic methods:
    \describe{
        \item{\code{x[[i]], x[[i]] <- value}:}{Get or set a single element of the list.}

        \item{\code{x[[i, j]]}:}{Get the \code{j}th sub-elements of the \code{i}th element of the list.}

        \item{\code{x[i], x[i] <- value}:}{Get or set the \code{i}th elements of the list.}

        \item{\code{lengths(x)}:}{Get the lengths of all elements in the list.}
    }
}

\value{
    An object of class \code{\linkS4class{matter_list}}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{matter}}
}

\examples{
x <- matter_list(list(c(TRUE,FALSE), 1:5, c(1.11, 2.22, 3.33)), lengths=c(2,5,3))
x[]
x[1]
x[[1]]

x[[3,1]]
x[[2,1:3]]
}

\keyword{classes}
\keyword{array}
\keyword{IO}
