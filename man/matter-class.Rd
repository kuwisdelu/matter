\name{matter-class}
\docType{class}

\alias{class:atoms}
\alias{atoms}
\alias{atoms-class}

\alias{class:matter}
\alias{matter}
\alias{matter-class}

\alias{adata}
\alias{adata,matter-method}
\alias{atomdata}
\alias{atomdata,matter-method}
\alias{atomdata<-}
\alias{atomdata<-,matter-method}
\alias{type}
\alias{type<-}
\alias{type,atoms-method}
\alias{type<-,atoms-method}
\alias{type,matter-method}
\alias{type<-,matter-method}
\alias{path}
\alias{path<-}
\alias{path,atoms-method}
\alias{path<-,atoms-method}
\alias{path,matter_-method}
\alias{path<-,matter_-method}
\alias{readonly}
\alias{readonly<-}
\alias{readonly,atoms-method}
\alias{readonly<-,atoms-method}
\alias{readonly,matter_-method}
\alias{readonly<-,matter_-method}

\alias{as.data.frame,atoms-method}
\alias{as.list,atoms-method}

\alias{length,atoms-method}
\alias{length,matter-method}
\alias{length<-,matter-method}
\alias{dims,atoms-method}
\alias{dim,atoms-method}
\alias{dim,matter-method}
\alias{dim<-,matter-method}
\alias{names,matter-method}
\alias{names<-,matter-method}
\alias{dimnames,matter-method}
\alias{dimnames<-,matter,ANY-method}

\alias{lengths,atoms-method}

\alias{[,atoms-method}
\alias{[,atoms,ANY,ANY,ANY-method}
\alias{[[,atoms-method}
\alias{[[,atoms,ANY,ANY-method}

\alias{combine,atoms,ANY-method}
\alias{cbind2,atoms,ANY-method}
\alias{rbind2,atoms,ANY-method}

\alias{c,atoms-method}
\alias{c,matter-method}

\alias{is.matter}
\alias{as.matter}

\title{Vectors, Matrices, and Arrays Stored in Virtual Memory}

\description{
    The \code{matter} class and its subclasses are designed for easy on-demand read/write access to binary virtual memory data structures stored in files and/or shared memory and working with them as vectors, matrices, arrays, and lists.
}

\usage{
## Instance creation
matter(data, \dots)

# Check if an object is a matter object
is.matter(x)

# Coerce an object to a matter object
as.matter(x)

## Additional methods documented below
}

\arguments{
    \item{data}{Data passed to the subclasse constructor.}

    \item{\dots}{Arguments passed to subclasses.}

    \item{x}{An object to check if it is a matter object or coerce to a matter object.}
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

\section{Creating Objects}{
    \code{matter} is a virtual class and cannot be instantiated directly, but instances of its subclasses can be created through \code{matter()}.
}

\section{Methods}{
    Class-specific methods:
    \describe{
        \item{\code{atomdata(x)}:}{Access the 'data' slot.}

        \item{\code{adata(x)}:}{An alias for atomdata(x).}

        \item{\code{type(x), type(x) <- value}:}{Get or set data 'type'.}
    }

    Standard generic methods:
    \describe{
        \item{\code{length(x), length(x) <- value}:}{Get or set length.}

        \item{\code{dim(x), dim(x) <- value}:}{Get or set 'dim'.}

        \item{\code{names(x), names(x) <- value}:}{Get or set 'names'.}

        \item{\code{dimnames(x), dimnames(x) <- value}:}{Get or set 'dimnames'.}
    }
}

\value{
    An object of class \code{\linkS4class{matter}}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{matter_arr}},
    \code{\linkS4class{matter_mat}},
    \code{\linkS4class{matter_vec}},
    \code{\linkS4class{matter_fct}},
    \code{\linkS4class{matter_list}},
    \code{\linkS4class{matter_str}}
}

\examples{
## Create a matter_vec vector
x <- matter(1:100, length=100)
x

## Create a matter_mat matrix
y <- matter(1:100, nrow=10, ncol=10)
y
}

\keyword{classes}
\keyword{array}
\keyword{IO}
