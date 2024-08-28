\name{matter_arr-class}
\docType{class}

\alias{class:matter_arr}
\alias{matter_arr}
\alias{matter_arr-class}

\alias{class:matter_mat}
\alias{matter_mat}
\alias{matter_mat-class}

\alias{class:matter_vec}
\alias{matter_vec}
\alias{matter_vec-class}

\alias{[,matter_arr-method}
\alias{[,matter_arr,ANY,ANY,ANY-method}
\alias{[<-,matter_arr-method}
\alias{[<-,matter_arr,ANY,ANY,ANY-method}

\alias{[,matter_mat-method}
\alias{[,matter_mat,ANY,ANY,ANY-method}
\alias{[<-,matter_mat-method}
\alias{[<-,matter_mat,ANY,ANY,ANY-method}

\alias{dim,matter_vec-method}
\alias{dim<-,matter_vec-method}

\alias{dim<-,matter_arr-method}

\alias{as.vector}
\alias{as.vector,matter_arr-method}
\alias{as.raw,matter_arr-method}
\alias{as.logical,matter_arr-method}
\alias{as.integer,matter_arr-method}
\alias{as.numeric,matter_arr-method}
\alias{as.matrix,matter_arr-method}
\alias{as.array,matter_arr-method}

\alias{exp,matter_arr-method}
\alias{log,matter_arr-method}
\alias{log2,matter_arr-method}
\alias{log10,matter_arr-method}
\alias{log1p,matter_arr-method}

\alias{rowMaj}
\alias{rowMaj,matter_arr-method}
\alias{rowMaj,matrix-method}
\alias{rowMaj,Matrix-method}

\alias{t,matter_arr-method}
\alias{t,matter_vec-method}

\alias{combine,matter_arr,ANY-method}
\alias{c,matter_arr-method}

\alias{cbind2,matter_vec,matter_vec-method}
\alias{cbind2,matter_vec,matter_mat-method}
\alias{cbind2,matter_mat,matter_vec-method}
\alias{cbind2,matter_mat,matter_mat-method}

\alias{rbind2,matter_vec,matter_vec-method}
\alias{rbind2,matter_vec,matter_mat-method}
\alias{rbind2,matter_mat,matter_vec-method}
\alias{rbind2,matter_mat,matter_mat-method}

\alias{\%*\%,vector,matter_mat-method}
\alias{\%*\%,matrix,matter_mat-method}
\alias{\%*\%,matter_mat,vector-method}
\alias{\%*\%,matter_mat,matrix-method}

\alias{crossprod,matter_mat,ANY-method}
\alias{crossprod,ANY,matter_mat-method}
\alias{tcrossprod,matter_mat,ANY-method}
\alias{tcrossprod,ANY,matter_mat-method}

\alias{fetch,matter_arr-method}
\alias{fetch,matter_mat-method}
\alias{fetch,matter_vec-method}

\alias{flash,matter_arr-method}
\alias{flash,matter_mat-method}
\alias{flash,matter_vec-method}

\title{Out-of-Memory Arrays}

\description{
    The \code{matter_arr} class implements out-of-memory arrays.
}

\usage{
## Instance creation
matter_arr(data, type = "double", path = NULL,
    dim = NA_integer_, dimnames = NULL, offset = 0, extent = NA_real_,
    readonly = NA, append = FALSE, rowMaj = FALSE, \dots)

matter_mat(data, type = "double", path = NULL,
    nrow = NA_integer_, ncol = NA_integer_, dimnames = NULL,
    offset = 0, extent = NA_real_, readonly = NA,
    append = FALSE, rowMaj = FALSE, \dots)

matter_vec(data, type = "double", path = NULL,
    length = NA_integer_, names = NULL, offset = 0, extent = NA_real_,
    readonly = NA, append = FALSE, rowMaj = FALSE, \dots)

## Additional methods documented below
}

\arguments{
        \item{data}{An optional data vector which will be initially written to virtual memory if provided.}

        \item{type}{A 'character' vector giving the storage mode of the data in virtual memory such. See \code{?"matter-types"} for possible values.}

        \item{path}{A 'character' vector of the path(s) to the file(s) where the data are stored. If \code{NULL}, then a temporary file is created using \code{\link{tempfile}}, which will be managed according the \code{getOption("matter.temp.gc")}.}

        \item{dim, nrow, ncol, length}{The dimensions of the array, or the number of rows and columns, or the length.}

        \item{dimnames, names}{The names of the matrix dimensions or vector elements.}

        \item{offset}{A vector giving the offsets in number of bytes from the beginning of each file in 'path', specifying the start of the data to be accessed for each file.}

        \item{extent}{A vector giving the length of the data for each file in 'path', specifying the number of elements of size 'type' to be accessed from each file.}

        \item{readonly}{Whether the data and file(s) should be treated as read-only or read/write.}

        \item{append}{If \code{TRUE}, then all offsets will be adjusted to be from the \emph{end-of-file} (for all files in \code{path}), and \code{readonly} will be set to \code{FALSE}.}

        \item{rowMaj}{Whether the data is stored in row-major or column-major order. The default is to use column-major order, which is the same as native R matrices.}

        \item{\dots}{Additional arguments to be passed to constructor.}
}

\section{Slots}{
    \describe{
        \item{\code{data}:}{This slot stores any information necessary to access the data for the object (which may include the data itself and/or paths to file locations, etc.).}

        \item{\code{type}:}{The storage mode of the \emph{accessed} data when read into R. This is a 'factor' with levels 'raw', 'logical', 'integer', 'numeric', or 'character'.}

        \item{\code{dim}:}{Either 'NULL' for vectors, or an integer vector of length one of more giving the maximal indices in each dimension for matrices and arrays.}

        \item{\code{names}:}{The names of the data elements for vectors.}

        \item{\code{dimnames}:}{Either 'NULL' or the names for the dimensions. If not 'NULL', then this should be a list of character vectors of the length given by 'dim' for each dimension. This is always 'NULL' for vectors.}

        \item{\code{ops}:}{Deferred arithmetic operations.}

        \item{\code{transpose}:}{Indicates whether the data is stored in row-major order (TRUE) or column-major order (FALSE). For a matrix, switching the order that the data is read is equivalent to transposing the matrix (without changing any data).}

        \item{\code{indexed}:}{For \code{matter_mat} only. Indicates whether the pointers to rows or columns are indexed for quick access or not.}
    }
}

\section{Extends}{
   \code{\linkS4class{matter}}
}

\section{Creating Objects}{
    \code{matter_arr} instances can be created through \code{matter_arr()} or \code{matter()}. Matrices and vectors can also be created through \code{matter_mat()} and \code{matter_vec()}.
}

\section{Methods}{
    Class-specific methods:
    \describe{
        \item{\code{path(x), path(x) <- value}:}{Get or set the data source names, i.e., file path(s).}

        \item{\code{fetch(x, ...)}:}{Pull data into shared memory.}

        \item{\code{flash(x, ...)}:}{Push data to a temporary file.}
    }

    Standard generic methods:
    \describe{
        \item{\code{length(x), length(x) <- value}:}{Get or set length.}

        \item{\code{dim(x), dim(x) <- value}:}{Get or set 'dim'.}

        \item{\code{names(x), names(x) <- value}:}{Get or set 'names'.}

        \item{\code{dimnames(x), dimnames(x) <- value}:}{Get or set 'dimnames'.}
        
        \item{\code{x[...], x[...] <- value}:}{Get or set the elements of the array.}

        \item{\code{as.vector(x)}:}{Coerce to a vector.}

        \item{\code{as.array(x)}:}{Coerce to an array.}

        \item{\code{cbind(x, ...), rbind(x, ...)}:}{Combine matrices by row or column.}

        \item{\code{t(x)}:}{Transpose a matrix. This is a quick operation which only changes metadata and does not touch the data representation.}

        \item{\code{rowMaj(x)}:}{Check the data orientation.}
    }
}

\value{
    An object of class \code{\linkS4class{matter_arr}}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{matter}}
}

\examples{
x <- matter_arr(1:1000, dim=c(10,10,10))
x
}

\keyword{classes}
\keyword{array}
\keyword{IO}
