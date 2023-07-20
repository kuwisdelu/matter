\name{sparse_arr-class}
\docType{class}

\alias{class:sparse_arr}
\alias{sparse_arr-class}

\alias{class:sparse_mat}
\alias{sparse_mat}
\alias{sparse_mat-class}

\alias{class:sparse_vec}
\alias{sparse_vec}
\alias{sparse_vec-class}

\alias{atomdata,sparse_arr-method}
\alias{aindex}
\alias{aindex,sparse_arr-method}
\alias{atomindex}
\alias{atomindex,sparse_arr-method}
\alias{atomindex<-}
\alias{atomindex<-,sparse_arr-method}
\alias{domain}
\alias{domain,sparse_arr-method}
\alias{domain<-}
\alias{domain<-,sparse_arr-method}
\alias{pointers}
\alias{pointers,sparse_arr-method}
\alias{pointers<-}
\alias{pointers<-,sparse_arr-method}
\alias{tolerance,sparse_arr-method}
\alias{tolerance<-,sparse_arr-method}
\alias{tolerance<-}
\alias{sampler}
\alias{sampler,sparse_arr-method}
\alias{sampler<-}
\alias{sampler<-,sparse_arr-method}
\alias{nnzero,sparse_arr-method}
\alias{length,sparse_arr-method}
\alias{lengths,sparse_arr-method}

\alias{keys,sparse_arr-method}
\alias{keys<-,sparse_arr-method}

\alias{dim,sparse_vec-method}

\alias{[,sparse_arr,ANY,ANY,ANY-method}
\alias{[<-,sparse_arr-method}
\alias{[<-,sparse_arr,ANY,ANY,ANY-method}

\alias{t,sparse_arr-method}

\alias{cbind2,sparse_mat,sparse_mat-method}
\alias{rbind2,sparse_mat,sparse_mat-method}

\alias{\%*\%,vector,sparse_mat-method}
\alias{\%*\%,matrix,sparse_mat-method}
\alias{\%*\%,sparse_mat,vector-method}
\alias{\%*\%,sparse_mat,matrix-method}

\alias{crossprod,sparse_mat,ANY-method}
\alias{crossprod,ANY,sparse_mat-method}
\alias{tcrossprod,sparse_mat,ANY-method}
\alias{tcrossprod,ANY,sparse_mat-method}

\alias{as.matrix,sparse_arr-method}
\alias{as.array,sparse_arr-method}

\alias{is.sparse}
\alias{as.sparse}

\title{Sparse Vectors and Matrices}

\description{
    The \code{sparse_mat} class implements sparse matrices, potentially stored out-of-memory. Both compressed-sparse-column (CSC) and compressed-sparse-row (CSR) formats are supported. Sparse vectors are also supported through the \code{sparse_vec} class.
}

\usage{
## Instance creation
sparse_mat(data, index, type = "double",
    nrow = NA_integer_, ncol = NA_integer_, dimnames = NULL,
    pointers = NULL, domain = NULL, offset = 0L, rowMaj = FALSE,
    tolerance = c(abs=0), sampler = "none", \dots)

sparse_vec(data, index, type = "double",
    length = NA_integer_, names = NULL,
    domain = NULL, offset = 0L, rowMaj = FALSE,
    tolerance = c(abs=0), sampler = "none", \dots)

# Check if an object is a sparse matrix
is.sparse(x)

# Coerce an object to a sparse matrix
as.sparse(x, \dots)

## Additional methods documented below
}

\arguments{
        \item{data}{Either the non-zero values of the sparse array, or (if \code{index} is missing) a numeric vector or matrix from which to create the sparse array. For a \code{sparse_vec}, these should be a numeric vector. For a \code{sparse_mat} these can be a numeric vector if \code{pointers} is supplied, or a list of numeric vectors if \code{pointers} is \code{NULL}.}

        \item{index}{For \code{sparse_vec}, the indices of the non-zero items. For \code{sparse_mat}, either the row-indices or column-indices of the non-zero items, depending on the value of \code{rowMaj}.}

        \item{type}{A 'character' vector giving the storage mode of the data in virtual memory. Allowable values are R numeric and logical types ('logical', 'integer', 'numeric') and their C equivalents.}

        \item{nrow, ncol, length}{The number of rows and columns, or the length of the array.}

        \item{dimnames}{The names of the sparse matrix dimensions.}

        \item{names}{The names of the sparse vector elements.}

        \item{pointers}{The (zero-indexed) pointers to the start of either the rows or columns (depending on the value of \code{rowMaj}) in \code{data} and \code{index} when they are numeric vectors rather than lists.}

        \item{domain}{Either \code{NULL} or a vector with length equal to the number of rows (for CSC matrices) or the number of columns (for CSR matrices). If \code{NULL}, then \code{index} is assumed to be row or column indices. If a vector, then they define the how the non-zero elements are matched to rows or columns. The \code{index} value of each non-zero element is matched against this domain using binary search. Must be numeric.}

        \item{offset}{If \code{domain} is \code{NULL} (i.e., \code{index} represents the actual row/column indices), then this is the index of the first row/column. The default of 0 means that \code{index} is indexed from 0.}

        \item{rowMaj}{Whether the data should be stored using compressed-sparse-row (CSR) representation (as opposed to compressed-sparse-column (CSC) representation). Defaults to 'FALSE', for efficient access to columns. Set to 'TRUE' for more efficient access to rows instead.}

        \item{tolerance}{For non-\code{NULL} domain, the tolerance used for floating-point equality when matching \code{index} to the \code{domain}. The vector should be named. Use 'absolute' to use absolute differences, and 'relative' to use relative differences.}

        \item{sampler}{For non-zero tolerances, how the \code{data} values should be combined when there are multiple \code{index} values within the tolerance. Must be of 'none', 'mean', 'sum', 'max', 'min', 'area', 'linear', 'cubic', 'gaussian', or 'lanczos'. Note that 'none' means nearest-neighbor interpolation.}

        \item{x}{An object to check if it is a sparse matrix or coerce to a sparse matrix.}

        \item{\dots}{Additional arguments to be passed to constructor.}
}

\section{Slots}{
    \describe{
        \item{\code{data}:}{The non-zero data values. Can be a numeric vector or a list of numeric vectors.}

        \item{\code{type}:}{The storage mode of the \emph{accessed} data when read into R. This is a 'factor' with levels 'raw', 'logical', 'integer', 'numeric', or 'character'.}

        \item{\code{dim}:}{Either \code{NULL} for vectors, or an integer vector of length one of more giving the maximal indices in each dimension for matrices and arrays.}

        \item{\code{names}:}{The names of the data elements for vectors.}

        \item{\code{dimnames}:}{Either \code{NULL} or the names for the dimensions. If not \code{NULL}, then this should be a list of character vectors of the length given by 'dim' for each dimension. This is always \code{NULL} for vectors.}

        \item{\code{index}:}{The indices of the non-zero items. Can be a numeric vector or a list of numeric vectors.}

        \item{\code{pointers}:}{The pointers to the beginning of the rows or columns if \code{index} and \code{data} use vector storage rather than list storage.}

        \item{\code{domain}:}{Either \code{NULL} or a vector with length equal to the number of rows (for CSC matrices) or the number of columns (for CSR matrices). If \code{NULL}, then \code{index} is assumed to be row or column indices. If a vector, then they define the how the non-zero elements are matched to rows or columns. The \code{index} value of each non-zero element is matched against this domain using binary search. Must be numeric.}

        \item{\code{offset}:}{If \code{domain} is \code{NULL} (i.e., \code{index} represents the actual row/column indices), then this is the index of the first row/column. The default of 0 means that \code{index} is indexed from 0.}

        \item{\code{tolerance}:}{For non-\code{NULL} domain, the tolerance used for floating-point equality when matching \code{index} to the \code{domain}. The vector should be named. Use 'absolute' to use absolute differences, and 'relative' to use relative differences.}

        \item{\code{sampler}:}{The type of summarization or interpolation performed when there are multiple \code{index} values within the tolerance of the requested \code{domain} value(s).}

        \item{\code{ops}:}{Deferred arithmetic operations.}

        \item{\code{transpose}}{Indicates whether the data is stored in row-major order (TRUE) or column-major order (FALSE). For a matrix, switching the order that the data is read is equivalent to transposing the matrix (without changing any data).}
    }
}

\section{Extends}{
   \code{\linkS4class{matter}}
}

\section{Creating Objects}{
    \code{sparse_mat} and \code{sparse_vec} instances can be created through \code{sparse_mat()} and \code{sparse_vec()}, respectively.
}

\section{Methods}{
    Standard generic methods:
    \describe{
        \item{\code{x[i, j, ..., drop], x[i, j] <- value}:}{Get or set the elements of the sparse matrix. Use \code{drop = NULL} to return a subset of the same class as the object.}

        \item{\code{cbind(x, ...), rbind(x, ...)}:}{Combine sparse matrices by row or column.}

        \item{\code{t(x)}:}{Transpose a matrix. This is a quick operation which only changes metadata and does not touch the data representation.}
    }
}

\value{
    An object of class \code{\linkS4class{sparse_mat}}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{matter}}
}

\examples{
x <- matrix(rbinom(100, 1, 0.2), nrow=10, ncol=10)

y <- sparse_mat(x)
y[]
}

\keyword{classes}
\keyword{array}
