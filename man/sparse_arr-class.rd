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

\alias{dim,sparse_vec-method}

\alias{[,sparse_arr,ANY,ANY,ANY-method}
\alias{[<-,sparse_arr-method}
\alias{[<-,sparse_arr,ANY,ANY,ANY-method}

\alias{t,sparse_arr-method}

\alias{\%*\%,vector,sparse_mat-method}
\alias{\%*\%,matrix,sparse_mat-method}
\alias{\%*\%,sparse_mat,vector-method}
\alias{\%*\%,sparse_mat,matrix-method}

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
        \item{data}{TODO}

        \item{index}{TODO}

        \item{type}{A 'character' vector giving the storage mode of the data in virtual memory. Allowable values are R numeric and logical types ('logical', 'integer', 'numeric') and their C equivalents.}

        \item{nrow, ncol, length}{The number of rows and columns, or the length of the array.}

        \item{domain}{Either NULL or a vector with length equal to the number of rows (for CSC matrices) or the number of columns (for CSR matrices). If NULL, then the 'key' portion of the key-value pairs that make up the non-zero elements are assumed to be row or column indices. If a vector, then they define the how the non-zero elements are matched to rows or columns. The 'key' portion of each non-zero element is matched against this canonical set of keys using binary search. Allowed types for keys are 'integer', 'numeric', and 'character'.}

        \item{offset}{TODO}

        \item{rowMaj}{Whether the data should be stored using compressed-sparse-row (CSR) representation (as opposed to compressed-sparse-column (CSC) representation). Defaults to 'FALSE', for efficient access to columns. Set to 'TRUE' for more efficient access to rows instead.}

        \item{dimnames}{The names of the sparse matrix dimensions.}

        \item{names}{The names of the sparse vector elements.}

        \item{tolerance}{For 'numeric' keys, the tolerance used for floating-point equality when determining key matches. The vector should be named. Use 'absolute' to use absolute differences, and 'relative' to use relative differences.}

        \item{sampler}{In the case of collisions when matching keys, how the row- or column-vectors should be combined. Acceptable values are "identity", "min", "max", "sum", and "mean". A user-specified function may also be provided. Using "identity" means collisions result in an error. Using "sum" or "mean" results in binning all matches.}

        \item{pointers}{TODO}

        \item{x}{An object to check if it is a sparse matrix or coerce to a sparse matrix.}

        \item{\dots}{Additional arguments to be passed to constructor.}
}

\section{Slots}{
    \describe{
        \item{\code{data}:}{This slot stores any information necessary to access the data for the object (which may include the data itself and/or paths to file locations, etc.).}

        \item{\code{type}:}{The storage mode of the \emph{accessed} data when read into R. This is a 'factor' with levels 'raw', 'logical', 'integer', 'numeric', or 'character'.}

        \item{\code{dim}:}{Either 'NULL' for vectors, or an integer vector of length one of more giving the maximal indices in each dimension for matrices and arrays.}

        \item{\code{names}:}{The names of the data elements for vectors.}

        \item{\code{dimnames}:}{Either 'NULL' or the names for the dimensions. If not 'NULL', then this should be a list of character vectors of the length given by 'dim' for each dimension. This is always 'NULL' for vectors.}

        \item{\code{index}:}{TODO}

        \item{\code{pointers}:}{TODO}

        \item{\code{domain}:}{TODO}

        \item{\code{offset}:}{TODO}

        \item{\code{tolerance}:}{The tolerance to be used when matching indices from \code{index} to the \code{domain}. An attribute 'tol_type' gives whether 'absolute' or 'relative' differences should be used for the comparison.}

        \item{\code{sampler}:}{This is a function determining how the row- or column-vectors should be combined (or not) when index matching collisions occur.}

        \item{\code{ops}:}{Deferred arithmetic operations.}

        \item{\code{transpose}}{Indicates whether the data is stored in row-major order (TRUE) or column-major order (FALSE). For a matrix, switching the order that the data is read is equivalent to transposing the matrix (without changing any data).}

        

        
    }
}

\section{Warning}{
    If 'data' is given as a length-2 list of key-value pairs, no checking is performed on the validity of the key-value pairs, as this may be a costly operation if the list is stored in virtual memory. Each element of the 'keys' element must be \emph{sorted} in increasing order, or behavior may be unexpected.

    Assigning a new data element to the sparse matrix will always sort the key-value pairs of the row or column into which it was assigned.
}

\section{Extends}{
   \code{\linkS4class{matter}}
}

\section{Creating Objects}{
    \code{sparse_mat} instances can be created through \code{sparse_mat()}.
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
