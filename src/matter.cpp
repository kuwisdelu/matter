
#include <cmath>

#include "matter.h"

//// Low-level utility functions
//-------------------------------

//// ----------------- R <----> C ----------------- /////

// Rbyte <----> Rbyte

template<>
Rbyte coerce_cast<Rbyte,Rbyte>(Rbyte x) {
    return x;
}

// (int,double) ----> int

template<>
int coerce_cast<int,int>(int x) {
    return x;
}

template<>
int coerce_cast<double,int>(double x) {
    if ( x < R_INT_MIN || x > R_INT_MAX || !R_FINITE(x) )
    {
        if ( !ISNA(x) )
            warning("value is out of range for type 'int', element will be set to NA");
        return NA_INTEGER;
    }
    warning("casting from 'double' to 'int', precision may be lost");
    return static_cast<int>(x);
}

// (int,double) ----> double

template<>
double coerce_cast<int,double>(int x) {
    if ( x == NA_INTEGER )
        return NA_REAL;
    else
        return static_cast<double>(x);
}

template<>
double coerce_cast<double,double>(double x) {
    return x;
}

//// ----------------- R -----> C ----------------- /////

// (Rbyte,int,double) ----> char

template<>
char coerce_cast<Rbyte,char>(Rbyte x) {
    return static_cast<char>(x);
}

template<>
char coerce_cast<int,char>(int x) {
    if ( x < R_CHAR_MIN || x > R_CHAR_MAX || x == NA_INTEGER )
    {
        if ( x != NA_INTEGER )
            warning("value is out of range for type 'char', element will be set to NA");
        return NA_CHAR;
    }
    return static_cast<char>(x);
}

template<>
char coerce_cast<double,char>(double x) {
    if ( x < R_CHAR_MIN || x > R_CHAR_MAX || !R_FINITE(x) )
    {
        if ( !ISNA(x) )
            warning("value is out of range for type 'char', element will be set to NA");
        return NA_CHAR;
    }
    warning("casting from 'double' to 'char', precision may be lost");
    return static_cast<char>(x);
}

// (int,double) ----> uchar

template<>
unsigned char coerce_cast<int,unsigned char>(int x) {
    if ( x < 0 || x > R_UCHAR_MAX || x == NA_INTEGER )
    {
        if ( x != NA_INTEGER )
            warning("value is out of range for type 'unsigned char', element will be set to 0");
        return 0;
    }
    return static_cast<unsigned char>(x);
}

template<>
unsigned char coerce_cast<double,unsigned char>(double x) {
    if ( x < 0 || x > R_UCHAR_MAX || !R_FINITE(x) )
    {
        if ( !ISNA(x) )
            warning("value is out of range for type 'unsigned char', element will be set to 0");
        return 0;
    }
    warning("casting from 'double' to 'unsigned char', precision may be lost");
    return static_cast<unsigned char>(x);
}

// (Rbyte,int,double) ----> short

template<>
short coerce_cast<Rbyte,short>(Rbyte x) {
    return static_cast<short>(x);
}

template<>
short coerce_cast<int,short>(int x) {
    if ( x < R_SHORT_MIN || x > R_SHORT_MAX || x == NA_INTEGER )
    {
        if ( x != NA_INTEGER )
            warning("value is out of range for type 'short', element will be set to NA");
        return NA_SHORT;
    }
    return static_cast<short>(x);
}

template<>
short coerce_cast<double,short>(double x) {
    if ( x < R_SHORT_MIN || x > R_SHORT_MAX || !R_FINITE(x) )
    {
        if ( !ISNA(x) )
            warning("value is out of range for type 'short', element will be set to NA");
        return NA_SHORT;
    }
    warning("casting from 'double' to 'short', precision may be lost");
    return static_cast<short>(x);
}


// (Rbyte,int,double) ----> ushort

template<>
unsigned short coerce_cast<Rbyte,unsigned short>(Rbyte x) {
    return static_cast<unsigned short>(x);
}

template<>
unsigned short coerce_cast<int,unsigned short>(int x) {
    if ( x < 0 || x > R_USHORT_MAX || x == NA_INTEGER )
    {
        if ( x != NA_INTEGER )
            warning("value is out of range for type 'unsigned short', element will be set to 0");
        return 0;
    }
    return static_cast<unsigned short>(x);
}

template<>
unsigned short coerce_cast<double,unsigned short>(double x) {
    if ( x < 0 || x > R_USHORT_MAX || !R_FINITE(x) )
    {
        if ( !ISNA(x) )
            warning("value is out of range for type 'unsigned short', element will be set to 0");
        return 0;
    }
    warning("casting from 'double' to 'unsigned short', precision may be lost");
    return static_cast<unsigned short>(x);
}


// (Rbyte,int,double) ----> uint

template<>
unsigned int coerce_cast<Rbyte,unsigned int>(Rbyte x) {
    return static_cast<unsigned int>(x);
}

template<>
unsigned int coerce_cast<int,unsigned int>(int x) {
    if ( x < 0 )
    {
        warning("value is out of range for type 'int', element will be set to 0");
        return 0;
    }
    return static_cast<int>(x);
}

template<>
unsigned int coerce_cast<double,unsigned int>(double x) {
    if ( x < 0 || x > R_UINT_MAX || !R_FINITE(x) )
    {
        if ( !ISNA(x) )
            warning("value is out of range for type 'unsigned int', element will be set to 0");
        return 0;
    }
    warning("casting from 'double' to 'unsigned int', precision may be lost");
    return static_cast<unsigned int>(x);
}

// (Rbyte,int,double) ----> long

template<>
long coerce_cast<Rbyte,long>(Rbyte x) {
    return static_cast<long>(x);
}

template<>
long coerce_cast<int,long>(int x) {
    if ( x == NA_INTEGER )
        return NA_LONG;
    else
        return static_cast<long>(x);
}

template<>
long coerce_cast<double,long>(double x) {
    if ( !R_FINITE(x) )
    {
        if ( !ISNA(x) )
            warning("value is out of range for type 'long', element will be set to NA");
        return NA_LONG;
    }
    warning("casting from 'double' to 'long', precision may be lost");
    return static_cast<long>(x);
}

// (Rbyte,int, double) ----> ulong

template<>
unsigned long coerce_cast<Rbyte,unsigned long>(Rbyte x) {
    return static_cast<unsigned long>(x);
}

template<>
unsigned long coerce_cast<int,unsigned long>(int x) {
    if ( x == NA_INTEGER )
    {
        warning("value is out of range for type 'long', element will be set to 0");
        return 0;
    }
    else
        return static_cast<unsigned long>(x);
}

template<>
unsigned long coerce_cast<double,unsigned long>(double x) {
    if ( !R_FINITE(x) )
    {
        if ( !ISNA(x) )
            warning("value is out of range for type 'long', element will be set to 0");
        return 0;
    }
    warning("casting from 'double' to 'unsigned long', precision may be lost");
    return static_cast<unsigned long>(x);
}

// (Rbyte,int,double) ----> float

template<>
float coerce_cast<Rbyte,float>(Rbyte x) {
    return static_cast<float>(x);
}

template<>
float coerce_cast<int,float>(int x) {
    if ( x == NA_INTEGER )
        return static_cast<float>(NA_REAL);
    else
        return static_cast<float>(x);
}

template<>
float coerce_cast<double,float>(double x) {
    if ( !R_FINITE(x) )
    {
        if ( !ISNA(x) )
            warning("value is out of range for type 'float' and will be set to NA");
        return static_cast<float>(NA_REAL);
    }
    warning("casting from 'double' to 'float', precision may be lost");
    return static_cast<float>(x);
}

//// C -----> R

// (char,short,ushort,uint,long,ulong,float) ----> Rbyte

template<>
Rbyte coerce_cast<char,Rbyte>(char x) {
    if ( x < 0 || x == NA_CHAR )
    {
        warning("value is out of range for type 'unsigned char', element will be set to 0");
        return 0;
    }
    else
        return static_cast<Rbyte>(x);
}

template<>
Rbyte coerce_cast<short,Rbyte>(short x) {
    if ( x < 0 || x > R_UCHAR_MAX || x == NA_SHORT )
    {
        warning("value is out of range for type 'unsigned char', element will be set to 0");
        return 0;
    }
    else
        return static_cast<Rbyte>(x);
}

template<>
Rbyte coerce_cast<unsigned short,Rbyte>(unsigned short x) {
    if ( x > R_UCHAR_MAX )
    {
        warning("value is out of range for type 'unsigned char', element will be set to 0");
        return 0;   
    }
    return static_cast<Rbyte>(x);
}

template<>
Rbyte coerce_cast<unsigned int,Rbyte>(unsigned int x) {
    if ( x > R_UCHAR_MAX )
    {
        warning("value is out of range for type 'unsigned char', element will be set to 0");
        return 0;
    }
    return static_cast<Rbyte>(x);
}

template<>
Rbyte coerce_cast<long,Rbyte>(long x) {
    if ( x < 0 || x > R_UCHAR_MAX || x == NA_LONG )
    {
        warning("value is out of range for type 'unsigned char', element will be set to 0");
        return 0;
    }
    else
        return static_cast<Rbyte>(x);
}

template<>
Rbyte coerce_cast<unsigned long,Rbyte>(unsigned long x) {
    if ( x > R_UCHAR_MAX )
    {
        warning("value is out of range for type 'unsigned char', element will be set to 0");
        return 0;
    }
    return static_cast<Rbyte>(x);
}

template<>
Rbyte coerce_cast<float,Rbyte>(float x) {
    if ( x < 0 || x > R_UCHAR_MAX || std::isnan(x) )
    {
        warning("value is out of range for type 'unsigned char', element will be set to 0");
        return 0;
    }
    else
    {
        warning("casting from 'float' to 'unsigned char', precision may be lost");
        return static_cast<Rbyte>(x);
    }
}

// (char,uchar,short,ushort,uint,long,ulong,float) ----> int

template<>
int coerce_cast<char,int>(char x) {
    if ( x == NA_CHAR )
        return NA_INTEGER;
    else
        return static_cast<int>(x);
}

template<>
int coerce_cast<unsigned char,int>(unsigned char x) {
    return static_cast<int>(x);
}

template<>
int coerce_cast<short,int>(short x) {
    if ( x == NA_SHORT )
        return NA_INTEGER;
    else
        return static_cast<int>(x);
}

template<>
int coerce_cast<unsigned short,int>(unsigned short x) {
    return static_cast<int>(x);
}

template<>
int coerce_cast<unsigned int,int>(unsigned int x) {
    if ( x > R_INT_MAX )
    {
        warning("value is out of range for type 'int', element will be set to NA");
        return NA_INTEGER;
    }
    return static_cast<int>(x);
}

template<>
int coerce_cast<long,int>(long x) {
    if ( x < R_INT_MIN || x > R_INT_MAX || x == NA_LONG )
    {
        if ( x != NA_LONG )
            warning("value is out of range for type 'int', element will be set to NA");
        return NA_INTEGER;
    }
    if ( x == NA_LONG )
        return NA_INTEGER;
    else
        return static_cast<int>(x);
}

template<>
int coerce_cast<unsigned long,int>(unsigned long x) {
    if ( x > R_INT_MAX )
    {
        warning("value is out of range for type 'int', element will be set to NA");
        return NA_INTEGER;
    }
    return static_cast<int>(x);
}

template<>
int coerce_cast<float,int>(float x) {
    warning("casting from 'float' to 'int', precision may be lost");
    if ( std::isnan(x) )
        return NA_INTEGER;
    else
        return static_cast<int>(x);
}

// (char,uchar,short,ushort,uint,long,ulong,float) ----> double

template<>
double coerce_cast<char,double>(char x) {
    return static_cast<double>(x);
}

template<>
double coerce_cast<unsigned char,double>(unsigned char x) {
    return static_cast<double>(x);
}

template<>
double coerce_cast<short,double>(short x) {
    if ( x == NA_SHORT )
        return NA_REAL;
    else
        return static_cast<double>(x);
}

template<>
double coerce_cast<unsigned short,double>(unsigned short x) {
    return static_cast<double>(x);
}

template<>
double coerce_cast<unsigned int,double>(unsigned int x) {
    return static_cast<double>(x);
}

template<>
double coerce_cast<long,double>(long x) {
    if ( x == NA_LONG )
        return NA_REAL;
    else
        return static_cast<double>(x);
}

template<>
double coerce_cast<unsigned long,double>(unsigned long x) {
    return static_cast<double>(x);
}

template<>
double coerce_cast<float,double>(float x) {
    if ( std::isnan(x) )
        return NA_REAL;
    else
        return static_cast<double>(x);
}



//// Delayed operations on atoms
//-------------------------------

Ops null_transform() {
    Ops ret;
    ret.center_t = none;
    ret.center = NULL;
    ret.scale_t = none;
    ret.scale = NULL;
    return ret;
}

//// Count # of consecutive indices after current one (for faster reads)
//----------------------------------------------------------------------

index_t num_consecutive(Rindex_t * pindex, long i, long length) {
    index_t n = 0;
    if ( ISNA(pindex[i + 1]) )
        return n;
    if ( i < length - 1 && pindex[i + 1] > pindex[i] ) {
        while ( i < length - 1 && !ISNA(pindex[i + 1]) && 
            static_cast<index_t>(pindex[i + 1] - pindex[i]) == 1 )
        {
            i++;
            n++;
        }
        return n;
    }
    else if ( i < length - 1 && pindex[i + 1] < pindex[i] ) {
        while ( i < length - 1 && !ISNA(pindex[i + 1]) && 
            static_cast<index_t>(pindex[i + 1] - pindex[i]) == -1 )
        {
            i++;
            n--;
        }
        return n;
    }
    else
        return n;
}

//// Vector methods implemented for class Matter
//-----------------------------------------------

template<typename RType>
SEXP Matter :: readVector() {
    SEXP retVec;
    Atoms atoms(data(), sources(), ops());
    PROTECT(retVec = allocVector(DataType<RType>(), length()));
    RType * pRetVec = DataPtr<RType>(retVec);
    atoms.read<RType>(pRetVec, 0, length());
    UNPROTECT(1);
    return retVec;
}

template<typename RType>
void Matter :: writeVector(SEXP value) {
    RType * pValue = DataPtr<RType>(value);
    Atoms atoms(data(), sources(), ops());
    atoms.write<RType>(pValue, 0, length());
}

template<typename RType>
SEXP Matter :: readVectorElements(SEXP i) {
    SEXP retVec;
    PROTECT(retVec = allocVector(DataType<RType>(), XLENGTH(i)));
    RType * pRetVec = DataPtr<RType>(retVec);
    Rindex_t * pIndex = INDEX_PTR(i);
    Atoms atoms(data(), sources(), ops());
    atoms.read_indices<RType>(pRetVec, pIndex, XLENGTH(i));
    UNPROTECT(1);
    return retVec;
}

template<typename RType>
void Matter :: writeVectorElements(SEXP i, SEXP value) {
    RType * pValue = DataPtr<RType>(value);
    Rindex_t * pIndex = INDEX_PTR(i);
    Atoms atoms(data(), sources(), ops());
    atoms.write_indices(pValue, pIndex, XLENGTH(i));
}

//// Matrix methods implemented for class Matter
//-----------------------------------------------

template<typename RType>
SEXP Matter :: readMatrix() {
    SEXP retMat;
    int nrows = this->nrows(), ncols = this->ncols();
    PROTECT(retMat = allocMatrix(DataType<RType>(), nrows, ncols));
    RType * pRetMat = DataPtr<RType>(retMat);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int col = 0; col < ncols; col++ ) {
                Atoms atoms(data(col), sources(), ops(col));
                atoms.read<RType>(pRetMat + col * nrows, 0, nrows);
            }
            break;
        case MATTER_MATR:
            for ( int row = 0; row < nrows; row++ ) {
                Atoms atoms(data(row), sources(), ops(row));
                atoms.read<RType>(pRetMat + row, 0, ncols, nrows);
            }
            break;
    }
    UNPROTECT(1);
    return retMat;
}

template<typename RType>
void Matter :: writeMatrix(SEXP value) {
    int nrows = this->nrows(), ncols = this->ncols();
    RType * pValue = DataPtr<RType>(value);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int col = 0; col < ncols; col++ ) {
                Atoms atoms(data(col), sources(), ops(col));
                atoms.write<RType>(pValue + col * nrows, 0, nrows);
            }
            break;
        case MATTER_MATR:
            for ( int row = 0; row < nrows; row++ ) {
                Atoms atoms(data(row), sources(), ops(row));
                atoms.write<RType>(pValue + row, 0, ncols, nrows);
            }
            break;
    }
}

template<typename RType>
SEXP Matter :: readMatrixRows(SEXP i) {
    SEXP retMat;
    int nrows = LENGTH(i), ncols = this->ncols();
    PROTECT(retMat = allocMatrix(DataType<RType>(), nrows, ncols));
    RType * pRetMat = DataPtr<RType>(retMat);
    Rindex_t * pRow = INDEX_PTR(i);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int col = 0; col < ncols; col++ ) {
                Atoms atoms(data(col), sources(), ops(col));
                atoms.read_indices<RType>(pRetMat + col * nrows, pRow, nrows);
            }
            break;
        case MATTER_MATR:
            for ( int l = 0; l < nrows; l++ ) {
                if ( ISNA(pRow[l]) )
                    fillNA<RType>(pRetMat + l, ncols, nrows);
                else {
                    index_t row = static_cast<index_t>(pRow[l]);
                    Atoms atoms(data(row), sources(), ops(row));
                    atoms.read<RType>(pRetMat + l, 0, ncols, nrows);
                }
            }
            break;
    }
    UNPROTECT(1);
    return retMat;
}

template<typename RType>
void Matter :: writeMatrixRows(SEXP i, SEXP value) {
    int nrows = LENGTH(i), ncols = this->ncols();
    RType * pValue = DataPtr<RType>(value);
    Rindex_t * pRow = INDEX_PTR(i);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int col = 0; col < ncols; col++ ) {
                Atoms atoms(data(col), sources(), ops(col));
                atoms.write_indices(pValue + col * nrows, pRow, nrows);
            }
            break;
        case MATTER_MATR:
            for ( int l = 0; l < nrows; l++ ) {
                if ( ISNA(pRow[l]) )
                    continue;
                index_t row = static_cast<index_t>(pRow[l]);
                Atoms atoms(data(row), sources(), ops(row));
                atoms.write<RType>(pValue + l, 0, ncols, nrows);
            }
            break;
    }
}

template<typename RType>
SEXP Matter :: readMatrixCols(SEXP j) {
    SEXP retMat;
    int nrows = this->nrows(), ncols = LENGTH(j);
    PROTECT(retMat = allocMatrix(DataType<RType>(), nrows, ncols));
    RType * pRetMat = DataPtr<RType>(retMat);
    Rindex_t * pCol = INDEX_PTR(j);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int l = 0; l < ncols; l++ ) {
                if ( ISNA(pCol[l]) )
                    fillNA<RType>(pRetMat + l * nrows, nrows);
                else {
                    index_t col = static_cast<index_t>(pCol[l]);
                    Atoms atoms(data(col), sources(), ops(col));
                    atoms.read<RType>(pRetMat + l * nrows, 0, nrows);
                }
            }
            break;
        case MATTER_MATR:
            for ( int row = 0; row < nrows; row++ ) {
                Atoms atoms(data(row), sources(), ops(row));
                atoms.read_indices<RType>(pRetMat + row, pCol, ncols, nrows);
            }
            break;
    }
    UNPROTECT(1);
    return retMat;
}

template<typename RType>
void Matter :: writeMatrixCols(SEXP j, SEXP value) {
    int nrows = this->nrows(), ncols = LENGTH(j);
    RType * pValue = DataPtr<RType>(value);
    Rindex_t * pCol = INDEX_PTR(j);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int l = 0; l < ncols; l++ ) {
                if ( ISNA(pCol[l]) )
                    continue;
                index_t col = static_cast<index_t>(pCol[l]);
                Atoms atoms(data(col), sources(), ops(col));
                atoms.write<RType>(pValue + l * nrows, 0, nrows);
            }
            break;
        case MATTER_MATR:
            for ( int row = 0; row < nrows; row++ ) {
                Atoms atoms(data(row), sources(), ops(row));
                atoms.write_indices(pValue + row, pCol, ncols, nrows);
            }
            break;
    }
}

template<typename RType>
SEXP Matter :: readMatrixElements(SEXP i, SEXP j) {
    SEXP retMat;
    int nrows = LENGTH(i), ncols = LENGTH(j);
    PROTECT(retMat = allocMatrix(DataType<RType>(), nrows, ncols));
    RType * pRetMat = DataPtr<RType>(retMat);
    Rindex_t * pRow = INDEX_PTR(i);
    Rindex_t * pCol = INDEX_PTR(j);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int l = 0; l < ncols; l++ ) {
                if ( ISNA(pCol[l]) )
                    fillNA<RType>(pRetMat + l * nrows, nrows);
                else {
                    index_t col = static_cast<index_t>(pCol[l]);
                    Atoms atoms(data(col), sources(), ops(col));
                    atoms.read_indices<RType>(pRetMat + l * nrows, pRow, nrows);
                }
            }
            break;
        case MATTER_MATR:
            for ( int l = 0; l < nrows; l++ ) {
                if ( ISNA(pRow[l]) )
                    fillNA<RType>(pRetMat + l, ncols, nrows);
                else {
                    index_t row = static_cast<index_t>(pRow[l]);
                    Atoms atoms(data(row), sources(), ops(row));
                    atoms.read_indices<RType>(pRetMat + l, pCol, ncols, nrows);
                }
            }
            break;
    }
    UNPROTECT(1);
    return retMat;
}

template<typename RType>
void Matter :: writeMatrixElements(SEXP i, SEXP j, SEXP value) {
    int nrows = LENGTH(i), ncols = LENGTH(j);
    RType * pValue = DataPtr<RType>(value);
    Rindex_t * pRow = INDEX_PTR(i);
    Rindex_t * pCol = INDEX_PTR(j);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int l = 0; l < ncols; l++ ) {
                if ( ISNA(pCol[l]) )
                    continue;
                index_t col = static_cast<index_t>(pCol[l]);
                Atoms atoms(data(col), sources(), ops(col));
                atoms.write_indices(pValue + l * nrows, pRow, nrows);
            }
            break;
        case MATTER_MATR:
            for ( int l = 0; l < nrows; l++ ) {
                if ( ISNA(pRow[l]) )
                    continue;
                index_t row = static_cast<index_t>(pRow[l]);
                Atoms atoms(data(row), sources(), ops(row));
                atoms.write_indices(pValue + l, pCol, ncols, nrows);
            }
            break;
    }
}

//// Statistical functions applied on MatterAccessor
//--------------------------------------------------

double sum(MatterAccessor<double> & x, bool na_rm) {
    double retVal = 0;
    while ( x ) {
        if ( R_FINITE(*x) )
        {
            retVal += *x;
        }
        else if ( !na_rm || R_INFINITE(*x) )
            return *x;
        ++x;
    }
    return retVal;
}

double mean(MatterAccessor<double> & x, bool na_rm) {
    double retVal = 0;
    index_t n = 0;
    while ( x ) {
        if ( R_FINITE(*x) )
        {
            retVal += *x;
            n++;
        }
        else if ( !na_rm || R_INFINITE(*x) )
            return *x;
        ++x;
    }
    return retVal /= n;
}

double var(MatterAccessor<double> & x, bool na_rm) {
    double m_old, m_new, s_old, s_new;
    index_t n = 0;
    while ( x ) {
        if ( R_FINITE(*x) )
        {
            if ( n < 1 )
            {
                n = 1;
                m_new = *x;
                s_new = 0;
            }
            else
            {
                n++;
                m_old = m_new;
                s_old = s_new;
                m_new = m_old + (*x - m_old) / n;
                s_new = s_old + (*x - m_old) * (*x - m_new);
            }
        }
        else if ( !na_rm && (ISNA(*x) || ISNAN(*x)) )
            return NA_REAL;
        else if ( R_INFINITE(*x) )
            return R_NaN;
        ++x;
    }
    if ( n < 2 )
        return R_NaN;
    else
        return s_new / (n - 1);
}

//// Statistical methods implemented for class Matter
//---------------------------------------------------

SEXP Matter :: sum(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = NEW_NUMERIC(1));
    MatterAccessor<double> x(*this);
    *REAL(retVal) = ::sum(x, na_rm);
    UNPROTECT(1);
    return retVal;
}

SEXP Matter :: mean(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = NEW_NUMERIC(1));
    MatterAccessor<double> x(*this);
    *REAL(retVal) = ::mean(x, na_rm);
    UNPROTECT(1);
    return retVal;
}

SEXP Matter :: var(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = NEW_NUMERIC(1));
    MatterAccessor<double> x(*this);
    *REAL(retVal) = ::var(x, na_rm);
    UNPROTECT(1);
    return retVal;
}

SEXP Matter :: colsums(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = NEW_NUMERIC(ncols()));
    double * pRetVal = REAL(retVal);
    switch(S4class()) {
        case MATTER_VEC:
            error("'x' must be an array of at least two dimensions");
        case MATTER_MATC:
            for ( int j = 0; j < ncols(); j++ ) {
                MatterAccessor<double> x(*this, j);
                pRetVal[j] = ::sum(x, na_rm);
            }
            break;
        case MATTER_MATR:
            for ( int j = 0; j < ncols(); j++ )
                pRetVal[j] = 0;
            for ( int i = 0; i < nrows(); i++ ) {
                MatterAccessor<double> x(*this, i);
                int j = 0;
                while ( x ) {
                    if ( R_FINITE(pRetVal[j]) )
                    {
                        if ( R_FINITE(*x) )
                            pRetVal[j] += *x;
                        else if ( !na_rm || R_INFINITE(*x) )
                            pRetVal[j] = *x;
                    }
                    j++;
                    ++x;
                }
            }
            break;
    }
    UNPROTECT(1);
    return retVal;
}

SEXP Matter :: colmeans(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = NEW_NUMERIC(ncols()));
    double * pRetVal = REAL(retVal);
    switch(S4class()) {
        case MATTER_VEC:
            error("'x' must be an array of at least two dimensions");
        case MATTER_MATC:
            for ( int j = 0; j < ncols(); j++ ) {
                MatterAccessor<double> x(*this, j);
                pRetVal[j] = ::mean(x, na_rm);
            }
            break;
        case MATTER_MATR:
            {
                double * n = (double *) Calloc(ncols(), double);
                for ( int j = 0; j < ncols(); j++ ) {
                    pRetVal[j] = 0;
                    n[j] = 0;
                }
                for ( int i = 0; i < nrows(); i++ ) {
                    MatterAccessor<double> x(*this, i);
                    int j = 0;
                    while ( x ) {
                        if ( R_FINITE(pRetVal[j]) )
                        {
                            if ( R_FINITE(*x) )
                            {
                                pRetVal[j] += *x;
                                n[j]++;
                            }
                            else if ( !na_rm || R_INFINITE(*x) )
                                pRetVal[j] = *x;
                        }
                        j++;
                        ++x;
                    }
                }
                for ( int j = 0; j < ncols(); j++ )
                    if ( R_FINITE(pRetVal[j]) )
                        pRetVal[j] /= n[j];
                Free(n);
            }
            break;
    }
    UNPROTECT(1);
    return retVal;
}

SEXP Matter :: colvar(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = NEW_NUMERIC(ncols()));
    double * pRetVal = REAL(retVal);
    switch(S4class()) {
        case MATTER_VEC:
            error("'x' must be an array of at least two dimensions");
        case MATTER_MATC:
            for ( int j = 0; j < ncols(); j++ ) {
                MatterAccessor<double> x(*this, j);
                pRetVal[j] = ::var(x, na_rm);
            }
            break;
        case MATTER_MATR:
            {
                double * m_old = (double *) Calloc(ncols(), double);
                double * m_new = (double *) Calloc(ncols(), double);
                double * n = (double *) Calloc(ncols(), double);
                for ( int j = 0; j < ncols(); j++ ) {
                    pRetVal[j] = 0;
                    n[j] = 0;
                }
                for ( int i = 0; i < nrows(); i++ ) {
                    MatterAccessor<double> x(*this, i);
                    int j = 0;
                    while ( x ) {
                        if ( R_FINITE(pRetVal[j]) )
                        {
                            if ( R_FINITE(*x) )
                            {
                                if ( n[j] < 1 )
                                {
                                    n[j]++;
                                    m_new[j] = *x;
                                    pRetVal[j] = 0;
                                }
                                else
                                {
                                    n[j]++;
                                    m_old[j] = m_new[j];
                                    m_new[j]= m_old[j] + (*x - m_old[j]) / n[j];
                                    pRetVal[j] = pRetVal[j] + (*x - m_old[j]) * (*x - m_new[j]);
                                }
                            }
                            else
                            {
                                if ( !na_rm && (ISNA(*x) || ISNAN(*x)) )
                                    pRetVal[j] = NA_REAL;
                                else if ( R_INFINITE(*x) )
                                    pRetVal[j] = R_NaN;
                            }
                        }
                        j++;
                        ++x;
                    }
                }
                for ( int j = 0; j < ncols(); j++ ) {
                    if ( R_FINITE(pRetVal[j]) )
                    {
                        if ( n[j] < 2 )
                            pRetVal[j] = NA_REAL;
                        else
                            pRetVal[j] = pRetVal[j] / (n[j] - 1);
                    }
                }
                Free(m_old);
                Free(m_new);
                Free(n);
            }
            break;
    }
    UNPROTECT(1);
    return retVal;
}

SEXP Matter :: rowsums(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = NEW_NUMERIC(nrows()));
    double * pRetVal = REAL(retVal);
    switch(S4class()) {
        case MATTER_VEC:
            error("'x' must be an array of at least two dimensions");
        case MATTER_MATC:
            for ( int i = 0; i < nrows(); i++ )
                pRetVal[i] = 0;
            for ( int j = 0; j < ncols(); j++ ) {
                MatterAccessor<double> x(*this, j);
                int i = 0;
                while ( x ) {
                    if ( R_FINITE(pRetVal[i]) )
                    {
                        if ( R_FINITE(*x) )
                            pRetVal[i] += *x;
                        else if ( !na_rm || R_INFINITE(*x) )
                            pRetVal[i] = *x;
                    }
                    i++;
                    ++x;
                }
            }
            break;
        case MATTER_MATR:
            for ( int i = 0; i < nrows(); i++ ) {
                MatterAccessor<double> x(*this, i);
                pRetVal[i] = ::sum(x, na_rm);
            }
            break;
    }
    UNPROTECT(1);
    return retVal;
}

SEXP Matter :: rowmeans(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = NEW_NUMERIC(nrows()));
    double * pRetVal = REAL(retVal);
    switch(S4class()) {
        case MATTER_VEC:
            error("'x' must be an array of at least two dimensions");
        case MATTER_MATC:
            {
                double * n = (double *) Calloc(nrows(), double);
                for ( int i = 0; i < nrows(); i++ ) {
                    pRetVal[i] = 0;
                    n[i] = 0;
                }
                for ( int j = 0; j < ncols(); j++ ) {
                    MatterAccessor<double> x(*this, j);
                    int i = 0;
                    while ( x ) {
                        if ( R_FINITE(pRetVal[i]) )
                        {
                            if ( R_FINITE(*x) )
                            {
                                pRetVal[i] += *x;
                                n[i]++;
                            }
                            else if ( !na_rm || R_INFINITE(*x) )
                                pRetVal[i] = *x;
                        }
                        i++;
                        ++x;
                    }
                }
                for ( int i = 0; i < nrows(); i++ )
                    if ( R_FINITE(pRetVal[i]) )
                        pRetVal[i] /= n[i];
                Free(n);
            }
            break;
        case MATTER_MATR:
            for ( int i = 0; i < nrows(); i++ ) {
                MatterAccessor<double> x(*this, i);
                pRetVal[i] = ::mean(x, na_rm);
            }
            break;
    }
    UNPROTECT(1);
    return retVal;
}

SEXP Matter :: rowvar(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = NEW_NUMERIC(nrows()));
    double * pRetVal = REAL(retVal);
    switch(S4class()) {
        case MATTER_VEC:
            error("'x' must be an array of at least two dimensions");
        case MATTER_MATC:
            {
                double * m_old = (double *) Calloc(nrows(), double);
                double * m_new = (double *) Calloc(nrows(), double);
                double * n = (double *) Calloc(nrows(), double);
                for ( int i = 0; i < nrows(); i++ ) {
                    pRetVal[i] = 0;
                    n[i] = 0;
                }
                for ( int j = 0; j < ncols(); j++ ) {
                    MatterAccessor<double> x(*this, j);
                    int i = 0;
                    while ( x ) {
                        if ( R_FINITE(pRetVal[i]) )
                        {
                            if ( R_FINITE(*x) )
                            {
                                if ( n[i] < 1 )
                                {
                                    n[i]++;
                                    m_new[i] = *x;
                                    pRetVal[i] = 0;
                                }
                                else
                                {
                                    n[i]++;
                                    m_old[i] = m_new[i];
                                    m_new[i]= m_old[i] + (*x - m_old[i]) / n[i];
                                    pRetVal[i] = pRetVal[i] + (*x - m_old[i]) * (*x - m_new[i]);
                                }
                            }
                            else
                            {
                                if ( !na_rm && (ISNA(*x) || ISNAN(*x)) )
                                    pRetVal[i] = NA_REAL;
                                else if ( R_INFINITE(*x) )
                                    pRetVal[i] = R_NaN;
                            }
                        }
                        i++;
                        ++x;
                    }
                }
                for ( int i = 0; i < nrows(); i++ ) {
                    if ( R_FINITE(pRetVal[i]) )
                    {
                        if ( n[i] < 2 )
                            pRetVal[i] = R_NaN;
                        else
                            pRetVal[i] = pRetVal[i] / (n[i] - 1);
                    }
                }
                Free(m_old);
                Free(m_new);
                Free(n);
            }
            break;
        case MATTER_MATR:
            for ( int i = 0; i < nrows(); i++ ) {
                MatterAccessor<double> x(*this, i);
                pRetVal[i] = ::var(x, na_rm);
            }
            break;
    }
    UNPROTECT(1);
    return retVal;
}

template<typename RType>
SEXP Matter :: rmult(SEXP y) {
    SEXP retMat;
    PROTECT(retMat = allocMatrix(DataType<double>(), nrows(), ::ncols(y)));
    double * pRetMat = REAL(retMat);
    RType * pY = DataPtr<RType>(y);
    for ( int k = 0; k < LENGTH(retMat); k++ )
        pRetMat[k] = 0;
    switch(S4class()) {
        case MATTER_MATC:
            for ( int j = 0; j < ncols(); j++ ) {
                MatterAccessor<double> x(*this, j);
                int i = 0;
                while ( x ) {
                    for ( int jj = 0; jj < ::ncols(retMat); jj++ ) {
                        double val = (*x) * coerce_cast<RType,double>(pY[j + jj * ::nrows(y)]);
                        pRetMat[i + jj * ::nrows(retMat)] += val;
                    }
                    i++;
                    ++x;
                }
            }
            break;
        case MATTER_MATR:
            for ( int i = 0; i < nrows(); i++ ) {
                MatterAccessor<double> x(*this, i);
                int j = 0;
                while ( x ) {
                    for ( int jj = 0; jj < ::ncols(retMat); jj++ ) {
                        double val = (*x) * coerce_cast<RType,double>(pY[j + jj * ::nrows(y)]);
                        pRetMat[i + jj * ::nrows(retMat)] += val;
                    }
                    j++;
                    ++x;
                }
            }
            break;
    }
    UNPROTECT(1);
    return retMat;
}

template<typename RType>
SEXP Matter :: lmult(SEXP x) {
    SEXP retMat;
    PROTECT(retMat = allocMatrix(DataType<double>(), ::nrows(x), ncols()));
    double * pRetMat = REAL(retMat);
    RType * pX = DataPtr<RType>(x);
    for ( int k = 0; k < LENGTH(retMat); k++ )
        pRetMat[k] = 0;
    switch(S4class()) {
        case MATTER_MATC:
            for ( int j = 0; j < ncols(); j++ ) {
                MatterAccessor<double> y(*this, j);
                int i = 0;
                while ( y ) {
                    for ( int ii = 0; ii < ::nrows(retMat); ii++ ) {
                        double val = coerce_cast<RType,double>(pX[ii + i * ::nrows(x)]) * (*y);
                        pRetMat[ii + j * ::nrows(retMat)] += val;
                    }
                    i++;
                    ++y;
                }
            }
            break;
        case MATTER_MATR:
            for ( int i = 0; i < nrows(); i++ ) {
                MatterAccessor<double> y(*this, i);
                int j = 0;
                while ( y ) {
                    for ( int ii = 0; ii < ::nrows(retMat); ii++ ) {
                        double val = coerce_cast<RType,double>(pX[ii + i * ::nrows(x)]) * (*y);
                        pRetMat[ii + j * ::nrows(retMat)] += val;
                    }
                    j++;
                    ++y;
                }
            }
            break;
    }
    UNPROTECT(1);
    return retMat;
}

//// Functions to be called from R
//---------------------------------

extern "C" {

    SEXP getVector(SEXP x) {
        Matter mVec(x);
        switch(mVec.datamode()) {
            case R_RAW:
                return mVec.readVector<Rbyte>();
            case R_INTEGER:
                return mVec.readVector<int>();
            case R_NUMERIC:
                return mVec.readVector<double>();
            default:
                return R_NilValue;
        }
    }

    void setVector(SEXP x, SEXP value) {
        Matter mVec(x);
        switch(TYPEOF(value)) {
            case RAWSXP:
                mVec.writeVector<Rbyte>(value);
                break;
            case INTSXP:
                mVec.writeVector<int>(value);
                break;
            case REALSXP:
                mVec.writeVector<double>(value);
                break;
        }
    }

    SEXP getVectorElements(SEXP x, SEXP i) {
        Matter mVec(x);
        switch(mVec.datamode()) {
            case R_RAW:
                return mVec.readVectorElements<Rbyte>(i);
            case R_INTEGER:
                return mVec.readVectorElements<int>(i);
            case R_NUMERIC:
                return mVec.readVectorElements<double>(i);
            default:
                return R_NilValue;
        }
    }

    void setVectorElements(SEXP x, SEXP i, SEXP value) {
        Matter mVec(x);
        switch(TYPEOF(value)) {
            case RAWSXP:
                mVec.writeVectorElements<Rbyte>(i, value);
                break;
            case INTSXP:
                mVec.writeVectorElements<int>(i, value);
                break;
            case REALSXP:
                mVec.writeVectorElements<double>(i, value);
                break;
        }
    }

    SEXP getMatrix(SEXP x) {
        Matter mMat(x);
        switch(mMat.datamode()) {
            case R_RAW:
                return mMat.readMatrix<Rbyte>();
            case R_INTEGER:
                return mMat.readMatrix<int>();
            case R_NUMERIC:
                return mMat.readMatrix<double>();
            default:
                return R_NilValue;
        }
    }

    void setMatrix(SEXP x, SEXP value) {
        Matter mMat(x);
        switch(TYPEOF(value)) {
            case RAWSXP:
                mMat.writeMatrix<Rbyte>(value);
                break;
            case INTSXP:
                mMat.writeMatrix<int>(value);
                break;
            case REALSXP:
                mMat.writeMatrix<double>(value);
                break;
        }
    }

    SEXP getMatrixRows(SEXP x, SEXP i) {
        Matter mMat(x);
        switch(mMat.datamode()) {
            case R_RAW:
                return mMat.readMatrixRows<Rbyte>(i);
            case R_INTEGER:
                return mMat.readMatrixRows<int>(i);
            case R_NUMERIC:
                return mMat.readMatrixRows<double>(i);
            default:
                return R_NilValue;
        }
    }

    void setMatrixRows(SEXP x, SEXP i, SEXP value) {
        Matter mMat(x);
        switch(TYPEOF(value)) {
            case RAWSXP:
                mMat.writeMatrixRows<Rbyte>(i, value);
                break;
            case INTSXP:
                mMat.writeMatrixRows<int>(i, value);
                break;
            case REALSXP:
                mMat.writeMatrixRows<double>(i, value);
                break;
        }
    }

    SEXP getMatrixCols(SEXP x, SEXP j) {
        Matter mMat(x);
        switch(mMat.datamode()) {
            case R_RAW:
                return mMat.readMatrixCols<Rbyte>(j);
            case R_INTEGER:
                return mMat.readMatrixCols<int>(j);
            case R_NUMERIC:
                return mMat.readMatrixCols<double>(j);
            default:
                return R_NilValue;
        }
    }

    void setMatrixCols(SEXP x, SEXP j, SEXP value) {
        Matter mMat(x);
        switch(TYPEOF(value)) {
            case RAWSXP:
                mMat.writeMatrixCols<Rbyte>(j, value);
                break;
            case INTSXP:
                mMat.writeMatrixCols<int>(j, value);
                break;
            case REALSXP:
                mMat.writeMatrixCols<double>(j, value);
                break;
        }
    }

    SEXP getMatrixElements(SEXP x, SEXP i, SEXP j) {
        Matter mMat(x);
        switch(mMat.datamode()) {
            case R_RAW:
                return mMat.readMatrixElements<Rbyte>(i, j);
            case R_INTEGER:
                return mMat.readMatrixElements<int>(i, j);
            case R_NUMERIC:
                return mMat.readMatrixElements<double>(i, j);
            default:
                return R_NilValue;
        }
    }

    void setMatrixElements(SEXP x, SEXP i, SEXP j, SEXP value) {
        Matter mMat(x);
        switch(TYPEOF(value)) {
            case RAWSXP:
                mMat.writeMatrixElements<Rbyte>(i, j, value);
                break;
            case INTSXP:
                mMat.writeMatrixElements<int>(i, j, value);
                break;
            case REALSXP:
                mMat.writeMatrixElements<double>(i, j, value);
                break;
        }
    }

    SEXP getSum(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        return mMat.sum(LOGICAL_VALUE(na_rm));
    }

    SEXP getMean(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        return mMat.mean(LOGICAL_VALUE(na_rm));
    }

    SEXP getVar(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        return mMat.var(LOGICAL_VALUE(na_rm));
    }

    SEXP getColSums(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        return mMat.colsums(LOGICAL_VALUE(na_rm));
    }

    SEXP getColMeans(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        return mMat.colmeans(LOGICAL_VALUE(na_rm));
    }

    SEXP getColVars(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        return mMat.colvar(LOGICAL_VALUE(na_rm));
    }

    SEXP getRowSums(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        return mMat.rowsums(LOGICAL_VALUE(na_rm));
    }

    SEXP getRowMeans(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        return mMat.rowmeans(LOGICAL_VALUE(na_rm));
    }

    SEXP getRowVars(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        return mMat.rowvar(LOGICAL_VALUE(na_rm));
    }

    SEXP rightMatrixMult(SEXP x, SEXP y) {
        Matter mMat(x);
        if ( TYPEOF(y) == INTSXP )
            return mMat.rmult<int>(y);
        else if ( TYPEOF(y) == REALSXP )
            return mMat.rmult<double>(y);
        else
            return R_NilValue;
    }

    SEXP leftMatrixMult(SEXP x, SEXP y) {
        Matter mMat(y);
        if ( TYPEOF(x) == INTSXP )
            return mMat.lmult<int>(x);
        else if ( TYPEOF(x) == REALSXP )
            return mMat.lmult<double>(x);
        else
            return R_NilValue;
    }
}
