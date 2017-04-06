
#include <cmath>

#include "matter.h"

//// Low-level utility functions
//-------------------------------

template<typename T>
T coerce_pow(T base, T exponent) {
    double b = static_cast<double>(base);
    double x = static_cast<double>(exponent);
    return static_cast<T>(pow(b, x));
}

template<typename T>
T coerce_exp(T x) {
    return static_cast<T>(exp(static_cast<double>(x)));
}

template<typename T>
T coerce_log(T x) {
    return static_cast<T>(log(static_cast<double>(x)));
}

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
        if ( x == NA_INTEGER )
            warning("NAs not supported for type 'unsigned char', element will be set to 0");
        else
            warning("value is out of range for type 'unsigned char', element will be set to 0");
        return 0;
    }
    return static_cast<unsigned char>(x);
}

template<>
unsigned char coerce_cast<double,unsigned char>(double x) {
    if ( x < 0 || x > R_UCHAR_MAX || !R_FINITE(x) )
    {
        if ( ISNA(x) )
            warning("NAs not supported for type 'unsigned char', element will be set to 0");
        else
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
        if ( x == NA_INTEGER )
            warning("NAs not supported for type 'unsigned short', element will be set to 0");
        else
            warning("value is out of range for type 'unsigned short', element will be set to 0");
        return 0;
    }
    return static_cast<unsigned short>(x);
}

template<>
unsigned short coerce_cast<double,unsigned short>(double x) {
    if ( x < 0 || x > R_USHORT_MAX || !R_FINITE(x) )
    {
        if ( ISNA(x) )
            warning("NAs not supported for type 'unsigned short', element will be set to 0");
        else
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
    if ( x < 0 || x == NA_INTEGER )
    {
        if ( x == NA_INTEGER )
            warning("NAs not supported for type 'unsigned int', element will be set to 0");
        else
            warning("value is out of range for type 'unsigned int', element will be set to 0");
        return 0;
    }
    return static_cast<int>(x);
}

template<>
unsigned int coerce_cast<double,unsigned int>(double x) {
    if ( x < 0 || x > R_UINT_MAX || !R_FINITE(x) )
    {
        if ( ISNA(x) )
            warning("NAs not supported for type 'unsigned int', element will be set to 0");
        else
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
        if ( ISNA(x) )
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
    if ( x < 0 || x == NA_INTEGER )
    {
        if ( x == NA_INTEGER )
            warning("NAs not supported for type 'unsigned long', element will be set to 0");
        else
            warning("value is out of range for type 'unsigned long', element will be set to 0");
        return 0;
    }
    else
        return static_cast<unsigned long>(x);
}

template<>
unsigned long coerce_cast<double,unsigned long>(double x) {
    if ( !R_FINITE(x) )
    {
        if ( ISNA(x) )
            warning("NAs not supported for type 'unsigned long', element will be set to 0");
        else
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
        if ( x == NA_CHAR )
            warning("value is out of range for type 'unsigned char', element will be set to 0");
        else
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
        if ( x == NA_CHAR )
            warning("value is out of range for type 'unsigned char', element will be set to 0");
        else
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
        if ( x == NA_LONG )
            warning("value is out of range for type 'unsigned char', element will be set to 0");
        else
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

// (char,uchar,short,ushort,int,uint,long,ulong,float,double) ----> bool

template<>
bool coerce_cast<char,bool>(char x) {
    if ( x != 0 && x != 1 )
        warning("value is out of range for type 'bool', element will be set to FALSE");
    return static_cast<bool>(x);
}

template<>
bool coerce_cast<unsigned char,bool>(unsigned char x) {
    if ( x != 0 && x != 1 )
        warning("value is out of range for type 'bool', element will be set to FALSE");
    return static_cast<bool>(x);
}

template<>
bool coerce_cast<short,bool>(short x) {
    if ( x != 0 && x != 1 )
        warning("value is out of range for type 'bool', element will be set to FALSE");
    return static_cast<bool>(x);
}

template<>
bool coerce_cast<unsigned short,bool>(unsigned short x) {
    if ( x != 0 && x != 1 )
        warning("value is out of range for type 'bool', element will be set to FALSE");
    return static_cast<bool>(x);
}

template<>
bool coerce_cast<int,bool>(int x) {
    if ( x != 0 && x != 1 )
        warning("value is out of range for type 'bool', element will be set to FALSE");
    return static_cast<bool>(x);
}

template<>
bool coerce_cast<unsigned int,bool>(unsigned int x) {
    if ( x != 0 && x != 1 )
        warning("value is out of range for type 'bool', element will be set to FALSE");
    return static_cast<bool>(x);
}

template<>
bool coerce_cast<long,bool>(long x) {
    if ( x != 0 && x != 1 )
        warning("value is out of range for type 'bool', element will be set to FALSE");
    return static_cast<bool>(x);
}

template<>
bool coerce_cast<unsigned long,bool>(unsigned long x) {
    if ( x != 0 && x != 1 )
        warning("value is out of range for type 'bool', element will be set to FALSE");
    return static_cast<bool>(x);
}

template<>
bool coerce_cast<float,bool>(float x) {
    if ( x != 0 && x != 1 )
        warning("value is out of range for type 'bool', element will be set to FALSE");
    return static_cast<bool>(x);
}

template<>
bool coerce_cast<double,bool>(double x) {
    if ( x != 0 && x != 1 )
        warning("value is out of range for type 'bool', element will be set to FALSE");
    return static_cast<bool>(x);
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

//// Delta run length encoding 
//-----------------------------

index_t count_consecutive(Rindex_t * pindex, long i, long length) {
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

template<>
int run_delta<int>(int * values, int i, int n) {
    if ( i < n - 1 ) {
        if ( values[i] == NA_INTEGER || values[i + 1] == NA_INTEGER )
            return 0;
        else
            return values[i + 1] - values[i];
    }
    else
        return 0;
}

template<>
double run_delta<double>(double * values, int i, int n) {
    if ( i < n - 1 ) {
        if ( ISNA(values[i]) || ISNA(values[i + 1]) )
            return 0;
        else
            return values[i + 1] - values[i];
    }
    else
        return 0;
}

template<>
int run_length<int>(int * values, int i, int n, int delta) {
    int length = 1;
    while ( i < n - 1 && values[i + 1] - values[i] == delta ) {
        if ( values[i] == NA_INTEGER && values[i + 1] != NA_INTEGER )
            break;
        if ( values[i] != NA_INTEGER && values[i + 1] == NA_INTEGER )
            break;
        length++;
        i++;
    }
    return length;
}

template<>
int run_length<double>(double * values, int i, int n, double delta) {
    int length = 1;
    while ( i < n - 1 && is_equal_double(values[i + 1] - values[i], delta) ) {
        if ( ISNA(values[i]) && !ISNA(values[i + 1]) )
            break;
        if ( !ISNA(values[i]) && ISNA(values[i + 1]) )
            break;
        length++;
        i++;
    }
    return length;
}

template<>
int count_runs<int>(int * values, int n) {
    int delta1, delta2, length1, length2, i = 0, nruns = 0;
    while ( i < n ) {
        delta1 = run_delta<int>(values, i, n);
        length1 = run_length<int>(values, i, n, delta1);
        if ( length1 == 2 ) {
            delta2 = run_delta<int>(values, i + 1, n);
            length2 = run_length<int>(values, i + 1, n, delta2);
            if ( length2 > 2 ) {
                if ( delta1 == delta2 )
                {
                    nruns++;
                    i += (length2 + 1);
                    continue;
                }
                else
                {
                    nruns += 2;
                    i += (length2 + 1);
                    continue;
                }
            }
        }
        nruns++;
        i += length1;
    }
    return nruns;
}

template<>
int count_runs<double>(double * values, int n) {
    double delta1, delta2;
    int length1, length2, i = 0, nruns = 0;
    while ( i < n ) {
        delta1 = run_delta<double>(values, i, n);
        length1 = run_length<double>(values, i, n, delta1);
        if ( length1 == 2 ) {
            delta2 = run_delta<double>(values, i + 1, n);
            length2 = run_length<double>(values, i + 1, n, delta2);
            if ( length2 > 2 ) {
                if ( is_equal_double(delta1, delta2) )
                {
                    nruns++;
                    i += (length2 + 1);
                    continue;
                }
                else
                {
                    nruns += 2;
                    i += (length2 + 1);
                    continue;
                }
            }
        }
        nruns++;
        i += length1;
    }
    return nruns;
}

template<>
SEXP makeDRLE<int>(SEXP x, SEXP nruns) {
    SEXP retVals, retLengths, retDeltas;
    int * pX, * pRetVals, * pRetLengths, * pRetDeltas;
    PROTECT(retVals = NEW_INTEGER(INTEGER_VALUE(nruns)));
    PROTECT(retLengths = NEW_INTEGER(INTEGER_VALUE(nruns)));
    PROTECT(retDeltas = NEW_INTEGER(INTEGER_VALUE(nruns)));
    pX = INTEGER(x);
    pRetVals = INTEGER(retVals);
    pRetLengths = INTEGER(retLengths);
    pRetDeltas = INTEGER(retDeltas);
    int delta1, delta2, length1, length2, i = 0, nrun = 0, n = LENGTH(x);
    while ( i < n ) {
        delta1 = run_delta<int>(pX, i, n);
        length1 = run_length<int>(pX, i, n, delta1);
        if ( length1 == 2 ) {
            delta2 = run_delta<int>(pX, i + 1, n);
            length2 = run_length<int>(pX, i + 1, n, delta2);
            if ( length2 > 2 ) {
                if ( delta1 == delta2 )
                {
                    pRetVals[nrun] = pX[i];
                    pRetLengths[nrun] = length2 + 1;
                    pRetDeltas[nrun] = delta1;
                    nrun++;
                    i += (length2 + 1);
                    continue;
                }
                else
                {
                    pRetVals[nrun] = pX[i];
                    pRetLengths[nrun] = 1;
                    pRetDeltas[nrun] = 0;
                    pRetVals[nrun + 1] = pX[i + 1];
                    pRetLengths[nrun + 1] = length2;
                    pRetDeltas[nrun + 1] = delta2;
                    nrun += 2;
                    i += (length2 + 1);
                    continue;
                }
            }
        }
        pRetVals[nrun] = pX[i];
        pRetLengths[nrun] = length1;
        pRetDeltas[nrun] = delta1;
        nrun++;
        i += length1;
    }
    SEXP classDef, retObj;
    PROTECT(classDef = MAKE_CLASS("drle"));
    PROTECT(retObj = NEW_OBJECT(classDef));
    SET_SLOT(retObj, install("values"), retVals);
    SET_SLOT(retObj, install("lengths"), retLengths);
    SET_SLOT(retObj, install("deltas"), retDeltas);
    UNPROTECT(5);
    return retObj;
}

template<>
SEXP makeDRLE<double>(SEXP x, SEXP nruns) {
    SEXP retVals, retLengths, retDeltas;
    double * pX, * pRetVals, * pRetDeltas;
    int * pRetLengths;
    PROTECT(retVals = NEW_NUMERIC(INTEGER_VALUE(nruns)));
    PROTECT(retLengths = NEW_INTEGER(INTEGER_VALUE(nruns)));
    PROTECT(retDeltas = NEW_NUMERIC(INTEGER_VALUE(nruns)));
    pX = REAL(x);
    pRetVals = REAL(retVals);
    pRetLengths = INTEGER(retLengths);
    pRetDeltas = REAL(retDeltas);
    double delta1, delta2;
    int length1, length2, i = 0, nrun = 0, n = LENGTH(x);
    while ( i < n ) {
        delta1 = run_delta<double>(pX, i, n);
        length1 = run_length<double>(pX, i, n, delta1);
        if ( length1 == 2 ) {
            delta2 = run_delta<double>(pX, i + 1, n);
            length2 = run_length<double>(pX, i + 1, n, delta2);
            if ( length2 > 2 ) {
                if ( is_equal_double(delta1, delta2) )
                {
                    pRetVals[nrun] = pX[i];
                    pRetLengths[nrun] = length2 + 1;
                    pRetDeltas[nrun] = delta1;
                    nrun++;
                    i += (length2 + 1);
                    continue;
                }
                else
                {
                    pRetVals[nrun] = pX[i];
                    pRetLengths[nrun] = 1;
                    pRetDeltas[nrun] = 0;
                    pRetVals[nrun + 1] = pX[i + 1];
                    pRetLengths[nrun + 1] = length2;
                    pRetDeltas[nrun + 1] = delta2;
                    nrun += 2;
                    i += (length2 + 1);
                    continue;
                }
            }
        }
        pRetVals[nrun] = pX[i];
        pRetLengths[nrun] = length1;
        pRetDeltas[nrun] = delta1;
        nrun++;
        i += length1;
    }
    SEXP classDef, retObj;
    PROTECT(classDef = MAKE_CLASS("drle"));
    PROTECT(retObj = NEW_OBJECT(classDef));
    SET_SLOT(retObj, install("values"), retVals);
    SET_SLOT(retObj, install("lengths"), retLengths);
    SET_SLOT(retObj, install("deltas"), retDeltas);
    UNPROTECT(5);
    return retObj;
}

template<typename RType, int SType>
SEXP VectorOrDRLE<RType,SType> :: readVector() {
    SEXP retVec;
    int nout = length();
    PROTECT(retVec = allocVector(SType, nout));
    RType * pRetVec = DataPtr<RType,SType>(retVec);
    if ( isDRLE ) {
        int cum_index = 0;
        for ( int nrun = 0; nrun < nruns; nrun++ ) {
            for ( int i = 0; i < lengths[nrun]; i++ )
                pRetVec[cum_index + i] = values[nrun] + deltas[nrun] * i;
            cum_index += lengths[nrun];
        }
    } else {
        for ( int i = 0; i < nout; i++ )
            pRetVec[i] = values[i];
    }
    UNPROTECT(1);
    return retVec;
}

template<typename RType, int SType>
SEXP VectorOrDRLE<RType,SType> :: readVectorElements(SEXP i) {
    SEXP retVec;
    PROTECT(retVec = allocVector(SType, LENGTH(i)));
    RType * pRetVec = DataPtr<RType,SType>(retVec);
    int * pIndex = INTEGER(i);
    for ( int k = 0; k < LENGTH(i); k++ )
        pRetVec[k] = (*this)[pIndex[k]];
    UNPROTECT(1);
    return retVec;   
}

template<typename RType, int SType>
int VectorOrDRLE<RType,SType> :: length() {
    int length;
    if ( isDRLE ) {
        length = 0;
        for ( int nrun = 0; nrun < nruns; nrun++ )
            length += lengths[nrun];
    }
    else
        length = nruns;
    return length;
}

template<>
int VectorOrDRLE<int,INTSXP> :: find(int value) {
    if ( isDRLE )
    {
        int cum_index = 0;
        for ( int nrun = 0; nrun < nruns; nrun++ ) {
            int diff = value - values[nrun];
            int del = deltas[nrun];
            if ( diff == 0 )
                return static_cast<int>(cum_index);
            else if ( (del != 0 && diff % del == 0) )
                return static_cast<int>(cum_index + diff / del);
            else
                cum_index += lengths[nrun];
        }
    }
    else
    {
        for ( int i = 0; i < nruns; i++ )
            if ( (*this)[i] == value )
                return i;
    }
    return NA_INTEGER;
}

template<>
int VectorOrDRLE<double,REALSXP> :: find(double value) {
    if ( isDRLE )
    {
        int cum_index = 0;
        for ( int nrun = 0; nrun < nruns; nrun++ ) {
            double diff = value - values[nrun];
            double del = deltas[nrun];
            if ( is_equal_double(diff, 0) )
                return static_cast<int>(cum_index);
            else if ( (!is_equal_double(del, 0)) && is_equal_double(fmod(diff, del), 0) )
                return static_cast<int>(cum_index + diff / del);
            else
                cum_index += lengths[nrun];
        }
    }
    else
    {
        for ( int i = 0; i < nruns; i++ )
            if ( is_equal_double((*this)[i], value) )
                return i;
    }
    return NA_REAL;
}

template<typename RType, int SType>
RType VectorOrDRLE<RType,SType> :: operator[](int i) {
    if ( i < 0 )
        error("subscript out of bounds");
    if ( !isDRLE ) {
        if ( i < nruns )
            return values[i];
        else
            error("subscript out of bounds");
    }
    if ( i >= ref_index )
    {
        while ( ref_run < nruns ) {
            if ( i < ref_index + lengths[ref_run] )
                return values[ref_run] + deltas[ref_run] * (i - ref_index);
            else
            {
                ref_index += lengths[ref_run];
                ref_run++;
            }
        }
    }
    else
    {
        ref_run--;
        ref_index -= lengths[ref_run];
        while ( ref_run >= 0 ) {
            if ( i >= ref_index )
                return values[ref_run] + deltas[ref_run] * (i - ref_index);
            else
            {
                ref_run--;
                ref_index -= lengths[ref_run];
            }
        }
    }
    if ( i >= ref_index )
        error("subscript out of bounds");
    return DataNA<RType>();
}

//// Delayed operations on atoms
//-------------------------------

void Ops :: init_args() {
    for ( int i = 0; i < length(); i++ ) {
        if ( has_lhs(i) ) {
            switch (type(i)) {
                case RAWSXP:
                    _arg[i].r = RAW(lhs(i));
                    break;
                case LGLSXP:
                    _arg[i].i = LOGICAL(lhs(i));
                    break;
                case INTSXP:
                    _arg[i].i = INTEGER(lhs(i));
                    break;
                case REALSXP:
                    _arg[i].d = REAL(lhs(i));
                    break;
                case STRSXP:
                    _arg[i].s = STRING_PTR(lhs(i));
                    break;
                case S4SXP:
                    // assumes 'matter' will be the only S4 class that ends up here
                    _arg[i].m = new Matter(lhs(i));
                    break;
            }
            if ( type(i) != S4SXP )
                _arglengths[i] = static_cast<index_t>(XLENGTH(lhs(i)));
            else
                _arglengths[i] = _arg[i].m->length();
        }
        else if ( has_rhs(i) ) {
            switch (type(i)) {
                case RAWSXP:
                    _arg[i].r = RAW(rhs(i));
                    break;
                case LGLSXP:
                    _arg[i].i = LOGICAL(rhs(i));
                    break;
                case INTSXP:
                    _arg[i].i = INTEGER(rhs(i));
                    break;
                case REALSXP:
                    _arg[i].d = REAL(rhs(i));
                    break;
                case STRSXP:
                    _arg[i].s = STRING_PTR(rhs(i));
                    break;
                case S4SXP:
                    // assumes 'matter' will be the only S4 class that ends up here
                    _arg[i].m = new Matter(rhs(i));
                    break;
            }
            if ( type(i) != S4SXP )
                _arglengths[i] = static_cast<index_t>(XLENGTH(rhs(i)));
            else
                _arglengths[i] = _arg[i].m->length();
        }
    }
}

void Ops :: finalize_args() {
    for ( int i = 0; i < length(); i++ )
        if ( type(i) == S4SXP )
            delete _arg[i].m;
}

template<>
Rbyte * Ops :: arg<Rbyte,RAWSXP>(int i) {
    return _arg[i].r;
}

template<>
int * Ops :: arg<int,LGLSXP>(int i) {
    return _arg[i].i;
}

template<>
int * Ops :: arg<int,INTSXP>(int i) {
    return _arg[i].i;
}

template<>
double * Ops :: arg<double,REALSXP>(int i) {
    return _arg[i].d;
}

template<>
SEXP * Ops :: arg<SEXP,STRSXP>(int i) {
    return _arg[i].s;
}

template<>
Matter * Ops :: arg<Matter,S4SXP>(int i) {
    return _arg[i].m;
}

template<typename T1, typename T2>
void Ops :: add(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip) {
    index_t xlen, ylen = arglength(i);
    if ( ylen == 1 ) {
        for ( index_t k = 0; k < count; k++ ) {
            *x += y[0];
            x += skip;
        }
        return;
    }
    switch(where(i)) {
        case BY_GROUP:
            xlen = atm->max_extent();
            if ( ylen == xlen )
            {
                for ( index_t k = 0; k < count; k++ ) {
                    *x += y[offset + k];
                    x += skip;
                }
            }
            else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
            {
                for ( index_t k = 0; k < count; k++ ) {
                    *x += y[((atm->group() * xlen) + (offset + k))];
                    x += skip;
                }
            }
            else
            {
                for ( index_t k = 0; k < count; k++ ) {
                    *x += y[((atm->group() * xlen) + (offset + k)) % ylen];
                    x += skip;
                }
            }
            break;
        case BY_EACH_GROUP:
            xlen = atm->length();
            if ( xlen == ylen )
            {
                for ( index_t k = 0; k < count; k++ ) {
                    *x += y[atm->group()];
                    x += skip;
                }
            }
            else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
            {
                for ( index_t k = 0; k < count; k++ ) {
                    *x += y[(((offset + k) * xlen + atm->group()))];
                    x += skip;
                }
            }
            else
            {
                for ( index_t k = 0; k < count; k++ ) {
                    *x += y[(((offset + k) * xlen + atm->group())) % ylen];
                    x += skip;
                }
            }
            break;
    }
}

template<typename T1, typename T2>
void Ops :: sub(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip) {
    index_t xlen, ylen = arglength(i);
    if ( has_lhs(i) )
    {
        if ( ylen == 1 ) {
            for ( index_t k = 0; k < count; k++ ) {
                *x = y[0] - (*x);
                x += skip;
            }
            return;
        }
        switch(where(i)) {
            case BY_GROUP:
                xlen = atm->max_extent();
                if ( ylen == xlen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x = y[offset + k] - (*x);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x = y[((atm->group() * xlen) + (offset + k))] - (*x);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x = y[((atm->group() * xlen) + (offset + k)) % ylen] - (*x);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x = y[atm->group()] - (*x);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x = y[(((offset + k) * xlen + atm->group()))] - (*x);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x = y[(((offset + k) * xlen + atm->group())) % ylen] - (*x);
                        x += skip;
                    }
                }
                break;
        }
    }
    else
    {
        if ( ylen == 1 ) {
            for ( index_t k = 0; k < count; k++ ) {
                *x -= y[0];
                x += skip;
            }
            return;
        }
        switch(where(i)) {
            case BY_GROUP:
                xlen = atm->length();
                if ( ylen == xlen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x -= y[offset + k];
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x -= y[((atm->group() * xlen) + (offset + k))];
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x -= y[((atm->group() * xlen) + (offset + k)) % ylen];
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x -= y[atm->group()];
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x -= y[(((offset + k) * xlen + atm->group()))];
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x -= y[(((offset + k) * xlen + atm->group())) % ylen];
                        x += skip;
                    }
                }
                break;
        }
    }
}

template<typename T1, typename T2>
void Ops :: mul(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip) {
    index_t xlen, ylen = arglength(i);
    if ( ylen == 1 ) {
        for ( index_t k = 0; k < count; k++ ) {
            *x *= y[0];
            x += skip;
        }
        return;
    }
    switch(where(i)) {
        case BY_GROUP:
            xlen = atm->max_extent();
            if ( ylen == xlen )
            {
                for ( index_t k = 0; k < count; k++ ) {
                    *x *= y[offset + k];
                    x += skip;
                }
            }
            else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
            {
                for ( index_t k = 0; k < count; k++ ) {
                    *x *= y[((atm->group() * xlen) + (offset + k))];
                    x += skip;
                }
            }
            else
            {
                for ( index_t k = 0; k < count; k++ ) {
                    *x *= y[((atm->group() * xlen) + (offset + k)) % ylen];
                    x += skip;
                }
            }
            break;
        case BY_EACH_GROUP:
            xlen = atm->length();
            if ( xlen == ylen )
            {
                for ( index_t k = 0; k < count; k++ ) {
                    *x *= y[atm->group()];
                    x += skip;
                }
            }
            else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
            {
                for ( index_t k = 0; k < count; k++ ) {
                    *x *= y[(((offset + k) * xlen + atm->group()))];
                    x += skip;
                }
            }
            else
            {
                for ( index_t k = 0; k < count; k++ ) {
                    *x *= y[(((offset + k) * xlen + atm->group())) % ylen];
                    x += skip;
                }
            }
            break;
    }
}

template<typename T1, typename T2>
void Ops :: div(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip) {
    index_t xlen, ylen = arglength(i);
    if ( has_lhs(i) )
    {
        if ( ylen == 1 ) {
            for ( index_t k = 0; k < count; k++ ) {
                *x = y[0] / (*x);
                x += skip;
            }
            return;
        }
        switch(where(i)) {
            case BY_GROUP:
                xlen = atm->max_extent();
                if ( ylen == xlen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x = y[offset + k] / (*x);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x = y[((atm->group() * xlen) + (offset + k))] / (*x);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x = y[((atm->group() * xlen) + (offset + k)) % ylen] / (*x);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x = y[atm->group()] / (*x);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x = y[(((offset + k) * xlen + atm->group()))] / (*x);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x = y[(((offset + k) * xlen + atm->group())) % ylen] / (*x);
                        x += skip;
                    }
                }
                break;
        }
    }
    else
    {
        if ( ylen == 1 ) {
            for ( index_t k = 0; k < count; k++ ) {
                *x /= y[0];
                x += skip;
            }
            return;
        }
        switch(where(i)) {
            case BY_GROUP:
                xlen = atm->length();
                if ( ylen == xlen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x /= y[offset + k];
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x /= y[((atm->group() * xlen) + (offset + k))];
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x /= y[((atm->group() * xlen) + (offset + k)) % ylen];
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x /= y[atm->group()];
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x /= y[(((offset + k) * xlen + atm->group()))];
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        *x /= y[(((offset + k) * xlen + atm->group())) % ylen];
                        x += skip;
                    }
                }
                break;
        }
    }
}

template<typename T>
void Ops :: exp(T * x, int i, Atoms * atm, index_t offset, index_t count, size_t skip) {
    for ( index_t k = 0; k < count; k++ ) {
        *x = coerce_exp<T>(*x);
        x += skip;
    }
}

template<typename T1, typename T2>
void Ops :: exp(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip) {
    index_t xlen, ylen = arglength(i);
    if ( has_lhs(i) )
    {
        if ( ylen == 1 ) {
            for ( index_t k = 0; k < count; k++ ) {
                T1 yt = static_cast<T1>(y[0]);
                *x = coerce_pow<T1>(yt, (*x));
                x += skip;
            }
            return;
        }
        switch(where(i)) {
            case BY_GROUP:
                xlen = atm->max_extent();
                if ( ylen == xlen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        T1 yt = static_cast<T1>(y[offset + k]);
                        *x = coerce_pow<T1>(yt, (*x));
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        T1 yt = static_cast<T1>(y[((atm->group() * xlen) + (offset + k))]);
                        *x = coerce_pow<T1>(yt, (*x));
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        T1 yt = static_cast<T1>(y[((atm->group() * xlen) + (offset + k)) % ylen]);
                        *x = coerce_pow<T1>(yt, (*x));
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        T1 yt = static_cast<T1>(y[atm->group()]);
                        *x = coerce_pow<T1>(yt, (*x));
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        T1 yt = static_cast<T1>(y[(((offset + k) * xlen + atm->group()))]);
                        *x = coerce_pow<T1>(yt, (*x));
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        T1 yt = static_cast<T1>(y[(((offset + k) * xlen + atm->group())) % ylen]);
                        *x = coerce_pow<T1>(yt, (*x));
                        x += skip;
                    }
                }
                break;
        }
    }
    else
    {
        if ( ylen == 1 ) {
            for ( index_t k = 0; k < count; k++ ) {
                T1 yt = static_cast<T1>(y[0]);
                *x = coerce_pow<T1>(*x, yt);
                x += skip;
            }
            return;
        }
        switch(where(i)) {
            case BY_GROUP:
                xlen = atm->max_extent();
                if ( ylen == xlen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        T1 yt = static_cast<T1>(y[offset + k]);
                        *x = coerce_pow<T1>((*x), yt);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        T1 yt = static_cast<T1>(y[((atm->group() * xlen) + (offset + k))]);
                        *x = coerce_pow<T1>((*x), yt);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        T1 yt = static_cast<T1>(y[((atm->group() * xlen) + (offset + k)) % ylen]);
                        *x = coerce_pow<T1>((*x), yt);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        T1 yt = static_cast<T1>(y[atm->group()]);
                        *x = coerce_pow<T1>((*x), yt);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        T1 yt = static_cast<T1>(y[(((offset + k) * xlen + atm->group()))]);
                        *x = coerce_pow<T1>((*x), yt);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        T1 yt = static_cast<T1>(y[(((offset + k) * xlen + atm->group())) % ylen]);
                        *x = coerce_pow<T1>((*x), yt);
                        x += skip;
                    }
                }
                break;
        }
    }
}

template<typename T>
void Ops :: log(T * x, int i, Atoms * atm, index_t offset, index_t count, size_t skip) {
    for ( index_t k = 0; k < count; k++ ) {
        *x = coerce_log<T>(*x);
        x += skip;
    }
}

template<typename T1, typename T2>
void Ops :: log(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip) {
    index_t xlen, ylen = arglength(i);
    if ( has_lhs(i) )
    {
        if ( ylen == 1) {
            for ( index_t k = 0; k < count; k++ ) {
                T1 yt = static_cast<T1>(y[0]);
                *x = coerce_log<T1>(*x) / coerce_log<T1>(yt);
                x += skip;
            }
            return;
        }
        switch(where(i)) {
            case BY_GROUP:
                xlen = atm->max_extent();
                if ( ylen == xlen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        T1 yt = static_cast<T1>(y[offset + k]);
                        *x = coerce_log<T1>(*x) / coerce_log<T1>(yt);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        T1 yt = static_cast<T1>(y[((atm->group() * xlen) + (offset + k))]);
                        *x = coerce_log<T1>(*x) / coerce_log<T1>(yt);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        T1 yt = static_cast<T1>(y[((atm->group() * xlen) + (offset + k)) % ylen]);
                        *x = coerce_log<T1>(*x) / coerce_log<T1>(yt);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        T1 yt = static_cast<T1>(y[atm->group()]);
                        *x = coerce_log<T1>(*x) / coerce_log<T1>(yt);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        T1 yt = static_cast<T1>(y[(((offset + k) * xlen + atm->group()))]);
                        *x = coerce_log<T1>(*x) / coerce_log<T1>(yt);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        T1 yt = static_cast<T1>(y[(((offset + k) * xlen + atm->group())) % ylen]);
                        *x = coerce_log<T1>(*x) / coerce_log<T1>(yt);
                        x += skip;
                    }
                }
                break;
        }
    }
}

template<typename T>
void Ops :: do_ops(T * x, Atoms * atm, index_t offset, index_t count, size_t skip) {
    for ( int i = 0; i < length(); i++ )
    {
        // assumes 'matter' will be the only S4 class that ends up here
        // T * y;
        // if ( type(i) == S4SXP ) {
        //     y = (T *) Calloc(count, T);
        //     arg<Matter,S4SXP>(i)->data().set_group(atm->group());
        //     arg<Matter,S4SXP>(i)->data().read<T>(y, offset, count);
        // }
        switch(op(i))
        {
            case OP_ADD:
                switch(type(i)) {
                    case INTSXP:
                        add<T,int>(x, arg<int,INTSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case REALSXP:
                        add<T,double>(x, arg<double,REALSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    // case S4SXP:
                    //     add<T,T>(x, y, i, atm, offset, count, skip);
                    //     break;
                    default:
                        error("non-numeric argument to binary operator");
                }
                break;
            case OP_SUB:
                switch(type(i)) {
                    case INTSXP:
                        sub<T,int>(x, arg<int,INTSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case REALSXP:
                        sub<T,double>(x, arg<double,REALSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    default:
                        error("non-numeric argument to binary operator");
                }
                break;
            case OP_MUL:
                switch(type(i)) {
                    case INTSXP:
                        mul<T,int>(x, arg<int,INTSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case REALSXP:
                        mul<T,double>(x, arg<double,REALSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    default:
                        error("non-numeric argument to binary operator");
                }
                break;
            case OP_DIV:
                switch(type(i)) {
                    case INTSXP:
                        div<T,int>(x, arg<int,INTSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case REALSXP:
                        div<T,double>(x, arg<double,REALSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    default:
                        error("non-numeric argument to binary operator");
                }
                break;
            case OP_EXP:
                switch(type(i)) {
                    case NILSXP:
                        exp<T>(x, i, atm, offset, count, skip);
                        break;
                    case INTSXP:
                        exp<T,int>(x, arg<int,INTSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case REALSXP:
                        exp<T,double>(x, arg<double,REALSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    default:
                        error("non-numeric argument to mathematical function");
                }
                break;
            case OP_LOG:
                switch(type(i)) {
                    case NILSXP:
                        log<T>(x, i, atm, offset, count, skip);
                        break;
                    case INTSXP:
                        log<T,int>(x, arg<int,INTSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case REALSXP:
                        log<T,double>(x, arg<double,REALSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    default:
                        error("non-numeric argument to mathematical function");
                }
                break;
        }
        // if ( type(i) == S4SXP )
        //     Free(y);
    }
}

//// Vector methods implemented for class Matter
//-----------------------------------------------

template<typename RType, int SType>
SEXP Matter :: readVector() {
    SEXP retVec;
    PROTECT(retVec = allocVector(SType, length()));
    RType * pRetVec = DataPtr<RType,SType>(retVec);
    data().read<RType>(pRetVec, 0, length());
    UNPROTECT(1);
    return retVec;
}

template<typename RType, int SType>
void Matter :: writeVector(SEXP value) {
    RType * pValue = DataPtr<RType,SType>(value);
    data().write<RType>(pValue, 0, length());
}

template<typename RType, int SType>
SEXP Matter :: readVectorElements(SEXP i) {
    SEXP retVec;
    PROTECT(retVec = allocVector(SType, XLENGTH(i)));
    RType * pRetVec = DataPtr<RType,SType>(retVec);
    Rindex_t * pIndex = R_INDEX_PTR(i);
    data().read_indices<RType>(pRetVec, pIndex, XLENGTH(i));
    UNPROTECT(1);
    return retVec;
}

template<typename RType, int SType>
void Matter :: writeVectorElements(SEXP i, SEXP value) {
    RType * pValue = DataPtr<RType,SType>(value);
    Rindex_t * pIndex = R_INDEX_PTR(i);
    data().write_indices(pValue, pIndex, XLENGTH(i));
}

//// Matrix methods implemented for class Matter
//-----------------------------------------------

template<typename RType, int SType>
SEXP Matter :: readMatrix() {
    SEXP retMat;
    int nrows = this->nrows(), ncols = this->ncols();
    PROTECT(retMat = allocMatrix(SType, nrows, ncols));
    RType * pRetMat = DataPtr<RType,SType>(retMat);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int col = 0; col < ncols; col++ ) {
                data().set_group(col);
                data().read<RType>(pRetMat + col * nrows, 0, nrows);
            }
            break;
        case MATTER_MATR:
            for ( int row = 0; row < nrows; row++ ) {
                data().set_group(row);
                data().read<RType>(pRetMat + row, 0, ncols, nrows);
            }
            break;
    }
    UNPROTECT(1);
    return retMat;
}

template<typename RType, int SType>
void Matter :: writeMatrix(SEXP value) {
    int nrows = this->nrows(), ncols = this->ncols();
    RType * pValue = DataPtr<RType,SType>(value);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int col = 0; col < ncols; col++ ) {
                data().set_group(col);
                data().write<RType>(pValue + col * nrows, 0, nrows);
            }
            break;
        case MATTER_MATR:
            for ( int row = 0; row < nrows; row++ ) {
                data().set_group(row);
                data().write<RType>(pValue + row, 0, ncols, nrows);
            }
            break;
    }
}

template<typename RType, int SType>
SEXP Matter :: readMatrixRows(SEXP i) {
    SEXP retMat;
    int nrows = LENGTH(i), ncols = this->ncols();
    PROTECT(retMat = allocMatrix(SType, nrows, ncols));
    RType * pRetMat = DataPtr<RType,SType>(retMat);
    Rindex_t * pRow = R_INDEX_PTR(i);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int col = 0; col < ncols; col++ ) {
                data().set_group(col);
                data().read_indices<RType>(pRetMat + col * nrows, pRow, nrows);
            }
            break;
        case MATTER_MATR:
            for ( int l = 0; l < nrows; l++ ) {
                if ( ISNA(pRow[l]) )
                    fillNA<RType>(pRetMat + l, ncols, nrows);
                else {
                    index_t row = static_cast<index_t>(pRow[l]);
                    data().set_group(row);
                    data().read<RType>(pRetMat + l, 0, ncols, nrows);
                }
            }
            break;
    }
    UNPROTECT(1);
    return retMat;
}

template<typename RType, int SType>
void Matter :: writeMatrixRows(SEXP i, SEXP value) {
    int nrows = LENGTH(i), ncols = this->ncols();
    RType * pValue = DataPtr<RType,SType>(value);
    Rindex_t * pRow = R_INDEX_PTR(i);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int col = 0; col < ncols; col++ ) {
                data().set_group(col);
                data().write_indices(pValue + col * nrows, pRow, nrows);
            }
            break;
        case MATTER_MATR:
            for ( int l = 0; l < nrows; l++ ) {
                if ( ISNA(pRow[l]) )
                    continue;
                index_t row = static_cast<index_t>(pRow[l]);
                data().set_group(row);
                data().write<RType>(pValue + l, 0, ncols, nrows);
            }
            break;
    }
}

template<typename RType, int SType>
SEXP Matter :: readMatrixCols(SEXP j) {
    SEXP retMat;
    int nrows = this->nrows(), ncols = LENGTH(j);
    PROTECT(retMat = allocMatrix(SType, nrows, ncols));
    RType * pRetMat = DataPtr<RType,SType>(retMat);
    Rindex_t * pCol = R_INDEX_PTR(j);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int l = 0; l < ncols; l++ ) {
                if ( ISNA(pCol[l]) )
                    fillNA<RType>(pRetMat + l * nrows, nrows);
                else {
                    index_t col = static_cast<index_t>(pCol[l]);
                    data().set_group(col);
                    data().read<RType>(pRetMat + l * nrows, 0, nrows);
                }
            }
            break;
        case MATTER_MATR:
            for ( int row = 0; row < nrows; row++ ) {
                data().set_group(row);
                data().read_indices<RType>(pRetMat + row, pCol, ncols, nrows);
            }
            break;
    }
    UNPROTECT(1);
    return retMat;
}

template<typename RType, int SType>
void Matter :: writeMatrixCols(SEXP j, SEXP value) {
    int nrows = this->nrows(), ncols = LENGTH(j);
    RType * pValue = DataPtr<RType,SType>(value);
    Rindex_t * pCol = R_INDEX_PTR(j);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int l = 0; l < ncols; l++ ) {
                if ( ISNA(pCol[l]) )
                    continue;
                index_t col = static_cast<index_t>(pCol[l]);
                data().set_group(col);
                data().write<RType>(pValue + l * nrows, 0, nrows);
            }
            break;
        case MATTER_MATR:
            for ( int row = 0; row < nrows; row++ ) {
                data().set_group(row);
                data().write_indices(pValue + row, pCol, ncols, nrows);
            }
            break;
    }
}

template<typename RType, int SType>
SEXP Matter :: readMatrixElements(SEXP i, SEXP j) {
    SEXP retMat;
    int nrows = LENGTH(i), ncols = LENGTH(j);
    PROTECT(retMat = allocMatrix(SType, nrows, ncols));
    RType * pRetMat = DataPtr<RType,SType>(retMat);
    Rindex_t * pRow = R_INDEX_PTR(i);
    Rindex_t * pCol = R_INDEX_PTR(j);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int l = 0; l < ncols; l++ ) {
                if ( ISNA(pCol[l]) )
                    fillNA<RType>(pRetMat + l * nrows, nrows);
                else {
                    index_t col = static_cast<index_t>(pCol[l]);
                    data().set_group(col);
                    data().read_indices<RType>(pRetMat + l * nrows, pRow, nrows);
                }
            }
            break;
        case MATTER_MATR:
            for ( int l = 0; l < nrows; l++ ) {
                if ( ISNA(pRow[l]) )
                    fillNA<RType>(pRetMat + l, ncols, nrows);
                else {
                    index_t row = static_cast<index_t>(pRow[l]);
                    data().set_group(row);
                    data().read_indices<RType>(pRetMat + l, pCol, ncols, nrows);
                }
            }
            break;
    }
    UNPROTECT(1);
    return retMat;
}

template<typename RType, int SType>
void Matter :: writeMatrixElements(SEXP i, SEXP j, SEXP value) {
    int nrows = LENGTH(i), ncols = LENGTH(j);
    RType * pValue = DataPtr<RType,SType>(value);
    Rindex_t * pRow = R_INDEX_PTR(i);
    Rindex_t * pCol = R_INDEX_PTR(j);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int l = 0; l < ncols; l++ ) {
                if ( ISNA(pCol[l]) )
                    continue;
                index_t col = static_cast<index_t>(pCol[l]);
                data().set_group(col);
                data().write_indices(pValue + l * nrows, pRow, nrows);
            }
            break;
        case MATTER_MATR:
            for ( int l = 0; l < nrows; l++ ) {
                if ( ISNA(pRow[l]) )
                    continue;
                index_t row = static_cast<index_t>(pRow[l]);
                data().set_group(row);
                data().write_indices(pValue + l, pCol, ncols, nrows);
            }
            break;
    }
}

//// Statistical functions applied on MatterIterator
//--------------------------------------------------

double sum(MatterIterator<double> & x, bool na_rm) {
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

double mean(MatterIterator<double> & x, bool na_rm) {
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

double var(MatterIterator<double> & x, bool na_rm) {
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
    MatterIterator<double> x(*this);
    *REAL(retVal) = ::sum(x, na_rm);
    UNPROTECT(1);
    return retVal;
}

SEXP Matter :: mean(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = NEW_NUMERIC(1));
    MatterIterator<double> x(*this);
    *REAL(retVal) = ::mean(x, na_rm);
    UNPROTECT(1);
    return retVal;
}

SEXP Matter :: var(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = NEW_NUMERIC(1));
    MatterIterator<double> x(*this);
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
                MatterIterator<double> x(*this, j);
                pRetVal[j] = ::sum(x, na_rm);
            }
            break;
        case MATTER_MATR:
            for ( int j = 0; j < ncols(); j++ )
                pRetVal[j] = 0;
            for ( int i = 0; i < nrows(); i++ ) {
                MatterIterator<double> x(*this, i);
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
                MatterIterator<double> x(*this, j);
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
                    MatterIterator<double> x(*this, i);
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
                MatterIterator<double> x(*this, j);
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
                    MatterIterator<double> x(*this, i);
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
                MatterIterator<double> x(*this, j);
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
                MatterIterator<double> x(*this, i);
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
                    MatterIterator<double> x(*this, j);
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
                MatterIterator<double> x(*this, i);
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
                    MatterIterator<double> x(*this, j);
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
                MatterIterator<double> x(*this, i);
                pRetVal[i] = ::var(x, na_rm);
            }
            break;
    }
    UNPROTECT(1);
    return retVal;
}

template<typename RType, int SType>
SEXP Matter :: rmult(SEXP y) {
    SEXP retMat;
    PROTECT(retMat = allocMatrix(REALSXP, nrows(), ::ncols(y)));
    double * pRetMat = REAL(retMat);
    RType * pY = DataPtr<RType,SType>(y);
    int nrRetMat = ::nrows(retMat);
    int ncRetMat = ::ncols(retMat);
    int nrY = ::nrows(y);
    R_xlen_t len = XLENGTH(retMat);
    for ( int k = 0; k < len; k++ )
        pRetMat[k] = 0;
    switch(S4class()) {
        case MATTER_MATC:
            for ( int j = 0; j < ncols(); j++ ) {
                MatterIterator<double> x(*this, j);
                int i = 0;
                while ( x ) {
                    for ( int jj = 0; jj < ncRetMat; jj++ ) {
                        double val = (*x) * coerce_cast<RType,double>(pY[j + jj * nrY]);
                        pRetMat[i + jj * nrRetMat] += val;
                    }
                    i++;
                    ++x;
                }
            }
            break;
        case MATTER_MATR:
            for ( int i = 0; i < nrows(); i++ ) {
                MatterIterator<double> x(*this, i);
                int j = 0;
                while ( x ) {
                    for ( int jj = 0; jj < ncRetMat; jj++ ) {
                        double val = (*x) * coerce_cast<RType,double>(pY[j + jj * nrY]);
                        pRetMat[i + jj * nrRetMat] += val;
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

template<typename RType, int SType>
SEXP Matter :: lmult(SEXP x) {
    SEXP retMat;
    PROTECT(retMat = allocMatrix(REALSXP, ::nrows(x), ncols()));
    double * pRetMat = REAL(retMat);
    RType * pX = DataPtr<RType,SType>(x);
    int nrRetMat = ::nrows(retMat);
    int nrX = ::nrows(x);
    int len = LENGTH(retMat);
    for ( int k = 0; k < len; k++ )
        pRetMat[k] = 0;
    switch(S4class()) {
        case MATTER_MATC:
            for ( int j = 0; j < ncols(); j++ ) {
                MatterIterator<double> y(*this, j);
                int i = 0;
                while ( y ) {
                    for ( int ii = 0; ii < nrRetMat; ii++ ) {
                        double val = coerce_cast<RType,double>(pX[ii + i * nrX]) * (*y);
                        pRetMat[ii + j * nrRetMat] += val;
                    }
                    i++;
                    ++y;
                }
            }
            break;
        case MATTER_MATR:
            for ( int i = 0; i < nrows(); i++ ) {
                MatterIterator<double> y(*this, i);
                int j = 0;
                while ( y ) {
                    for ( int ii = 0; ii < nrRetMat; ii++ ) {
                        double val = coerce_cast<RType,double>(pX[ii + i * nrX]) * (*y);
                        pRetMat[ii + j * nrRetMat] += val;
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

    SEXP createAtoms(
        SEXP group_id,
        SEXP source_id,
        SEXP datamode,
        SEXP offset,
        SEXP extent)
    {

        SEXP natoms, ngroups, index_offset, index_extent;
        double * pIndexOffset, * pIndexExtent;
        VectorOrDRLE<int,INTSXP> vGroupID(group_id);
        VectorOrDRLE<double,REALSXP> vExtent(extent);
        int n = vGroupID.length();
        PROTECT(natoms = NEW_INTEGER(1));
        PROTECT(ngroups = NEW_INTEGER(1));
        PROTECT(index_offset = NEW_NUMERIC(n));
        PROTECT(index_extent = NEW_NUMERIC(n));
        pIndexOffset = REAL(index_offset);
        pIndexExtent = REAL(index_extent);
        int ext, gid, g_cur = 0, g_n = 0, cum_sum = 0;
        for ( int i = 0; i < n; i++ ) {
            gid = vGroupID[i];
            ext = vExtent[i];
            if ( gid != g_cur ) {
                g_cur = gid;
                cum_sum = 0;
                g_n++;
            }
            pIndexOffset[i] = cum_sum;
            cum_sum += ext;
            pIndexExtent[i] = cum_sum;
        }
        INTEGER(natoms)[0] = n;
        INTEGER(ngroups)[0] = g_n;
        SEXP classDef, retObj;
        PROTECT(classDef = MAKE_CLASS("atoms"));
        PROTECT(retObj = NEW_OBJECT(classDef));
        SET_SLOT(retObj, install("natoms"), natoms);
        SET_SLOT(retObj, install("ngroups"), ngroups);
        SET_SLOT(retObj, install("group_id"), group_id);
        SET_SLOT(retObj, install("source_id"), source_id);
        SET_SLOT(retObj, install("datamode"), datamode);
        SET_SLOT(retObj, install("offset"), offset);
        SET_SLOT(retObj, install("extent"), extent);
        SET_SLOT(retObj, install("index_offset"), index_offset);
        SET_SLOT(retObj, install("index_extent"), index_extent);
        UNPROTECT(6);
        return retObj;
    }

    SEXP getVector(SEXP x) {
        Matter mVec(x);
        switch(mVec.datamode()) {
            case R_RAW:
                return mVec.readVector<Rbyte,RAWSXP>();
            case R_LOGICAL:
                return mVec.readVector<int,LGLSXP>();
            case R_INTEGER:
                return mVec.readVector<int,INTSXP>();
            case R_NUMERIC:
                return mVec.readVector<double,REALSXP>();
            default:
                return R_NilValue;
        }
    }

    void setVector(SEXP x, SEXP value) {
        Matter mVec(x);
        switch(TYPEOF(value)) {
            case RAWSXP:
                mVec.writeVector<Rbyte,RAWSXP>(value);
                break;
            case LGLSXP:
                mVec.writeVector<int,LGLSXP>(value);
                break;
            case INTSXP:
                mVec.writeVector<int,INTSXP>(value);
                break;
            case REALSXP:
                mVec.writeVector<double,REALSXP>(value);
                break;
        }
    }

    SEXP getVectorElements(SEXP x, SEXP i) {
        Matter mVec(x);
        switch(mVec.datamode()) {
            case R_RAW:
                return mVec.readVectorElements<Rbyte,RAWSXP>(i);
            case R_LOGICAL:
                return mVec.readVectorElements<int,LGLSXP>(i);
            case R_INTEGER:
                return mVec.readVectorElements<int,INTSXP>(i);
            case R_NUMERIC:
                return mVec.readVectorElements<double,REALSXP>(i);
            default:
                return R_NilValue;
        }
    }

    void setVectorElements(SEXP x, SEXP i, SEXP value) {
        Matter mVec(x);
        switch(TYPEOF(value)) {
            case RAWSXP:
                mVec.writeVectorElements<Rbyte,RAWSXP>(i, value);
                break;
            case LGLSXP:
                mVec.writeVectorElements<int,LGLSXP>(i, value);
                break;
            case INTSXP:
                mVec.writeVectorElements<int,INTSXP>(i, value);
                break;
            case REALSXP:
                mVec.writeVectorElements<double,REALSXP>(i, value);
                break;
        }
    }

    SEXP getMatrix(SEXP x) {
        Matter mMat(x);
        switch(mMat.datamode()) {
            case R_RAW:
                return mMat.readMatrix<Rbyte,RAWSXP>();
            case R_LOGICAL:
                return mMat.readMatrix<int,LGLSXP>();
            case R_INTEGER:
                return mMat.readMatrix<int,INTSXP>();
            case R_NUMERIC:
                return mMat.readMatrix<double,REALSXP>();
            default:
                return R_NilValue;
        }
    }

    void setMatrix(SEXP x, SEXP value) {
        Matter mMat(x);
        switch(TYPEOF(value)) {
            case RAWSXP:
                mMat.writeMatrix<Rbyte,RAWSXP>(value);
                break;
            case LGLSXP:
                mMat.writeMatrix<int,LGLSXP>(value);
                break;
            case INTSXP:
                mMat.writeMatrix<int,INTSXP>(value);
                break;
            case REALSXP:
                mMat.writeMatrix<double,REALSXP>(value);
                break;
        }
    }

    SEXP getMatrixRows(SEXP x, SEXP i) {
        Matter mMat(x);
        switch(mMat.datamode()) {
            case R_RAW:
                return mMat.readMatrixRows<Rbyte,RAWSXP>(i);
            case R_LOGICAL:
                return mMat.readMatrixRows<int,LGLSXP>(i);
            case R_INTEGER:
                return mMat.readMatrixRows<int,INTSXP>(i);
            case R_NUMERIC:
                return mMat.readMatrixRows<double,REALSXP>(i);
            default:
                return R_NilValue;
        }
    }

    void setMatrixRows(SEXP x, SEXP i, SEXP value) {
        Matter mMat(x);
        switch(TYPEOF(value)) {
            case RAWSXP:
                mMat.writeMatrixRows<Rbyte,RAWSXP>(i, value);
                break;
            case LGLSXP:
                mMat.writeMatrixRows<int,LGLSXP>(i, value);
                break;
            case INTSXP:
                mMat.writeMatrixRows<int,INTSXP>(i, value);
                break;
            case REALSXP:
                mMat.writeMatrixRows<double,REALSXP>(i, value);
                break;
        }
    }

    SEXP getMatrixCols(SEXP x, SEXP j) {
        Matter mMat(x);
        switch(mMat.datamode()) {
            case R_RAW:
                return mMat.readMatrixCols<Rbyte,RAWSXP>(j);
            case R_LOGICAL:
                return mMat.readMatrixCols<int,LGLSXP>(j);
            case R_INTEGER:
                return mMat.readMatrixCols<int,INTSXP>(j);
            case R_NUMERIC:
                return mMat.readMatrixCols<double,REALSXP>(j);
            default:
                return R_NilValue;
        }
    }

    void setMatrixCols(SEXP x, SEXP j, SEXP value) {
        Matter mMat(x);
        switch(TYPEOF(value)) {
            case RAWSXP:
                mMat.writeMatrixCols<Rbyte,RAWSXP>(j, value);
                break;
            case LGLSXP:
                mMat.writeMatrixCols<int,LGLSXP>(j, value);
                break;
            case INTSXP:
                mMat.writeMatrixCols<int,INTSXP>(j, value);
                break;
            case REALSXP:
                mMat.writeMatrixCols<double,REALSXP>(j, value);
                break;
        }
    }

    SEXP getMatrixElements(SEXP x, SEXP i, SEXP j) {
        Matter mMat(x);
        switch(mMat.datamode()) {
            case R_RAW:
                return mMat.readMatrixElements<Rbyte,RAWSXP>(i, j);
            case R_LOGICAL:
                return mMat.readMatrixElements<int,LGLSXP>(i, j);
            case R_INTEGER:
                return mMat.readMatrixElements<int,INTSXP>(i, j);
            case R_NUMERIC:
                return mMat.readMatrixElements<double,REALSXP>(i, j);
            default:
                return R_NilValue;
        }
    }

    void setMatrixElements(SEXP x, SEXP i, SEXP j, SEXP value) {
        Matter mMat(x);
        switch(TYPEOF(value)) {
            case RAWSXP:
                mMat.writeMatrixElements<Rbyte,RAWSXP>(i, j, value);
                break;
            case LGLSXP:
                mMat.writeMatrixElements<int,LGLSXP>(i, j, value);
                break;
            case INTSXP:
                mMat.writeMatrixElements<int,INTSXP>(i, j, value);
                break;
            case REALSXP:
                mMat.writeMatrixElements<double,REALSXP>(i, j, value);
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
            return mMat.rmult<int,INTSXP>(y);
        else if ( TYPEOF(y) == REALSXP )
            return mMat.rmult<double,REALSXP>(y);
        else
            return R_NilValue;
    }

    SEXP leftMatrixMult(SEXP x, SEXP y) {
        Matter mMat(y);
        if ( TYPEOF(x) == INTSXP )
            return mMat.lmult<int,INTSXP>(x);
        else if ( TYPEOF(x) == REALSXP )
            return mMat.lmult<double,REALSXP>(x);
        else
            return R_NilValue;
    }

    SEXP countRuns(SEXP x) {
        SEXP ret;
        PROTECT(ret = NEW_INTEGER(1));
        if ( TYPEOF(x) == INTSXP )
        {
            INTEGER(ret)[0] = count_runs<int>(INTEGER(x), LENGTH(x));
        }
        else if ( TYPEOF(x) == REALSXP )
        {
            INTEGER(ret)[0] = count_runs<double>(REAL(x), LENGTH(x));
        }
        UNPROTECT(1);
        return ret;
    }

    SEXP createDRLE(SEXP x, SEXP nruns) {
        if ( TYPEOF(x) == INTSXP )
        {
            return makeDRLE<int>(x, nruns);
        }
        else if ( TYPEOF(x) == REALSXP )
        {
            return makeDRLE<double>(x, nruns);;
        }
        return R_NilValue;
    }

    SEXP getDRLE(SEXP x) {
        SEXP values = GET_SLOT(x, install("values"));
        if ( TYPEOF(values) == INTSXP )
        {
            VectorOrDRLE<int,INTSXP> dVec(x);
            return dVec.readVector();
        }
        else if ( TYPEOF(values) == REALSXP )
        {
            VectorOrDRLE<double,REALSXP> dVec(x);
            return dVec.readVector();
        }
        return R_NilValue;
    }

    SEXP getDRLEElements(SEXP x, SEXP i) {
        SEXP values = GET_SLOT(x, install("values"));
        if ( TYPEOF(values) == INTSXP )
        {
            VectorOrDRLE<int,INTSXP> dVec(x);
            return dVec.readVectorElements(i);
        }
        else if ( TYPEOF(values) == REALSXP )
        {
            VectorOrDRLE<double,REALSXP> dVec(x);
            return dVec.readVectorElements(i);
        }
        return R_NilValue;
    }
}
