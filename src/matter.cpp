
#include "matter.h"

//// Low-level utility functions
//-------------------------------

MATTER_OPTIONS matter_options;

void set_matter_options() {
    SEXP e, result;
    PROTECT(e = Rf_lang2(Rf_install("getOption"), Rf_mkString("matter.cast.warning")));
    PROTECT(result = Rf_eval(e, R_GlobalEnv));
    matter_options.cast_warning = Rf_asLogical(result);
    UNPROTECT(2);
}

SEXP raw_to_char(Rbyte * x, int length) {
    int i, j, nchar;
    for ( i = 0, j = -1; i < length; i++ )
        if ( x[i] )
            j = i;
    nchar = j + 1;
    return Rf_mkCharLenCE((const char *) x, nchar, CE_NATIVE);
}

SEXP raw_to_char(SEXP x) {
    return raw_to_char(RAW(x), XLENGTH(x));
}

int coerce_logical(Rbyte x) {
    return (x == 0 ? 0 : 1);
}

int coerce_logical(int x) {
    if ( x == NA_INTEGER )
        return NA_LOGICAL;
    else
        return (x == 0 ? 0 : 1);
}

int coerce_logical(double x) {
    if ( ISNA(x) || ISNAN(x) )
        return NA_LOGICAL;
    else
        return (x == 0 ? 0 : 1);
}

template<typename T1, typename T2, typename T3>
T3 coerce_mod(T1 x, T2 y) {
    double numer = static_cast<double>(x);
    double denom = static_cast<double>(y);
    return static_cast<T3>(fmod(numer, denom));
}

template<typename T1, typename T2, typename T3>
T3 coerce_idiv(T1 x, T2 y) {
    double numer = static_cast<double>(x);
    double denom = static_cast<double>(y);
    return static_cast<T3>(floor(numer / denom));
}

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
int32_t coerce_cast<int32_t,int32_t>(int32_t x) {
    return x;
}

template<>
int32_t coerce_cast<double,int32_t>(double x) {
    if ( x < R_INT_MIN || x > R_INT_MAX || !R_FINITE(x) )
    {
        if ( !ISNA(x) )
            Rf_warning("value is out of range for type 'int', element will be set to NA");
        return NA_INTEGER;
    }
    if ( matter_options.cast_warning )
        Rf_warning("casting from 'double' to 'int', precision may be lost");
    return static_cast<int32_t>(x);
}

// (int,double) ----> double

template<>
double coerce_cast<int32_t,double>(int32_t x) {
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
char coerce_cast<int32_t,char>(int32_t x) {
    if ( x < R_CHAR_MIN || x > R_CHAR_MAX || x == NA_INTEGER )
    {
        if ( x != NA_INTEGER )
            Rf_warning("value is out of range for type 'char', element will be set to NA");
        return NA_CHAR;
    }
    return static_cast<char>(x);
}

template<>
char coerce_cast<double,char>(double x) {
    if ( x < R_CHAR_MIN || x > R_CHAR_MAX || !R_FINITE(x) )
    {
        if ( !ISNA(x) )
            Rf_warning("value is out of range for type 'char', element will be set to NA");
        return NA_CHAR;
    }
    if ( matter_options.cast_warning )
        Rf_warning("casting from 'double' to 'char', precision may be lost");
    return static_cast<char>(x);
}

// (int,double) ----> uchar

template<>
unsigned char coerce_cast<int32_t,unsigned char>(int32_t x) {
    if ( x < 0 || x > R_UCHAR_MAX || x == NA_INTEGER )
    {
        if ( x == NA_INTEGER )
            Rf_warning("NAs not supported for type 'uchar', element will be set to 0");
        else
            Rf_warning("value is out of range for type 'uchar', element will be set to 0");
        return 0;
    }
    return static_cast<unsigned char>(x);
}

template<>
unsigned char coerce_cast<double,unsigned char>(double x) {
    if ( x < 0 || x > R_UCHAR_MAX || !R_FINITE(x) )
    {
        if ( ISNA(x) )
            Rf_warning("NAs not supported for type 'uchar', element will be set to 0");
        else
            Rf_warning("value is out of range for type 'uchar', element will be set to 0");
        return 0;
    }
    if ( matter_options.cast_warning )
        Rf_warning("casting from 'double' to 'uchar', precision may be lost");
    return static_cast<unsigned char>(x);
}

// (Rbyte,int,double) ----> short

template<>
int16_t coerce_cast<Rbyte,int16_t>(Rbyte x) {
    return static_cast<int16_t>(x);
}

template<>
int16_t coerce_cast<int32_t,int16_t>(int32_t x) {
    if ( x < R_SHORT_MIN || x > R_SHORT_MAX || x == NA_INTEGER )
    {
        if ( x != NA_INTEGER )
            Rf_warning("value is out of range for type 'short', element will be set to NA");
        return NA_SHORT;
    }
    return static_cast<int16_t>(x);
}

template<>
int16_t coerce_cast<double,int16_t>(double x) {
    if ( x < R_SHORT_MIN || x > R_SHORT_MAX || !R_FINITE(x) )
    {
        if ( !ISNA(x) )
            Rf_warning("value is out of range for type 'short', element will be set to NA");
        return NA_SHORT;
    }
    if ( matter_options.cast_warning )
        Rf_warning("casting from 'double' to 'short', precision may be lost");
    return static_cast<int16_t>(x);
}


// (Rbyte,int,double) ----> ushort

template<>
uint16_t coerce_cast<Rbyte,uint16_t>(Rbyte x) {
    return static_cast<uint16_t>(x);
}

template<>
uint16_t coerce_cast<int32_t,uint16_t>(int32_t x) {
    if ( x < 0 || x > R_USHORT_MAX || x == NA_INTEGER )
    {
        if ( x == NA_INTEGER )
            Rf_warning("NAs not supported for type 'ushort', element will be set to 0");
        else
            Rf_warning("value is out of range for type 'ushort', element will be set to 0");
        return 0;
    }
    return static_cast<uint16_t>(x);
}

template<>
uint16_t coerce_cast<double,uint16_t>(double x) {
    if ( x < 0 || x > R_USHORT_MAX || !R_FINITE(x) )
    {
        if ( ISNA(x) )
            Rf_warning("NAs not supported for type 'ushort', element will be set to 0");
        else
            Rf_warning("value is out of range for type 'ushort', element will be set to 0");
        return 0;
    }
    if ( matter_options.cast_warning )
        Rf_warning("casting from 'double' to 'ushort', precision may be lost");
    return static_cast<uint16_t>(x);
}


// (Rbyte,int,double) ----> uint

template<>
uint32_t coerce_cast<Rbyte,uint32_t>(Rbyte x) {
    return static_cast<uint32_t>(x);
}

template<>
uint32_t coerce_cast<int32_t,uint32_t>(int32_t x) {
    if ( x < 0 || x == NA_INTEGER )
    {
        if ( x == NA_INTEGER )
            Rf_warning("NAs not supported for type 'uint', element will be set to 0");
        else
            Rf_warning("value is out of range for type 'uint', element will be set to 0");
        return 0;
    }
    return static_cast<uint32_t>(x);
}

template<>
uint32_t coerce_cast<double,uint32_t>(double x) {
    if ( x < 0 || x > R_UINT_MAX || !R_FINITE(x) )
    {
        if ( ISNA(x) )
            Rf_warning("NAs not supported for type 'uint', element will be set to 0");
        else
            Rf_warning("value is out of range for type 'uint', element will be set to 0");
        return 0;
    }
    if ( matter_options.cast_warning )
        Rf_warning("casting from 'double' to 'uint', precision may be lost");
    return static_cast<uint32_t>(x);
}

// (Rbyte,int,double) ----> long

template<>
int64_t coerce_cast<Rbyte,int64_t>(Rbyte x) {
    return static_cast<int64_t>(x);
}

template<>
int64_t coerce_cast<int32_t,int64_t>(int32_t x) {
    if ( x == NA_INTEGER )
        return NA_LONG;
    else
        return static_cast<int64_t>(x);
}

template<>
int64_t coerce_cast<double,int64_t>(double x) {
    if ( !R_FINITE(x) )
    {
        if ( ISNA(x) )
            Rf_warning("value is out of range for type 'long', element will be set to NA");
        return NA_LONG;
    }
    if ( matter_options.cast_warning )
        Rf_warning("casting from 'double' to 'long', precision may be lost");
    return static_cast<int64_t>(x);
}

// (Rbyte,int, double) ----> ulong

template<>
uint64_t coerce_cast<Rbyte,uint64_t>(Rbyte x) {
    return static_cast<uint64_t>(x);
}

template<>
uint64_t coerce_cast<int32_t,uint64_t>(int32_t x) {
    if ( x < 0 || x == NA_INTEGER )
    {
        if ( x == NA_INTEGER )
            Rf_warning("NAs not supported for type 'ulong', element will be set to 0");
        else
            Rf_warning("value is out of range for type 'ulong', element will be set to 0");
        return 0;
    }
    else
        return static_cast<uint64_t>(x);
}

template<>
uint64_t coerce_cast<double,uint64_t>(double x) {
    if ( x < 0 || !R_FINITE(x) )
    {
        if ( ISNA(x) )
            Rf_warning("NAs not supported for type 'ulong', element will be set to 0");
        else
            Rf_warning("value is out of range for type 'ulong', element will be set to 0");
        return 0;
    }
    if ( matter_options.cast_warning )
        Rf_warning("casting from 'double' to 'ulong', precision may be lost");
    return static_cast<uint64_t>(x);
}

// (Rbyte,int,double) ----> float

template<>
float coerce_cast<Rbyte,float>(Rbyte x) {
    return static_cast<float>(x);
}

template<>
float coerce_cast<int32_t,float>(int32_t x) {
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
            Rf_warning("value is out of range for type 'float' and will be set to NA");
        return static_cast<float>(NA_REAL);
    }
    if ( matter_options.cast_warning )
        Rf_warning("casting from 'double' to 'float', precision may be lost");
    return static_cast<float>(x);
}

//// C -----> R

// (char,short,ushort,uint,long,ulong,float) ----> Rbyte

template<>
Rbyte coerce_cast<char,Rbyte>(char x) {
    if ( x < 0 || x == NA_CHAR )
    {
        if ( x == NA_CHAR )
            Rf_warning("value is out of range for type 'uchar', element will be set to 0");
        else
            Rf_warning("value is out of range for type 'uchar', element will be set to 0");
        return 0;
    }
    else
        return static_cast<Rbyte>(x);
}

template<>
Rbyte coerce_cast<int16_t,Rbyte>(int16_t x) {
    if ( x < 0 || x > R_UCHAR_MAX || x == NA_SHORT )
    {
        if ( x == NA_CHAR )
            Rf_warning("value is out of range for type 'uchar', element will be set to 0");
        else
            Rf_warning("value is out of range for type 'uchar', element will be set to 0");
        return 0;
    }
    else
        return static_cast<Rbyte>(x);
}

template<>
Rbyte coerce_cast<uint16_t,Rbyte>(uint16_t x) {
    if ( x > R_UCHAR_MAX )
    {
        Rf_warning("value is out of range for type 'uchar', element will be set to 0");
        return 0;   
    }
    return static_cast<Rbyte>(x);
}

template<>
Rbyte coerce_cast<uint32_t,Rbyte>(uint32_t x) {
    if ( x > R_UCHAR_MAX )
    {
        Rf_warning("value is out of range for type 'uchar', element will be set to 0");
        return 0;
    }
    return static_cast<Rbyte>(x);
}

template<>
Rbyte coerce_cast<int64_t,Rbyte>(int64_t x) {
    if ( x < 0 || x > R_UCHAR_MAX || x == NA_LONG )
    {
        if ( x == NA_LONG )
            Rf_warning("value is out of range for type 'uchar', element will be set to 0");
        else
            Rf_warning("value is out of range for type 'uchar', element will be set to 0");
        return 0;
    }
    else
        return static_cast<Rbyte>(x);
}

template<>
Rbyte coerce_cast<uint64_t,Rbyte>(uint64_t x) {
    if ( x > R_UCHAR_MAX )
    {
        Rf_warning("value is out of range for type 'uchar', element will be set to 0");
        return 0;
    }
    return static_cast<Rbyte>(x);
}

template<>
Rbyte coerce_cast<float,Rbyte>(float x) {
    if ( x < 0 || x > R_UCHAR_MAX || std::isnan(x) )
    {
        Rf_warning("value is out of range for type 'uchar', element will be set to 0");
        return 0;
    }
    else
    {
        if ( matter_options.cast_warning )
            Rf_warning("casting from 'float' to 'uchar', precision may be lost");
        return static_cast<Rbyte>(x);
    }
}

// (char,uchar,short,ushort,uint,long,ulong,float) ----> int

template<>
int32_t coerce_cast<char,int32_t>(char x) {
    if ( x == NA_CHAR )
        return NA_INTEGER;
    else
        return static_cast<int32_t>(x);
}

template<>
int32_t coerce_cast<unsigned char,int32_t>(unsigned char x) {
    return static_cast<int32_t>(x);
}

template<>
int32_t coerce_cast<int16_t,int32_t>(short x) {
    if ( x == NA_SHORT )
        return NA_INTEGER;
    else
        return static_cast<int32_t>(x);
}

template<>
int32_t coerce_cast<uint16_t,int32_t>(uint16_t x) {
    return static_cast<int32_t>(x);
}

template<>
int32_t coerce_cast<uint32_t,int32_t>(uint32_t x) {
    if ( x > R_INT_MAX )
    {
        Rf_warning("value is out of range for type 'int', element will be set to NA");
        return NA_INTEGER;
    }
    return static_cast<int32_t>(x);
}

template<>
int32_t coerce_cast<int64_t,int32_t>(int64_t x) {
    if ( x < R_INT_MIN || x > R_INT_MAX || x == NA_LONG )
    {
        if ( x != NA_LONG )
            Rf_warning("value is out of range for type 'int', element will be set to NA");
        return NA_INTEGER;
    }
    if ( x == NA_LONG )
        return NA_INTEGER;
    else
        return static_cast<int>(x);
}

template<>
int32_t coerce_cast<uint64_t,int32_t>(uint64_t x) {
    if ( x > R_INT_MAX )
    {
        Rf_warning("value is out of range for type 'int', element will be set to NA");
        return NA_INTEGER;
    }
    return static_cast<int32_t>(x);
}

template<>
int32_t coerce_cast<float,int32_t>(float x) {
    if ( matter_options.cast_warning )
        Rf_warning("casting from 'float' to 'int', precision may be lost");
    if ( std::isnan(x) )
        return NA_INTEGER;
    else
        return static_cast<int32_t>(x);
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
double coerce_cast<int16_t,double>(int16_t x) {
    if ( x == NA_SHORT )
        return NA_REAL;
    else
        return static_cast<double>(x);
}

template<>
double coerce_cast<uint16_t,double>(uint16_t x) {
    return static_cast<double>(x);
}

template<>
double coerce_cast<uint32_t,double>(uint32_t x) {
    return static_cast<double>(x);
}

template<>
double coerce_cast<int64_t,double>(int64_t x) {
    if ( x == NA_LONG )
        return NA_REAL;
    else
        return static_cast<double>(x);
}

template<>
double coerce_cast<uint64_t,double>(uint64_t x) {
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
int count_runs<int>(int * values, int n, bool delta) {
    int delta1, delta2, length1, length2, i = 0, nruns = 0;
    while ( i < n ) {
        if ( delta )
            delta1 = run_delta<int>(values, i, n);
        else
            delta1 = 0;
        length1 = run_length<int>(values, i, n, delta1);
        if ( delta && length1 == 2 ) {
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
int count_runs<double>(double * values, int n, bool delta) {
    double delta1, delta2;
    int length1, length2, i = 0, nruns = 0;
    while ( i < n ) {
        if ( delta )
            delta1 = run_delta<double>(values, i, n);
        else
            delta1 = 0;
        length1 = run_length<double>(values, i, n, delta1);
        if ( delta && length1 == 2 ) {
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
SEXP makeDRLE<int>(SEXP x, SEXP nruns, bool delta) {
    SEXP retVals, retLengths, retDeltas;
    int * pX, * pRetVals, * pRetLengths, * pRetDeltas;
    PROTECT(retVals = Rf_allocVector(INTSXP, Rf_asInteger(nruns)));
    PROTECT(retLengths = Rf_allocVector(INTSXP, Rf_asInteger(nruns)));
    PROTECT(retDeltas = Rf_allocVector(INTSXP, Rf_asInteger(nruns)));
    pX = INTEGER(x);
    pRetVals = INTEGER(retVals);
    pRetLengths = INTEGER(retLengths);
    pRetDeltas = INTEGER(retDeltas);
    int delta1, delta2, length1, length2, i = 0, nrun = 0, n = LENGTH(x);
    while ( i < n ) {
        if ( delta )
            delta1 = run_delta<int>(pX, i, n);
        else
            delta1 = 0;
        length1 = run_length<int>(pX, i, n, delta1);
        if ( delta && length1 == 2 ) {
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
    PROTECT(classDef = R_do_MAKE_CLASS("drle"));
    PROTECT(retObj = R_do_new_object(classDef));
    R_do_slot_assign(retObj, Rf_install("values"), retVals);
    R_do_slot_assign(retObj, Rf_install("lengths"), retLengths);
    R_do_slot_assign(retObj, Rf_install("deltas"), retDeltas);
    UNPROTECT(5);
    return retObj;
}

template<>
SEXP makeDRLE<double>(SEXP x, SEXP nruns, bool delta) {
    SEXP retVals, retLengths, retDeltas;
    double * pX, * pRetVals, * pRetDeltas;
    int * pRetLengths;
    PROTECT(retVals = Rf_allocVector(REALSXP, Rf_asInteger(nruns)));
    PROTECT(retLengths = Rf_allocVector(INTSXP, Rf_asInteger(nruns)));
    PROTECT(retDeltas = Rf_allocVector(REALSXP, Rf_asInteger(nruns)));
    pX = REAL(x);
    pRetVals = REAL(retVals);
    pRetLengths = INTEGER(retLengths);
    pRetDeltas = REAL(retDeltas);
    double delta1, delta2;
    int length1, length2, i = 0, nrun = 0, n = LENGTH(x);
    while ( i < n ) {
        if ( delta )
            delta1 = run_delta<double>(pX, i, n);
        else
            delta1 = 0;
        length1 = run_length<double>(pX, i, n, delta1);
        if ( delta && length1 == 2 ) {
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
    PROTECT(classDef = R_do_MAKE_CLASS("drle"));
    PROTECT(retObj = R_do_new_object(classDef));
    R_do_slot_assign(retObj, Rf_install("values"), retVals);
    R_do_slot_assign(retObj, Rf_install("lengths"), retLengths);
    R_do_slot_assign(retObj, Rf_install("deltas"), retDeltas);
    UNPROTECT(5);
    return retObj;
}

template<typename RType, int SType>
SEXP VectorOrDRLE<RType,SType> :: decode() {
    SEXP retVec;
    int nout = length();
    PROTECT(retVec = Rf_allocVector(SType, nout));
    RType * pRetVec = DataPtr<RType>(retVec);
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
SEXP VectorOrDRLE<RType,SType> :: decodeElements(SEXP i) {
    SEXP retVec;
    PROTECT(retVec = Rf_allocVector(SType, LENGTH(i)));
    RType * pRetVec = DataPtr<RType>(retVec);
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
        Rf_error("subscript out of bounds");
    if ( !isDRLE ) {
        if ( i < nruns )
            return values[i];
        else
            Rf_error("subscript out of bounds");
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
        Rf_error("subscript out of bounds");
    return NA<RType>();
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
                _arglengths[i] = _arg[i].m->data().max_extent();
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
    T2 tmp;
    if ( ylen == 1 ) {
        tmp = y[0];
        for ( index_t k = 0; k < count; k++ ) {
            if ( isNA(*x) || isNA(tmp) )
                *x = NA<T1>();
            else
                *x += tmp;
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
                    tmp = y[offset + k];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x += tmp;
                    x += skip;
                }
            }
            else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[((atm->group() * xlen) + (offset + k))];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x += tmp;
                    x += skip;
                }
            }
            else
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[((atm->group() * xlen) + (offset + k)) % ylen];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x += tmp;
                    x += skip;
                }
            }
            break;
        case BY_EACH_GROUP:
            xlen = atm->length();
            if ( xlen == ylen )
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[atm->group()];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x += tmp;
                    x += skip;
                }
            }
            else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[(((offset + k) * xlen + atm->group()))];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x += tmp;
                    x += skip;
                }
            }
            else
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[(((offset + k) * xlen + atm->group())) % ylen];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x += tmp;
                    x += skip;
                }
            }
            break;
    }
}

template<typename T1, typename T2>
void Ops :: sub(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip) {
    index_t xlen, ylen = arglength(i);
    T2 tmp;
    if ( has_lhs(i) )
    {
        if ( ylen == 1 ) {
            tmp = y[0];
            for ( index_t k = 0; k < count; k++ ) {
                if ( isNA(*x) || isNA(tmp) )
                    *x = NA<T1>();
                else
                    *x = tmp - (*x);
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
                        tmp = y[offset + k];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = tmp - (*x);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[((atm->group() * xlen) + (offset + k))];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = tmp - (*x);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[((atm->group() * xlen) + (offset + k)) % ylen];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = tmp - (*x);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[atm->group()];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = tmp - (*x);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[(((offset + k) * xlen + atm->group()))];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = tmp - (*x);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[(((offset + k) * xlen + atm->group())) % ylen];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = tmp - (*x);
                        x += skip;
                    }
                }
                break;
        }
    }
    else
    {
        if ( ylen == 1 ) {
            tmp = y[0];
            for ( index_t k = 0; k < count; k++ ) {
                if ( isNA(*x) || isNA(tmp) )
                    *x = NA<T1>();
                else
                    *x -= tmp;
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
                        tmp = y[offset + k];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x -= tmp;
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[((atm->group() * xlen) + (offset + k))];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x -= tmp;
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[((atm->group() * xlen) + (offset + k)) % ylen];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x -= tmp;
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[atm->group()];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x -= tmp;
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[(((offset + k) * xlen + atm->group()))];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x -= tmp;
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[(((offset + k) * xlen + atm->group())) % ylen];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x -= tmp;
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
    T2 tmp;
    if ( ylen == 1 ) {
        tmp = y[0];
        for ( index_t k = 0; k < count; k++ ) {
            if ( isNA(*x) || isNA(tmp) )
                *x = NA<T1>();
            else
                *x *= tmp;
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
                    tmp = y[offset + k];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x *= tmp;
                    x += skip;
                }
            }
            else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[((atm->group() * xlen) + (offset + k))];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x *= tmp;
                    x += skip;
                }
            }
            else
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[((atm->group() * xlen) + (offset + k)) % ylen];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x *= tmp;
                    x += skip;
                }
            }
            break;
        case BY_EACH_GROUP:
            xlen = atm->length();
            if ( xlen == ylen )
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[atm->group()];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x *= tmp;
                    x += skip;
                }
            }
            else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[(((offset + k) * xlen + atm->group()))];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x *= tmp;
                    x += skip;
                }
            }
            else
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[(((offset + k) * xlen + atm->group())) % ylen];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x *= tmp;
                    x += skip;
                }
            }
            break;
    }
}

template<typename T1, typename T2>
void Ops :: div(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip) {
    index_t xlen, ylen = arglength(i);
    T2 tmp;
    if ( has_lhs(i) )
    {
        if ( ylen == 1 ) {
            tmp = y[0];
            for ( index_t k = 0; k < count; k++ ) {
                if ( isNA(*x) || isNA(tmp) )
                    *x = NA<T1>();
                else
                    *x = static_cast<double>(tmp) / (*x);
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
                        tmp = y[offset + k];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = static_cast<double>(tmp) / (*x);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[((atm->group() * xlen) + (offset + k))];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = static_cast<double>(tmp) / (*x);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[((atm->group() * xlen) + (offset + k)) % ylen];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = static_cast<double>(tmp) / (*x);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[atm->group()];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = static_cast<double>(tmp) / (*x);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[(((offset + k) * xlen + atm->group()))];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = static_cast<double>(tmp) / (*x);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[(((offset + k) * xlen + atm->group())) % ylen];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = static_cast<double>(tmp) / (*x);
                        x += skip;
                    }
                }
                break;
        }
    }
    else
    {
        if ( ylen == 1 ) {
            tmp = y[0];
            for ( index_t k = 0; k < count; k++ ) {
                if ( isNA(*x) || isNA(tmp) )
                    *x = NA<T1>();
                else
                    *x /= static_cast<double>(tmp);
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
                        tmp = y[offset + k];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x /= static_cast<double>(tmp);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[((atm->group() * xlen) + (offset + k))];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x /= static_cast<double>(tmp);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[((atm->group() * xlen) + (offset + k)) % ylen];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x /= static_cast<double>(tmp);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[atm->group()];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x /= static_cast<double>(tmp);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[(((offset + k) * xlen + atm->group()))];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x /= static_cast<double>(tmp);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[(((offset + k) * xlen + atm->group())) % ylen];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x /= static_cast<double>(tmp);
                        x += skip;
                    }
                }
                break;
        }
    }
}

template<typename T1, typename T2>
void Ops :: mod(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip) {
    index_t xlen, ylen = arglength(i);
    T2 tmp;
    if ( has_lhs(i) )
    {
        if ( ylen == 1 ) {
            tmp = y[0];
            for ( index_t k = 0; k < count; k++ ) {
                if ( isNA(*x) || isNA(tmp) )
                    *x = NA<T1>();
                else
                    *x = coerce_mod<T2,T1,T1>(tmp, *x);
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
                        tmp = y[offset + k];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_mod<T2,T1,T1>(tmp, *x);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[((atm->group() * xlen) + (offset + k))];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_mod<T2,T1,T1>(tmp, *x);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[((atm->group() * xlen) + (offset + k)) % ylen];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_mod<T2,T1,T1>(tmp, *x);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[atm->group()];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_mod<T2,T1,T1>(tmp, *x);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[(((offset + k) * xlen + atm->group()))];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_mod<T2,T1,T1>(tmp, *x);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[(((offset + k) * xlen + atm->group())) % ylen];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_mod<T2,T1,T1>(tmp, *x);
                        x += skip;
                    }
                }
                break;
        }
    }
    else
    {
        if ( ylen == 1 ) {
            tmp = y[0];
            for ( index_t k = 0; k < count; k++ ) {
                if ( isNA(*x) || isNA(tmp) )
                    *x = NA<T1>();
                else
                    *x = coerce_mod<T1,T2,T1>(*x, tmp);
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
                        tmp = y[offset + k];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_mod<T1,T2,T1>(*x, tmp);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[((atm->group() * xlen) + (offset + k))];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_mod<T1,T2,T1>(*x, tmp);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[((atm->group() * xlen) + (offset + k)) % ylen];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_mod<T1,T2,T1>(*x, tmp);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[atm->group()];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_mod<T1,T2,T1>(*x, tmp);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[(((offset + k) * xlen + atm->group()))];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_mod<T1,T2,T1>(*x, tmp);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[(((offset + k) * xlen + atm->group())) % ylen];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_mod<T1,T2,T1>(*x, tmp);
                        x += skip;
                    }
                }
                break;
        }
    }
}

template<typename T1, typename T2>
void Ops :: idiv(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip) {
    index_t xlen, ylen = arglength(i);
    T2 tmp;
    if ( has_lhs(i) )
    {
        if ( ylen == 1 ) {
            tmp = y[0];
            for ( index_t k = 0; k < count; k++ ) {
                if ( isNA(*x) || isNA(tmp) )
                    *x = NA<T1>();
                else
                    *x = coerce_idiv<T2,T1,T1>(tmp, *x);
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
                        tmp = y[offset + k];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_idiv<T2,T1,T1>(tmp, *x);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[((atm->group() * xlen) + (offset + k))];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_idiv<T2,T1,T1>(tmp, *x);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[((atm->group() * xlen) + (offset + k)) % ylen];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_idiv<T2,T1,T1>(tmp, *x);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[atm->group()];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_idiv<T2,T1,T1>(tmp, *x);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[(((offset + k) * xlen + atm->group()))];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_idiv<T2,T1,T1>(tmp, *x);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[(((offset + k) * xlen + atm->group())) % ylen];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_idiv<T2,T1,T1>(tmp, *x);
                        x += skip;
                    }
                }
                break;
        }
    }
    else
    {
        if ( ylen == 1 ) {
            tmp = y[0];
            for ( index_t k = 0; k < count; k++ ) {
                if ( isNA(*x) || isNA(tmp) )
                    *x = NA<T1>();
                else
                    *x = coerce_idiv<T1,T2,T1>(*x, tmp);
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
                        tmp = y[offset + k];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_idiv<T1,T2,T1>(*x, tmp);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[((atm->group() * xlen) + (offset + k))];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_idiv<T1,T2,T1>(*x, tmp);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[((atm->group() * xlen) + (offset + k)) % ylen];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_idiv<T1,T2,T1>(*x, tmp);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[atm->group()];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_idiv<T1,T2,T1>(*x, tmp);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[(((offset + k) * xlen + atm->group()))];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_idiv<T1,T2,T1>(*x, tmp);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = y[(((offset + k) * xlen + atm->group())) % ylen];
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_idiv<T1,T2,T1>(*x, tmp);
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
    T1 tmp;
    if ( has_lhs(i) )
    {
        if ( ylen == 1 ) {
            tmp = static_cast<T1>(y[0]);
            for ( index_t k = 0; k < count; k++ ) {
                if ( isNA(*x) || isNA(tmp) )
                    *x = NA<T1>();
                else
                    *x = coerce_pow<T1>(tmp, (*x));
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
                        tmp = static_cast<T1>(y[offset + k]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_pow<T1>(tmp, (*x));
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_pow<T1>(tmp, (*x));
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k)) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_pow<T1>(tmp, (*x));
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[atm->group()]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_pow<T1>(tmp, (*x));
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group()))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_pow<T1>(tmp, (*x));
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group())) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_pow<T1>(tmp, (*x));
                        x += skip;
                    }
                }
                break;
        }
    }
    else
    {
        if ( ylen == 1 ) {
            tmp = static_cast<T1>(y[0]);
            for ( index_t k = 0; k < count; k++ ) {
                if ( isNA(*x) || isNA(tmp) )
                    *x = NA<T1>();
                else
                    *x = coerce_pow<T1>((*x), tmp);
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
                        tmp = static_cast<T1>(y[offset + k]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_pow<T1>((*x), tmp);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_pow<T1>((*x), tmp);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k)) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_pow<T1>((*x), tmp);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[atm->group()]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_pow<T1>((*x), tmp);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group()))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_pow<T1>((*x), tmp);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group())) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_pow<T1>((*x), tmp);
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
    T1 tmp;
    if ( has_lhs(i) )
    {
        if ( ylen == 1) {
            tmp = static_cast<T1>(y[0]);
            for ( index_t k = 0; k < count; k++ ) {
                if ( isNA(*x) || isNA(tmp) )
                    *x = NA<T1>();
                else
                    *x = coerce_log<T1>(*x) / coerce_log<T1>(tmp);
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
                        tmp = static_cast<T1>(y[offset + k]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_log<T1>(*x) / coerce_log<T1>(tmp);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_log<T1>(*x) / coerce_log<T1>(tmp);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k)) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_log<T1>(*x) / coerce_log<T1>(tmp);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[atm->group()]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_log<T1>(*x) / coerce_log<T1>(tmp);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group()))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_log<T1>(*x) / coerce_log<T1>(tmp);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group())) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = coerce_log<T1>(*x) / coerce_log<T1>(tmp);
                        x += skip;
                    }
                }
                break;
        }
    }
}

template<typename T1, typename T2>
void Ops :: eq(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip) {
    index_t xlen, ylen = arglength(i);
    T2 tmp;
    if ( ylen == 1 ) {
        tmp = y[0];
        for ( index_t k = 0; k < count; k++ ) {
            if ( isNA(*x) || isNA(tmp) )
                *x = NA<T1>();
            else
                *x = (*x == tmp);
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
                    tmp = y[offset + k];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x == tmp);
                    x += skip;
                }
            }
            else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[((atm->group() * xlen) + (offset + k))];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x == tmp);
                    x += skip;
                }
            }
            else
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[((atm->group() * xlen) + (offset + k)) % ylen];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x == tmp);
                    x += skip;
                }
            }
            break;
        case BY_EACH_GROUP:
            xlen = atm->length();
            if ( xlen == ylen )
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[atm->group()];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x == tmp);
                    x += skip;
                }
            }
            else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[(((offset + k) * xlen + atm->group()))];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x == tmp);
                    x += skip;
                }
            }
            else
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[(((offset + k) * xlen + atm->group())) % ylen];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x == tmp);
                    x += skip;
                }
            }
            break;
    }
}

template<typename T1, typename T2>
void Ops :: ne(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip) {
    index_t xlen, ylen = arglength(i);
    T2 tmp;
    if ( ylen == 1 ) {
        tmp = y[0];
        for ( index_t k = 0; k < count; k++ ) {
            if ( isNA(*x) || isNA(tmp) )
                *x = NA<T1>();
            else
                *x = (*x != tmp);
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
                    tmp = y[offset + k];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x != tmp);
                    x += skip;
                }
            }
            else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[((atm->group() * xlen) + (offset + k))];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x != tmp);
                    x += skip;
                }
            }
            else
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[((atm->group() * xlen) + (offset + k)) % ylen];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x != tmp);
                    x += skip;
                }
            }
            break;
        case BY_EACH_GROUP:
            xlen = atm->length();
            if ( xlen == ylen )
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[atm->group()];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x != tmp);
                    x += skip;
                }
            }
            else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[(((offset + k) * xlen + atm->group()))];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x != tmp);
                    x += skip;
                }
            }
            else
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[(((offset + k) * xlen + atm->group())) % ylen];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x != tmp);
                    x += skip;
                }
            }
            break;
    }
}

template<typename T1, typename T2>
void Ops :: gt(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip) {
    index_t xlen, ylen = arglength(i);
    T1 tmp;
    if ( has_lhs(i) )
    {
        if ( ylen == 1 ) {
            tmp = static_cast<T1>(y[0]);
            for ( index_t k = 0; k < count; k++ ) {
                if ( isNA(*x) || isNA(tmp) )
                    *x = NA<T1>();
                else
                    *x = (tmp > *x);
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
                        tmp = static_cast<T1>(y[offset + k]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp > *x);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp > *x);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k)) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp > *x);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[atm->group()]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp > *x);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group()))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp > *x);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group())) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp > *x);
                        x += skip;
                    }
                }
                break;
        }
    }
    else
    {
        if ( ylen == 1 ) {
            tmp = static_cast<T1>(y[0]);
            for ( index_t k = 0; k < count; k++ ) {
                if ( isNA(*x) || isNA(tmp) )
                    *x = NA<T1>();
                else
                    *x = (*x > tmp);
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
                        tmp = static_cast<T1>(y[offset + k]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x > tmp);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x > tmp);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k)) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x > tmp);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[atm->group()]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x > tmp);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group()))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x > tmp);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group())) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x > tmp);
                        x += skip;
                    }
                }
                break;
        }
    }
}

template<typename T1, typename T2>
void Ops :: lt(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip) {
    index_t xlen, ylen = arglength(i);
    T1 tmp;
    if ( has_lhs(i) )
    {
        if ( ylen == 1 ) {
            tmp = static_cast<T1>(y[0]);
            for ( index_t k = 0; k < count; k++ ) {
                if ( isNA(*x) || isNA(tmp) )
                    *x = NA<T1>();
                else
                    *x = (tmp < *x);
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
                        tmp = static_cast<T1>(y[offset + k]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp < *x);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp < *x);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k)) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp < *x);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[atm->group()]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp < *x);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group()))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp < *x);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group())) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp < *x);
                        x += skip;
                    }
                }
                break;
        }
    }
    else
    {
        if ( ylen == 1 ) {
            tmp = static_cast<T1>(y[0]);
            for ( index_t k = 0; k < count; k++ ) {
                if ( isNA(*x) || isNA(tmp) )
                    *x = NA<T1>();
                else
                    *x = (*x < tmp);
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
                        tmp = static_cast<T1>(y[offset + k]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x < tmp);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x < tmp);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k)) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x < tmp);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[atm->group()]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x < tmp);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group()))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x < tmp);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group())) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x < tmp);
                        x += skip;
                    }
                }
                break;
        }
    }
}

template<typename T1, typename T2>
void Ops :: ge(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip) {
    index_t xlen, ylen = arglength(i);
    T1 tmp;
    if ( has_lhs(i) )
    {
        if ( ylen == 1 ) {
            tmp = static_cast<T1>(y[0]);
            for ( index_t k = 0; k < count; k++ ) {
                if ( isNA(*x) || isNA(tmp) )
                    *x = NA<T1>();
                else
                    *x = (tmp >= *x);
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
                        tmp = static_cast<T1>(y[offset + k]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp >= *x);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp >= *x);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k)) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp >= *x);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[atm->group()]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp >= *x);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group()))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp >= *x);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group())) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp >= *x);
                        x += skip;
                    }
                }
                break;
        }
    }
    else
    {
        if ( ylen == 1 ) {
            tmp = static_cast<T1>(y[0]);
            for ( index_t k = 0; k < count; k++ ) {
                if ( isNA(*x) || isNA(tmp) )
                    *x = NA<T1>();
                else
                    *x = (*x >= tmp);
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
                        tmp = static_cast<T1>(y[offset + k]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x >= tmp);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x >= tmp);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k)) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x >= tmp);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[atm->group()]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x >= tmp);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group()))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x >= tmp);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group())) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x >= tmp);
                        x += skip;
                    }
                }
                break;
        }
    }
}

template<typename T1, typename T2>
void Ops :: le(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip) {
    index_t xlen, ylen = arglength(i);
    T1 tmp;
    if ( has_lhs(i) )
    {
        if ( ylen == 1 ) {
            tmp = static_cast<T1>(y[0]);
            for ( index_t k = 0; k < count; k++ ) {
                if ( isNA(*x) || isNA(tmp) )
                    *x = NA<T1>();
                else
                    *x = (tmp <= *x);
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
                        tmp = static_cast<T1>(y[offset + k]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp <= *x);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp <= *x);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k)) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp <= *x);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[atm->group()]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp <= *x);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group()))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp <= *x);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group())) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (tmp <= *x);
                        x += skip;
                    }
                }
                break;
        }
    }
    else
    {
        if ( ylen == 1 ) {
            tmp = static_cast<T1>(y[0]);
            for ( index_t k = 0; k < count; k++ ) {
                if ( isNA(*x) || isNA(tmp) )
                    *x = NA<T1>();
                else
                    *x = (*x <= tmp);
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
                        tmp = static_cast<T1>(y[offset + k]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x <= tmp);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x <= tmp);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[((atm->group() * xlen) + (offset + k)) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x <= tmp);
                        x += skip;
                    }
                }
                break;
            case BY_EACH_GROUP:
                xlen = atm->length();
                if ( xlen == ylen )
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[atm->group()]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x <= tmp);
                        x += skip;
                    }
                }
                else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group()))]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x <= tmp);
                        x += skip;
                    }
                }
                else
                {
                    for ( index_t k = 0; k < count; k++ ) {
                        tmp = static_cast<T1>(y[(((offset + k) * xlen + atm->group())) % ylen]);
                        if ( isNA(*x) || isNA(tmp) )
                            *x = NA<T1>();
                        else
                            *x = (*x <= tmp);
                        x += skip;
                    }
                }
                break;
        }
    }
}

template<typename T1, typename T2>
void Ops :: AND(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip) {
    index_t xlen, ylen = arglength(i);
    T2 tmp;
    if ( ylen == 1 ) {
        tmp = y[0];
        for ( index_t k = 0; k < count; k++ ) {
            if ( isNA(*x) || isNA(tmp) )
                *x = NA<T1>();
            else
                *x = (*x && tmp);
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
                    tmp = y[offset + k];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x && tmp);
                    x += skip;
                }
            }
            else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[((atm->group() * xlen) + (offset + k))];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x && tmp);
                    x += skip;
                }
            }
            else
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[((atm->group() * xlen) + (offset + k)) % ylen];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x && tmp);
                    x += skip;
                }
            }
            break;
        case BY_EACH_GROUP:
            xlen = atm->length();
            if ( xlen == ylen )
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[atm->group()];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x && tmp);
                    x += skip;
                }
            }
            else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[(((offset + k) * xlen + atm->group()))];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x && tmp);
                    x += skip;
                }
            }
            else
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[(((offset + k) * xlen + atm->group())) % ylen];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x && tmp);
                    x += skip;
                }
            }
            break;
    }
}

template<typename T1, typename T2>
void Ops :: OR(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip) {
    index_t xlen, ylen = arglength(i);
    T2 tmp;
    if ( ylen == 1 ) {
        tmp = y[0];
        for ( index_t k = 0; k < count; k++ ) {
            if ( isNA(*x) || isNA(tmp) )
                *x = NA<T1>();
            else
                *x = (*x || tmp);
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
                    tmp = y[offset + k];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x || tmp);
                    x += skip;
                }
            }
            else if ( ylen == xlen * atm->length() ) // assumes equal group sizes
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[((atm->group() * xlen) + (offset + k))];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x || tmp);
                    x += skip;
                }
            }
            else
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[((atm->group() * xlen) + (offset + k)) % ylen];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x || tmp);
                    x += skip;
                }
            }
            break;
        case BY_EACH_GROUP:
            xlen = atm->length();
            if ( xlen == ylen )
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[atm->group()];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x || tmp);
                    x += skip;
                }
            }
            else if ( ylen == xlen * atm->max_extent() ) // assumes equal group sizes
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[(((offset + k) * xlen + atm->group()))];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x || tmp);
                    x += skip;
                }
            }
            else
            {
                for ( index_t k = 0; k < count; k++ ) {
                    tmp = y[(((offset + k) * xlen + atm->group())) % ylen];
                    if ( isNA(*x) || isNA(tmp) )
                        *x = NA<T1>();
                    else
                        *x = (*x || tmp);
                    x += skip;
                }
            }
            break;
    }
}

template<typename T>
void Ops :: do_ops(T * x, Atoms * atm, index_t offset, index_t count, size_t skip) {
    for ( int i = 0; i < length(); i++ )
    {
        // assumes 'matter' will be the only S4 class that ends up here
        T * y;
        if ( type(i) == S4SXP ) {
            y = (T *) Calloc(count, T);
            arg<Matter,S4SXP>(i)->data().set_group(atm->group());
            arg<Matter,S4SXP>(i)->data().read<T>(y, offset, count);
        }
        switch(op(i))
        {
            // Arith
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
                    case S4SXP:
                        add<T,T>(x, y, i, atm, 0, count, skip);
                        break;
                    default:
                        Rf_error("non-numeric argument to binary operator");
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
                    case S4SXP:
                        sub<T,T>(x, y, i, atm, 0, count, skip);
                        break;
                    default:
                        Rf_error("non-numeric argument to binary operator");
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
                    case S4SXP:
                        mul<T,T>(x, y, i, atm, 0, count, skip);
                        break;
                    default:
                        Rf_error("non-numeric argument to binary operator");
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
                    case S4SXP:
                        div<T,T>(x, y, i, atm, 0, count, skip);
                        break;
                    default:
                        Rf_error("non-numeric argument to binary operator");
                }
                break;
            case OP_EXP:
                switch(type(i)) {
                    case NILSXP:
                        // null arg means e^x
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
                    case S4SXP:
                        exp<T,T>(x, y, i, atm, 0, count, skip);
                        break;
                    default:
                        Rf_error("non-numeric argument to binary operator");
                }
                break;
            case OP_MOD:
                switch(type(i)) {
                    case INTSXP:
                        mod<T,int>(x, arg<int,INTSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case REALSXP:
                        mod<T,double>(x, arg<double,REALSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case S4SXP:
                        mod<T,T>(x, y, i, atm, 0, count, skip);
                        break;
                    default:
                        Rf_error("non-numeric argument to binary operator");
                }
                break;
            case OP_IDIV:
                switch(type(i)) {
                    case INTSXP:
                        idiv<T,int>(x, arg<int,INTSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case REALSXP:
                        idiv<T,double>(x, arg<double,REALSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case S4SXP:
                        idiv<T,T>(x, y, i, atm, 0, count, skip);
                        break;
                    default:
                        Rf_error("non-numeric argument to binary operator");
                }
                break;
            // Compare
            case OP_EQ:
                switch(type(i)) {
                    case RAWSXP:
                        eq<T,Rbyte>(x, arg<Rbyte,RAWSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case INTSXP:
                        eq<T,int>(x, arg<int,INTSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case REALSXP:
                        eq<T,double>(x, arg<double,REALSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case S4SXP:
                        eq<T,T>(x, y, i, atm, 0, count, skip);
                        break;
                    default:
                        Rf_error("non-numeric argument to binary operator");
                }
                break;
            case OP_NE:
                switch(type(i)) {
                    case RAWSXP:
                        ne<T,Rbyte>(x, arg<Rbyte,RAWSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case INTSXP:
                        ne<T,int>(x, arg<int,INTSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case REALSXP:
                        ne<T,double>(x, arg<double,REALSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case S4SXP:
                        ne<T,T>(x, y, i, atm, 0, count, skip);
                        break;
                    default:
                        Rf_error("non-numeric argument to binary operator");
                }
                break;
            case OP_GT:
                switch(type(i)) {
                    case RAWSXP:
                        gt<T,Rbyte>(x, arg<Rbyte,RAWSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case INTSXP:
                        gt<T,int>(x, arg<int,INTSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case REALSXP:
                        gt<T,double>(x, arg<double,REALSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case S4SXP:
                        gt<T,T>(x, y, i, atm, 0, count, skip);
                        break;
                    default:
                        Rf_error("non-numeric argument to binary operator");
                }
                break;
            case OP_LT:
                switch(type(i)) {
                    case RAWSXP:
                        lt<T,Rbyte>(x, arg<Rbyte,RAWSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case INTSXP:
                        lt<T,int>(x, arg<int,INTSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case REALSXP:
                        lt<T,double>(x, arg<double,REALSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case S4SXP:
                        lt<T,T>(x, y, i, atm, 0, count, skip);
                        break;
                    default:
                        Rf_error("non-numeric argument to binary operator");
                }
                break;
            case OP_GE:
                switch(type(i)) {
                    case RAWSXP:
                        ge<T,Rbyte>(x, arg<Rbyte,RAWSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case INTSXP:
                        ge<T,int>(x, arg<int,INTSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case REALSXP:
                        ge<T,double>(x, arg<double,REALSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case S4SXP:
                        ge<T,T>(x, y, i, atm, 0, count, skip);
                        break;
                    default:
                        Rf_error("non-numeric argument to binary operator");
                }
                break;
            case OP_LE:
                switch(type(i)) {
                    case RAWSXP:
                        le<T,Rbyte>(x, arg<Rbyte,RAWSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case INTSXP:
                        le<T,int>(x, arg<int,INTSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case REALSXP:
                        le<T,double>(x, arg<double,REALSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case S4SXP:
                        le<T,T>(x, y, i, atm, 0, count, skip);
                        break;
                    default:
                        Rf_error("non-numeric argument to binary operator");
                }
                break;
            // Logic
            case OP_AND:
                switch(type(i)) {
                    case LGLSXP:
                        AND<T,int>(x, arg<int,LGLSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case S4SXP:
                        AND<T,T>(x, y, i, atm, 0, count, skip);
                        break;
                    default:
                        Rf_error("non-numeric argument to binary operator");
                }
                break;
            case OP_OR:
                switch(type(i)) {
                    case LGLSXP:
                        OR<T,int>(x, arg<int,LGLSXP>(i),
                            i, atm, offset, count, skip);
                        break;
                    case S4SXP:
                        OR<T,T>(x, y, i, atm, 0, count, skip);
                        break;
                    default:
                        Rf_error("non-numeric argument to binary operator");
                }
                break;
            // Math
            case OP_LOG:
                switch(type(i)) {
                    case NILSXP:
                        // null arg means natural log
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
                    case S4SXP:
                        log<T,T>(x, y, i, atm, 0, count, skip);
                        break;
                    default:
                        Rf_error("non-numeric argument to mathematical function");
                }
                break;
        }
        if ( type(i) == S4SXP )
            Free(y);
    }
}

//// Vector and array methods implemented for class Matter
//--------------------------------------------------------

template<typename RType, int SType>
SEXP Matter :: readVector() {
    SEXP retVec;
    PROTECT(retVec = Rf_allocVector(SType, length()));
    RType * pRetVec = DataPtr<RType>(retVec);
    data().read<RType>(pRetVec, 0, length());
    UNPROTECT(1);
    return retVec;
}

template<typename RType, int SType>
void Matter :: writeVector(SEXP value) {
    RType * pValue = DataPtr<RType>(value);
    if ( XLENGTH(value) == 1 )
        data().write<RType>(pValue, 0, length(), 0);
    else
        data().write<RType>(pValue, 0, length());
}

template<typename RType, int SType>
SEXP Matter :: readVectorElements(SEXP i) {
    SEXP retVec;
    PROTECT(retVec = Rf_allocVector(SType, XLENGTH(i)));
    RType * pRetVec = DataPtr<RType>(retVec);
    Rindex_t * pIndex = INDEX_PTR(i);
    data().read_indices<RType>(pRetVec, pIndex, XLENGTH(i));
    UNPROTECT(1);
    return retVec;
}

template<typename RType, int SType>
void Matter :: writeVectorElements(SEXP i, SEXP value) {
    RType * pValue = DataPtr<RType>(value);
    Rindex_t * pIndex = INDEX_PTR(i);
    if ( XLENGTH(value) == 1 )
        data().write_indices<RType>(pValue, pIndex, XLENGTH(i), 0);
    else
        data().write_indices<RType>(pValue, pIndex, XLENGTH(i));
}

//// List (ragged array) methods implemented for class Matter
//------------------------------------------------------------

template<typename RType, int SType>
SEXP Matter :: readListElements(int i) {
    SEXP retVec;
    PROTECT(retVec = Rf_allocVector(SType, dim(i)));
    RType * pRetVec = DataPtr<RType>(retVec);
    data().set_group(i);
    data().read<RType>(pRetVec, 0, dim(i));
    UNPROTECT(1);
    return retVec;
}

template<typename RType, int SType>
SEXP Matter :: readListElements(int i, SEXP j) {
    SEXP retVec;
    PROTECT(retVec = Rf_allocVector(SType, LENGTH(j)));
    RType * pRetVec = DataPtr<RType>(retVec);
    data().set_group(i);
    data().read_indices<RType>(pRetVec, INDEX_PTR(j), XLENGTH(j));
    UNPROTECT(1);
    return retVec;
}

template<typename RType, int SType>
void Matter :: writeListElements(int i, SEXP value) {
    RType * pValue = DataPtr<RType>(value);
    data().set_group(i);
    if ( XLENGTH(value) == 1 )
        data().write<RType>(pValue, 0, dim(i), 0);
    else
        data().write<RType>(pValue, 0, dim(i));
}

template<typename RType, int SType>
void Matter :: writeListElements(int i, SEXP j, SEXP value) {
    RType * pValue = DataPtr<RType>(value);
    data().set_group(i);
    if ( XLENGTH(value) == 1 )
        data().write_indices<RType>(pValue, INDEX_PTR(j), XLENGTH(j), 0);
    else
        data().write_indices<RType>(pValue, INDEX_PTR(j), XLENGTH(j));
}


//// Matrix methods implemented for class Matter
//-----------------------------------------------

template<typename RType, int SType>
SEXP Matter :: readMatrix() {
    SEXP retMat;
    int nrows = this->nrows(), ncols = this->ncols();
    PROTECT(retMat = Rf_allocMatrix(SType, nrows, ncols));
    RType * pRetMat = DataPtr<RType>(retMat);
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
        default:
            Rf_error("unrecognized 'matter_mat' subclass");
    }
    UNPROTECT(1);
    return retMat;
}

template<typename RType, int SType>
void Matter :: writeMatrix(SEXP value) {
    int nrows = this->nrows(), ncols = this->ncols();
    RType * pValue = DataPtr<RType>(value);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int col = 0; col < ncols; col++ ) {
                data().set_group(col);
                if ( XLENGTH(value) == 1 )
                    data().write<RType>(pValue, 0, nrows, 0);
                else    
                    data().write<RType>(pValue + col * nrows, 0, nrows);
            }
            break;
        case MATTER_MATR:
            for ( int row = 0; row < nrows; row++ ) {
                data().set_group(row);
                if ( XLENGTH(value) == 1 )
                    data().write<RType>(pValue, 0, ncols, 0);
                else
                    data().write<RType>(pValue + row, 0, ncols, nrows);
            }
            break;
        default:
            Rf_error("unrecognized 'matter_mat' subclass");
    }
}

template<typename RType, int SType>
SEXP Matter :: readMatrixRows(SEXP i) {
    SEXP retMat;
    int nrows = LENGTH(i), ncols = this->ncols();
    PROTECT(retMat = Rf_allocMatrix(SType, nrows, ncols));
    RType * pRetMat = DataPtr<RType>(retMat);
    Rindex_t * pRow = INDEX_PTR(i);
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
        default:
            Rf_error("unrecognized 'matter_mat' subclass");
    }
    UNPROTECT(1);
    return retMat;
}

template<typename RType, int SType>
void Matter :: writeMatrixRows(SEXP i, SEXP value) {
    int nrows = LENGTH(i), ncols = this->ncols();
    RType * pValue = DataPtr<RType>(value);
    Rindex_t * pRow = INDEX_PTR(i);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int col = 0; col < ncols; col++ ) {
                data().set_group(col);
                if ( XLENGTH(value) == 1 )
                    data().write_indices<RType>(pValue, pRow, nrows, 0);
                else
                    data().write_indices<RType>(pValue + col * nrows, pRow, nrows);
            }
            break;
        case MATTER_MATR:
            for ( int l = 0; l < nrows; l++ ) {
                if ( ISNA(pRow[l]) )
                    continue;
                index_t row = static_cast<index_t>(pRow[l]);
                data().set_group(row);
                if ( XLENGTH(value) == 1 )
                    data().write<RType>(pValue, 0, ncols, 0);
                else
                    data().write<RType>(pValue + l, 0, ncols, nrows);
            }
            break;
        default:
            Rf_error("unrecognized 'matter_mat' subclass");
    }
}

template<typename RType, int SType>
SEXP Matter :: readMatrixCols(SEXP j) {
    SEXP retMat;
    int nrows = this->nrows(), ncols = LENGTH(j);
    PROTECT(retMat = Rf_allocMatrix(SType, nrows, ncols));
    RType * pRetMat = DataPtr<RType>(retMat);
    Rindex_t * pCol = INDEX_PTR(j);
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
        default:
            Rf_error("unrecognized 'matter_mat' subclass");
    }
    UNPROTECT(1);
    return retMat;
}

template<typename RType, int SType>
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
                data().set_group(col);
                if ( XLENGTH(value) == 1 )
                    data().write<RType>(pValue, 0, nrows, 0);
                else
                    data().write<RType>(pValue + l * nrows, 0, nrows);
            }
            break;
        case MATTER_MATR:
            for ( int row = 0; row < nrows; row++ ) {
                data().set_group(row);
                if ( XLENGTH(value) == 1 )
                    data().write_indices<RType>(pValue, pCol, ncols, 0);
                else
                    data().write_indices<RType>(pValue + row, pCol, ncols, nrows);
            }
            break;
        default:
            Rf_error("unrecognized 'matter_mat' subclass");
    }
}

template<typename RType, int SType>
SEXP Matter :: readMatrixElements(SEXP i, SEXP j) {
    SEXP retMat;
    int nrows = LENGTH(i), ncols = LENGTH(j);
    PROTECT(retMat = Rf_allocMatrix(SType, nrows, ncols));
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
        default:
            Rf_error("unrecognized 'matter_mat' subclass");
    }
    UNPROTECT(1);
    return retMat;
}

template<typename RType, int SType>
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
                data().set_group(col);
                if ( XLENGTH(value) == 1 )
                    data().write_indices<RType>(pValue, pRow, nrows, 0);
                else
                    data().write_indices<RType>(pValue + l * nrows, pRow, nrows);
            }
            break;
        case MATTER_MATR:
            for ( int l = 0; l < nrows; l++ ) {
                if ( ISNA(pRow[l]) )
                    continue;
                index_t row = static_cast<index_t>(pRow[l]);
                data().set_group(row);
                if ( XLENGTH(value) == 1 )
                    data().write_indices<RType>(pValue, pCol, ncols, 0);
                else
                    data().write_indices<RType>(pValue + l, pCol, ncols, nrows);
            }
            break;
        default:
            Rf_error("unrecognized 'matter_mat' subclass");
    }
}

//// SparseVector methods
//------------------------

template<typename TKey, typename TVal>
TVal SparseVector :: get(size_t i)
{
    TKey subset = has_keys() ? DataPtr<TKey>(keys())[i] : i;
    TVal val = keyval_search<TKey,TVal>(subset, index(), data(),
        0, length(), tol(), tol_ref(), 0, dups_handler(), TRUE).second;
    return val;
}

template<typename TKey, typename TVal>
size_t SparseVector :: getRegion(size_t i, size_t size, TVal * buffer)
{
    TKey * region_keys = (TKey *) Calloc(size, TKey);
    copy_keys<TKey>(i, size, region_keys);
    size_t num_read = do_keyval_search<TKey,TVal>(buffer, region_keys, size,
        index(), data(), 0, length(), tol(), tol_ref(), 0, dups_handler(), TRUE);
    Free(region_keys);
    return num_read;
}

template<typename TKey, typename TVal, int S>
SEXP SparseVector :: getElements(SEXP i)
{
    TKey * region_keys = (TKey *) Calloc(XLENGTH(i), TKey);
    copy_keys(i, XLENGTH(i), region_keys);
    SEXP retVec = do_keyval_search<TKey,TVal,S>(region_keys, index(), data(),
        tol(), tol_ref(), 0, dups_handler(), TRUE);
    Free(region_keys);
    return retVec;
}

//// Statistical functions applied on MatterIterator
//--------------------------------------------------

template<typename T>
pair_double range(MatterIterator<T> & x, bool na_rm) {
    pair_double retVal;
    retVal.first = R_DOUBLE_MAX;
    retVal.second = R_DOUBLE_MIN;
    while ( x ) {
        if ( R_FINITE(*x) || R_INFINITE(*x) )
        {
            if ( *x < retVal.first )
                retVal.first = *x;
            if ( *x > retVal.second )
                retVal.second = *x;
        }
        else if ( !na_rm )
        {
            retVal.first = *x;
            retVal.second = *x;
            return retVal;
        }
        ++x;
    }
    return retVal;
}

template<typename T>
double prod(MatterIterator<T> & x, bool na_rm) {
    double retVal = 1;
    while ( x ) {
        if ( R_FINITE(*x) )
        {
            retVal *= *x;
        }
        else if ( !na_rm || R_INFINITE(*x) )
            return *x;
        ++x;
    }
    return retVal;
}

template<typename T>
double sum(MatterIterator<T> & x, bool na_rm) {
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

template<typename T>
double mean(MatterIterator<T> & x, bool na_rm) {
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

template<typename T>
double var(MatterIterator<T> & x, bool na_rm) {
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

template<typename T>
int any(MatterIterator<T> & x, bool na_rm) {
    int retVal = 0;
    while ( x ) {
        if ( *x == NA_LOGICAL )
            retVal = *x;
        else if ( *x == 1 )
            return *x;
        ++x;
    }
    return retVal;
}

template<typename T>
int all(MatterIterator<T> & x, bool na_rm) {
    int retVal = 1;
    while ( x ) {
        if ( *x == NA_LOGICAL )
            retVal = *x;
        else if ( *x == 0 )
            return *x;
        ++x;
    }
    return retVal;
}

//// Statistical methods implemented for class Matter
//---------------------------------------------------

template<typename T>
SEXP Matter :: range(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = Rf_allocVector(REALSXP, 2));
    MatterIterator<T> x(*this);
    pair_double tmp = ::range<T>(x, na_rm);
    REAL(retVal)[0] = tmp.first;
    REAL(retVal)[1] = tmp.second;
    UNPROTECT(1);
    return retVal;
}

template<typename T>
SEXP Matter :: prod(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = Rf_allocVector(REALSXP, 1));
    MatterIterator<T> x(*this);
    *REAL(retVal) = ::prod<T>(x, na_rm);
    UNPROTECT(1);
    return retVal;
}

template<typename T>
SEXP Matter :: sum(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = Rf_allocVector(REALSXP, 1));
    MatterIterator<T> x(*this);
    *REAL(retVal) = ::sum<T>(x, na_rm);
    UNPROTECT(1);
    return retVal;
}

template<typename T>
SEXP Matter :: mean(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = Rf_allocVector(REALSXP, 1));
    MatterIterator<T> x(*this);
    *REAL(retVal) = ::mean<T>(x, na_rm);
    UNPROTECT(1);
    return retVal;
}

template<typename T>
SEXP Matter :: var(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = Rf_allocVector(REALSXP, 1));
    MatterIterator<T> x(*this);
    *REAL(retVal) = ::var<T>(x, na_rm);
    UNPROTECT(1);
    return retVal;
}

template<typename T>
SEXP Matter :: any(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = Rf_allocVector(LGLSXP, 1));
    MatterIterator<T> x(*this);
    *LOGICAL(retVal) = ::any<T>(x, na_rm);
    UNPROTECT(1);
    return retVal;
}

template<typename T>
SEXP Matter :: all(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = Rf_allocVector(LGLSXP, 1));
    MatterIterator<T> x(*this);
    *LOGICAL(retVal) = ::all<T>(x, na_rm);
    UNPROTECT(1);
    return retVal;
}

template<typename T>
SEXP Matter :: colsums(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = Rf_allocVector(REALSXP, ncols()));
    double * pRetVal = REAL(retVal);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int j = 0; j < ncols(); j++ ) {
                MatterIterator<T> x(*this, j);
                pRetVal[j] = ::sum<T>(x, na_rm);
            }
            break;
        case MATTER_MATR:
            for ( int j = 0; j < ncols(); j++ )
                pRetVal[j] = 0;
            for ( int i = 0; i < nrows(); i++ ) {
                MatterIterator<T> x(*this, i);
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
        default:
            Rf_error("unrecognised matrix subclass");
    }
    UNPROTECT(1);
    return retVal;
}

template<typename T>
SEXP Matter :: colmeans(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = Rf_allocVector(REALSXP, ncols()));
    double * pRetVal = REAL(retVal);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int j = 0; j < ncols(); j++ ) {
                MatterIterator<T> x(*this, j);
                pRetVal[j] = ::mean<T>(x, na_rm);
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
                    MatterIterator<T> x(*this, i);
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
        default:
            Rf_error("unrecognised matrix subclass");
    }
    UNPROTECT(1);
    return retVal;
}

template<typename T>
SEXP Matter :: colvar(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = Rf_allocVector(REALSXP, ncols()));
    double * pRetVal = REAL(retVal);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int j = 0; j < ncols(); j++ ) {
                MatterIterator<T> x(*this, j);
                pRetVal[j] = ::var<T>(x, na_rm);
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
                    MatterIterator<T> x(*this, i);
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
        default:
            Rf_error("unrecognised matrix subclass");
    }
    UNPROTECT(1);
    return retVal;
}

template<typename T>
SEXP Matter :: rowsums(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = Rf_allocVector(REALSXP, nrows()));
    double * pRetVal = REAL(retVal);
    switch(S4class()) {
        case MATTER_MATC:
            for ( int i = 0; i < nrows(); i++ )
                pRetVal[i] = 0;
            for ( int j = 0; j < ncols(); j++ ) {
                MatterIterator<T> x(*this, j);
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
                MatterIterator<T> x(*this, i);
                pRetVal[i] = ::sum<T>(x, na_rm);
            }
            break;
        default:
            Rf_error("unrecognised matrix subclass");
    }
    UNPROTECT(1);
    return retVal;
}

template<typename T>
SEXP Matter :: rowmeans(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = Rf_allocVector(REALSXP, nrows()));
    double * pRetVal = REAL(retVal);
    switch(S4class()) {
        case MATTER_MATC:
            {
                double * n = (double *) Calloc(nrows(), double);
                for ( int i = 0; i < nrows(); i++ ) {
                    pRetVal[i] = 0;
                    n[i] = 0;
                }
                for ( int j = 0; j < ncols(); j++ ) {
                    MatterIterator<T> x(*this, j);
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
                MatterIterator<T> x(*this, i);
                pRetVal[i] = ::mean<T>(x, na_rm);
            }
            break;
        default:
            Rf_error("unrecognised matrix subclass");
    }
    UNPROTECT(1);
    return retVal;
}

template<typename T>
SEXP Matter :: rowvar(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = Rf_allocVector(REALSXP, nrows()));
    double * pRetVal = REAL(retVal);
    switch(S4class()) {
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
                    MatterIterator<T> x(*this, j);
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
                MatterIterator<T> x(*this, i);
                pRetVal[i] = ::var<T>(x, na_rm);
            }
            break;
        default:
            Rf_error("unrecognised matrix subclass");
    }
    UNPROTECT(1);
    return retVal;
}

template<typename T, typename RType, int SType>
SEXP Matter :: rmult(SEXP y) {
    SEXP retMat;
    PROTECT(retMat = Rf_allocMatrix(REALSXP, nrows(), Rf_ncols(y)));
    double * pRetMat = REAL(retMat);
    RType * pY = DataPtr<RType>(y);
    int nrRetMat = Rf_nrows(retMat);
    int ncRetMat = Rf_ncols(retMat);
    int nrY = Rf_nrows(y);
    R_xlen_t len = XLENGTH(retMat);
    for ( int k = 0; k < len; k++ )
        pRetMat[k] = 0;
    switch(S4class()) {
        case MATTER_MATC:
            for ( int j = 0; j < ncols(); j++ ) {
                MatterIterator<T> x(*this, j);
                int i = 0;
                while ( x ) {
                    for ( int jj = 0; jj < ncRetMat; jj++ ) {
                        double val = (*x) * coerce_cast<RType,T>(pY[j + jj * nrY]);
                        pRetMat[i + jj * nrRetMat] += val;
                    }
                    i++;
                    ++x;
                }
            }
            break;
        case MATTER_MATR:
            for ( int i = 0; i < nrows(); i++ ) {
                MatterIterator<T> x(*this, i);
                int j = 0;
                while ( x ) {
                    for ( int jj = 0; jj < ncRetMat; jj++ ) {
                        double val = (*x) * coerce_cast<RType,T>(pY[j + jj * nrY]);
                        pRetMat[i + jj * nrRetMat] += val;
                    }
                    j++;
                    ++x;
                }
            }
            break;
        default:
            Rf_error("unrecognised matrix subclass");
    }
    UNPROTECT(1);
    return retMat;
}

template<typename T, typename RType, int SType>
SEXP Matter :: lmult(SEXP x) {
    SEXP retMat;
    PROTECT(retMat = Rf_allocMatrix(REALSXP, Rf_nrows(x), ncols()));
    double * pRetMat = REAL(retMat);
    RType * pX = DataPtr<RType>(x);
    int nrRetMat = Rf_nrows(retMat);
    int nrX = Rf_nrows(x);
    int len = LENGTH(retMat);
    for ( int k = 0; k < len; k++ )
        pRetMat[k] = 0;
    switch(S4class()) {
        case MATTER_MATC:
            for ( int j = 0; j < ncols(); j++ ) {
                MatterIterator<T> y(*this, j);
                int i = 0;
                while ( y ) {
                    for ( int ii = 0; ii < nrRetMat; ii++ ) {
                        double val = coerce_cast<RType,T>(pX[ii + i * nrX]) * (*y);
                        pRetMat[ii + j * nrRetMat] += val;
                    }
                    i++;
                    ++y;
                }
            }
            break;
        case MATTER_MATR:
            for ( int i = 0; i < nrows(); i++ ) {
                MatterIterator<T> y(*this, i);
                int j = 0;
                while ( y ) {
                    for ( int ii = 0; ii < nrRetMat; ii++ ) {
                        double val = coerce_cast<RType,T>(pX[ii + i * nrX]) * (*y);
                        pRetMat[ii + j * nrRetMat] += val;
                    }
                    j++;
                    ++y;
                }
            }
            break;
        default:
            Rf_error("unrecognised matrix subclass");
    }
    UNPROTECT(1);
    return retMat;
}

SEXP Matter :: which() {
    SEXP retVec;
    size_t buffersize = chunksize(), i = 1, count = 0;
    int * buffer = Calloc(buffersize, int);
    MatterIterator<int> x(*this);
    while ( x ) {
        if ( count >= buffersize ) {
            buffersize = count * 2;
            int * tmp = Realloc(buffer, buffersize, int);
            if ( !tmp ) {
                Free(buffer);
                Rf_error("could not allocate return vector");
            }
            buffer = tmp;
        }
        if ( *x == TRUE ) {
            buffer[count] = i;
            count++;
        }
        ++x;
        i++;
    }
    PROTECT(retVec = Rf_allocVector(INTSXP, count));
    memcpy(INTEGER(retVec), buffer, sizeof(int) * count);
    Free(buffer);
    UNPROTECT(1);
    return retVec;
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
        PROTECT(natoms = Rf_allocVector(INTSXP, 1));
        PROTECT(ngroups = Rf_allocVector(INTSXP, 1));
        PROTECT(index_offset = Rf_allocVector(REALSXP, n));
        PROTECT(index_extent = Rf_allocVector(REALSXP, n));
        pIndexOffset = REAL(index_offset);
        pIndexExtent = REAL(index_extent);
        int gid, g_cur = 0, g_n = 0;
        index_t cum_index = 0;
        for ( int i = 0; i < n; i++ ) {
            gid = vGroupID[i];
            if ( gid != g_cur ) {
                g_cur = gid;
                cum_index = 0;
                g_n++;
            }
            pIndexOffset[i] = static_cast<double>(cum_index);
            cum_index += static_cast<index_t>(vExtent[i]);
            pIndexExtent[i] = static_cast<double>(cum_index);
        }
        INTEGER(natoms)[0] = n;
        INTEGER(ngroups)[0] = g_n;
        SEXP classDef, retObj;
        PROTECT(classDef = R_do_MAKE_CLASS("atoms"));
        PROTECT(retObj = R_do_new_object(classDef));
        R_do_slot_assign(retObj, Rf_install("natoms"), natoms);
        R_do_slot_assign(retObj, Rf_install("ngroups"), ngroups);
        R_do_slot_assign(retObj, Rf_install("group_id"), group_id);
        R_do_slot_assign(retObj, Rf_install("source_id"), source_id);
        R_do_slot_assign(retObj, Rf_install("datamode"), datamode);
        R_do_slot_assign(retObj, Rf_install("offset"), offset);
        R_do_slot_assign(retObj, Rf_install("extent"), extent);
        R_do_slot_assign(retObj, Rf_install("index_offset"), index_offset);
        R_do_slot_assign(retObj, Rf_install("index_extent"), index_extent);
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

    SEXP getList(SEXP x) {
        Matter mVec(x);
        SEXP retVec;
        PROTECT(retVec = Rf_allocVector(VECSXP, mVec.length()));
        for ( int i = 0; i < mVec.length(); i++ ) {
            switch(mVec.datamode(i)) {
                case R_RAW:
                    SET_VECTOR_ELT(retVec, i, mVec.readListElements<Rbyte,RAWSXP>(i));
                    break;
                case R_LOGICAL:
                    SET_VECTOR_ELT(retVec, i, mVec.readListElements<int,LGLSXP>(i));
                    break;
                case R_INTEGER:
                    SET_VECTOR_ELT(retVec, i, mVec.readListElements<int,INTSXP>(i));
                    break;
                case R_NUMERIC:
                    SET_VECTOR_ELT(retVec, i, mVec.readListElements<double,REALSXP>(i));
                    break;
                case R_CHARACTER:
                    SET_VECTOR_ELT(retVec, i, mVec.readListElements<Rbyte,RAWSXP>(i));
                    break;
                default:
                    SET_VECTOR_ELT(retVec, i, R_NilValue);
                    break;
            }
        }
        UNPROTECT(1);
        return retVec;
    }

    void setList(SEXP x, SEXP value) {
        Matter mVec(x);
        for ( int i = 0; i < mVec.length(); i++ ) {
            SEXP elt = VECTOR_ELT(value, i);
            switch(TYPEOF(elt)) {
                case RAWSXP:
                    mVec.writeListElements<Rbyte,RAWSXP>(i, elt);
                    break;
                case LGLSXP:
                    mVec.writeListElements<int,LGLSXP>(i, elt);
                    break;
                case INTSXP:
                    mVec.writeListElements<int,INTSXP>(i, elt);
                    break;
                case REALSXP:
                    mVec.writeListElements<double,REALSXP>(i, elt);
                    break;
            }
        }
    }

    SEXP getListElements(SEXP x, SEXP i, SEXP j) {
        Matter mVec(x);
        if ( j == R_NilValue ) {
            switch(mVec.datamode(Rf_asInteger(i))) {
                case R_RAW:
                    return mVec.readListElements<Rbyte,RAWSXP>(Rf_asInteger(i));
                case R_LOGICAL:
                    return mVec.readListElements<int,LGLSXP>(Rf_asInteger(i));
                case R_INTEGER:
                    return mVec.readListElements<int,INTSXP>(Rf_asInteger(i));
                case R_NUMERIC:
                    return mVec.readListElements<double,REALSXP>(Rf_asInteger(i));
                case R_CHARACTER:
                    return mVec.readListElements<Rbyte,RAWSXP>(Rf_asInteger(i));
                default:
                    return R_NilValue;
            }
        } else {
            switch(mVec.datamode(Rf_asInteger(i))) {
                case R_RAW:
                    return mVec.readListElements<Rbyte,RAWSXP>(Rf_asInteger(i), j);
                case R_LOGICAL:
                    return mVec.readListElements<int,LGLSXP>(Rf_asInteger(i), j);
                case R_INTEGER:
                    return mVec.readListElements<int,INTSXP>(Rf_asInteger(i), j);
                case R_NUMERIC:
                    return mVec.readListElements<double,REALSXP>(Rf_asInteger(i), j);
                case R_CHARACTER:
                    return mVec.readListElements<Rbyte,RAWSXP>(Rf_asInteger(i));
                default:
                    return R_NilValue;
            }
        }
    }

    void setListElements(SEXP x, SEXP i, SEXP j, SEXP value) {
        Matter mVec(x);
        if ( j == R_NilValue ) {
            switch(TYPEOF(value)) {
                case RAWSXP:
                    mVec.writeListElements<Rbyte,RAWSXP>(Rf_asInteger(i), value);
                    break;
                case LGLSXP:
                    mVec.writeListElements<int,LGLSXP>(Rf_asInteger(i), value);
                    break;
                case INTSXP:
                    mVec.writeListElements<int,INTSXP>(Rf_asInteger(i), value);
                    break;
                case REALSXP:
                    mVec.writeListElements<double,REALSXP>(Rf_asInteger(i), value);
                    break;
            }
        } else {
            switch(TYPEOF(value)) {
                case RAWSXP:
                    mVec.writeListElements<Rbyte,RAWSXP>(Rf_asInteger(i), j, value);
                    break;
                case LGLSXP:
                    mVec.writeListElements<int,LGLSXP>(Rf_asInteger(i), j, value);
                    break;
                case INTSXP:
                    mVec.writeListElements<int,INTSXP>(Rf_asInteger(i), j, value);
                    break;
                case REALSXP:
                    mVec.writeListElements<double,REALSXP>(Rf_asInteger(i), j, value);
                    break;
            }
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

    SEXP getSparseVector(SEXP x) {
        SparseVector sVec(x);
        SEXP retVec;
        switch(sVec.datamode()) {
            case R_INTEGER:
                PROTECT(retVec = Rf_allocVector(INTSXP, sVec.length()));
                switch(TYPEOF(sVec.keys())) {
                    case INTSXP:
                        sVec.getRegion<int,int>(0, sVec.length(), INTEGER(retVec));
                        break;
                    case REALSXP:
                        sVec.getRegion<double,int>(0, sVec.length(), INTEGER(retVec));
                        break;
                }
                break;
            case R_NUMERIC:
                PROTECT(retVec = Rf_allocVector(REALSXP, sVec.length()));
                switch(TYPEOF(sVec.keys())) {
                    case INTSXP:
                        sVec.getRegion<int,double>(0, sVec.length(), REAL(retVec));
                        break;
                    case REALSXP:
                        sVec.getRegion<double,double>(0, sVec.length(), REAL(retVec));
                        break;
                }
                break;
        }
        UNPROTECT(1);
        return retVec;
    }

    // SEXP getSparseVectorElements(SEXP x, SEXP i) {

    // }

    SEXP getString(SEXP x) {
        Matter mVec(x);
        SEXP retVec;
        PROTECT(retVec = Rf_allocVector(STRSXP, mVec.length()));
        for ( int i = 0; i < mVec.length(); i++ )
            SET_STRING_ELT(retVec, i, raw_to_char(mVec.readListElements<Rbyte,RAWSXP>(i)));
        UNPROTECT(1);
        return retVec;
    }

    SEXP getStringElements(SEXP x, SEXP i) {
        Matter mVec(x);
        SEXP retVec;
        Rindex_t * ii = INDEX_PTR(i);
        PROTECT(retVec = Rf_allocVector(STRSXP, XLENGTH(i)));
        for ( int j = 0; j < XLENGTH(i); j++ ) {
            int k = static_cast<int>(ii[j]);
            if ( ISNA(ii[j]) )
                SET_STRING_ELT(retVec, j, NA_STRING);
            else
                SET_STRING_ELT(retVec, j, raw_to_char(mVec.readListElements<Rbyte,RAWSXP>(k)));
        }
        UNPROTECT(1);
        return retVec;   
    }

    SEXP getRange(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        switch ( mMat.datamode() ) {
            case R_LOGICAL:
                return mMat.range<double>(Rf_asLogical(na_rm));
            case R_INTEGER:
                return mMat.range<double>(Rf_asLogical(na_rm));
            case R_NUMERIC:
                return mMat.range<double>(Rf_asLogical(na_rm));
            default:
                return R_NilValue;
        }
    }

    SEXP getProd(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        switch ( mMat.datamode() ) {
            case R_LOGICAL:
                return mMat.prod<double>(Rf_asLogical(na_rm));
            case R_INTEGER:
                return mMat.prod<double>(Rf_asLogical(na_rm));
            case R_NUMERIC:
                return mMat.prod<double>(Rf_asLogical(na_rm));
            default:
                return R_NilValue;
        }
    }

    SEXP getSum(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        switch ( mMat.datamode() ) {
            case R_LOGICAL:
                return mMat.sum<double>(Rf_asLogical(na_rm));
            case R_INTEGER:
                return mMat.sum<double>(Rf_asLogical(na_rm));
            case R_NUMERIC:
                return mMat.sum<double>(Rf_asLogical(na_rm));
            default:
                return R_NilValue;
        }
    }

    SEXP getMean(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        switch ( mMat.datamode() ) {
            case R_LOGICAL:
                return mMat.mean<double>(Rf_asLogical(na_rm));
            case R_INTEGER:
                return mMat.mean<double>(Rf_asLogical(na_rm));
            case R_NUMERIC:
                return mMat.mean<double>(Rf_asLogical(na_rm));
            default:
                return R_NilValue;
        }
    }

    SEXP getVar(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        switch ( mMat.datamode() ) {
            case R_LOGICAL:
                return mMat.var<double>(Rf_asLogical(na_rm));
            case R_INTEGER:
                return mMat.var<double>(Rf_asLogical(na_rm));
            case R_NUMERIC:
                return mMat.var<double>(Rf_asLogical(na_rm));
            default:
                return R_NilValue;
        }
    }

    SEXP getAny(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        switch ( mMat.datamode() ) {
            case R_LOGICAL:
                return mMat.any<int>(Rf_asLogical(na_rm));
            case R_INTEGER:
                return mMat.any<int>(Rf_asLogical(na_rm));
            case R_NUMERIC:
                return mMat.any<int>(Rf_asLogical(na_rm));
            default:
                return R_NilValue;
        }
    }

    SEXP getAll(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        switch ( mMat.datamode() ) {
            case R_LOGICAL:
                return mMat.all<int>(Rf_asLogical(na_rm));
            case R_INTEGER:
                return mMat.all<int>(Rf_asLogical(na_rm));
            case R_NUMERIC:
                return mMat.all<int>(Rf_asLogical(na_rm));
            default:
                return R_NilValue;
        }
    }

    SEXP getColSums(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        switch ( mMat.datamode() ) {
            case R_LOGICAL:
                return mMat.colsums<double>(Rf_asLogical(na_rm));
            case R_INTEGER:
                return mMat.colsums<double>(Rf_asLogical(na_rm));
            case R_NUMERIC:
                return mMat.colsums<double>(Rf_asLogical(na_rm));
            default:
                return R_NilValue;
        }
    }

    SEXP getColMeans(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        switch ( mMat.datamode() ) {
            case R_LOGICAL:
                return mMat.colmeans<double>(Rf_asLogical(na_rm));
            case R_INTEGER:
                return mMat.colmeans<double>(Rf_asLogical(na_rm));
            case R_NUMERIC:
                return mMat.colmeans<double>(Rf_asLogical(na_rm));
            default:
                return R_NilValue;
        }
    }

    SEXP getColVars(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        switch ( mMat.datamode() ) {
            case R_LOGICAL:
                return mMat.colvar<double>(Rf_asLogical(na_rm));
            case R_INTEGER:
                return mMat.colvar<double>(Rf_asLogical(na_rm));
            case R_NUMERIC:
                return mMat.colvar<double>(Rf_asLogical(na_rm));
            default:
                return R_NilValue;
        }
    }

    SEXP getRowSums(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        switch ( mMat.datamode() ) {
            case R_LOGICAL:
                return mMat.rowsums<double>(Rf_asLogical(na_rm));
            case R_INTEGER:
                return mMat.rowsums<double>(Rf_asLogical(na_rm));
            case R_NUMERIC:
                return mMat.rowsums<double>(Rf_asLogical(na_rm));
            default:
                return R_NilValue;
        }
    }

    SEXP getRowMeans(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        switch ( mMat.datamode() ) {
            case R_LOGICAL:
                return mMat.rowmeans<double>(Rf_asLogical(na_rm));
            case R_INTEGER:
                return mMat.rowmeans<double>(Rf_asLogical(na_rm));
            case R_NUMERIC:
                return mMat.rowmeans<double>(Rf_asLogical(na_rm));
            default:
                return R_NilValue;
        }
    }

    SEXP getRowVars(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        switch ( mMat.datamode() ) {
            case R_LOGICAL:
                return mMat.rowvar<double>(Rf_asLogical(na_rm));
            case R_INTEGER:
                return mMat.rowvar<double>(Rf_asLogical(na_rm));
            case R_NUMERIC:
                return mMat.rowvar<double>(Rf_asLogical(na_rm));
            default:
                return R_NilValue;
        }
    }

    SEXP rightMatrixMult(SEXP x, SEXP y) {
        Matter mMat(x);
        switch ( mMat.datamode() ) {
            case R_LOGICAL:
            case R_INTEGER:
                if ( TYPEOF(y) == LGLSXP )
                    return mMat.rmult<double,int,LGLSXP>(y);
                else if ( TYPEOF(y) == INTSXP )
                    return mMat.rmult<double,int,INTSXP>(y);
                else if ( TYPEOF(y) == REALSXP )
                    return mMat.rmult<double,double,REALSXP>(y);
                return R_NilValue;
            case R_NUMERIC:
                if ( TYPEOF(y) == LGLSXP )
                    return mMat.rmult<double,int,LGLSXP>(y);
                else if ( TYPEOF(y) == INTSXP )
                    return mMat.rmult<double,int,INTSXP>(y);
                else if ( TYPEOF(y) == REALSXP )
                    return mMat.rmult<double,double,REALSXP>(y);
                return R_NilValue;
            default:
                return R_NilValue;
        }
    }

    SEXP leftMatrixMult(SEXP x, SEXP y) {
        Matter mMat(y);
        switch ( mMat.datamode() ) {
            case R_LOGICAL:
            case R_INTEGER:
                if ( TYPEOF(x) == LGLSXP )
                    return mMat.lmult<double,int,LGLSXP>(x);
                else if ( TYPEOF(x) == INTSXP )
                    return mMat.lmult<double,int,INTSXP>(x);
                else if ( TYPEOF(x) == REALSXP )
                    return mMat.lmult<double,double,REALSXP>(x);
                return R_NilValue;
            case R_NUMERIC:
                if ( TYPEOF(x) == LGLSXP )
                    return mMat.lmult<double,int,LGLSXP>(x);
                else if ( TYPEOF(x) == INTSXP )
                    return mMat.lmult<double,int,INTSXP>(x);
                else if ( TYPEOF(x) == REALSXP )
                    return mMat.lmult<double,double,REALSXP>(x);
                return R_NilValue;
            default:
                return R_NilValue;
        }
    }

    SEXP getWhich(SEXP x) {
        Matter mVec(x);
        return mVec.which();
    }

    SEXP countRuns(SEXP x, SEXP delta) {
        SEXP ret;
        PROTECT(ret = Rf_allocVector(INTSXP, 1));
        if ( TYPEOF(x) == INTSXP )
        {
            INTEGER(ret)[0] = count_runs<int>(INTEGER(x),
                LENGTH(x), Rf_asLogical(delta));
        }
        else if ( TYPEOF(x) == REALSXP )
        {
            INTEGER(ret)[0] = count_runs<double>(REAL(x),
                LENGTH(x), Rf_asLogical(delta));
        }
        UNPROTECT(1);
        return ret;
    }

    SEXP createDRLE(SEXP x, SEXP nruns, SEXP delta) {
        if ( TYPEOF(x) == INTSXP )
        {
            return makeDRLE<int>(x, nruns, Rf_asLogical(delta));
        }
        else if ( TYPEOF(x) == REALSXP )
        {
            return makeDRLE<double>(x, nruns, Rf_asLogical(delta));;
        }
        return R_NilValue;
    }

    SEXP getDRLE(SEXP x) {
        SEXP values = R_do_slot(x, Rf_install("values"));
        if ( TYPEOF(values) == INTSXP )
        {
            VectorOrDRLE<int,INTSXP> dVec(x);
            return dVec.decode();
        }
        else if ( TYPEOF(values) == REALSXP )
        {
            VectorOrDRLE<double,REALSXP> dVec(x);
            return dVec.decode();
        }
        return R_NilValue;
    }

    SEXP getDRLEElements(SEXP x, SEXP i) {
        SEXP values = R_do_slot(x, Rf_install("values"));
        if ( TYPEOF(values) == INTSXP )
        {
            VectorOrDRLE<int,INTSXP> dVec(x);
            return dVec.decodeElements(i);
        }
        else if ( TYPEOF(values) == REALSXP )
        {
            VectorOrDRLE<double,REALSXP> dVec(x);
            return dVec.decodeElements(i);
        }
        return R_NilValue;
    }

}
