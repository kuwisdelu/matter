#ifndef COERCE
#define COERCE

#include "matterDefines.h"

//// Type coercion 
//------------------

// defaults

template<typename Tout, typename Tin>
Tout coerce_cast(Tin x) {
	return static_cast<Tout>(x);
}

// char

template<> inline
char coerce_cast(int32_t x) {
	if ( x < R_CHAR_MIN || x > R_CHAR_MAX )
	{
		if ( x != NA_INTEGER )
			Rf_warning("value is out of range for type 'char', element will be set to NA");
		return NA_CHAR;
	}
	return static_cast<char>(x);
}

template<> inline
char coerce_cast(double x) {
	if ( x < R_CHAR_MIN || x > R_CHAR_MAX || !R_FINITE(x) )
	{
		if ( !ISNA(x) )
			Rf_warning("value is out of range for type 'char', element will be set to NA");
		return NA_CHAR;
	}
	return static_cast<char>(x);
}

// unsigned char

template<> inline
Rbyte coerce_cast(char x) {
	if ( x < 0 )
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

template<> inline
Rbyte coerce_cast(int16_t x) {
	if ( x < 0 || x > R_UCHAR_MAX )
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

template<> inline
Rbyte coerce_cast(uint16_t x) {
	if ( x > R_UCHAR_MAX )
	{
		Rf_warning("value is out of range for type 'uchar', element will be set to 0");
		return 0;   
	}
	return static_cast<Rbyte>(x);
}

template<> inline
Rbyte coerce_cast(int32_t x) {
	if ( x < 0 || x > R_UCHAR_MAX )
	{
		if ( x == NA_INTEGER )
			Rf_warning("NAs not supported for type 'uchar', element will be set to 0");
		else
			Rf_warning("value is out of range for type 'uchar', element will be set to 0");
		return 0;
	}
	return static_cast<Rbyte>(x);
}

template<> inline
Rbyte coerce_cast(uint32_t x) {
	if ( x > R_UCHAR_MAX )
	{
		Rf_warning("value is out of range for type 'uchar', element will be set to 0");
		return 0;
	}
	return static_cast<Rbyte>(x);
}

template<> inline
Rbyte coerce_cast(int64_t x) {
	if ( x < 0 || x > R_UCHAR_MAX )
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

template<> inline
Rbyte coerce_cast(uint64_t x) {
	if ( x > R_UCHAR_MAX )
	{
		Rf_warning("value is out of range for type 'uchar', element will be set to 0");
		return 0;
	}
	return static_cast<Rbyte>(x);
}

template<> inline
Rbyte coerce_cast(float x) {
	if ( x < 0 || x > R_UCHAR_MAX || std::isnan(x) )
	{
		Rf_warning("value is out of range for type 'uchar', element will be set to 0");
		return 0;
	}
	else
		return static_cast<Rbyte>(x);
}

template<> inline
Rbyte coerce_cast(double x) {
	if ( x < 0 || x > R_UCHAR_MAX || !R_FINITE(x) )
	{
		if ( ISNA(x) )
			Rf_warning("NAs not supported for type 'uchar', element will be set to 0");
		else
			Rf_warning("value is out of range for type 'uchar', element will be set to 0");
		return 0;
	}
	return static_cast<Rbyte>(x);
}

// short

template<> inline
int16_t coerce_cast(int32_t x) {
	if ( x < R_SHORT_MIN || x > R_SHORT_MAX )
	{
		if ( x != NA_INTEGER )
			Rf_warning("value is out of range for type 'short', element will be set to NA");
		return NA_SHORT;
	}
	return static_cast<int16_t>(x);
}

template<> inline
int16_t coerce_cast(double x) {
	if ( x < R_SHORT_MIN || x > R_SHORT_MAX || !R_FINITE(x) )
	{
		if ( !ISNA(x) )
			Rf_warning("value is out of range for type 'short', element will be set to NA");
		return NA_SHORT;
	}
	return static_cast<int16_t>(x);
}

// unsigned short

template<> inline
uint16_t coerce_cast(int32_t x) {
	if ( x < 0 || x > R_USHORT_MAX )
	{
		if ( x == NA_INTEGER )
			Rf_warning("NAs not supported for type 'ushort', element will be set to 0");
		else
			Rf_warning("value is out of range for type 'ushort', element will be set to 0");
		return 0;
	}
	return static_cast<uint16_t>(x);
}

template<> inline
uint16_t coerce_cast(double x) {
	if ( x < 0 || x > R_USHORT_MAX || !R_FINITE(x) )
	{
		if ( ISNA(x) )
			Rf_warning("NAs not supported for type 'ushort', element will be set to 0");
		else
			Rf_warning("value is out of range for type 'ushort', element will be set to 0");
		return 0;
	}
	return static_cast<uint16_t>(x);
}

// int

template<> inline
int32_t coerce_cast(char x) {
	if ( x == NA_CHAR )
		return NA_INTEGER;
	else
		return static_cast<int32_t>(x);
}

template<> inline
int32_t coerce_cast(short x) {
	if ( x == NA_SHORT )
		return NA_INTEGER;
	else
		return static_cast<int32_t>(x);
}

template<> inline
int32_t coerce_cast(uint32_t x) {
	if ( x > R_INT_MAX )
	{
		Rf_warning("value is out of range for type 'int', element will be set to NA");
		return NA_INTEGER;
	}
	return static_cast<int32_t>(x);
}

template<> inline
int32_t coerce_cast(int64_t x) {
	if ( x < R_INT_MIN || x > R_INT_MAX )
	{
		if ( x != NA_LONG )
			Rf_warning("value is out of range for type 'int', element will be set to NA");
		return NA_INTEGER;
	}
	else
		return static_cast<int>(x);
}

template<> inline
int32_t coerce_cast(uint64_t x) {
	if ( x > R_INT_MAX )
	{
		Rf_warning("value is out of range for type 'int', element will be set to NA");
		return NA_INTEGER;
	}
	return static_cast<int32_t>(x);
}

template<> inline
int32_t coerce_cast(float x) {
	if ( std::isnan(x) )
		return NA_INTEGER;
	else
		return static_cast<int32_t>(x);
}

template<> inline
int32_t coerce_cast(double x) {
	if ( x < R_INT_MIN || x > R_INT_MAX || !R_FINITE(x) )
	{
		if ( !ISNA(x) )
			Rf_warning("value is out of range for type 'int', element will be set to NA");
		return NA_INTEGER;
	}
	return static_cast<int32_t>(x);
}

// unsigned int

template<> inline
uint32_t coerce_cast(int32_t x) {
	if ( x < 0 )
	{
		if ( x == NA_INTEGER )
			Rf_warning("NAs not supported for type 'uint', element will be set to 0");
		else
			Rf_warning("value is out of range for type 'uint', element will be set to 0");
		return 0;
	}
	return static_cast<uint32_t>(x);
}

template<> inline
uint32_t coerce_cast(double x) {
	if ( x < 0 || x > R_UINT_MAX || !R_FINITE(x) )
	{
		if ( ISNA(x) )
			Rf_warning("NAs not supported for type 'uint', element will be set to 0");
		else
			Rf_warning("value is out of range for type 'uint', element will be set to 0");
		return 0;
	}
	return static_cast<uint32_t>(x);
}

// long

template<> inline
int64_t coerce_cast(int32_t x) {
	if ( x == NA_INTEGER )
		return NA_LONG;
	else
		return static_cast<int64_t>(x);
}

template<> inline
int64_t coerce_cast(double x) {
	if ( !R_FINITE(x) )
	{
		if ( ISNA(x) )
			Rf_warning("value is out of range for type 'long', element will be set to NA");
		return NA_LONG;
	}
	return static_cast<int64_t>(x);
}

// unsigned long

template<> inline
uint64_t coerce_cast(int32_t x) {
	if ( x < 0 )
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

template<> inline
uint64_t coerce_cast(double x) {
	if ( x < 0 || !R_FINITE(x) )
	{
		if ( ISNA(x) )
			Rf_warning("NAs not supported for type 'ulong', element will be set to 0");
		else
			Rf_warning("value is out of range for type 'ulong', element will be set to 0");
		return 0;
	}
	return static_cast<uint64_t>(x);
}

// float

template<> inline
float coerce_cast(int32_t x) {
	if ( x == NA_INTEGER )
		return static_cast<float>(NA_REAL);
	else
		return static_cast<float>(x);
}

// double

template<> inline
double coerce_cast(int16_t x) {
	if ( x == NA_SHORT )
		return NA_REAL;
	else
		return static_cast<double>(x);
}

template<> inline
double coerce_cast(int32_t x) {
	if ( isNA(x) )
		return NA_REAL;
	else
		return static_cast<double>(x);
}

template<> inline
double coerce_cast(int64_t x) {
	if ( x == NA_LONG )
		return NA_REAL;
	else
		return static_cast<double>(x);
}

// SEXP

template<> inline
Rbyte coerce_cast(SEXP x) {
	Rf_error("don't know how to coerce 'SEXP' to 'uchar'");
}

template<> inline
int32_t coerce_cast(SEXP x) {
	Rf_error("don't know how to coerce 'SEXP' to 'int'");
}

template<> inline
double coerce_cast(SEXP x) {
	Rf_error("don't know how to coerce 'SEXP' to 'double'");
}

#endif // COERCE
