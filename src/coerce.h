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
		if ( x != NA_INT32 )
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
		if ( x == NA_INT32 )
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
		if ( x == NA_INT64 )
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

// int16

template<> inline
int16_t coerce_cast(int32_t x) {
	if ( x < R_INT16_MIN || x > R_INT16_MAX )
	{
		if ( x != NA_INT32 )
			Rf_warning("value is out of range for type 'int16', element will be set to NA");
		return NA_INT16;
	}
	return static_cast<int16_t>(x);
}

template<> inline
int16_t coerce_cast(double x) {
	if ( x < R_INT16_MIN || x > R_INT16_MAX || !R_FINITE(x) )
	{
		if ( !ISNA(x) )
			Rf_warning("value is out of range for type 'int16', element will be set to NA");
		return NA_INT16;
	}
	return static_cast<int16_t>(x);
}

// unsigned int16

template<> inline
uint16_t coerce_cast(int32_t x) {
	if ( x < 0 || x > R_UINT16_MAX )
	{
		if ( x == NA_INT32 )
			Rf_warning("NAs not supported for type 'uint16', element will be set to 0");
		else
			Rf_warning("value is out of range for type 'uint16', element will be set to 0");
		return 0;
	}
	return static_cast<uint16_t>(x);
}

template<> inline
uint16_t coerce_cast(double x) {
	if ( x < 0 || x > R_UINT16_MAX || !R_FINITE(x) )
	{
		if ( ISNA(x) )
			Rf_warning("NAs not supported for type 'uint16', element will be set to 0");
		else
			Rf_warning("value is out of range for type 'uint16', element will be set to 0");
		return 0;
	}
	return static_cast<uint16_t>(x);
}

// int32

template<> inline
int32_t coerce_cast(char x) {
	if ( x == NA_CHAR )
		return NA_INT32;
	else
		return static_cast<int32_t>(x);
}

template<> inline
int32_t coerce_cast(int16_t x) {
	if ( x == NA_INT16 )
		return NA_INT32;
	else
		return static_cast<int32_t>(x);
}

template<> inline
int32_t coerce_cast(uint32_t x) {
	if ( x > R_INT32_MAX )
	{
		Rf_warning("value is out of range for type 'int32', element will be set to NA");
		return NA_INT32;
	}
	return static_cast<int32_t>(x);
}

template<> inline
int32_t coerce_cast(int64_t x) {
	if ( x < R_INT32_MIN || x > R_INT32_MAX )
	{
		if ( x != NA_INT64 )
			Rf_warning("value is out of range for type 'int32', element will be set to NA");
		return NA_INT32;
	}
	else
		return static_cast<int>(x);
}

template<> inline
int32_t coerce_cast(uint64_t x) {
	if ( x > R_INT32_MAX )
	{
		Rf_warning("value is out of range for type 'int32', element will be set to NA");
		return NA_INT32;
	}
	return static_cast<int32_t>(x);
}

template<> inline
int32_t coerce_cast(float x) {
	if ( std::isnan(x) )
		return NA_INT32;
	else
		return static_cast<int32_t>(x);
}

template<> inline
int32_t coerce_cast(double x) {
	if ( x < R_INT32_MIN || x > R_INT32_MAX || !R_FINITE(x) )
	{
		if ( !ISNA(x) )
			Rf_warning("value is out of range for type 'int32', element will be set to NA");
		return NA_INT32;
	}
	return static_cast<int32_t>(x);
}

// unsigned int32

template<> inline
uint32_t coerce_cast(int32_t x) {
	if ( x < 0 )
	{
		if ( x == NA_INT32 )
			Rf_warning("NAs not supported for type 'uint32', element will be set to 0");
		else
			Rf_warning("value is out of range for type 'uint32', element will be set to 0");
		return 0;
	}
	return static_cast<uint32_t>(x);
}

template<> inline
uint32_t coerce_cast(double x) {
	if ( x < 0 || x > R_UINT32_MAX || !R_FINITE(x) )
	{
		if ( ISNA(x) )
			Rf_warning("NAs not supported for type 'uint32', element will be set to 0");
		else
			Rf_warning("value is out of range for type 'uint32', element will be set to 0");
		return 0;
	}
	return static_cast<uint32_t>(x);
}

// int64

template<> inline
int64_t coerce_cast(int32_t x) {
	if ( x == NA_INT32 )
		return NA_INT64;
	else
		return static_cast<int64_t>(x);
}

template<> inline
int64_t coerce_cast(double x) {
	if ( !R_FINITE(x) )
	{
		if ( ISNA(x) )
			Rf_warning("value is out of range for type 'int64', element will be set to NA");
		return NA_INT64;
	}
	return static_cast<int64_t>(x);
}

// unsigned int64

template<> inline
uint64_t coerce_cast(int32_t x) {
	if ( x < 0 )
	{
		if ( x == NA_INT32 )
			Rf_warning("NAs not supported for type 'uint64', element will be set to 0");
		else
			Rf_warning("value is out of range for type 'uint64', element will be set to 0");
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
			Rf_warning("NAs not supported for type 'uint64', element will be set to 0");
		else
			Rf_warning("value is out of range for type 'uint64', element will be set to 0");
		return 0;
	}
	return static_cast<uint64_t>(x);
}

// float32

template<> inline
float coerce_cast(int32_t x) {
	if ( x == NA_INT32 )
		return static_cast<float>(NA_REAL);
	else
		return static_cast<float>(x);
}

// float64

template<> inline
double coerce_cast(int16_t x) {
	if ( x == NA_INT16 )
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
	if ( x == NA_INT64 )
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
	Rf_error("don't know how to coerce 'SEXP' to 'int32'");
}

template<> inline
double coerce_cast(SEXP x) {
	Rf_error("don't know how to coerce 'SEXP' to 'float64'");
}

#endif // COERCE
