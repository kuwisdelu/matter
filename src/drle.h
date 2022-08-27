#ifndef DRLE
#define DRLE

#include "Rutils.h"

//// Delta run length encoding 
//-----------------------------

template<typename T>
Pair<T,R_xlen_t> compute_run(T * x, size_t i, size_t len, bool nz = true)
{
	R_xlen_t n = 1;
	T delta = 0;
	if ( nz )
		delta = (i + 1 < len) ? x[i + 1] - x[i] : 0;
	while( i < len - 1 )
	{
		T delta2 = x[i + 1] - x[i];
		bool both_na = isNA(x[i] && isNA(x[i + 1]));
		if ( equal<T>(delta, delta2) || both_na ) {
			n++;
			i++;
		}
		else
			break;
	}
	if ( len <= 2 && !equal<T>(delta, 0) )
	{
		R_xlen_t n2 = 1;
		T delta2 = x[i + 2] - x[i + 1];
		while( i + 1 < len - 1 )
		{
			T delta3 = x[i + 2] - x[i + 1];
			bool both_na = isNA(x[i + 1] && isNA(x[i + 2]));
			if ( equal<T>(delta2, delta3) || both_na ) {
				n2++;
				i++;
			}
			else
				break;
		}
		if ( n2 > n ) {
			n = 1;
			delta = 0;
		}
	}
	Pair<T,R_xlen_t> run = {delta, n};
	return run;
}

template<typename T>
R_xlen_t num_runs(T * x, R_xlen_t len, bool nz = true)
{
	R_xlen_t i = 0, n = 0;
	while ( i < len )
	{
		Pair<T,R_xlen_t> run = compute_run<T>(x, i, len, nz);
		i += run.second;
		n++;
	}
	return n;
}

R_xlen_t num_runs(SEXP x, bool nz = true)
{
	R_xlen_t len = XLENGTH(x);
	switch(TYPEOF(x)) {
		case INTSXP:
			return num_runs<int>(INTEGER(x), len, nz);
		case REALSXP:
			return num_runs<double>(REAL(x), len, nz);
		default:
			Rf_error("unsupported data type");
	}
}

template<typename Tv, typename Tl>
size_t encode_drle(Tv * x, size_t len, Tv * values,
	Tv * deltas, Tl * lengths, size_t nruns)
{
	size_t i = 0, j = 0;
	while ( i < len && j < nruns )
	{
		Pair<Tv,R_xlen_t> run = compute_run<Tv>(x, i, len, true);
		values[j] = static_cast<Tv>(x[i]);
		deltas[j] = static_cast<Tv>(run.first);
		lengths[j] = static_cast<Tl>(run.second);
		i += lengths[j];
		j++;
	}
	return j;
}

SEXP encode_drle(SEXP x, bool is_long_vec = false)
{
	SEXP values, deltas, lengths;
	SEXPTYPE lengthstype = is_long_vec ? REALSXP : INTSXP;
	switch(TYPEOF(x)) {
		case INTSXP: {
			size_t nruns = num_runs(INTEGER(x), XLENGTH(x), true);
			PROTECT(values = Rf_allocVector(INTSXP, nruns));
			PROTECT(deltas = Rf_allocVector(INTSXP, nruns));
			PROTECT(lengths = Rf_allocVector(lengthstype, nruns));
			switch(lengthstype) {
				case INTSXP:
					encode_drle<int,int>(INTEGER(x), XLENGTH(x), INTEGER(values),
						INTEGER(deltas), INTEGER(lengths), nruns);
					break;
				case REALSXP:
					encode_drle<int,double>(INTEGER(x), XLENGTH(x), INTEGER(values),
						INTEGER(deltas), REAL(lengths), nruns);
					break;
			}
			break;
		}
		case REALSXP: {
			size_t nruns = num_runs(REAL(x), XLENGTH(x), true);
			PROTECT(values = Rf_allocVector(REALSXP, nruns));
			PROTECT(deltas = Rf_allocVector(REALSXP, nruns));
			PROTECT(lengths = Rf_allocVector(lengthstype, nruns));
			switch(lengthstype) {
				case INTSXP:
					encode_drle<double,int>(REAL(x), XLENGTH(x), REAL(values),
						REAL(deltas), INTEGER(lengths), nruns);
					break;
				case REALSXP:
					encode_drle<double,double>(REAL(x), XLENGTH(x), REAL(values),
						REAL(deltas), REAL(lengths), nruns);
					break;
			}
			break;
		}
		default:
			Rf_error("unsupported data type");
	}	
	SEXP classDef, obj;
	PROTECT(classDef = R_do_MAKE_CLASS("drle"));
	PROTECT(obj = R_do_new_object(classDef));
	R_do_slot_assign(obj, Rf_install("values"), values);
	R_do_slot_assign(obj, Rf_install("deltas"), deltas);
	R_do_slot_assign(obj, Rf_install("lengths"), lengths);
	UNPROTECT(5);
	return obj;
}

template<typename Tv, typename Tl>
size_t decode_drle(Tv * x, size_t len, Tv * values,
	Tv * deltas, Tl * lengths, size_t nruns)
{
	size_t n = 0;
	for ( size_t j = 0; j < nruns; j++ )
	{
		size_t nj = static_cast<size_t>(lengths[j]);
		for ( size_t i = 0; i < nj; i++ )
			x[n + i] = values[j] + i * deltas[j];
		n += nj;
	}
	return n;
}

SEXP decode_drle(SEXP values, SEXP deltas, SEXP lengths)
{
	size_t nruns = XLENGTH(lengths);
	R_xlen_t len = 0;
	switch(TYPEOF(lengths)) {
		case INTSXP:
			for ( size_t i = 0; i < nruns; i++ )
				len += INTEGER_ELT(lengths, i);
			break;
		case REALSXP:
			for ( size_t i = 0; i < nruns; i++ )
				len += REAL_ELT(lengths, i);
			break;
	}
	SEXP x;
	PROTECT(x = Rf_allocVector(TYPEOF(values), len));
	switch(TYPEOF(values)) {
		case INTSXP:
			switch(TYPEOF(lengths)) {
				case INTSXP:
					decode_drle<int,int>(INTEGER(x), len, INTEGER(values),
						INTEGER(deltas), INTEGER(lengths), nruns);
					break;
				case REALSXP:
					decode_drle<int,double>(INTEGER(x), len, INTEGER(values),
						INTEGER(deltas), REAL(lengths), nruns);
					break;
			}
			break;
		case REALSXP:
			switch(TYPEOF(lengths)) {
				case INTSXP:
					decode_drle<double,int>(REAL(x), len, REAL(values),
						REAL(deltas), INTEGER(lengths), nruns);
					break;
				case REALSXP:
					decode_drle<double,double>(REAL(x), len, REAL(values),
						REAL(deltas), REAL(lengths), nruns);
					break;
			}
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return x;
}

template<typename T>
class CompressedVector {

	public:

		CompressedVector(SEXP x)
		{
			if ( Rf_isS4(x) )
			{
				_type = TYPEOF(R_do_slot(x, Rf_install("values")));
				_pvalues = DataPtr<T>(R_do_slot(x, Rf_install("values")));
				_pdeltas = DataPtr<T>(R_do_slot(x, Rf_install("deltas")));
				_lengths = R_do_slot(x, Rf_install("lengths"));
				_truelength = XLENGTH(R_do_slot(x, Rf_install("values")));
				_length = 0;
				switch(TYPEOF(_lengths)) {
					case INTSXP:
						for ( size_t i = 0; i < _truelength; i++ )
							_length += INTEGER_ELT(_lengths, i);
						break;
					case REALSXP:
						for ( size_t i = 0; i < _truelength; i++ )
							_length += REAL_ELT(_lengths, i);
						break;
				}
				_is_compressed = true;
			}
			else
			{
				_type = TYPEOF(x);
				_pvalues = DataPtr<T>(x);
				_length = XLENGTH(x);
				_truelength = _length;
				_is_compressed = false;
			}
		}

		~CompressedVector() {}

		R_xlen_t length() {
			return _length;
		}

		R_xlen_t truelength() {
			return _truelength;
		}

		bool is_compressed() {
			return _is_compressed;
		}

		T values(index_t i) {
			if ( i < 0 || i >= truelength() )
				Rf_error("subscript out of bounds");
			return _pvalues[i];
		}

		T deltas(index_t i) {
			if ( !is_compressed() )
				return NA<T>();
			if ( i < 0 || i >= truelength() )
				Rf_error("subscript out of bounds");
			return _pdeltas[i];
		}

		R_xlen_t lengths(index_t i) {
			if ( !is_compressed() )
				return NA_INTEGER;
			if ( i < 0 || i >= truelength() )
				Rf_error("subscript out of bounds");
			switch(TYPEOF(_lengths)) {
				case INTSXP:
					return static_cast<R_xlen_t>(INTEGER_ELT(_lengths, i));
				case REALSXP:
					return static_cast<R_xlen_t>(REAL_ELT(_lengths, i));
				default:
					Rf_error("invalid lengths type");
			}
		}

		T get(index_t i)
		{
			if ( i < 0 || i >= length()  )
				Rf_error("subscript out of bounds");
			if ( isNA(i) )
				return NA<T>();
			if ( !is_compressed() )
				return values(i);
			index_t j = _last_index; // track last run accessed
			index_t run = _last_run; // to give ~O(1) iter access
			if ( i >= j )
			{
				while ( j < length() && run < truelength() ) {
					if ( i < j + lengths(run) ) {
						_last_index = j;
						_last_run = run;
						return values(run) + deltas(run) * (i - j);
					}
					else {
						j += lengths(run);
						run++;
					}
				}
				return NA<T>();
			}
			else
			{
				while ( j >= 0 && run >= 0 ) {
					if ( i >= j ) {
						_last_index = j;
						_last_run = run;
						return values(run) + deltas(run) * (i - j);
					}
					else
					{
						run--;
						j -= lengths(run);
					}
				}
				return NA<T>();
			}
		}

		size_t getRegion(index_t i, size_t size, T * buffer)
		{
			size_t j;
			for ( j = 0; j < size; j++ )
				buffer[j] = get(i + j);
			return j;
		}

		SEXP getRegion(index_t i, size_t size)
		{
			SEXP x;
			PROTECT(x = Rf_allocVector(_type, size));
			getRegion(i, size, DataPtr<T>(x));
			UNPROTECT(1);
			return x;
		}

		size_t getElements(SEXP indx, T * buffer)
		{
			R_xlen_t size = XLENGTH(indx);
			size_t j;
			for ( j = 0; j < size; j++ )
			{
				index_t i = IndexElt(indx, j);
				buffer[j] = get(i - 1);
			}
			return j;
		}

		SEXP getElements(SEXP indx)
		{
			SEXP x;
			if ( indx == R_NilValue )
				return getRegion(0, length());
			PROTECT(x = Rf_allocVector(_type, XLENGTH(indx)));
			getElements(indx, DataPtr<T>(x));
			UNPROTECT(1);
			return x;
		}

		index_t find(T value)
		{
			if ( !is_compressed() )
			{
				index_t i = 0;
				for ( size_t run = 0; run < truelength(); run++ ) {
					T delta = value - values(run);
					T mvalue = values(run) + (deltas(run) * (lengths(run) - 1));
					if ( equal<T>(delta, 0) )
						return i;
					if ( value <= mvalue && equal(fmod(delta, deltas(run)), 0) )
						return i + static_cast<index_t>(delta / deltas(run));
					i += lengths(run);
				}
			}
			else
			{
				for ( index_t i = 0; i < length(); i++ )
					if ( equal<T>(get(i), value) )
						return i;
			}
			return NA_INTEGER;
		}

		T operator[](index_t i) {
			return get(i);
		}

	protected:

		SEXPTYPE _type;
		T * _pvalues;
		T * _pdeltas;
		SEXP _lengths;
		R_xlen_t _length;
		R_xlen_t _truelength;
		index_t _last_index = 0; // cache most recent access
		index_t _last_run = 0;   // cache most recent access
		bool _is_compressed;

};

#endif // DRLE
