#ifndef MATTER2
#define MATTER2

#include "matterDefines.h"
#include "atoms.h"

//// Matter class
//----------------

class Matter2 {

	public:

		Matter2(SEXP x) : _data(R_do_slot(x, Rf_install("data")))
		{
			_type = R_do_slot(x, Rf_install("type"));
			_dim = R_do_slot(x, Rf_install("dim"));
			_names = R_do_slot(x, Rf_install("names"));
			_dimnames = R_do_slot(x, Rf_install("dimnames"));
		}

		~Matter2() {
			_data.self_destruct();
		}

		void self_destruct() {
			_data.self_destruct();
		}

		Atoms2 * data() {
			return &_data;
		}

		int type() {
			return INTEGER_ELT(_type, 0);
		}

		int type(int i) {
			if ( XLENGTH(_type) > i )
				return INTEGER_ELT(_type, i);
			else
				return NA_INTEGER;
		}

		int rank() {
			return LENGTH(_dim);
		}

		SEXP dim() {
			return _dim;
		}

		R_xlen_t dim(int i) {
			if ( i < rank() )
				return IndexElt(_dim, i);
			else
				return NA_INTEGER;
		}

		R_xlen_t length() {
			R_xlen_t size = 1;
			for ( size_t i = 0; i < rank(); i++ )
				size *= dim(i);
			return size;
		}

		SEXP names() {
			return _names;
		}

		SEXP names(int i) {
			if ( XLENGTH(_names) > i )
				return STRING_ELT(_names, i);
			else
				return NA_STRING;
		}

		SEXP dimnames() {
			return _dimnames;
		}

		SEXP dimnames(int i) {
			if ( i < rank() )
				return VECTOR_ELT(_dimnames, i);
			else
				return R_NilValue;
		}

	protected:

		Atoms2 _data;
		SEXP _type;
		SEXP _dim;
		SEXP _names;
		SEXP _dimnames;

};

class Matter2Array : public Matter2 {

	public:

		Matter2Array(SEXP x) : Matter2(x)
		{
			_ops = R_do_slot(x, Rf_install("ops"));
			_transpose = Rf_asLogical(R_do_slot(x, Rf_install("transpose")));
		}

		bool is_transposed() {
			return _transpose;
		}

		// convert col-major to row-major index
		template<typename T>
		size_t transpose_range(T * tindx, index_t i, size_t size, bool ind1 = false)
		{
			size_t nind = 0;
			int s1 [rank()], s2 [rank()];
			int arr_ind [rank()];
			s1[0] = 1, s2[rank()] = 1;
			for ( int j = 1; j < rank(); j++ )
				s1[j] = s1[j - 1] * dim(j - 1);
			for ( int j = rank() - 2; j >= 0; j-- )
				s2[j] = s1[j + 1] * dim(j + 1);
			for ( size_t k = 0; k < size; k++ )
			{
				tindx[k] = 0;
				for ( int j = 0; j < rank(); j++ )
					arr_ind[j] = ((i + k) / s1[j]) % dim(j);
				for ( int j = 0; j < rank(); j++ )
					tindx[k] += arr_ind[j] * s2[j];
				tindx[k] += ind1;
				nind++;
			}
			return nind;
		}

		// convert col-major to row-major index
		template<typename T>
		size_t transpose_index(T * tindx, SEXP indx, bool ind1 = false)
		{
			size_t nind = 0;
			index_t s1 [rank()];
			index_t s2 [rank()];
			index_t arr_ind [rank()];
			s1[0] = 1, s2[rank()] = 1;
			for ( int j = 1; j < rank(); j++ )
				s1[j] = s1[j - 1] * dim(j - 1);
			for ( int j = rank() - 2; j >= 0; j-- )
				s2[j] = s1[j + 1] * dim(j + 1);
			for ( size_t k = 0; k < XLENGTH(indx); k++ )
			{
				tindx[k] = 0;
				for ( int j = 0; j < rank(); j++ )
					arr_ind[j] = ((IndexElt(indx, k) - ind1) / s1[j]) % dim(j);
				for ( int j = 0; j < rank(); j++ )
					tindx[k] += arr_ind[j] * s2[j];
				tindx[k] += ind1;
				nind++;
			}
			return nind;
		}

		template<typename T>
		size_t getRegion(index_t i, size_t size, T * buffer, int stride = 1)
		{
			if ( is_transposed() )
			{
				index_t tindx [size];
				transpose_range(tindx, i, size);
				return data()->flatten()->get_elements<index_t,T>(buffer, tindx, size, 0, stride);
			}
			else
				return data()->flatten()->get_region<T>(buffer, i, size, 0, stride);
		}

		template<typename T>
		size_t setRegion(index_t i, size_t size, T * buffer, int stride = 1)
		{
			if ( is_transposed() )
			{
				index_t tindx [size];
				transpose_range(tindx, i, size);
				return data()->flatten()->set_elements<index_t,T>(buffer, tindx, size, 0, stride);
			}
			else
				return data()->flatten()->set_region<T>(buffer, i, size, 0, stride);
		}

		SEXP getRegion(index_t i, size_t size)
		{
			SEXP x;
			switch(type()) {
				case R_RAW:
					PROTECT(x = Rf_allocVector(RAWSXP, size));
					getRegion(i, size, RAW(x));
					break;
				case R_LOGICAL:
					PROTECT(x = Rf_allocVector(LGLSXP, size));
					getRegion(i, size, LOGICAL(x));
					break;
				case R_INTEGER:
					PROTECT(x = Rf_allocVector(INTSXP, size));
					getRegion(i, size, INTEGER(x));
					break;
				case R_DOUBLE:
					PROTECT(x = Rf_allocVector(REALSXP, size));
					getRegion(i, size, REAL(x));
					break;
				default:
					self_destruct();
					Rf_error("invalid matter array data type");
			}
			UNPROTECT(1);
			return x;
		}

		void setRegion(index_t i, size_t size, SEXP value)
		{
			int stride = XLENGTH(value) <= 1 ? 0 : 1;
			if ( size > XLENGTH(value) && stride != 0 )
				size = XLENGTH(value);
			switch(TYPEOF(value)) {
				case RAWSXP:
					setRegion(i, size, RAW(value), stride);
					break;
				case LGLSXP:
					setRegion(i, size, LOGICAL(value), stride);
					break;
				case INTSXP:
					setRegion(i, size, INTEGER(value), stride);
					break;
				case REALSXP:
					setRegion(i, size, REAL(value), stride);
					break;
				default:
					self_destruct();
					Rf_error("invalid replacement data type");
			}
		}

		template<typename T>
		size_t getElements(SEXP indx, T * buffer, int stride = 1)
		{
			if ( is_transposed() )
			{
				R_xlen_t size = XLENGTH(indx);
				index_t tindx [size];
				transpose_index(tindx, indx, true);
				return data()->flatten()->get_elements<index_t,T>(buffer, tindx, size, 0, stride);
			}
			else
				return data()->flatten()->get_elements<T>(buffer, indx, 0, stride);
		}

		template<typename T>
		size_t setElements(SEXP indx, T * buffer, int stride = 1)
		{
			if ( is_transposed() )
			{
				R_xlen_t size = XLENGTH(indx);
				index_t tindx [size];
				transpose_index(tindx, indx, true);
				return data()->flatten()->set_elements<index_t,T>(buffer, tindx, size, 0, stride);
			}
			else
				return data()->flatten()->set_elements<T>(buffer, indx, 0, stride);
		}

		SEXP getElements(SEXP indx)
		{
			SEXP x;
			if ( Rf_isNull(indx) )
				return getRegion(0, length());
			switch(type()) {
				case R_RAW:
					PROTECT(x = Rf_allocVector(RAWSXP, XLENGTH(indx)));
					getElements(indx, RAW(x));
					break;
				case R_LOGICAL:
					PROTECT(x = Rf_allocVector(LGLSXP, XLENGTH(indx)));
					getElements(indx, LOGICAL(x));
					break;
				case R_INTEGER:
					PROTECT(x = Rf_allocVector(INTSXP, XLENGTH(indx)));
					getElements(indx, INTEGER(x));
					break;
				case R_DOUBLE:
					PROTECT(x = Rf_allocVector(REALSXP, XLENGTH(indx)));
					getElements(indx, REAL(x));
					break;
				default:
					self_destruct();
					Rf_error("invalid matter array data type");
			}
			UNPROTECT(1);
			return x;
		}

		void setElements(SEXP indx, SEXP value)
		{
			int stride = XLENGTH(value) <= 1 ? 0 : 1;
			if ( Rf_isNull(indx) )
				return setRegion(0, length(), value);
			switch(TYPEOF(value)) {
				case RAWSXP:
					setElements(indx, RAW(value), stride);
					break;
				case LGLSXP:
					setElements(indx, LOGICAL(value), stride);
					break;
				case INTSXP:
					setElements(indx, INTEGER(value), stride);
					break;
				case REALSXP:
					setElements(indx, REAL(value), stride);
					break;
				default:
					self_destruct();
					Rf_error("invalid replacement data type");
			}
		}

	protected:

		SEXP _ops;
		bool _transpose;

};

#endif // MATTER2
