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
			return INTEGER_ELT(_type, i % XLENGTH(_type));
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
			if ( XLENGTH(_names) > i && !Rf_isNull(_names) )
				return STRING_ELT(_names, i);
			else
				return NA_STRING;
		}

		SEXP dimnames() {
			return _dimnames;
		}

		SEXP dimnames(int i) {
			if ( i < rank() && !Rf_isNull(_dimnames) )
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
			s1[0] = 1, s2[rank() - 1] = 1;
			for ( int j = 1; j < rank(); j++ ) {
				s1[j] = s1[j - 1] * dim(j - 1);
				// Rprintf("stride[%d] = %d\n", j, s1[j]);
			}
			for ( int j = rank() - 2; j >= 0; j-- ) {
				s2[j] = s2[j + 1] * dim(j + 1);
				// Rprintf("trans stride[%d] = %d\n", j, s2[j]);
			}
			for ( size_t k = 0; k < size; k++ )
			{
				tindx[k] = 0;
				for ( int j = 0; j < rank(); j++ )
					arr_ind[j] = ((i + k) / s1[j]) % dim(j);
				for ( int j = 0; j < rank(); j++ )
					tindx[k] += arr_ind[j] * s2[j];
				tindx[k] += ind1;
				nind++;
				// Rprintf("i = %d, trans i = %d\n", i + k, tindx[k]);
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
			s1[0] = 1, s2[rank() - 1] = 1;
			for ( int j = 1; j < rank(); j++ ) {
				s1[j] = s1[j - 1] * dim(j - 1);
				// Rprintf("stride[%d] = %d\n", j, s1[j]);
			}
			for ( int j = rank() - 2; j >= 0; j-- ) {
				s2[j] = s2[j + 1] * dim(j + 1);
				// Rprintf("trans stride[%d] = %d\n", j, s2[j]);
			}
			for ( size_t k = 0; k < XLENGTH(indx); k++ )
			{
				index_t i = IndexElt(indx, k);
				if ( isNA(i) ) {
					tindx[k] = NA<T>();
					nind++;
					continue;
				}
				tindx[k] = 0;
				for ( int j = 0; j < rank(); j++ )
					arr_ind[j] = ((i - ind1) / s1[j]) % dim(j);
				for ( int j = 0; j < rank(); j++ )
					tindx[k] += arr_ind[j] * s2[j];
				tindx[k] += ind1;
				nind++;
				// Rprintf("i = %d, trans i = %d\n", i, tindx[k]);
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

		template<typename T>
		size_t getElements(SEXP indx, T * buffer, int stride = 1)
		{
			if ( is_transposed() )
			{
				R_xlen_t size = XLENGTH(indx);
				index_t tindx [size];
				transpose_index(tindx, indx, true);
				return data()->flatten()->get_elements<index_t,T>(buffer, tindx, size, 0, stride, true);
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
				return data()->flatten()->set_elements<index_t,T>(buffer, tindx, size, 0, stride, true);
			}
			else
				return data()->flatten()->set_elements<T>(buffer, indx, 0, stride);
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
			int stride = XLENGTH(value) == 1 ? 0 : 1;
			if ( size > XLENGTH(value) && stride != 0 ) {
				self_destruct();
				Rf_error("number of items to replace is ",
					"longer than replacement length");
			}
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
			if ( Rf_isNull(indx) )
				return setRegion(0, length(), value);
			int stride = XLENGTH(value) == 1 ? 0 : 1;
			if ( XLENGTH(indx) > XLENGTH(value) && stride != 0 ) {
				self_destruct();
				Rf_error("number of items to replace is ",
					"longer than replacement length");
			}
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

class Matter2List : public Matter2 {

	public:

		Matter2List(SEXP x) : Matter2(x) {}

		template<typename T>
		size_t getElement(index_t i, SEXP j, T * buffer)
		{
			return data()->get_elements<T>(buffer, j, i);
		}

		template<typename T>
		size_t setElement(index_t i, SEXP j, T * buffer)
		{
			return data()->set_elements<T>(buffer, j, i);
		}

		template<typename T>
		size_t getElement(index_t i, T * buffer)
		{
			return data()->get_region<T>(buffer, 0, dim(i), i);
		}

		template<typename T>
		size_t setElement(index_t i, T * buffer)
		{
			return data()->set_region<T>(buffer, 0, dim(i), i);
		}

		SEXP getElement(index_t i, SEXP j)
		{
			SEXP x;
			switch(type(i)) {
				case R_RAW:
					PROTECT(x = Rf_allocVector(RAWSXP, XLENGTH(j)));
					getElement(i, j, RAW(x));
					break;
				case R_LOGICAL:
					PROTECT(x = Rf_allocVector(LGLSXP, XLENGTH(j)));
					getElement(i, j, LOGICAL(x));
					break;
				case R_INTEGER:
					PROTECT(x = Rf_allocVector(INTSXP, XLENGTH(j)));
					getElement(i, j, INTEGER(x));
					break;
				case R_DOUBLE:
					PROTECT(x = Rf_allocVector(REALSXP, XLENGTH(j)));
					getElement(i, j, REAL(x));
					break;
				case R_CHARACTER:
					{
						PROTECT(x = Rf_allocVector(RAWSXP, XLENGTH(j)));
						getElement(i, RAW(x));
						const char * c = reinterpret_cast<const char *>(RAW(x));
						R_xlen_t len = strlen(c);
						if ( len < XLENGTH(j) )
							Rf_warning("truncating string with embedded nuls");
						else
							len = dim(i);
						PROTECT(x = Rf_ScalarString(Rf_mkCharLen(c, len)));
						UNPROTECT(1);
						break;
					}
				default:
					self_destruct();
					Rf_error("unsupported data type");
			}
			UNPROTECT(1);
			return x;
		}

		void setElement(index_t i, SEXP j, SEXP value)
		{
			if ( XLENGTH(j) > XLENGTH(value) ) {
				self_destruct();
				Rf_error("number of items to replace is ",
					"longer than replacement length");
			}
			switch(TYPEOF(value)) {
				case R_RAW:
					setElement(i, j, RAW(value));
					break;
				case R_LOGICAL:
					setElement(i, j, LOGICAL(value));
					break;
				case R_INTEGER:
					setElement(i, j, INTEGER(value));
					break;
				case R_DOUBLE:
					setElement(i, j, REAL(value));
					break;
				case R_CHARACTER:
					self_destruct();
					Rf_error("character assignment not allowed here");
					break;
				default:
					self_destruct();
					Rf_error("unsupported data type");
			}
		}

		SEXP getElement(index_t i)
		{
			SEXP x;
			switch(type(i)) {
				case R_RAW:
					PROTECT(x = Rf_allocVector(RAWSXP, dim(i)));
					getElement(i, RAW(x));
					break;
				case R_LOGICAL:
					PROTECT(x = Rf_allocVector(LGLSXP, dim(i)));
					getElement(i, LOGICAL(x));
					break;
				case R_INTEGER:
					PROTECT(x = Rf_allocVector(INTSXP, dim(i)));
					getElement(i, INTEGER(x));
					break;
				case R_DOUBLE:
					PROTECT(x = Rf_allocVector(REALSXP, dim(i)));
					getElement(i, REAL(x));
					break;
				case R_CHARACTER:
				{
					PROTECT(x = Rf_allocVector(RAWSXP, dim(i)));
					getElement(i, RAW(x));
					const char * c = reinterpret_cast<const char *>(RAW(x));
					R_xlen_t len = strlen(c);
					if ( len < dim(i) )
						Rf_warning("truncating string with embedded nuls");
					else
						len = dim(i);
					PROTECT(x = Rf_ScalarString(Rf_mkCharLen(c, len)));
					UNPROTECT(1);
					break;
				}
				default:
					self_destruct();
					Rf_error("unsupported data type");
			}
			UNPROTECT(1);
			return x;
		}

		void setElement(index_t i, SEXP value)
		{
			if ( dim(i) > XLENGTH(value) ) {
				self_destruct();
				Rf_error("number of items to replace is ",
					"longer than replacement length");
			}
			switch(TYPEOF(value)) {
				case R_RAW:
					setElement(i, RAW(value));
					break;
				case R_LOGICAL:
					setElement(i, LOGICAL(value));
					break;
				case R_INTEGER:
					setElement(i, INTEGER(value));
					break;
				case R_DOUBLE:
					setElement(i, REAL(value));
					break;
				case R_CHARACTER:
					self_destruct();
					Rf_error("character assignment not allowed here");
					break;
				default:
					self_destruct();
					Rf_error("unsupported data type");
			}
		}

		SEXP getElements(SEXP i, SEXP j)
		{
			SEXP x;
			PROTECT(x = Rf_allocVector(VECSXP, XLENGTH(i)));
			for ( size_t k = 0; k < XLENGTH(i); k++ )
				SET_VECTOR_ELT(x, k, getElement(IndexElt(i, k), j));
			UNPROTECT(1);
			return x;
		}

		void setElements(SEXP i, SEXP j, SEXP value)
		{
			if ( XLENGTH(i) != XLENGTH(value) ) {
				self_destruct();
				Rf_error("number of items to replace ",
					"does not match replacement length");
			}
			for ( size_t k = 0; k < XLENGTH(i); k++ )
				setElement(IndexElt(i, k), j, VECTOR_ELT(value, k));
		}

		SEXP getElements(SEXP i)
		{
			SEXP x;
			PROTECT(x = Rf_allocVector(VECSXP, XLENGTH(i)));
			for ( size_t k = 0; k < XLENGTH(i); k++ )
				SET_VECTOR_ELT(x, k, getElement(IndexElt(i, k)));
			UNPROTECT(1);
			return x;
		}

		void setElements(SEXP i, SEXP value)
		{
			if ( XLENGTH(i) != XLENGTH(value) ) {
				self_destruct();
				Rf_error("number of items to replace ",
					"does not match replacement length");
			}
			for ( size_t k = 0; k < XLENGTH(i); k++ )
				setElement(IndexElt(i, k), VECTOR_ELT(value, k));
		}

};


#endif // MATTER2
