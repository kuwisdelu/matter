#ifndef MATTER
#define MATTER

#include "matterDefines.h"
#include "atoms.h"
#include "dops.h"

//// Matter class
//----------------

class Matter {

	public:

		Matter(SEXP x) : _data(R_do_slot(x, Rf_install("data")))
		{
			_type = R_do_slot(x, Rf_install("type"));
			_dim = R_do_slot(x, Rf_install("dim"));
			_names = R_do_slot(x, Rf_install("names"));
			_dimnames = R_do_slot(x, Rf_install("dimnames"));
		}

		~Matter() {
			_data.self_destruct();
		}

		void self_destruct() {
			_data.self_destruct();
		}

		Atoms * data() {
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

		Atoms _data;
		SEXP _type;
		SEXP _dim;
		SEXP _names;
		SEXP _dimnames;

};

class MatterArray : public Matter {

	public:

		MatterArray(SEXP x) : Matter(x), _ops(x)
		{
			_transpose = Rf_asLogical(R_do_slot(x, Rf_install("transpose")));
		}

		R_xlen_t length() {
			R_xlen_t size = 1;
			for ( size_t i = 0; i < rank(); i++ )
				size *= dim(i);
			return size;
		}

		bool is_transposed() {
			return _transpose;
		}

		DeferredOps * ops() {
			return &_ops;
		}

		bool has_ops() {
			return ops()->nops() > 0;
		}

		// convert col-major to row-major index
		template<typename T>
		size_t transpose_range(T * tindx, index_t i, size_t size, bool ind1 = false)
		{
			size_t nind = 0;
			int s1 [rank()], s2 [rank()];
			int arr_ind [rank()];
			s1[0] = 1, s2[rank() - 1] = 1;
			for ( int k = 1; k < rank(); k++ )
				s1[k] = s1[k - 1] * dim(k - 1);
			for ( int k = rank() - 2; k >= 0; k-- )
				s2[k] = s2[k + 1] * dim(k + 1);
			for ( size_t j = 0; j < size; j++ )
			{
				tindx[j] = 0;
				for ( int k = 0; k < rank(); k++ )
					arr_ind[k] = ((i + j) / s1[k]) % dim(k);
				for ( int k = 0; k < rank(); k++ )
					tindx[j] += arr_ind[k] * s2[k];
				tindx[j] += ind1;
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
			s1[0] = 1, s2[rank() - 1] = 1;
			for ( int k = 1; k < rank(); k++ )
				s1[k] = s1[k - 1] * dim(k - 1);
			for ( int k = rank() - 2; k >= 0; k-- )
				s2[k] = s2[k + 1] * dim(k + 1);
			for ( size_t j = 0; j < XLENGTH(indx); j++ )
			{
				index_t i = IndexElt(indx, j);
				if ( isNA(i) ) {
					tindx[j] = NA<T>();
					nind++;
					continue;
				}
				tindx[j] = 0;
				for ( int k = 0; k < rank(); k++ )
					arr_ind[k] = ((i - ind1) / s1[k]) % dim(k);
				for ( int k = 0; k < rank(); k++ )
					tindx[j] += arr_ind[k] * s2[k];
				tindx[j] += ind1;
				nind++;
			}
			return nind;
		}

		template<typename T>
		size_t get_region(index_t i, size_t size, T * buffer, int stride = 1)
		{
			R_xlen_t len = length();
			size = len - i > size ? size : len - i;
			if ( is_transposed() )
			{
				index_t tindx [size];
				transpose_range(tindx, i, size);
				data()->flatten()->get_elements<index_t,T>(buffer, tindx, size, 0, stride);
			}
			else
				data()->flatten()->get_region<T>(buffer, i, size, 0, stride);
			if ( has_ops() )
				ops()->apply<T>(buffer, i, size, stride);
			return size;
		}

		template<typename T>
		size_t set_region(index_t i, size_t size, T * buffer, int stride = 1)
		{
			if ( has_ops() ) {
				self_destruct();
				Rf_error("can't assign to array with deferred operations");
			}
			R_xlen_t len = length();
			size = len - i > size ? size : len - i;
			if ( is_transposed() )
			{
				index_t tindx [size];
				transpose_range(tindx, i, size);
				data()->flatten()->set_elements<index_t,T>(buffer, tindx, size, 0, stride);
			}
			else
				data()->flatten()->set_region<T>(buffer, i, size, 0, stride);
			return size;
		}

		template<typename T>
		size_t get_elements(SEXP indx, T * buffer, int stride = 1)
		{
			R_xlen_t size = XLENGTH(indx);
			if ( is_transposed() )
			{
				index_t tindx [size];
				transpose_index(tindx, indx, true);
				data()->flatten()->get_elements<index_t,T>(buffer, tindx, size, 0, stride, true);
			}
			else
				data()->flatten()->get_elements<T>(buffer, indx, 0, stride);
			if ( has_ops() )
				ops()->apply<T>(buffer, indx, stride);
			return size;
		}

		template<typename T>
		size_t set_elements(SEXP indx, T * buffer, int stride = 1)
		{
			if ( has_ops() ) {
				self_destruct();
				Rf_error("can't assign to array with deferred operations");
			}
			R_xlen_t size = XLENGTH(indx);
			if ( is_transposed() )
			{
				index_t tindx [size];
				transpose_index(tindx, indx, true);
				data()->flatten()->set_elements<index_t,T>(buffer, tindx, size, 0, stride, true);
			}
			else
				data()->flatten()->set_elements<T>(buffer, indx, 0, stride);
			return size;
		}

		SEXP get_region(index_t i, size_t size)
		{
			SEXP x;
			switch(type()) {
				case R_RAW:
					PROTECT(x = Rf_allocVector(RAWSXP, size));
					get_region(i, size, RAW(x));
					break;
				case R_LOGICAL:
					PROTECT(x = Rf_allocVector(LGLSXP, size));
					get_region(i, size, LOGICAL(x));
					break;
				case R_INTEGER:
					PROTECT(x = Rf_allocVector(INTSXP, size));
					get_region(i, size, INTEGER(x));
					break;
				case R_DOUBLE:
					PROTECT(x = Rf_allocVector(REALSXP, size));
					get_region(i, size, REAL(x));
					break;
				default:
					self_destruct();
					Rf_error("invalid matter array data type");
			}
			UNPROTECT(1);
			return x;
		}

		void set_region(index_t i, size_t size, SEXP value)
		{
			int stride = XLENGTH(value) == 1 ? 0 : 1;
			if ( size > XLENGTH(value) && stride != 0 ) {
				self_destruct();
				Rf_error("number of items to replace is longer than replacement length");
			}
			switch(TYPEOF(value)) {
				case RAWSXP:
					set_region(i, size, RAW(value), stride);
					break;
				case LGLSXP:
					set_region(i, size, LOGICAL(value), stride);
					break;
				case INTSXP:
					set_region(i, size, INTEGER(value), stride);
					break;
				case REALSXP:
					set_region(i, size, REAL(value), stride);
					break;
				default:
					self_destruct();
					Rf_error("invalid replacement data type");
			}
		}

		SEXP get_elements(SEXP indx)
		{
			SEXP x;
			if ( Rf_isNull(indx) )
				return get_region(0, length());
			switch(type()) {
				case R_RAW:
					PROTECT(x = Rf_allocVector(RAWSXP, XLENGTH(indx)));
					get_elements(indx, RAW(x));
					break;
				case R_LOGICAL:
					PROTECT(x = Rf_allocVector(LGLSXP, XLENGTH(indx)));
					get_elements(indx, LOGICAL(x));
					break;
				case R_INTEGER:
					PROTECT(x = Rf_allocVector(INTSXP, XLENGTH(indx)));
					get_elements(indx, INTEGER(x));
					break;
				case R_DOUBLE:
					PROTECT(x = Rf_allocVector(REALSXP, XLENGTH(indx)));
					get_elements(indx, REAL(x));
					break;
				default:
					self_destruct();
					Rf_error("invalid matter array data type");
			}
			UNPROTECT(1);
			return x;
		}

		void set_elements(SEXP indx, SEXP value)
		{
			if ( Rf_isNull(indx) )
				return set_region(0, length(), value);
			int stride = XLENGTH(value) == 1 ? 0 : 1;
			if ( XLENGTH(indx) > XLENGTH(value) && stride != 0 ) {
				self_destruct();
				Rf_error("number of items to replace is longer than replacement length");
			}
			switch(TYPEOF(value)) {
				case RAWSXP:
					set_elements(indx, RAW(value), stride);
					break;
				case LGLSXP:
					set_elements(indx, LOGICAL(value), stride);
					break;
				case INTSXP:
					set_elements(indx, INTEGER(value), stride);
					break;
				case REALSXP:
					set_elements(indx, REAL(value), stride);
					break;
				default:
					self_destruct();
					Rf_error("invalid replacement data type");
			}
		}

	protected:

		DeferredOps _ops;
		bool _transpose;

};

class MatterList : public Matter {

	public:

		MatterList(SEXP x) : Matter(x) {}

		R_xlen_t length() {
			return rank();
		}

		SEXP get(index_t i)
		{
			SEXP x;
			if ( i < 0 || i >= length() )
				Rf_error("subscript out of bounds");
			switch(type(i)) {
				case R_RAW:
					PROTECT(x = Rf_allocVector(RAWSXP, dim(i)));
					data()->get_region<Rbyte>(RAW(x), 0, dim(i), i);
					break;
				case R_LOGICAL:
					PROTECT(x = Rf_allocVector(LGLSXP, dim(i)));
					data()->get_region<int>(LOGICAL(x), 0, dim(i), i);
					break;
				case R_INTEGER:
					PROTECT(x = Rf_allocVector(INTSXP, dim(i)));
					data()->get_region<int>(INTEGER(x), 0, dim(i), i);
					break;
				case R_DOUBLE:
					PROTECT(x = Rf_allocVector(REALSXP, dim(i)));
					data()->get_region<double>(REAL(x), 0, dim(i), i);
					break;
				case R_CHARACTER: {
					char s [dim(i)];
					data()->get_region<char>(s, 0, dim(i), i);
					R_xlen_t len = strlen(s);
					if ( len < dim(i) )
						Rf_warning("truncating string with embedded nuls");
					len = len < dim(i) ? len : dim(i);
					PROTECT(x = Rf_ScalarString(Rf_mkCharLen(s, len)));
					break;
				}
				default:
					self_destruct();
					Rf_error("unsupported data type");
			}
			UNPROTECT(1);
			return x;
		}

		void set(index_t i, SEXP value)
		{
			if ( i < 0 || i >= length() )
				Rf_error("subscript out of bounds");
			if ( !Rf_isString(value) && dim(i) != LENGTH(value) ) {
				self_destruct();
				Rf_error("length of replacement value and items to replace are not equal");
			}
			switch(TYPEOF(value)) {
				case RAWSXP:
					data()->set_region<Rbyte>(RAW(value), 0, dim(i), i);
					break;
				case LGLSXP:
					data()->set_region<int>(LOGICAL(value), 0, dim(i), i);
					break;
				case INTSXP:
					data()->set_region<int>(INTEGER(value), 0, dim(i), i);
					break;
				case REALSXP:
					data()->set_region<double>(REAL(value), 0, dim(i), i);
					break;
				case STRSXP: {
					value = Rf_asChar(value);
					const char * s = CHAR(value);
					if ( LENGTH(value) != dim(i) )
						Rf_error("replacement string is wrong length; is this a multibyte string?");
					data()->set_region<const char>(s, 0, dim(i), i);
					break;
				}
				default:
					self_destruct();
					Rf_error("unsupported data type");
			}
		}

		SEXP get(index_t i, SEXP j)
		{
			SEXP x;
			if ( Rf_isNull(j) )
				return get(i);
			if ( i < 0 || i >= length() )
				Rf_error("subscript out of bounds");
			switch(type(i)) {
				case R_RAW:
					PROTECT(x = Rf_allocVector(RAWSXP, XLENGTH(j)));
					data()->get_elements<Rbyte>(RAW(x), j, i);
					break;
				case R_LOGICAL:
					PROTECT(x = Rf_allocVector(LGLSXP, XLENGTH(j)));
					data()->get_elements<int>(LOGICAL(x), j, i);
					break;
				case R_INTEGER:
					PROTECT(x = Rf_allocVector(INTSXP, XLENGTH(j)));
					data()->get_elements<int>(INTEGER(x), j, i);
					break;
				case R_DOUBLE:
					PROTECT(x = Rf_allocVector(REALSXP, XLENGTH(j)));
					data()->get_elements<double>(REAL(x), j, i);
					break;
				case R_CHARACTER: {
					char s [LENGTH(j)];
					data()->get_elements<char>(s, j, i);
					R_xlen_t len = strlen(s);
					if ( len < LENGTH(j) )
						Rf_warning("truncating string with embedded nuls");
					len = len < LENGTH(j) ? len : LENGTH(j);
					PROTECT(x = Rf_ScalarString(Rf_mkCharLen(s, len)));
					break;
				}
				default:
					self_destruct();
					Rf_error("unsupported data type");
			}
			UNPROTECT(1);
			return x;
		}

		void set(index_t i, SEXP j, SEXP value)
		{
			if ( Rf_isNull(j) )
				return set(i, value);
			if ( i < 0 || i >= length() )
				Rf_error("subscript out of bounds");
			if ( !Rf_isString(value) && LENGTH(j) != LENGTH(value) ) {
				self_destruct();
				Rf_error("length of replacement value and items to replace are not equal");
			}
			switch(TYPEOF(value)) {
				case RAWSXP:
					data()->set_elements<Rbyte>(RAW(value), j, i);
					break;
				case LGLSXP:
					data()->set_elements<int>(LOGICAL(value), j, i);
					break;
				case INTSXP:
					data()->set_elements<int>(INTEGER(value), j, i);
					break;
				case REALSXP:
					data()->set_elements<double>(REAL(value), j, i);
					break;
				case STRSXP: {
					value = Rf_asChar(value);
					const char * s = CHAR(value);
					if ( LENGTH(value) != LENGTH(j) )
						Rf_error("replacement string is wrong length; is this a multibyte string?");
					data()->set_elements<const char>(s, j, i);
					break;
				}
				default:
					self_destruct();
					Rf_error("unsupported data type");
			}
		}

		SEXP get_elements(SEXP i, SEXP j)
		{
			SEXP x;
			R_xlen_t len;
			if ( Rf_isNull(i) )
				len = length();
			else
				len = XLENGTH(i);
			PROTECT(x = Rf_allocVector(VECSXP, len));
			for ( size_t k = 0; k < len; k++ )
			{
				index_t indk;
				if ( Rf_isNull(i) )
					indk = k;
				else
					indk = IndexElt(i, k) - 1;
				SET_VECTOR_ELT(x, k, get(indk, j));
			}
			UNPROTECT(1);
			return x;
		}

		void set_elements(SEXP i, SEXP j, SEXP value)
		{
			R_xlen_t len;
			if ( Rf_isNull(i) )
				len = length();
			else
				len = XLENGTH(i);
			if ( len != LENGTH(value) ) {
				self_destruct();
				Rf_error("length of replacement value and items to replace are not equal");
			}
			for ( size_t k = 0; k < len; k++ )
			{
				index_t indk;
				if ( Rf_isNull(i) )
					indk = k;
				else
					indk = IndexElt(i, k) - 1;
				set(indk, j, VECTOR_ELT(value, k));
			}
		}

		SEXP get_elements(SEXP i)
		{
			return get_elements(i, R_NilValue);
		}

		void set_elements(SEXP i, SEXP value)
		{
			set_elements(i, R_NilValue, value);
		}

};

class MatterStringList : public MatterList {

	public:

		MatterStringList(SEXP x) : MatterList(x)
		{
			if ( type() != R_CHARACTER ) {
				self_destruct();
				Rf_error("matter object is not a string");
			}
		}

		SEXP get_char(index_t i)
		{
			return Rf_asChar(get(i));
		}

		void set_char(index_t i, SEXP value)
		{
			if ( TYPEOF(value) != CHARSXP ) {
				self_destruct();
				Rf_error("replacement value is not a string");
			}
			set(i, Rf_ScalarString(value));
		}

		SEXP get_char(index_t i, SEXP j)
		{
			return Rf_asChar(get(i, j));
		}

		void set_char(index_t i, SEXP j, SEXP value)
		{
			if ( TYPEOF(value) != CHARSXP ) {
				self_destruct();
				Rf_error("replacement value is not a string");
			}
			set(i, j, Rf_ScalarString(value));
		}

		SEXP get_strings(SEXP i, SEXP j)
		{
			SEXP x;
			R_xlen_t len;
			if ( Rf_isNull(i) )
				len = length();
			else
				len = XLENGTH(i);
			PROTECT(x = Rf_allocVector(STRSXP, len));
			for ( size_t k = 0; k < len; k++ )
			{
				index_t indk;
				if ( Rf_isNull(i) )
					indk = k;
				else
					indk = IndexElt(i, k) - 1;
				SET_STRING_ELT(x, k, get_char(indk, j));
			}
			UNPROTECT(1);
			return x;
		}

		void set_strings(SEXP i, SEXP j, SEXP value)
		{
			R_xlen_t len;
			if ( Rf_isNull(i) )
				len = length();
			else
				len = XLENGTH(i);
			if ( len != LENGTH(value) ) {
				self_destruct();
				Rf_error("length of replacement value and items to replace are not equal");
			}
			for ( size_t k = 0; k < len; k++ )
			{
				index_t indk;
				if ( Rf_isNull(i) )
					indk = k;
				else
					indk = IndexElt(i, k) - 1;
				set_char(indk, j, STRING_ELT(value, k));
			}
		}

		SEXP get_strings(SEXP i)
		{
			return get_strings(i, R_NilValue);
		}

		void set_strings(SEXP i, SEXP value)
		{
			set_strings(i, R_NilValue, value);
		}

};

#endif // MATTER
