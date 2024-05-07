#ifndef MATTER
#define MATTER

#include "matterDefines.h"
#include "atoms.h"
#include "ops.h"

//// Matter class
//----------------

class Matter : public ArrayInterface {

	public:

		Matter(SEXP x) : _data(R_do_slot(x, Rf_install("data")))
		{
			_type = R_do_slot(x, Rf_install("type"));
			_dim = R_do_slot(x, Rf_install("dim"));
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

	protected:

		Atoms _data;
		SEXP _type;

};

class MatterArray : public Matter {

	public:

		MatterArray(SEXP x) : Matter(x), _ops(x)
		{
			_transpose = Rf_asLogical(R_do_slot(x, Rf_install("transpose")));
		}

		bool is_transposed() {
			return _transpose;
		}

		DeferredOps * ops() {
			return &_ops;
		}

		bool has_ops() {
			return ops()->nops() != 0;
		}

		template<typename T>
		size_t get_region(index_t i, size_t size, T * buffer, int stride = 1)
		{
			R_xlen_t len = length();
			size = len - i > size ? size : len - i;
			if ( is_transposed() && stride != 0 )
			{
				index_t tindx [size];
				transpose_range(tindx, i, size);
				data()->flatten()->get_elements<index_t,T>(buffer, tindx, size, 0, stride);
			}
			else
				data()->flatten()->get_region<T>(buffer, i, size, 0, stride);
			if ( has_ops() )
				ops()->apply<T>(buffer, i, size, stride);
			data()->flatten(false);
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
			if ( is_transposed() && stride != 0 )
			{
				index_t tindx [size];
				transpose_range(tindx, i, size);
				data()->flatten()->set_elements<index_t,T>(buffer, tindx, size, 0, stride);
			}
			else
				data()->flatten()->set_region<T>(buffer, i, size, 0, stride);
			data()->flatten(false);
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
			data()->flatten(false);
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
			data()->flatten(false);
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
			int stride = (XLENGTH(value) == 1) ? 0 : 1;
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
			int stride = (XLENGTH(value) == 1) ? 0 : 1;
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

class MatterMatrix : public MatterArray {

	public:

		MatterMatrix(SEXP x) : MatterArray(x)
		{
			_indexed = Rf_asLogical(R_do_slot(x, Rf_install("indexed")));
		}

		bool is_indexed() {
			return _indexed;
		}

		template<typename T>
		size_t get_submatrix(SEXP i, SEXP j, T * buffer, int stride = 1)
		{
			if ( !is_indexed() ) {
				self_destruct();
				Rf_error("matter array is not indexed for matrix subscripting");
			}
			size_t n = 0;
			int nr = Rf_isNull(i) ? nrow() : LENGTH(i);
			int nc = Rf_isNull(j) ? ncol() : LENGTH(j);
			int s1 = is_transposed() ? (nr * stride) : stride;
			int s2 = is_transposed() ? stride : (nr * stride);
			if ( is_transposed() )
			{
				for ( index_t k = 0; k < nr; k++ )
				{
					index_t row = k;
					if ( !Rf_isNull(i) ) {
						row = IndexElt(i, k);
						row = isNA(row) ? row : row - 1;
					}
					if ( isNA(row) )
						n += fill<T>(buffer + k * s2, nc, NA<T>(), s1);
					else if ( Rf_isNull(j) )
						n += data()->get_region<T>(buffer + k * s2, 0, ncol(), row, s1);
					else
						n += data()->get_elements<T>(buffer + k * s2, j, row, s1);
				}
			}
			else
			{
				for ( index_t k = 0; k < nc; k++ )
				{
					index_t col = k;
					if ( !Rf_isNull(j) ) {
						col = IndexElt(j, k);
						col = isNA(col) ? col : col - 1;
					}
					if ( isNA(col) )
						n += fill<T>(buffer + k * s2, nr, NA<T>(), s1);
					else if ( Rf_isNull(i) )
						n += data()->get_region<T>(buffer + k * s2, 0, nrow(), col, s1);
					else
						n += data()->get_elements<T>(buffer + k * s2, i, col, s1);
				}
			}
			if ( has_ops() )
				ops()->apply<T>(buffer, i, j, stride);
			return n;
		}

		template<typename T>
		size_t set_submatrix(SEXP i, SEXP j, T * buffer, int stride = 1)
		{
			if ( !is_indexed() ) {
				self_destruct();
				Rf_error("matter array is not indexed for matrix subscripting");
			}
			if ( has_ops() ) {
				self_destruct();
				Rf_error("can't assign to array with deferred operations");
			}
			size_t n = 0;
			int nr = Rf_isNull(i) ? nrow() : LENGTH(i);
			int nc = Rf_isNull(j) ? ncol() : LENGTH(j);
			int s1 = is_transposed() ? (nr * stride) : stride;
			int s2 = is_transposed() ? stride : (nr * stride);
			if ( is_transposed() )
			{
				for ( index_t k = 0; k < nr; k++ )
				{
					index_t row = k;
					if ( !Rf_isNull(i) ) {
						row = IndexElt(i, k);
						if ( isNA(row) ) {
							self_destruct();
							Rf_error("NAs are not allowed in subscripted assignments");
						}
						row = row - 1;
					}
					if ( Rf_isNull(j) )
						n += data()->set_region<T>(buffer + k * s2, 0, ncol(), row, s1);
					else
						n += data()->set_elements<T>(buffer + k * s2, j, row, s1);
				}
			}
			else
			{
				for ( index_t k = 0; k < nc; k++ )
				{
					index_t col = k;
					if ( !Rf_isNull(j) ) {
						col = IndexElt(j, k);
						if ( isNA(col) ) {
							self_destruct();
							Rf_error("NAs are not allowed in subscripted assignments");
						}
						col = col - 1;
					}
					if ( Rf_isNull(i) )
						n += data()->set_region<T>(buffer + k * s2, 0, nrow(), col, s1);
					else
						n += data()->set_elements<T>(buffer + k * s2, i, col, s1);
				}
			}
			return n;
		}

		SEXP get_submatrix(SEXP i, SEXP j)
		{
			if ( !is_indexed() ) {
				self_destruct();
				Rf_error("matter array is not indexed for matrix subscripting");
			}
			SEXP x;
			int nr = Rf_isNull(i) ? nrow() : LENGTH(i);
			int nc = Rf_isNull(j) ? ncol() : LENGTH(j);
			switch(type()) {
				case R_RAW:
					PROTECT(x = Rf_allocMatrix(RAWSXP, nr, nc));
					get_submatrix(i, j, RAW(x));
					break;
				case R_LOGICAL:
					PROTECT(x = Rf_allocMatrix(LGLSXP, nr, nc));
					get_submatrix(i, j, LOGICAL(x));
					break;
				case R_INTEGER:
					PROTECT(x = Rf_allocMatrix(INTSXP, nr, nc));
					get_submatrix(i, j, INTEGER(x));
					break;
				case R_DOUBLE:
					PROTECT(x = Rf_allocMatrix(REALSXP, nr, nc));
					get_submatrix(i, j, REAL(x));
					break;
				default:
					self_destruct();
					Rf_error("invalid matter array data type");
			}
			UNPROTECT(1);
			return x;
		}

		void set_submatrix(SEXP i, SEXP j, SEXP value)
		{
			if ( !is_indexed() ) {
				self_destruct();
				Rf_error("matter array is not indexed for matrix subscripting");
			}
			int nr = Rf_isNull(i) ? nrow() : LENGTH(i);
			int nc = Rf_isNull(j) ? ncol() : LENGTH(j);
			int stride = (XLENGTH(value) == 1) ? 0 : 1;
			if ( (nr * nc) > XLENGTH(value) && stride != 0 ) {
				self_destruct();
				Rf_error("number of items to replace is longer than replacement length");
			}
			switch(TYPEOF(value)) {
				case RAWSXP:
					set_submatrix(i, j, RAW(value), stride);
					break;
				case LGLSXP:
					set_submatrix(i, j, LOGICAL(value), stride);
					break;
				case INTSXP:
					set_submatrix(i, j, INTEGER(value), stride);
					break;
				case REALSXP:
					set_submatrix(i, j, REAL(value), stride);
					break;
				default:
					self_destruct();
					Rf_error("invalid replacement data type");
			}
		}

	protected:

		bool _indexed;

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
			if ( dim(i) != LENGTH(value) && LENGTH(value) != 1 ) {
				self_destruct();
				Rf_error("length of replacement value and items to replace are not equal");
			}
			if ( dim(i) == 0 )
				return;
			int stride = (LENGTH(value) == 1) ? 0 : 1;
			switch(TYPEOF(value)) {
				case RAWSXP:
					data()->set_region<Rbyte>(RAW(value), 0, dim(i), i, stride);
					break;
				case LGLSXP:
					data()->set_region<int>(LOGICAL(value), 0, dim(i), i, stride);
					break;
				case INTSXP:
					data()->set_region<int>(INTEGER(value), 0, dim(i), i, stride);
					break;
				case REALSXP:
					data()->set_region<double>(REAL(value), 0, dim(i), i, stride);
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
			if ( LENGTH(j) != LENGTH(value) && LENGTH(value) != 1 ) {
				self_destruct();
				Rf_error("length of replacement value and items to replace are not equal");
			}
			if ( dim(i) == 0 )
				return;
			int stride = (LENGTH(value) == 1) ? 0 : 1;
			switch(TYPEOF(value)) {
				case RAWSXP:
					data()->set_elements<Rbyte>(RAW(value), j, i, stride);
					break;
				case LGLSXP:
					data()->set_elements<int>(LOGICAL(value), j, i, stride);
					break;
				case INTSXP:
					data()->set_elements<int>(INTEGER(value), j, i, stride);
					break;
				case REALSXP:
					data()->set_elements<double>(REAL(value), j, i, stride);
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
			for ( index_t k = 0; k < len; k++ )
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
			for ( index_t k = 0; k < len; k++ )
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
			for ( index_t k = 0; k < len; k++ )
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
			for ( index_t k = 0; k < len; k++ )
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
