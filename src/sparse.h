
#ifndef SPARSE
#define SPARSE

#include "matter.h"
#include "search.h"

class Sparse
{
	public:

		Sparse(SEXP x)
		{
			_data = R_do_slot(x, Rf_install("data"));
			_datamode = INTEGER(R_do_slot(x, Rf_install("datamode")));
			_index = R_do_slot(x, Rf_install("index"));
			_offset = Rf_asInteger(R_do_slot(x, Rf_install("offset")));
			_domain = R_do_slot(x, Rf_install("domain"));
			_pointers = R_do_slot(x, Rf_install("pointers"));
			_length = static_cast<index_t>(Rf_asReal(R_do_slot(x, Rf_install("length"))));
			_dim = R_do_slot(x, Rf_install("dim"));
			_dimnames = R_do_slot(x, Rf_install("dimnames"));
			_names = R_do_slot(x, Rf_install("names"));
			SEXP tol = R_do_slot(x, Rf_install("tolerance"));
			SEXP tol_type = Rf_getAttrib(tol, Rf_install("tol_type"));
			_tol = Rf_asReal(tol);
			_tol_type = Rf_asInteger(tol_type);
			_sampler = Rf_asInteger(R_do_slot(x, Rf_install("sampler")));
			set_matter_options();
		}

		~Sparse(){}

		SEXP data() {
			return _data;
		}

		SEXPTYPE datatype() {
			switch(_datamode[0]) {
				case R_INTEGER:
					return INTSXP;
				case R_NUMERIC:
					return REALSXP;
				default:
					Rf_error("unsupported 'datamode'");
			}
		}

		SEXP index() {
			return _index;
		}

		SEXPTYPE indextype() {
			if ( Rf_isReal(domain()) )
				return REALSXP;
			if ( Rf_isS4(_index) )
			{
				SEXP mode = R_do_slot(_index, Rf_install("datamode"));
				switch(Rf_asInteger(mode)) {
					case R_INTEGER:
						return INTSXP;
					case R_NUMERIC:
						return REALSXP;
					default:
						Rf_error("unsupported 'datamode'");
				}
			}
			else
			{
				if ( Rf_isVectorList(_index) )
					return TYPEOF(VECTOR_ELT(_index, 0));
				else
					return TYPEOF(_index);
			}
		}

		int offset() {
			return _offset;
		}

		SEXP domain() {
			return _domain;
		}

		bool has_domain() {
			return _domain != R_NilValue;
		}

		SEXP pointers() {
			return _pointers;
		}

		bool has_pointers() {
			return _pointers != R_NilValue;
		}

		Pair<index_t,index_t> pointers(size_t i)
		{
			Pair<index_t,index_t> p;
			if ( has_pointers() )
			{
				Rindex_t * ptr = DataPtr<Rindex_t>(_pointers);
				p.first = static_cast<index_t>(ptr[i]);
				p.second = static_cast<index_t>(ptr[i + 1]);
			}
			else
			{
				p.first = NA_INTEGER;
				p.second = NA_INTEGER;
			}
			return p;
		}

		index_t nnz() {
			return NA_INTEGER;
		}

		index_t length() {
			return _length;
		}

		int dim(int i) {
			return INTEGER(_dim)[i];
		}

		bool has_dims() {
			return _dim != R_NilValue;
		}

		int ndims() {
			if ( has_dims() )
				return LENGTH(_dim);
			else
				return 0;
		}

		SEXP dimnames() {
			return _dimnames;
		}

		SEXP names() {
			return _names;
		}

		double zero() {
			return 0;
		}

		double tol() {
			return _tol;
		}

		int tol_ref() {
			return _tol_type == ABS_COMPARE ? ABS_DIFF : REL_DIFF_Y;
		}

		int sampler() {
			return _sampler;
		}

		template<typename T>
		void copy_domain(size_t i, size_t size, T * buffer)
		{
			for ( size_t j = 0; j < size; i++, j++ ) {
				switch(TYPEOF(domain())) {
					case NILSXP:
						buffer[j] = i + offset();
						break;
					case INTSXP:
						buffer[j] = INTEGER_ELT(domain(), i);
						break;
					case REALSXP:
						buffer[j] = REAL_ELT(domain(), i);
						break;
				}
			}
		}

		template<typename T>
		void copy_domain(SEXP indx, T * buffer, bool ind1 = TRUE)
		{
			size_t i = 0, len = XLENGTH(indx);
			for ( size_t j = 0; j < len; j++ )
			{
				switch(TYPEOF(indx)) {
					case INTSXP:
						i = INTEGER_ELT(indx, j);
						break;
					case REALSXP:
						i = REAL_ELT(indx, j);
						break;
				}
				switch(TYPEOF(domain())) {
					case NILSXP:
						buffer[j] = i - ind1 + offset();
						break;
					case INTSXP:
						buffer[j] = INTEGER_ELT(domain(), i - ind1);
						break;
					case REALSXP:
						buffer[j] = REAL_ELT(domain(), i - ind1);
						break;
				}
			}
		}

	protected:

		SEXP _data;
		int * _datamode;  // 1 = raw, 2 = logical, 3 = integer, 4 = numeric
		SEXP _index;
		int _offset;
		SEXP _domain;
		SEXP _pointers;
		index_t _length;
		SEXP _dim;
		SEXP _dimnames;
		SEXP _names;
		double _tol;
		int _tol_type; // 1 = absolute, 2 = relative
		int _sampler;
};

class SparseVector : public Sparse
{
	public:

		SparseVector(SEXP x) : Sparse(x) {}

		SEXP data() {
			SEXP data0;
			if ( Rf_isS4(_data) )
				data0 = getVector(_data);
			else
				data0 = _data;
			PROTECT(data0);
			data0 = Rf_coerceVector(data0, datatype());
			UNPROTECT(1);
			return data0;
		}

		SEXP index() {
			SEXP index0;
			if ( Rf_isS4(_index) )
				index0 = getVector(_index);
			else
				index0 = _index;
			PROTECT(index0);
			index0 = Rf_coerceVector(index0, indextype());
			UNPROTECT(1);
			return index0;
		}

		index_t nnz() {
			if ( Rf_isS4(_data) )
			{
				SEXP len = R_do_slot(_data, Rf_install("length"));
				return static_cast<index_t>(Rf_asReal(len));
			}
			else
				return XLENGTH(_data);
		}

		template<typename Tind, typename Tval>
		Tval get(size_t i)
		{
			SEXP index0, data0;
			Tind x = NA_INTEGER;
			switch(TYPEOF(domain())) {
				case NILSXP:
					x = i;
					break;
				case INTSXP:
					x = INTEGER_ELT(domain(), i);
					break;
				case REALSXP:
					x = REAL_ELT(domain(), i);
					break;
			}
			PROTECT(index0 = index());
			PROTECT(data0 = data());
			Tval val = approx_search(x, index0, data0, 0, nnz(),
				tol(), tol_ref(), zero(), sampler()).second;
			UNPROTECT(2);
			return val;
		}

		template<typename Tind, typename Tval>
		size_t getRegion(size_t i, size_t size, Tval * buffer)
		{
			SEXP index0, data0;
			PROTECT(index0 = index());
			PROTECT(data0 = data());
			size_t num_nz = 0;
			if ( has_domain() ) {
				Tind subscripts [size];
				copy_domain<Tind>(i, size, subscripts);
				num_nz = do_approx_search<Tind,Tval>(buffer, subscripts, size,
					index0, data0, tol(), tol_ref(), zero(), sampler());
			}
			else {
				for ( size_t j = 0; j < size; j++ )
					buffer[j] = zero();
				for ( size_t k = 0; k < nnz(); k++ )
				{
					Tind * pindex = DataPtr<Tind>(index0);
					Tval * pdata = DataPtr<Tval>(data0);
					index_t indk = static_cast<index_t>(pindex[k]);
					buffer[indk + i - offset()] = pdata[k];
					num_nz++;
				}
			}
			UNPROTECT(2);
			return num_nz;
		}

		SEXP getRegion(size_t i, size_t size)
		{
			SEXP result;
			PROTECT(result = Rf_allocVector(datatype(), size));
			switch(datatype()) {
				case INTSXP:
				{
					switch(indextype()) {
						case INTSXP:
							getRegion<int,int>(i, size, INTEGER(result));
							break;
						case REALSXP:
							getRegion<double,int>(i, size, INTEGER(result));
							break;
					}
					break;
				}
				case REALSXP:
				{
					switch(indextype()) {
						case INTSXP:
							getRegion<int,double>(i, size, REAL(result));
							break;
						case REALSXP:
							getRegion<double,double>(i, size, REAL(result));
							break;
					}
					break;
				}
				default:
					Rf_error("unsupported 'datamode'");
			}
			UNPROTECT(1);
			return result;
		}

		template<typename Tind, typename Tval>
		size_t getElements(SEXP indx, Tval * buffer)
		{
			SEXP index0, data0;
			PROTECT(index0 = index());
			PROTECT(data0 = data());
			R_xlen_t size = XLENGTH(indx);
			Tind subscripts [size];
			copy_domain<Tind>(indx, subscripts, TRUE);
			size_t num_nz = do_approx_search<Tind,Tval>(buffer, subscripts, size,
				index0, data0, tol(), tol_ref(), zero(), sampler());
			UNPROTECT(2);
			return num_nz;
		}

		SEXP getElements(SEXP indx)
		{
			SEXP result;
			if ( indx == R_NilValue )
				return getRegion(0, length());
			PROTECT(result = Rf_allocVector(datatype(), XLENGTH(indx)));
			switch(datatype()) {
				case INTSXP:
				{
					switch(indextype()) {
						case INTSXP:
							getElements<int,int>(indx, INTEGER(result));
							break;
						case REALSXP:
							getElements<double,int>(indx, INTEGER(result));
							break;
					}
					break;
				}
				break;
				case REALSXP:
				{
					switch(indextype()) {
						case INTSXP:
							getElements<int,double>(indx, REAL(result));
							break;
						case REALSXP:
							getElements<double,double>(indx, REAL(result));
							break;
					}
					break;
				}
				default:
					Rf_error("unsupported 'datamode'");
			}
			UNPROTECT(1);
			return result;
		}

};

class SparseMatrix : public Sparse
{
	public:

		SparseMatrix(SEXP x) : Sparse(x) {}

		SEXP data(size_t i)
		{
			SEXP indx, data0;
			Pair<index_t,index_t> p = pointers(i);
			if ( has_pointers() && p.first == p.second )
				return Rf_allocVector(datatype(), 0);
			if ( Rf_isS4(_data) )
			{
				SEXP dims = R_do_slot(_data, Rf_install("dim"));
				if ( dims != R_NilValue ) {
					// matter list
					data0 = getListElements(_data, Rf_ScalarReal(i), R_NilValue);
				}
				else {
					// matter vector
					if ( has_pointers() )
						indx = R_compact_intrange(p.first, p.second - 1);
					else {
						size_t n = XLENGTH(_index);
						indx = R_compact_intrange(i * n + 1, (i + 1) * n);
					}
					PROTECT(indx);
					// FIXME: double indexing only for now...
					PROTECT(indx = Rf_coerceVector(indx, REALSXP));
					data0 = getVectorElements(_data, indx);
					UNPROTECT(2);
				}
			}
			else if ( Rf_isVectorList(_data) ) {
				// R list
				data0 = VECTOR_ELT(_data, i);
			}
			else {
				// R vector
				if ( has_pointers() )
					indx = R_compact_intrange(p.first + 1, p.second);
				else {
					size_t n = XLENGTH(_index);
					indx = R_compact_intrange(i * n + 1, (i + 1) * n);
				}
				PROTECT(indx);
				data0 = Rf_ExtractSubset(_data, indx, R_NilValue);
				UNPROTECT(1);
			}
			PROTECT(data0);
			data0 = Rf_coerceVector(data0, datatype());
			UNPROTECT(1);
			return data0;
		}

		SEXP index(size_t i)
		{
			SEXP indx, index0;
			Pair<index_t,index_t> p = pointers(i);
			if ( has_pointers() && p.first == p.second )
				return Rf_allocVector(indextype(), 0);
			if ( Rf_isS4(_index) )
			{
				SEXP dims = R_do_slot(_index, Rf_install("dim"));
				if ( dims != R_NilValue ) {
					// matter list
					index0 = getListElements(_index, Rf_ScalarReal(i), R_NilValue);
				}
				else {
					// matter vector
					if ( has_pointers() ) {
						PROTECT(indx = R_compact_intrange(p.first, p.second - 1));
						// FIXME: double indexing only for now...
						PROTECT(indx = Rf_coerceVector(indx, REALSXP));
						index0 = getVectorElements(_index, indx);
						UNPROTECT(2);
					}
					else
						index0 = getVector(_index);
				}
			}
			else if ( Rf_isVectorList(_index) ) {
				// R list
				index0 = VECTOR_ELT(_index, i);
			}
			else {
				// R vector
				if ( has_pointers() ) {
					PROTECT(indx = R_compact_intrange(p.first + 1, p.second));
					index0 = Rf_ExtractSubset(_index, indx, R_NilValue);
					UNPROTECT(1);
				}
				else
					index0 = _index;
			}
			PROTECT(index0);
			index0 = Rf_coerceVector(index0, indextype());
			UNPROTECT(1);
			return index0;
		}

		int nrow() {
			return dim(0);
		}

		int ncol() {
			return dim(1);
		}

};

class SparseMatrixC : public SparseMatrix
{
	public:

		SparseMatrixC(SEXP x) : SparseMatrix(x) {}

		template<typename Tind, typename Tval>
		size_t getCol(size_t i, Tval * buffer, size_t stride = 1)
		{
			SEXP index0, data0;
			PROTECT(index0 = index(i));
			PROTECT(data0 = data(i));
			size_t num_nz = LENGTH(data0);
			if ( has_domain() ) {
				Tind subscripts [nrow()];
				copy_domain<Tind>(0, nrow(), subscripts);
				num_nz = do_approx_search<Tind,Tval>(buffer, subscripts, nrow(),
					index0, data0, tol(), tol_ref(), zero(),
					sampler(), TRUE, stride);
			}
			else {
				for ( size_t j = 0; j < nrow(); j++ )
					buffer[j * stride] = zero();
				for ( size_t k = 0; k < num_nz; k++ )
				{
					Tind * pindex = DataPtr<Tind>(index0);
					Tval * pdata = DataPtr<Tval>(data0);
					index_t indk = static_cast<index_t>(pindex[k]);
					buffer[(indk - offset()) * stride] = pdata[k];
				}
			}
			UNPROTECT(2);
			return num_nz;
		}

		SEXP getCol(size_t i)
		{
			SEXP result;
			PROTECT(result = Rf_allocVector(datatype(), nrow()));
			switch(datatype()) {
				case INTSXP:
				{
					switch(indextype()) {
						case INTSXP:
							getCol<int,int>(i, INTEGER(result));
							break;
						case REALSXP:
							getCol<double,int>(i, INTEGER(result));
							break;
					}
					break;
				}
				case REALSXP:
				{
					switch(indextype()) {
						case INTSXP:
							getCol<int,double>(i, REAL(result));
							break;
						case REALSXP:
							getCol<double,double>(i, REAL(result));
							break;
					}
					break;
				}
				default:
					Rf_error("unsupported 'datamode'");
			}
			UNPROTECT(1);
			return result;
		}

		template<typename Tind, typename Tval>
		size_t getElements(SEXP i, SEXP j, Tval * buffer)
		{
			SEXP index0, data0;
			size_t num_nz = 0;
			size_t ni = i != R_NilValue ? LENGTH(i) : dim(0);
			size_t nj = j != R_NilValue ? LENGTH(j) : dim(1);
			if ( j == R_NilValue )
				j = R_compact_intrange(1, nj);
			PROTECT(j);
			if ( i == R_NilValue && !has_domain() )
			{
				// get full columns
				for ( size_t k = 0; k < nj; k++ ) {
					Tval * buffer0 = buffer + (k * ni);
					switch(TYPEOF(j)) {
						case INTSXP:
							num_nz += getCol<Tind,Tval>(INTEGER_ELT(j, k) - 1, buffer0);
							break;
						case REALSXP:
							num_nz += getCol<Tind,Tval>(REAL_ELT(j, k) - 1, buffer0);
							break;
					}
				}
			}
			else
			{
				// get indexed columns
				Tind subscripts [ni];
				if ( i == R_NilValue )
					copy_domain<Tind>(0, ni, subscripts);
				else
					copy_domain<Tind>(i, subscripts, TRUE);
				for ( size_t k = 0; k < nj; k++ ) {
					switch(TYPEOF(j)) {
						case INTSXP:
							PROTECT(index0 = index(INTEGER_ELT(j, k) - 1));
							PROTECT(data0 = data(INTEGER_ELT(j, k) - 1));
							break;
						case REALSXP:
							PROTECT(index0 = index(REAL_ELT(j, k) - 1));
							PROTECT(data0 = data(REAL_ELT(j, k) - 1));
							break;
					}
					Tval * buffer0 = buffer + (k * ni);
					num_nz += do_approx_search<Tind,Tval>(buffer0, subscripts, ni,
						index0, data0, tol(), tol_ref(), zero(), sampler());
					UNPROTECT(2);
				}
			}
			UNPROTECT(1);
			return num_nz;
		}

		SEXP getElements(SEXP i, SEXP j)
		{
			SEXP result;
			size_t ni = i != R_NilValue ? LENGTH(i) : dim(0);
			size_t nj = j != R_NilValue ? LENGTH(j) : dim(1);
			PROTECT(result = Rf_allocMatrix(datatype(), ni, nj));
			switch(datatype()) {
				case INTSXP:
				{
					switch(indextype()) {
						case INTSXP:
							getElements<int,int>(i, j, INTEGER(result));
							break;
						case REALSXP:
							getElements<double,int>(i, j, INTEGER(result));
							break;
					}
					break;
				}
				break;
				case REALSXP:
				{
					switch(indextype()) {
						case INTSXP:
							getElements<int,double>(i, j, REAL(result));
							break;
						case REALSXP:
							getElements<double,double>(i, j, REAL(result));
							break;
					}
					break;
				}
				default:
					Rf_error("unsupported 'datamode'");
			}
			UNPROTECT(1);
			return result;
		}

};

class SparseMatrixR : public SparseMatrix
{
	public:

		SparseMatrixR(SEXP x) : SparseMatrix(x) {}

		template<typename Tind, typename Tval>
		size_t getRow(size_t i, Tval * buffer, size_t stride = 1)
		{
			SEXP index0, data0;
			PROTECT(index0 = index(i));
			PROTECT(data0 = data(i));
			size_t num_nz = LENGTH(data0);
			if ( has_domain() ) {
				Tind subscripts [ncol()];
				copy_domain<Tind>(0, ncol(), subscripts);
				num_nz = do_approx_search<Tind,Tval>(buffer, subscripts, ncol(),
					index0, data0, tol(), tol_ref(), zero(),
					sampler(), TRUE, stride);
			}
			else {
				for ( size_t j = 0; j < ncol(); j++ )
					buffer[j * stride] = zero();
				for ( size_t k = 0; k < num_nz; k++ )
				{
					Tind * pindex = DataPtr<Tind>(index0);
					Tval * pdata = DataPtr<Tval>(data0);
					index_t indk = static_cast<index_t>(pindex[k]);
					buffer[(indk - offset()) * stride] = pdata[k];
				}
			}
			UNPROTECT(2);
			return num_nz;
		}

		SEXP getRow(size_t i)
		{
			SEXP result;
			PROTECT(result = Rf_allocVector(datatype(), ncol()));
			switch(datatype()) {
				case INTSXP:
				{
					switch(indextype()) {
						case INTSXP:
							getRow<int,int>(i, INTEGER(result));
							break;
						case REALSXP:
							getRow<double,int>(i, INTEGER(result));
							break;
					}
					break;
				}
				case REALSXP:
				{
					switch(indextype()) {
						case INTSXP:
							getRow<int,double>(i, REAL(result));
							break;
						case REALSXP:
							getRow<double,double>(i, REAL(result));
							break;
					}
					break;
				}
				default:
					Rf_error("unsupported 'datamode'");
			}
			UNPROTECT(1);
			return result;
		}

		template<typename Tind, typename Tval>
		size_t getElements(SEXP i, SEXP j, Tval * buffer)
		{
			SEXP index0, data0;
			size_t num_nz = 0;
			size_t ni = i != R_NilValue ? LENGTH(i) : dim(0);
			size_t nj = j != R_NilValue ? LENGTH(j) : dim(1);
			if ( i == R_NilValue )
				i = R_compact_intrange(1, ni);
			PROTECT(i);
			if ( j == R_NilValue && !has_domain() )
			{
				// get full rows
				for ( size_t k = 0; k < ni; k++ ) {
					Tval * buffer0 = buffer + k;
					switch(TYPEOF(i)) {
						case INTSXP:
							num_nz += getRow<Tind,Tval>(INTEGER_ELT(i, k) - 1, buffer0, ni);
							break;
						case REALSXP:
							num_nz += getRow<Tind,Tval>(REAL_ELT(i, k) - 1, buffer0, ni);
							break;
					}
				}
			}
			else
			{
				// get indexed rows
				Tind subscripts [nj];
				if ( j == R_NilValue )
					copy_domain<Tind>(0, nj, subscripts);
				else
					copy_domain<Tind>(j, subscripts, TRUE);
				for ( size_t k = 0; k < ni; k++ ) {
					switch(TYPEOF(i)) {
						case INTSXP:
							PROTECT(index0 = index(INTEGER_ELT(i, k) - 1));
							PROTECT(data0 = data(INTEGER_ELT(i, k) - 1));
							break;
						case REALSXP:
							PROTECT(index0 = index(REAL_ELT(i, k) - 1));
							PROTECT(data0 = data(REAL_ELT(i, k) - 1));
							break;
					}
					Tval * buffer0 = buffer + k;
					num_nz += do_approx_search<Tind,Tval>(buffer0, subscripts, nj,
						index0, data0, tol(), tol_ref(), zero(),
						sampler(), TRUE, ni);
					UNPROTECT(2);
				}
			}
			UNPROTECT(1);
			return num_nz;
		}

		SEXP getElements(SEXP i, SEXP j)
		{
			SEXP result;
			size_t ni = i != R_NilValue ? LENGTH(i) : dim(0);
			size_t nj = j != R_NilValue ? LENGTH(j) : dim(1);
			PROTECT(result = Rf_allocMatrix(datatype(), ni, nj));
			switch(datatype()) {
				case INTSXP:
				{
					switch(indextype()) {
						case INTSXP:
							getElements<int,int>(i, j, INTEGER(result));
							break;
						case REALSXP:
							getElements<double,int>(i, j, INTEGER(result));
							break;
					}
					break;
				}
				break;
				case REALSXP:
				{
					switch(indextype()) {
						case INTSXP:
							getElements<int,double>(i, j, REAL(result));
							break;
						case REALSXP:
							getElements<double,double>(i, j, REAL(result));
							break;
					}
					break;
				}
				default:
					Rf_error("unsupported 'datamode'");
			}
			UNPROTECT(1);
			return result;
		}

};

#endif // SPARSE
