
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

		int datamode() {
			return _datamode[0];
		}

		SEXP index() {
			return _index;
		}

		int indexmode() {
			if ( Rf_isS4(_index) )
				return Rf_asInteger(R_do_slot(_index, Rf_install("datamode")));
			else {
				SEXPTYPE mode;
				if ( TYPEOF(_index) == VECSXP )
					mode = TYPEOF(VECTOR_ELT(_index, 0));
				else
					mode = TYPEOF(_index);
				switch(mode) {
					case INTSXP:
						return R_INTEGER;
					case REALSXP:
						return R_NUMERIC;
					case STRSXP:
						return R_CHARACTER;
				}
			}
			return 0;
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

		template<typename Tind>
		void copy_domain(size_t i, size_t size, Tind * buffer)
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

		template<typename Tind>
		void copy_domain(SEXP indx, Tind * buffer, bool ind1 = TRUE)
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
					default:
						Rf_error("unsupported subscript type");
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
			if ( Rf_isS4(_data) )
				return getVector(_data);
			else
				return _data;
		}

		SEXP index() {
			if ( Rf_isS4(_index) )
				return getVector(_index);
			else
				return _index;
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
			Tind subset;
			switch(TYPEOF(domain())) {
				case NILSXP:
					subset = i;
					break;
				case INTSXP:
					subset = INTEGER_ELT(domain(), i);
					break;
				case REALSXP:
					subset = REAL_ELT(domain(), i);
					break;
			}
			Tval val = approx_search<Tind,Tval>(subset, index(), data(), 0, nnz(),
				tol(), tol_ref(), zero(), sampler()).second;
			return val;
		}

		template<typename Tind, typename Tval>
		size_t getRegion(size_t i, size_t size, Tval * buffer)
		{
			size_t num_nz = 0;
			if ( has_domain() ) {
				Tind * region_idx = (Tind *) Calloc(size, Tind);
				copy_domain<Tind>(i, size, region_idx);
				num_nz = do_approx_search<Tind,Tval>(buffer, region_idx, size,
					index(), data(), 0, nnz(), tol(), tol_ref(), zero(), sampler());
				Free(region_idx);
			}
			else {
				for ( size_t j = 0; j < size; j++ )
					buffer[j] = zero();
				for ( size_t k = 0; k < nnz(); k++ )
				{
					Tind * pIndex = DataPtr<Tind>(index());
					Tval * pData = DataPtr<Tval>(data());
					index_t indk = static_cast<index_t>(pIndex[k]);
					buffer[indk + i - offset()] = pData[k];
					num_nz++;
				}
			}
			return num_nz;
		}

		SEXP getRegion(size_t i, size_t size)
		{
			SEXP result;
			switch(datamode()) {
				case R_INTEGER:
				{
					PROTECT(result = Rf_allocVector(INTSXP, size));
					switch(indexmode()) {
						case R_INTEGER:
							getRegion<int,int>(i, size, INTEGER(result));
							break;
						case R_NUMERIC:
							getRegion<double,int>(i, size, INTEGER(result));
							break;
					}
					break;
				}
				case R_NUMERIC:
				{
					PROTECT(result = Rf_allocVector(REALSXP, size));
					switch(indexmode()) {
						case R_INTEGER:
							getRegion<int,double>(i, size, REAL(result));
							break;
						case R_NUMERIC:
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
			R_xlen_t size = XLENGTH(indx);
			Tind * element_idx = (Tind *) Calloc(size, Tind);
			copy_domain<Tind>(indx, element_idx, TRUE);
			size_t num_nz = do_approx_search<Tind,Tval>(buffer, element_idx, size,
				index(), data(), 0, nnz(), tol(), tol_ref(), zero(), sampler());
			Free(element_idx);
			return num_nz;
		}

		SEXP getElements(SEXP indx)
		{
			SEXP result;
			if ( indx == R_NilValue ) {
				result = getRegion(0, length());
				return result;
			}
			switch(datamode()) {
				case R_INTEGER:
				{
					PROTECT(result = Rf_allocVector(INTSXP, XLENGTH(indx)));
					switch(indexmode()) {
						case R_INTEGER:
							getElements<int,int>(indx, INTEGER(result));
							break;
						case R_NUMERIC:
							getElements<double,int>(indx, INTEGER(result));
							break;
					}
					break;
				}
				break;
				case R_NUMERIC:
				{
					PROTECT(result = Rf_allocVector(REALSXP, XLENGTH(indx)));
					switch(indexmode()) {
						case R_INTEGER:
							getElements<int,double>(indx, REAL(result));
							break;
						case R_NUMERIC:
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
			SEXP indx;
			Pair<index_t,index_t> p = pointers(i);
			if ( Rf_isS4(_data) )
			{
				SEXP dims = R_do_slot(_data, Rf_install("dim"));
				if ( dims != R_NilValue ) {
					// matter list
					return getListElements(_data, Rf_ScalarReal(i), R_NilValue);
				}
				else {
					// matter vector
					indx = R_compact_intrange(p.first, p.second - 1);
					indx = Rf_coerceVector(indx, REALSXP);
					return getVectorElements(_data, indx);
				}
			}
			else if ( TYPEOF(_data) == VECSXP ) {
				// R list
				return VECTOR_ELT(_data, i);
			}
			else {
				// R vector
				indx = R_compact_intrange(p.first + 1, p.second);
				return Rf_ExtractSubset(_data, indx, R_NilValue);
			}
		}

		SEXP index(size_t i)
		{
			SEXP indx;
			Pair<index_t,index_t> p = pointers(i);
			if ( Rf_isS4(_index) )
			{
				SEXP dims = R_do_slot(_index, Rf_install("dim"));
				if ( dims != R_NilValue ) {
					// matter list
					return getListElements(_index, Rf_ScalarReal(i), R_NilValue);
				}
				else {
					// matter vector
					indx = R_compact_intrange(p.first, p.second - 1);
					indx = Rf_coerceVector(indx, REALSXP);
					return getVectorElements(_index, indx);
				}
			}
			else if ( TYPEOF(_index) == VECSXP ) {
				// R list
				return VECTOR_ELT(_index, i);
			}
			else {
				// R vector
				indx = R_compact_intrange(p.first + 1, p.second);
				return Rf_ExtractSubset(_index, indx, R_NilValue);
			}
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
			size_t num_nz = 0;
			SEXP cindex = index(i);
			SEXP cdata = data(i);
			size_t cnnz = LENGTH(cdata);
			if ( has_domain() ) {
				Tind * row_idx = (Tind *) Calloc(nrow(), Tind);
				copy_domain<Tind>(0, nrow(), row_idx);
				num_nz = do_approx_search<Tind,Tval>(buffer, row_idx, nrow(),
					cindex, cdata, 0, cnnz, tol(), tol_ref(), zero(),
					sampler(), TRUE, stride);
				Free(row_idx);
			}
			else {
				for ( size_t j = 0; j < nrow(); j++ )
					buffer[j * stride] = zero();
				for ( size_t k = 0; k < cnnz; k++ )
				{
					Tind * pIndex = DataPtr<Tind>(cindex);
					Tval * pData = DataPtr<Tval>(cdata);
					index_t indk = static_cast<index_t>(pIndex[k]);
					buffer[(indk - offset()) * stride] = pData[k];
					num_nz++;
				}
			}
			return num_nz;
		}

		SEXP getCol(size_t i)
		{
			SEXP result;
			switch(datamode()) {
				case R_INTEGER:
				{
					PROTECT(result = Rf_allocVector(INTSXP, nrow()));
					switch(indexmode()) {
						case R_INTEGER:
							getCol<int,int>(i, INTEGER(result));
							break;
						case R_NUMERIC:
							getCol<double,int>(i, INTEGER(result));
							break;
					}
					break;
				}
				case R_NUMERIC:
				{
					PROTECT(result = Rf_allocVector(REALSXP, nrow()));
					switch(indexmode()) {
						case R_INTEGER:
							getCol<int,double>(i, REAL(result));
							break;
						case R_NUMERIC:
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
			SEXP cindex, cdata;
			size_t num_nz = 0;
			size_t ni = i != R_NilValue ? LENGTH(i) : dim(0);
			size_t nj = j != R_NilValue ? LENGTH(j) : dim(1);
			if ( j == R_NilValue )
				j = R_compact_intrange(1, nj);
			if ( i == R_NilValue )
			{
				for ( size_t k = 0; k < nj; k++ ) {
					Tval * cbuffer = buffer + (k * ni);
					switch(TYPEOF(j)) {
						case INTSXP:
							num_nz += getCol<Tind,Tval>(INTEGER_ELT(j, k) - 1, cbuffer);
							break;
						case REALSXP:
							num_nz += getCol<Tind,Tval>(REAL_ELT(j, k) - 1, cbuffer);
							break;
					}
				}
			}
			else
			{
				Tind * row_idx = (Tind *) Calloc(ni, Tind);
				copy_domain<Tind>(i, row_idx, TRUE);
				for ( size_t k = 0; k < nj; k++ ) {
					switch(TYPEOF(j)) {
						case INTSXP:
							cindex = index(INTEGER_ELT(j, k) - 1);
							cdata = data(INTEGER_ELT(j, k) - 1);
							break;
						case REALSXP:
							cindex = index(REAL_ELT(j, k) - 1);
							cdata = data(REAL_ELT(j, k) - 1);
							break;
					}
					size_t cnnz = LENGTH(cdata);
					Tval * cbuffer = buffer + (k * ni);
					num_nz += do_approx_search<Tind,Tval>(cbuffer, row_idx, LENGTH(i),
						cindex, cdata, 0, cnnz, tol(), tol_ref(), zero(), sampler());
				}
				Free(row_idx);
			}
			return num_nz;
		}

		SEXP getElements(SEXP i, SEXP j)
		{
			SEXP result;
			size_t ni = i != R_NilValue ? LENGTH(i) : dim(0);
			size_t nj = j != R_NilValue ? LENGTH(j) : dim(1);
			switch(datamode()) {
				case R_INTEGER:
				{
					PROTECT(result = Rf_allocMatrix(INTSXP, ni, nj));
					switch(indexmode()) {
						case R_INTEGER:
							getElements<int,int>(i, j, INTEGER(result));
							break;
						case R_NUMERIC:
							getElements<double,int>(i, j, INTEGER(result));
							break;
					}
					break;
				}
				break;
				case R_NUMERIC:
				{
					PROTECT(result = Rf_allocMatrix(REALSXP, ni, nj));
					switch(indexmode()) {
						case R_INTEGER:
							getElements<int,double>(i, j, REAL(result));
							break;
						case R_NUMERIC:
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
			size_t num_nz = 0;
			SEXP rindex = index(i);
			SEXP rdata = data(i);
			size_t rnnz = LENGTH(rdata);
			if ( has_domain() ) {
				Tind * col_idx = (Tind *) Calloc(ncol(), Tind);
				copy_domain<Tind>(0, ncol(), col_idx);
				num_nz = do_approx_search<Tind,Tval>(buffer, col_idx, ncol(),
					rindex, rdata, 0, rnnz, tol(), tol_ref(), zero(),
					sampler(), TRUE, stride);
				Free(col_idx);
			}
			else {
				for ( size_t j = 0; j < ncol(); j++ )
					buffer[j * stride] = zero();
				for ( size_t k = 0; k < rnnz; k++ )
				{
					Tind * pIndex = DataPtr<Tind>(rindex);
					Tval * pData = DataPtr<Tval>(rdata);
					index_t indk = static_cast<index_t>(pIndex[k]);
					buffer[(indk - offset()) * stride] = pData[k];
					num_nz++;
				}
			}
			return num_nz;
		}

		SEXP getRow(size_t i)
		{
			SEXP result;
			switch(datamode()) {
				case R_INTEGER:
				{
					PROTECT(result = Rf_allocVector(INTSXP, ncol()));
					switch(indexmode()) {
						case R_INTEGER:
							getRow<int,int>(i, INTEGER(result));
							break;
						case R_NUMERIC:
							getRow<double,int>(i, INTEGER(result));
							break;
					}
					break;
				}
				case R_NUMERIC:
				{
					PROTECT(result = Rf_allocVector(REALSXP, ncol()));
					switch(indexmode()) {
						case R_INTEGER:
							getRow<int,double>(i, REAL(result));
							break;
						case R_NUMERIC:
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
			SEXP rindex, rdata;
			size_t num_nz = 0;
			size_t ni = i != R_NilValue ? LENGTH(i) : dim(0);
			size_t nj = j != R_NilValue ? LENGTH(j) : dim(1);
			if ( i == R_NilValue )
				i = R_compact_intrange(1, ni);
			if ( j == R_NilValue )
			{
				for ( size_t k = 0; k < ni; k++ ) {
					Tval * rbuffer = buffer + k;
					switch(TYPEOF(i)) {
						case INTSXP:
							num_nz += getRow<Tind,Tval>(INTEGER_ELT(i, k) - 1, rbuffer, ni);
							break;
						case REALSXP:
							num_nz += getRow<Tind,Tval>(REAL_ELT(i, k) - 1, rbuffer, ni);
							break;
					}
				}
			}
			else
			{
				Tind * col_idx = (Tind *) Calloc(nj, Tind);
				copy_domain<Tind>(j, col_idx, TRUE);
				for ( size_t k = 0; k < ni; k++ ) {
					switch(TYPEOF(i)) {
						case INTSXP:
							rindex = index(INTEGER_ELT(i, k) - 1);
							rdata = data(INTEGER_ELT(i, k) - 1);
							break;
						case REALSXP:
							rindex = index(REAL_ELT(i, k) - 1);
							rdata = data(REAL_ELT(i, k) - 1);
							break;
					}
					size_t rnnz = LENGTH(rdata);
					Tval * rbuffer = buffer + k;
					num_nz += do_approx_search<Tind,Tval>(rbuffer, col_idx, LENGTH(j),
						rindex, rdata, 0, rnnz, tol(), tol_ref(), zero(),
						sampler(), TRUE, ni);
				}
				Free(col_idx);
			}
			return num_nz;
		}

		SEXP getElements(SEXP i, SEXP j)
		{
			SEXP result;
			size_t ni = i != R_NilValue ? LENGTH(i) : dim(0);
			size_t nj = j != R_NilValue ? LENGTH(j) : dim(1);
			switch(datamode()) {
				case R_INTEGER:
				{
					PROTECT(result = Rf_allocMatrix(INTSXP, ni, nj));
					switch(indexmode()) {
						case R_INTEGER:
							getElements<int,int>(i, j, INTEGER(result));
							break;
						case R_NUMERIC:
							getElements<double,int>(i, j, INTEGER(result));
							break;
					}
					break;
				}
				break;
				case R_NUMERIC:
				{
					PROTECT(result = Rf_allocMatrix(REALSXP, ni, nj));
					switch(indexmode()) {
						case R_INTEGER:
							getElements<int,double>(i, j, REAL(result));
							break;
						case R_NUMERIC:
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
