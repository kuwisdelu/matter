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

		~Sparse() {}

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
						Rf_error("unsupported index type");
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

		index_t length() {
			return _length;
		}

		index_t majordim() {
			return _majordim;
		}

		index_t minordim() {
			return _minordim;
		}

		int dim(int i) {
			return INTEGER(_dim)[i];
		}

		bool has_dims() {
			return _dim != R_NilValue;
		}

		SEXP dimnames() {
			return _dimnames;
		}

		SEXP names() {
			return _names;
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
		T zero() {
			return static_cast<T>(0);
		}

		template<typename T>
		void copy_domain(index_t i, size_t size, T * buffer)
		{
			if ( i < 0 || i + size > minordim() )
				Rf_error("subscript out of bounds");
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
		void copy_domain(SEXP indx, T * buffer, bool ind1 = true)
		{
			index_t i = 0, len = XLENGTH(indx);
			for ( index_t j = 0; j < len; j++ )
			{
				i = IndexElt(indx, j);
				if ( isNA(i) ) {
					buffer[j] = NA<T>();
					continue;
				}
				if ( i - ind1 < 0 || i - ind1 >= minordim() )
					Rf_error("subscript out of bounds");
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
		int * _datamode;
		SEXP _index;
		int _offset;
		SEXP _domain;
		SEXP _pointers;
		index_t _length;
		index_t _majordim = 0; // set in derived class
		index_t _minordim = 0; // set in derived class
		SEXP _dim;
		SEXP _dimnames;
		SEXP _names;
		double _tol;
		int _tol_type;
		int _sampler;
};

class SparseVector : public Sparse
{
	public:

		SparseVector(SEXP x) : Sparse(x)
		{
			_majordim = 1;
			_minordim = length();
		}

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

		template<typename Tind, typename Tval>
		Tval get(index_t i)
		{
			SEXP index0, data0;
			Tind x = NA_INTEGER;
			if ( i < 0 || i >= length() )
				Rf_error("subscript out of bounds");
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
			Tval val = approx_search(x, index0, data0, 0, XLENGTH(index0),
				tol(), tol_ref(), zero<Tval>(), sampler()).second;
			UNPROTECT(2);
			return val;
		}

		template<typename Tind, typename Tval>
		size_t getRegion(index_t i, size_t size, Tval * buffer)
		{
			SEXP index0, data0;
			PROTECT(index0 = index());
			PROTECT(data0 = data());
			size_t num_nz = 0;
			if ( has_domain() ) {
				Tind subscripts [size];
				copy_domain<Tind>(i, size, subscripts);
				num_nz = do_approx_search<Tind,Tval>(buffer, subscripts, size,
					index0, data0, tol(), tol_ref(), zero<Tval>(), sampler());
			}
			else {
				if ( i < 0 || i + size > length() )
					Rf_error("subscript out of bounds");
				for ( size_t j = 0; j < size; j++ )
					buffer[j] = zero<Tval>();
				for ( size_t k = 0; k < XLENGTH(index0); k++ )
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

		SEXP getRegion(index_t i, size_t size)
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
			copy_domain<Tind>(indx, subscripts, true);
			size_t num_nz = do_approx_search<Tind,Tval>(buffer, subscripts, size,
				index0, data0, tol(), tol_ref(), zero<Tval>(), sampler());
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

		int nrow() {
			return dim(0);
		}

		int ncol() {
			return dim(1);
		}

		SEXP data(index_t i)
		{
			SEXP indx, data0;
			if ( i < 0 || i >= majordim() )
				Rf_error("subscript out of bounds");
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
						indx = seq_range(p.first, p.second - 1);
					else {
						size_t n = XLENGTH(_index);
						indx = seq_range(i * n + 1, (i + 1) * n);
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
					data0 = extract_range(_data, p.first + 1, p.second);
				else {
					size_t n = XLENGTH(_index);
					data0 = extract_range(_data, i * n + 1, (i + 1) * n);
				}
			}
			PROTECT(data0);
			data0 = Rf_coerceVector(data0, datatype());
			UNPROTECT(1);
			return data0;
		}

		SEXP index(index_t i)
		{
			SEXP indx, index0;
			if ( i < 0 || i >= majordim() )
				Rf_error("subscript out of bounds");
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
						PROTECT(indx = seq_range(p.first, p.second - 1));
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
				if ( has_pointers() )
					index0 = extract_range(_index, p.first + 1, p.second);
				else
					index0 = _index;
			}
			PROTECT(index0);
			index0 = Rf_coerceVector(index0, indextype());
			UNPROTECT(1);
			return index0;
		}

		template<typename Tind, typename Tval>
		size_t getSubVector(index_t i, Tval * buffer, int stride = 1)
		{
			if ( isNA(i) ) {
				fill<Tval>(buffer, minordim(), NA<Tval>(), stride);
				return 0;
			}
			SEXP index0, data0;
			PROTECT(index0 = index(i));
			PROTECT(data0 = data(i));
			size_t num_nz = LENGTH(data0);
			if ( has_domain() ) {
				Tind subscripts [minordim()];
				copy_domain<Tind>(0, minordim(), subscripts);
				num_nz = do_approx_search<Tind,Tval>(buffer, subscripts, minordim(),
					index0, data0, tol(), tol_ref(), zero<Tval>(),
					sampler(), true, stride);
			}
			else {
				fill<Tval>(buffer, minordim(), zero<Tval>(), stride);
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

};

class SparseMatrixC : public SparseMatrix
{
	public:

		SparseMatrixC(SEXP x) : SparseMatrix(x)
		{
			_majordim = ncol();
			_minordim = nrow();
		}

		template<typename Tind, typename Tval>
		size_t getCol(index_t i, Tval * buffer, int stride = 1)
		{
			return getSubVector<Tind,Tval>(i, buffer, stride);
		}

		template<typename Tind, typename Tval>
		size_t getSubMatrix(SEXP i, SEXP j, Tval * buffer)
		{
			SEXP index0, data0;
			size_t num_nz = 0;
			size_t ni = i != R_NilValue ? LENGTH(i) : nrow();
			size_t nj = j != R_NilValue ? LENGTH(j) : ncol();
			if ( j == R_NilValue )
				j = seq_range(1, nj);
			PROTECT(j);
			if ( i == R_NilValue && !has_domain() )
			{
				// get full columns
				for ( size_t k = 0; k < nj; k++ ) {
					Tval * buffer0 = buffer + (k * ni);
					index_t indk = IndexElt(j, k);
					if ( isNA(indk) )
						fill<Tval>(buffer0, nrow(), NA<Tval>(), 1);
					else
						num_nz += getCol<Tind,Tval>(indk - 1, buffer0, 1);
				}
			}
			else
			{
				// get indexed columns
				Tind subscripts [ni];
				if ( i == R_NilValue )
					copy_domain<Tind>(0, ni, subscripts);
				else
					copy_domain<Tind>(i, subscripts, true);
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
						index0, data0, tol(), tol_ref(), zero<Tval>(),
						sampler(), true, 1);
					UNPROTECT(2);
				}
			}
			UNPROTECT(1);
			return num_nz;
		}

		SEXP getSubMatrix(SEXP i, SEXP j)
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
							getSubMatrix<int,int>(i, j, INTEGER(result));
							break;
						case REALSXP:
							getSubMatrix<double,int>(i, j, INTEGER(result));
							break;
					}
					break;
				}
				break;
				case REALSXP:
				{
					switch(indextype()) {
						case INTSXP:
							getSubMatrix<int,double>(i, j, REAL(result));
							break;
						case REALSXP:
							getSubMatrix<double,double>(i, j, REAL(result));
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

		SparseMatrixR(SEXP x) : SparseMatrix(x)
		{
			_majordim = nrow();
			_minordim = ncol();
		}

		template<typename Tind, typename Tval>
		size_t getRow(index_t i, Tval * buffer, int stride = 1)
		{
			return getSubVector<Tind,Tval>(i, buffer, stride);
		}

		template<typename Tind, typename Tval>
		size_t getSubMatrix(SEXP i, SEXP j, Tval * buffer)
		{
			SEXP index0, data0;
			size_t num_nz = 0;
			size_t ni = i != R_NilValue ? LENGTH(i) : nrow();
			size_t nj = j != R_NilValue ? LENGTH(j) : ncol();
			if ( i == R_NilValue )
				i = seq_range(1, ni);
			PROTECT(i);
			if ( j == R_NilValue && !has_domain() )
			{
				// get full rows
				for ( size_t k = 0; k < ni; k++ ) {
					Tval * buffer0 = buffer + k;
					index_t indk = IndexElt(i, k);
					if ( isNA(indk) )
						fill<Tval>(buffer0, ncol(), NA<Tval>(), ni);
					else
						num_nz += getRow<Tind,Tval>(indk - 1, buffer0, ni);
				}
			}
			else
			{
				// get indexed rows
				Tind subscripts [nj];
				if ( j == R_NilValue )
					copy_domain<Tind>(0, nj, subscripts);
				else
					copy_domain<Tind>(j, subscripts, true);
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
						index0, data0, tol(), tol_ref(), zero<Tval>(),
						sampler(), true, ni);
					UNPROTECT(2);
				}
			}
			UNPROTECT(1);
			return num_nz;
		}

		SEXP getSubMatrix(SEXP i, SEXP j)
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
							getSubMatrix<int,int>(i, j, INTEGER(result));
							break;
						case REALSXP:
							getSubMatrix<double,int>(i, j, INTEGER(result));
							break;
					}
					break;
				}
				break;
				case REALSXP:
				{
					switch(indextype()) {
						case INTSXP:
							getSubMatrix<int,double>(i, j, REAL(result));
							break;
						case REALSXP:
							getSubMatrix<double,double>(i, j, REAL(result));
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
