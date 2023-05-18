#ifndef SPARSE
#define SPARSE

#include "matter.h"
#include "search.h"

class Sparse : public ArrayInterface {

	public:

		Sparse(SEXP x)
		{
			_data = R_do_slot(x, Rf_install("data"));
			_type = Rf_asInteger(R_do_slot(x, Rf_install("type")));
			_index = R_do_slot(x, Rf_install("index"));
			_pointers = R_do_slot(x, Rf_install("pointers"));
			_domain = R_do_slot(x, Rf_install("domain"));
			_offset = Rf_asInteger(R_do_slot(x, Rf_install("offset")));
			_dim = R_do_slot(x, Rf_install("dim"));
			SEXP tol = R_do_slot(x, Rf_install("tolerance"));
			SEXP tol_type = Rf_getAttrib(tol, Rf_install("tol_type"));
			_tol = Rf_asReal(tol);
			_tol_type = Rf_asInteger(tol_type);
			_sampler = Rf_asInteger(R_do_slot(x, Rf_install("sampler")));
		}

		~Sparse() {}

		SEXP data() {
			return _data;
		}

		SEXPTYPE datatype() {
			switch(_type) {
				case R_INTEGER:
					return INTSXP;
				case R_DOUBLE:
					return REALSXP;
				default:
					Rf_error("unsupported sparse data type");
			}
		}

		SEXP index() {
			return _index;
		}

		SEXPTYPE indextype() {
			if ( Rf_isReal(domain()) )
				return REALSXP; // promote if domain is double
			if ( Rf_isS4(_index) )
			{
				SEXP mode = R_do_slot(_index, Rf_install("type"));
				switch(Rf_asInteger(mode)) {
					case R_INTEGER:
						return INTSXP;
					case R_DOUBLE:
						return REALSXP;
					default:
						Rf_error("unsupported sparse index type");
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

		SEXP domain() {
			return _domain;
		}

		bool has_domain() {
			return !Rf_isNull(_domain);
		}

		SEXP pointers() {
			return _pointers;
		}

		bool has_pointers() {
			return !Rf_isNull(_pointers);
		}

		Pair<index_t,index_t> pointers(index_t i)
		{
			Pair<index_t,index_t> p;
			if ( has_pointers() )
			{
				p.first = IndexElt(_pointers, i);
				p.second = IndexElt(_pointers, i + 1);
			}
			else
				p = {0, 0};
			return p;
		}

		int offset() {
			return _offset;
		}

		double tol() {
			return _tol;
		}

		int tol_ref() {
			return (_tol_type == ABS_DIFF) ? ABS_DIFF : REL_DIFF_Y;
		}

		int sampler() {
			return _sampler;
		}

		template<typename T>
		T zero() {
			return static_cast<T>(0);
		}

	protected:

		SEXP _data;
		int _type;
		SEXP _index;
		SEXP _pointers;
		SEXP _domain;
		int _offset;
		double _tol;
		int _tol_type;
		int _sampler;
};


class SparseArray : public Sparse {

	public:

		SparseArray(SEXP x) : Sparse(x), _ops(x)
		{
			_transpose = Rf_asLogical(R_do_slot(x, Rf_install("transpose")));
			if ( rank() == 1 )
			{
				_dense_extent = 1;
				_sparse_extent = dim(0);
			}
			else if ( _transpose )
			{
				_dense_extent = dim(0);
				_sparse_extent = length() / dim(0);
			}
			else
			{
				_dense_extent = dim(rank() - 1);
				_sparse_extent = length() / dim(rank() - 1);
			}
		}

		bool is_transposed() {
			return _transpose;
		}

		index_t dense_extent() {
			return _dense_extent;
		}

		index_t sparse_extent() {
			return _sparse_extent;
		}

		DeferredOps * ops() {
			return &_ops;
		}

		bool has_ops() {
			return ops()->nops() != 0;
		}

		bool has_list_storage() {
			if ( Rf_isS4(_data) )
				return Rf_inherits(_data, "matter_list");
			else
				return Rf_isVectorList(_data);
		}

		template<typename T>
		void copy_domain(index_t i, size_t size, T * buffer)
		{
			if ( i < 0 || i + size > sparse_extent() )
				Rf_error("subscript out of bounds");
			for ( size_t j = 0; j < size; i++, j++ ) {
				switch(TYPEOF(domain())) {
					case NILSXP:
						buffer[j] = i + j + offset();
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
				if ( i - ind1 < 0 || i - ind1 >= sparse_extent() )
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

		SEXP data(index_t i)
		{
			SEXP ans;
			if ( i < 0 || i >= dense_extent() )
				Rf_error("subscript out of bounds");
			Pair<index_t,index_t> p = pointers(i);
			if ( has_pointers() && p.first == p.second )
				return Rf_allocVector(datatype(), 0);
			if ( has_list_storage() )
			{
				if ( Rf_isS4(_data) )
				{
					MatterList x(_data);
					ans = x.get(i);
				}
				else
					ans = VECTOR_ELT(_data, i);
			}
			else if ( has_pointers() )
			{
				if ( Rf_isS4(_data) )
				{
					MatterArray x(_data);
					ans = x.get_region(p.first, p.second - p.first);
				}
				else
					ans = extract_region(_data, p.first, p.second - p.first);
			}
			else
			{
				size_t n;
				if ( Rf_isS4(_index) )
				{
					MatterArray x(_index);
					n = x.length();
				}
				else
					n = XLENGTH(_index);
				if ( Rf_isS4(_data) )
				{
					MatterArray x(_data);
					ans = x.get_region(i * n, n);
				}
				else
					ans = extract_region(_data, i * n, n);
			}
			PROTECT(ans);
			ans = Rf_coerceVector(ans, datatype());
			UNPROTECT(1);
			return ans;
		}

		SEXP index(index_t i)
		{
			SEXP ans;
			if ( i < 0 || i >= dense_extent() )
				Rf_error("subscript out of bounds");
			Pair<index_t,index_t> p = pointers(i);
			if ( has_pointers() && p.first == p.second )
				return Rf_allocVector(indextype(), 0);
			if ( has_list_storage() )
			{
				if ( Rf_isS4(_index) )
				{
					MatterList x(_index);
					ans = x.get(i);
				}
				else
					ans = VECTOR_ELT(_index, i);
			}
			else if ( has_pointers() )
			{
				if ( Rf_isS4(_index) )
				{
					MatterArray x(_index);
					ans = x.get_region(p.first, p.second - p.first);
				}
				else
					ans = extract_region(_index, p.first, p.second - p.first);
			}
			else
			{
				if ( Rf_isS4(_index) )
				{
					MatterArray x(_index);
					ans = x.get_region(0, x.length());
				}
				else
					ans = _index;
			}
			PROTECT(ans);
			ans = Rf_coerceVector(ans, indextype());
			UNPROTECT(1);
			return ans;
		}

		template<typename Tind, typename Tval>
		size_t get_compressed_region(index_t at,
			index_t i, size_t size, Tval * buffer, int stride = 1)
		{
			if ( at < 0 || at > dense_extent() )
				Rf_error("subscript out of bounds");
			if ( i < 0 || i + size > sparse_extent() )
				Rf_error("subscript out of bounds");
			if ( isNA(at) ) {
				fill<Tval>(buffer, size, NA<Tval>(), stride);
				return 0;
			}
			SEXP j, x;
			PROTECT(j = index(at));
			PROTECT(x = data(at));
			size_t nnz = 0;
			if ( has_domain() )
			{
				Tind subscripts [size];
				copy_domain<Tind>(i, size, subscripts);
				nnz = do_approx_search<Tind,Tval>(buffer, subscripts, size,
					DataPtr<Tind>(j), DataPtr<Tval>(x), 0, XLENGTH(j),
					tol(), tol_ref(), zero<Tval>(), sampler(), stride);
			}
			else
			{
				fill<Tval>(buffer, size, zero<Tval>(), stride);
				for ( index_t k = 0; k < XLENGTH(x); k++ )
				{
					Tind * pj = DataPtr<Tind>(j);
					Tval * px = DataPtr<Tval>(x);
					index_t ii = static_cast<index_t>(pj[k]) - i;
					if ( ii < 0 || ii >= size )
						continue;
					buffer[(ii - offset()) * stride] = px[k];
					nnz++;
				}
			}
			UNPROTECT(2);
			return nnz;
		}

		template<typename Tind, typename Tval>
		size_t get_compressed_elements(index_t at,
			SEXP indx, Tval * buffer, int stride = 1)
		{
			if ( Rf_isNull(indx) )
				return get_compressed_region<Tind,Tval>(at,
					0, sparse_extent(), buffer, stride);
			if ( at < 0 || at > dense_extent() )
				Rf_error("subscript out of bounds");
			if ( isNA(at) ) {
				fill<Tval>(buffer, XLENGTH(indx), NA<Tval>(), stride);
				return 0;
			}
			SEXP j, x;
			PROTECT(j = index(at));
			PROTECT(x = data(at));
			Tind subscripts [XLENGTH(indx)];
			copy_domain<Tind>(indx, subscripts);
			size_t nnz = do_approx_search<Tind,Tval>(buffer, subscripts,
				XLENGTH(indx), DataPtr<Tind>(j), DataPtr<Tval>(x), 0, XLENGTH(j),
				tol(), tol_ref(), zero<Tval>(), sampler(), stride);
			UNPROTECT(2);
			return nnz;
		}

		template<typename Tind, typename Tval>
		size_t get_region(index_t i, size_t size, Tval * buffer, int stride = 1)
		{
			size_t nnz;
			if ( rank() == 1 )
				nnz = get_compressed_region<Tind,Tval>(0, i, size, buffer, stride);
			else
				Rf_error("linear indexing for sparse arrays is not supported yet");
			if ( has_ops() )
				ops()->apply<Tval>(buffer, i, size, stride);
			return nnz;
		}

		template<typename Tind, typename Tval>
		size_t get_elements(SEXP indx, Tval * buffer, int stride = 1)
		{
			size_t nnz;
			if ( rank() == 1 )
				nnz = get_compressed_elements<Tind,Tval>(0, indx, buffer, stride);
			else
				Rf_error("linear indexing for sparse arrays is not supported yet");
			if ( has_ops() )
				ops()->apply<Tval>(buffer, indx, stride);
			return nnz;
		}

		SEXP get_region(index_t i, size_t size)
		{
			SEXP x;
			if ( rank() != 1 )
				Rf_error("linear indexing for sparse arrays is not supported yet");
			PROTECT(x = Rf_allocVector(datatype(), size));
			switch(datatype()) {
				case INTSXP:
					switch(indextype()) {
						case INTSXP:
							get_region<int,int>(i, size, INTEGER(x));
							break;
						case REALSXP:
							get_region<double,int>(i, size, INTEGER(x));
							break;
						default:
							Rf_error("unsupported sparse index type");
					}
					break;
				break;
				case REALSXP:
					switch(indextype()) {
						case INTSXP:
							get_region<int,double>(i, size, REAL(x));
							break;
						case REALSXP:
							get_region<double,double>(i, size, REAL(x));
							break;
						default:
							Rf_error("unsupported sparse index type");
					}
					break;
				default:
					Rf_error("unsupported sparse data type");
			}
			UNPROTECT(1);
			return x;
		}

		SEXP get_elements(SEXP indx)
		{
			SEXP x;
			if ( rank() != 1 )
				Rf_error("linear indexing for sparse arrays is not supported yet");
			if ( Rf_isNull(indx) )
				return get_region(0, length());
			PROTECT(x = Rf_allocVector(datatype(), XLENGTH(indx)));
			switch(datatype()) {
				case INTSXP:
					switch(indextype()) {
						case INTSXP:
							get_elements<int,int>(indx, INTEGER(x));
							break;
						case REALSXP:
							get_elements<double,int>(indx, INTEGER(x));
							break;
						default:
							Rf_error("unsupported sparse index type");
					}
					break;
				break;
				case REALSXP:
					switch(indextype()) {
						case INTSXP:
							get_elements<int,double>(indx, REAL(x));
							break;
						case REALSXP:
							get_elements<double,double>(indx, REAL(x));
							break;
						default:
							Rf_error("unsupported sparse index type");
					}
					break;
				default:
					Rf_error("unsupported sparse data type");
			}
			UNPROTECT(1);
			return x;
		}

	protected:

		DeferredOps _ops;
		index_t _dense_extent;
		index_t _sparse_extent;
		bool _transpose;

};

class SparseMatrix : public SparseArray
{
	public:

		SparseMatrix(SEXP x) : SparseArray(x) {}

		template<typename Tind, typename Tval>
		size_t get_submatrix(SEXP i, SEXP j, Tval * buffer, int stride = 1)
		{
			size_t nnz = 0;
			int nr = Rf_isNull(i) ? nrow() : LENGTH(i);
			int nc = Rf_isNull(j) ? ncol() : LENGTH(j);
			int s1 = is_transposed() ? (nr * stride) : stride;
			int s2 = is_transposed() ? stride : (nr * stride);
			if ( is_transposed() )
			{
				for ( size_t k = 0; k < nr; k++ )
				{
					index_t row = k;
					if ( !Rf_isNull(i) ) {
						row = IndexElt(i, k);
						row = isNA(row) ? row : row - 1;
					}
					if ( isNA(row) )
						nnz += fill<Tval>(buffer + k * s2, nc, NA<Tval>(), s1);
					else if ( Rf_isNull(j) )
						nnz += get_compressed_region<Tind,Tval>(row, 0, ncol(), buffer + k * s2, s1);
					else
						nnz += get_compressed_elements<Tind,Tval>(row, j, buffer + k * s2, s1);
				}
			}
			else
			{
				for ( size_t k = 0; k < nc; k++ )
				{
					index_t col = k;
					if ( !Rf_isNull(j) ) {
						col = IndexElt(j, k);
						col = isNA(col) ? col : col - 1;
					}
					if ( isNA(col) )
						nnz += fill<Tval>(buffer + k * s2, nr, NA<Tval>(), s1);
					else if ( Rf_isNull(i) )
						nnz += get_compressed_region<Tind,Tval>(col, 0, nrow(), buffer + k * s2, s1);
					else
						nnz += get_compressed_elements<Tind,Tval>(col, i, buffer + k * s2, s1);
				}
			}
			if ( has_ops() )
				ops()->apply<Tval>(buffer, i, j, stride);
			return nnz;
		}

		SEXP get_submatrix(SEXP i, SEXP j)
		{
			SEXP x;
			int nr = Rf_isNull(i) ? nrow() : LENGTH(i);
			int nc = Rf_isNull(j) ? ncol() : LENGTH(j);
			PROTECT(x = Rf_allocMatrix(datatype(), nr, nc));
			switch(datatype()) {
				case INTSXP:
					switch(indextype()) {
						case INTSXP:
							get_submatrix<int,int>(i, j, INTEGER(x));
							break;
						case REALSXP:
							get_submatrix<double,int>(i, j, INTEGER(x));
							break;
						default:
							Rf_error("unsupported sparse index type");
					}
					break;
				break;
				case REALSXP:
					switch(indextype()) {
						case INTSXP:
							get_submatrix<int,double>(i, j, REAL(x));
							break;
						case REALSXP:
							get_submatrix<double,double>(i, j, REAL(x));
							break;
						default:
							Rf_error("unsupported sparse index type");
					}
					break;
				default:
					Rf_error("unsupported sparse data type");
			}
			UNPROTECT(1);
			return x;
		}

};

#endif // SPARSE
