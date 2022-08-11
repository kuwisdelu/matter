
#ifndef SPARSE
#define SPARSE

#include "matter.h"

extern "C" {

	SEXP Mt_getSparseVector(SEXP x, SEXP i);

}

class Sparse
{
	public:

		Sparse(SEXP x)
		{
			_data = R_do_slot(x, Rf_install("data"));
			_datamode = INTEGER(R_do_slot(x, Rf_install("datamode")));
			_length = static_cast<index_t>(Rf_asReal(R_do_slot(x, Rf_install("length"))));
			_dim = R_do_slot(x, Rf_install("dim"));
			_dimnames = R_do_slot(x, Rf_install("dimnames"));
			_names = R_do_slot(x, Rf_install("names"));
			_index = R_do_slot(x, Rf_install("index"));
			_offset = Rf_asInteger(R_do_slot(x, Rf_install("offset")));
			_domain = R_do_slot(x, Rf_install("domain"));
			SEXP tol = R_do_slot(x, Rf_install("tolerance"));
			SEXP tol_type = Rf_getAttrib(tol, Rf_install("tol_type"));
			_tol = Rf_asReal(tol);
			_tol_type = Rf_asInteger(tol_type);
			_combiner = Rf_asInteger(R_do_slot(x, Rf_install("combiner")));
			set_matter_options();
		}

		~Sparse(){}

		SEXP data() {
			if ( Rf_isS4(_data) )
				return getVector(_data);
			else
				return _data;
		}

		int datamode() {
			return _datamode[0];
		}

		index_t nnz() {
			if ( Rf_isS4(_data) )
				return static_cast<index_t>(Rf_asReal(R_do_slot(_data, Rf_install("length"))));
			else
				return XLENGTH(_data);
		}

		index_t length() {
			return _length;
		}

		int dim(int i) {
			return INTEGER(_dim)[i];
		}

		int dimlength() {
			return LENGTH(_dim);
		}

		SEXP dimnames() {
			return _dimnames;
		}

		SEXP names() {
			return _names;
		}

		SEXP index() {
			if ( Rf_isS4(_index) )
				return getVector(_index);
			else
				return _index;
		}

		int offset() {
			return _offset;
		}

		bool has_domain() {
			return _domain != R_NilValue;
		}

		SEXP domain() {
			return _domain;
		}

		template<typename TKey>
		void copy_domain(size_t i, size_t size, TKey * buffer)
		{
			TKey * pKeys = DataPtr<TKey>(domain());
			for ( size_t j = 0; j < size; j++ ) {
				buffer[j] = has_domain() ? pKeys[i] : i + offset();
				i++;
			}
		}

		template<typename TKey, typename TInd>
		void copy_domain(TInd * pindex, size_t size, TKey * buffer)
		{
			TKey * pKeys = DataPtr<TKey>(domain());
			for ( size_t j = 0; j < size; j++ )
			{
				index_t i = static_cast<index_t>(pindex[j]);
				buffer[j] = has_domain() ? pKeys[i] : i + offset();
			}
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

		int combiner() {
			return _combiner;
		}

	protected:

		SEXP _data;
		int * _datamode;  // 1 = raw, 2 = logical, 3 = integer, 4 = numeric
		SEXP _index;
		int _offset;
		SEXP _domain;
		index_t _length;
		SEXP _dim;
		SEXP _dimnames;
		SEXP _names;
		double _tol;
		int _tol_type; // 1 = absolute, 2 = relative
		int _combiner;
};

class SparseVector : public Sparse
{
	public:

		SparseVector(SEXP x) : Sparse(x) {}

		template<typename TKey, typename TVal>
		TVal get(size_t i)
		{
			TKey subset = has_domain() ? DataPtr<TKey>(domain())[i] : i;
			TVal val = approx_search<TKey,TVal>(subset, index(), data(),
				0, length(), tol(), tol_ref(), zero(), combiner(), TRUE).second;
			return val;
		}

		template<typename TKey, typename TVal>
		size_t getRegion(size_t i, size_t size, TVal * buffer)
		{
			size_t num_nz = 0;
			if ( has_domain() ) {
				TKey * region_idx = (TKey *) Calloc(size, TKey);
				copy_domain<TKey>(i, size, region_idx);
				num_nz = do_approx_search<TKey,TVal>(buffer, region_idx, size,
					index(), data(), 0, nnz(), tol(), tol_ref(), zero(), combiner(), TRUE);
				Free(region_idx);
			}
			else {
				for ( size_t j = 0; j < size; j++ )
					buffer[j] = zero();
				for ( size_t k = 0; k < nnz(); k++ )
				{
					TKey * pIndex = DataPtr<TKey>(index());
					TVal * pData = DataPtr<TVal>(data());
					index_t indk = static_cast<index_t>(pIndex[k]);
					buffer[indk + i - offset()] = pData[k];
					num_nz++;
				}
			}
			return num_nz;
		}

		template<typename TKey, typename TVal>
		size_t getElements(SEXP i, TVal * buffer)
		{
			TKey * element_idx = (TKey *) Calloc(XLENGTH(i), TKey);
			switch(TYPEOF(i)) {
				case INTSXP:
					copy_domain<TKey,int>(INTEGER(i), XLENGTH(i), element_idx);
					break;
				case REALSXP:
					copy_domain<TKey,double>(REAL(i), XLENGTH(i), element_idx);
					break;
			}
			size_t num_nz = do_approx_search<TKey,TVal>(buffer, element_idx, XLENGTH(i),
				index(), data(), 0, nnz(), tol(), tol_ref(), zero(), combiner(), TRUE);
			Free(element_idx);
			return num_nz;
		}

		template<typename TKey, typename TVal, int S>
		SEXP getElements(SEXP i)
		{
			SEXP retVec;
			if ( i == R_NilValue ) {
				PROTECT(retVec = Rf_allocVector(S, length()));
				getRegion<TKey,TVal>(0, length(), DataPtr<TVal>(retVec));
			}
			else {
				PROTECT(retVec = Rf_allocVector(S, XLENGTH(i)));
				getElements<TKey,TVal>(i, DataPtr<TVal>(retVec));
			}
			UNPROTECT(1);
			return retVec;
		}

};

#endif
