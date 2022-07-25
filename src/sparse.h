
#ifndef SPARSE
#define SPARSE

#include "matter.h"

extern "C" {

	SEXP getSparseVector(SEXP x, SEXP i);

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
			_keys = R_do_slot(x, Rf_install("keys"));
			SEXP tol = R_do_slot(x, Rf_install("tolerance"));
			SEXP tol_type = Rf_getAttrib(tol, Rf_install("tol_type"));
			_tol = Rf_asReal(tol);
			_tol_type = Rf_asInteger(tol_type);
			_combiner = Rf_asInteger(R_do_slot(x, Rf_install("combiner")));
			set_matter_options();
		}

		~Sparse(){}

		SEXP data() {
			return _data;
		}

		int datamode() {
			return _datamode[0];
		}

		int nnz() {
			return XLENGTH(data());
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
			return _index;
		}

		bool has_keys() {
			return XLENGTH(_keys) > 1; // scalar = offset
		}

		SEXP keys() {
			return _keys;
		}

		template<typename TKey>
		void copy_keys(size_t i, size_t size, TKey * buffer)
		{
			TKey * pKeys = DataPtr<TKey>(keys());
			for ( size_t j = 0; j < size; i++, j++ )
			{
				int offset = static_cast<TKey>(pKeys[0]);
				buffer[j] = has_keys() ? pKeys[i] : i + offset;
			}
		}

		template<typename TKey, typename TInd>
		void copy_keys(TInd * pindex, size_t size, TKey * buffer)
		{
			TKey * pKeys = DataPtr<TKey>(keys());
			for ( size_t j = 0; j < size; j++ )
			{
				index_t i = static_cast<index_t>(pindex[j]);
				buffer[j] = has_keys() ? pKeys[i] : i;
			}
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
		index_t _length;
		SEXP _dim;
		SEXP _dimnames;
		SEXP _names;
		SEXP _index;
		SEXP _keys;
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
			TKey subset = has_keys() ? DataPtr<TKey>(keys())[i] : i;
			TVal val = keyval_search<TKey,TVal>(subset, index(), data(),
				0, length(), tol(), tol_ref(), 0, combiner(), TRUE).second;
			return val;
		}

		template<typename TKey, typename TVal>
		size_t getRegion(size_t i, size_t size, TVal * buffer)
		{
			TKey * region_idx = (TKey *) Calloc(size, TKey);
			copy_keys<TKey>(i, size, region_idx);
			size_t num_read = do_keyval_search<TKey,TVal>(buffer, region_idx, size,
				index(), data(), 0, nnz(), tol(), tol_ref(), 0, combiner(), TRUE);
			Free(region_idx);
			return num_read;
		}

		template<typename TKey, typename TVal>
		size_t getElements(SEXP i, TVal * buffer)
		{
			TKey * element_idx = (TKey *) Calloc(XLENGTH(i), TKey);
			switch(TYPEOF(i)) {
				case INTSXP:
					copy_keys<TKey,int>(INTEGER(i), XLENGTH(i), element_idx);
					break;
				case REALSXP:
					copy_keys<TKey,double>(REAL(i), XLENGTH(i), element_idx);
					break;
			}
			size_t num_read = do_keyval_search<TKey,TVal>(buffer, element_idx, XLENGTH(i),
				index(), data(), 0, nnz(), tol(), tol_ref(), 0, combiner(), TRUE);
			Free(element_idx);
			return num_read;
		}

		template<typename TKey, typename TVal, int S>
		SEXP getElements(SEXP i)
		{
			SEXP retVec;
			if ( i != R_NilValue ) {
				PROTECT(retVec = Rf_allocVector(S, XLENGTH(i)));
				getElements<TKey,TVal>(i, DataPtr<TVal>(retVec));
			}
			else {
				PROTECT(retVec = Rf_allocVector(S, length()));
				getRegion<TKey,TVal>(0, length(), DataPtr<TVal>(retVec));
			}
			UNPROTECT(1);
			return retVec;
		}

};

#endif
