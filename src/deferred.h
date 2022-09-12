#ifndef DEFERRED
#define DEFERRED

#include "matterDefines.h"
#include "coerce.h"

//// DeferredOps class
//---------------------

class DeferredOps {

	public:

		DeferredOps(SEXP x, SEXP dim)
		{
			if ( Rf_isNull(x) )
			{
				_ops = INTEGER(R_do_slot(x, Rf_install("ops")));
				_arg = R_do_slot(x, Rf_install("arg"));
				_rhs = INTEGER(R_do_slot(x, Rf_install("rhs")));
				_margin = INTEGER(R_do_slot(x, Rf_install("margin")));
				_group = R_do_slot(x, Rf_install("group"));
				_nops = LENGTH(_ops);
			}
			_dim = dim;
		}

		DeferredOps(SEXP x)
		{
			DeferredOps(
				R_do_slot(x, Rf_install("ops")),
				R_do_slot(x, Rf_install("dim")));
		}

		~DeferredOps() {}

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

		int op(int i) {
			return _ops[i];
		}

		SEXP arg(int i) {
			return VECTOR_ELT(_arg, i);
		}

		template<typename T>
		T arg(int i, int j) {
			int s = arglen(i);
			int grp = group(i, j);
			switch(argtype(i)) {
				case RAWSXP:
					return coerce_cast<T>(RAW(arg(i))[s * grp + j]);
				case LGLSXP:
					return coerce_cast<T>(LOGICAL(arg(i))[s * grp + j]);
				case INTSXP:
					return coerce_cast<T>(INTEGER(arg(i))[s * grp + j]);
				case REALSXP:
					return coerce_cast<T>(REAL(arg(i))[s * grp + j]);
				default:
					return 0;
			}
		}

		SEXPTYPE argtype(int i) {
			return TYPEOF(arg(i));
		}

		R_xlen_t arglen(int i) {
			if ( has_groups(i) )
				return Rf_nrows(arg(i));
			else
				XLENGTH(arg(i));
		}

		bool is_unary(int i) {
			return Rf_isNull(_arg[i]);
		}

		bool is_rhs(int i) {
			return _rhs[i];
		}

		int margin(int i) {
			return _margin[i];
		}

		bool has_groups(int i) {
			return !Rf_isNull(_group[i]);
		}

		bool ngroups(int i) {
			if ( has_groups(i) )
				return Rf_ncols(arg(i));
			else
				return 0;
		}

		SEXP group(int i) {
			if ( has_groups(i) )
				return VECTOR_ELT(_group, i);
			else
				return R_NilValue;
		}

		int group(int i, int j) {
			if ( has_groups(i) )
				return INTEGER(group(i))[j];
			else
				return 0;
		}

	protected:

		int _nops;
		int * _ops;
		SEXP _arg;
		int * _rhs;
		int * _margin;
		SEXP _group;
		SEXP _dim;

};

#endif // DEFERRED
