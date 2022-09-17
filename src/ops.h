#ifndef DEFERRED_OPS
#define DEFERRED_OPS

#include "matterDefines.h"
#include "coerce.h"

//// DeferredOps class
//---------------------

class DeferredOps : public ArrayInterface {

	public:

		DeferredOps(SEXP x)
		{
			SEXP ops = R_do_slot(x, Rf_install("ops"));
			SEXP dim = R_do_slot(x, Rf_install("dim"));
			init(ops, dim);
		}

		DeferredOps(SEXP ops, SEXP dim)
		{
			init(ops, dim);
		}

		~DeferredOps() {}

		void init(SEXP ops, SEXP dim) {
			if ( !Rf_isNull(ops) )
			{
				_ops = INTEGER(R_do_slot(ops, Rf_install("ops")));
				_arg = R_do_slot(ops, Rf_install("arg"));
				_rhs = INTEGER(R_do_slot(ops, Rf_install("rhs")));
				_margins = INTEGER(R_do_slot(ops, Rf_install("margins")));
				_group = R_do_slot(ops, Rf_install("group"));
				_nops = LENGTH(R_do_slot(ops, Rf_install("ops")));
			}
			else
			{
				_ops = NULL;
				_arg = NULL;
				_rhs = NULL;
				_margins = NULL;
				_group = NULL;
				_nops = 0;	
			}
			_dim = dim;
		}

		int nops() {
			return _nops;
		}

		int op(int i) {
			return _ops[i];
		}

		bool is_unary(int i) {
			return Rf_isNull(arg(i));
		}

		SEXP arg(int i) {
			return VECTOR_ELT(_arg, i);
		}

		template<typename T>
		T arg(int i, int j, int grp = 0) {
			if ( is_unary(i) )
				return NA<T>();
			if ( arglen(i) == 1 )
				j = 0;
			int s = arglen(i);
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
			if ( is_unary(i) )
				return 0;
			if ( is_grouped(i) )
				return Rf_nrows(arg(i));
			else
				return XLENGTH(arg(i));
		}

		int argdim(int i) {
			if ( is_unary(i) )
				return 0;
			else
				return _margins[i] - 1;
		}

		bool is_rhs(int i) {
			return _rhs[i];
		}

		bool is_grouped(int i) {
			return !Rf_isNull(group(i));
		}

		SEXP group(int i) {
			return VECTOR_ELT(_group, i);
		}

		int group(int i, int j) {
			if ( is_grouped(i) )
				return INTEGER(group(i))[j];
			else
				return 0;
		}

		int grouplen(int i) {
			if ( is_grouped(i) )
				return XLENGTH(group(i));
			else
				return 0;
		}

		int groupdim(int i) {
			if ( is_grouped(i) )
				return _margins[nops() + i] - 1;
			else
				return 0;
		}

		template<typename T>
		size_t apply(T * x, index_t i, size_t size, int stride = 1)
		{
			size_t n = 0;
			int s [rank()];
			int arr_ind [rank()];
			s[0] = 1;
			for ( int k = 1; k < rank(); k++ )
				s[k] = s[k - 1] * dim(k - 1);
			for ( index_t j = 0; j < size; j++ )
			{
				for ( int l = 0; l < nops(); l++ )
				{
					T yj, xj = x[stride * j];
					T xjnew;
					if ( isNA(xj) )
						continue;
					if ( is_unary(l) )
					{
						switch(op(l)) {
							case MATH_LOG:
								xjnew = std::log(xj);
								break;
							case MATH_LOG10:
								xjnew = std::log10(xj);
								break;
							case MATH_LOG2:
								xjnew = std::log2(xj);
								break;
							case MATH_LOG1P:
								xjnew = std::log1p(xj);
								break;
							case MATH_EXP:
								xjnew = std::exp(xj);
								break;
							default:
								xjnew = NA<T>();
						}
					}
					else
					{
						for ( int k = 0; k < rank(); k++ )
							arr_ind[k] = ((i + j) / s[k]) % dim(k);
						int grp = group(l, arr_ind[groupdim(l)]);
						yj = arg<T>(l, arr_ind[argdim(l)], grp);
						if ( isNA(yj) ) {
							x[stride * j] = NA<T>();
							continue;
						}
						if ( is_rhs(l) ) {
							T tmp = xj;
							xj = yj;
							yj = tmp;
						}
						switch(op(l)) {
							case OP_ADD:
								xjnew = xj + yj;
								break;
							case OP_SUB:
								xjnew = xj - yj;
								break;
							case OP_MUL:
								xjnew = xj * yj;
								break;
							case OP_POW:
								xjnew = std::pow(xj, yj);
								break;
							case OP_MOD:
								xjnew = std::fmod(xj, yj);
								break;
							case OP_IDIV:
								xjnew = std::floor(xj / yj);
								break;
							case OP_DIV:
								xjnew = xj / yj;
								break;
							default:
								xjnew = NA<T>();
						}
					}
					x[stride * j] = xjnew;
				}
				n++;
			}
			return n;
		}

		template<typename T>
		size_t apply(T * x, SEXP indx, int stride = 1)
		{
			size_t n = 0;
			int s [rank()];
			int arr_ind [rank()];
			s[0] = 1;
			for ( int k = 1; k < rank(); k++ )
				s[k] = s[k - 1] * dim(k - 1);
			for ( index_t j = 0; j < XLENGTH(indx); j++ )
			{
				for ( int l = 0; l < nops(); l++ )
				{
					T yj, xj = x[stride * j];
					T xjnew;
					if ( isNA(xj) )
						continue;
					if ( is_unary(l) )
					{
						switch(op(l)) {
							case MATH_LOG:
								xjnew = std::log(xj);
								break;
							case MATH_LOG10:
								xjnew = std::log10(xj);
								break;
							case MATH_LOG2:
								xjnew = std::log2(xj);
								break;
							case MATH_LOG1P:
								xjnew = std::log1p(xj);
								break;
							case MATH_EXP:
								xjnew = std::exp(xj);
								break;
							default:
								xjnew = NA<T>();
						}
					}
					else
					{
						index_t i = IndexElt(indx, j) - 1;
						for ( int k = 0; k < rank(); k++ )
							arr_ind[k] = (i / s[k]) % dim(k);
						int grp = group(l, arr_ind[groupdim(l)]);
						yj = arg<T>(l, arr_ind[argdim(l)], grp);
						if ( isNA(yj) ) {
							x[stride * j] = NA<T>();
							continue;
						}
						if ( is_rhs(l) ) {
							T tmp = xj;
							xj = yj;
							yj = tmp;
						}
						switch(op(l)) {
							case OP_ADD:
								xjnew = xj + yj;
								break;
							case OP_SUB:
								xjnew = xj - yj;
								break;
							case OP_MUL:
								xjnew = xj * yj;
								break;
							case OP_POW:
								xjnew = std::pow(xj, yj);
								break;
							case OP_MOD:
								xjnew = std::fmod(xj, yj);
								break;
							case OP_IDIV:
								xjnew = std::floor(xj / yj);
								break;
							case OP_DIV:
								xjnew = xj / yj;
								break;
							default:
								xjnew = NA<T>();
						}
					}
					x[stride * j] = xjnew;
				}
				n++;
			}
			return n;
		}

	protected:

		int _nops;
		int * _ops;
		SEXP _arg;
		int * _rhs;
		int * _margins;
		SEXP _group;

};

#endif // DEFERRED_OPS
