
#ifndef MATTER_ALT
#define MATTER_ALT

#include "utils.h"
#include "matter.h"

extern "C"
{
  #include <R_ext/Altrep.h>
}

//// MatterAlt class
//------------------

static R_altrep_class_t MatterAlt_raw;
static R_altrep_class_t MatterAlt_logical;
static R_altrep_class_t MatterAlt_integer;
static R_altrep_class_t MatterAlt_real;
static R_altrep_class_t MatterAlt_string;

/* Matter-backed ALTREP objects

	data1: the original matter object (as a SEXP)
	data2: either NULL or a SEXP of the in-memory R object

*/

extern "C" {

	SEXP makeAltrep(SEXP x, SEXP attr, SEXP nm, SEXP dm, SEXP dnm, SEXP wrap);

	void init_MatterAlt_raw(DllInfo * info);

	void init_MatterAlt_logical(DllInfo * info);

	void init_MatterAlt_integer(DllInfo * info);

	void init_MatterAlt_real(DllInfo * info);

	void init_MatterAlt_string(DllInfo * info);

}

struct MatterAlt {

	// ALTREP methods

	static SEXP Serialized_state(SEXP x)
	{
		return R_altrep_data1(x);
	}

	static SEXP Unserialize(SEXP cls, SEXP state)
	{
		return makeAltrep(state, R_NilValue, R_NilValue,
			R_NilValue, R_NilValue, Rf_ScalarLogical(FALSE));
	}

	static R_xlen_t Length(SEXP x)
	{
		Matter mVec(R_altrep_data1(x));
		return static_cast<R_xlen_t>(mVec.length());
	}

	static Rboolean Inspect(SEXP x, int pre, int deep, int pvec,
		void (*inspect_subtree)(SEXP, int, int, int))
	{
		Matter mVec(R_altrep_data1(x));
		int inmem = R_altrep_data2(x) != R_NilValue;
		Rprintf("matter vector (mode=%d, len=%d, mem=%d)\n", mVec.datamode(), mVec.length(), inmem);
		return TRUE;
	}

	// ALTVEC methods

	static void * Dataptr(SEXP x, Rboolean writeable)
	{
		if ( R_altrep_data2(x) == R_NilValue )
		{
			SEXP data;
			Matter mVec(R_altrep_data1(x));
			if ( mVec.S4class() == MATTER_STR )
				PROTECT(data = getString(R_altrep_data1(x)));
			else
				PROTECT(data = getVector(R_altrep_data1(x)));
			R_set_altrep_data2(x, data);
			UNPROTECT(1);
		}
		return DATAPTR(R_altrep_data2(x));
	}

	static const void * Dataptr_or_null(SEXP x)
	{
		if ( R_altrep_data2(x) == R_NilValue )
			return NULL;
		else
			return DATAPTR(R_altrep_data2(x));
	}

	// ALTRAW methods

	static Rbyte raw_Elt(SEXP x, R_xlen_t i)
	{
		Matter mVec(R_altrep_data1(x));
		Rbyte ans;
		mVec.data().read<Rbyte>(&ans, i, 1);
		return ans;
	}

	static R_xlen_t raw_Get_region(SEXP x, R_xlen_t i, R_xlen_t size, Rbyte * buffer)
	{
		Matter mVec(R_altrep_data1(x));
		return mVec.data().read<Rbyte>(buffer, i, size);
	}

	// ALTLOGICAL methods

	static int logical_Elt(SEXP x, R_xlen_t i)
	{
		Matter mVec(R_altrep_data1(x));
		int ans;
		mVec.data().read<int>(&ans, i, 1);
		return ans;
	}

	static R_xlen_t logical_Get_region(SEXP x, R_xlen_t i, R_xlen_t size, int * buffer)
	{
		Matter mVec(R_altrep_data1(x));
		return mVec.data().read<int>(buffer, i, size);
	}

	// ALTINTEGER methods

	static int integer_Elt(SEXP x, R_xlen_t i)
	{
		Matter mVec(R_altrep_data1(x));
		int ans;
		mVec.data().read<int>(&ans, i, 1);
		return ans;
	}

	static R_xlen_t integer_Get_region(SEXP x, R_xlen_t i, R_xlen_t size, int * buffer)
	{
		Matter mVec(R_altrep_data1(x));
		return mVec.data().read<int>(buffer, i, size);
	}

	// ALTREAL methods

	static double real_Elt(SEXP x, R_xlen_t i)
	{
		Matter mVec(R_altrep_data1(x));
		double ans;
		mVec.data().read<double>(&ans, i, 1);
		return ans;
	}

	static R_xlen_t real_Get_region(SEXP x, R_xlen_t i, R_xlen_t size, double * buffer)
	{
		Matter mVec(R_altrep_data1(x));
		return mVec.data().read<double>(buffer, i, size);
	}

	// ALTSTRING methods

	static SEXP string_Elt(SEXP x, R_xlen_t i)
	{
		return STRING_ELT(getStringElements(R_altrep_data1(x), Rf_ScalarReal(i)), 0);
	}

};

#endif
