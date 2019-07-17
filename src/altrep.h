
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

/* Matter-backed ALTREP objects

	data1: the original matter object (as a SEXP)
	data2: either NULL or a SEXP of the in-memory R object

*/

static SEXP makeMatterAltrep(SEXP x)
{
	R_altrep_class_t cls;
	if ( MAYBE_REFERENCED(x) )
		x = Rf_duplicate(x);
	PROTECT(x);
	Matter mVec(x);
    switch(mVec.datamode()) {
    	case R_RAW:
            cls = MatterAlt_raw;
            break;
        case R_LOGICAL:
            cls = MatterAlt_logical;
            break;
        case R_INTEGER:
            cls = MatterAlt_integer;
            break;
        case R_NUMERIC:
            cls = MatterAlt_real;
            break;
        default:
            Rf_error("ALTREP not supported for this matter datamode");
    }
    SEXP ret = R_new_altrep(cls, x, R_NilValue);
    MARK_NOT_MUTABLE(ret);
    UNPROTECT(1);
	return ret;
}

struct MatterAlt {

	// ALTREP methods

	static SEXP Serialized_state(SEXP x)
	{
		return R_altrep_data1(x);
	}

	static SEXP Unserialize(SEXP cls, SEXP state)
	{
		return makeMatterAltrep(state);
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

};

static void init_MatterAlt_raw(DllInfo * info)
{
	R_altrep_class_t cls = R_make_altraw_class("MatterAlt_raw", MATTER_PKG, info);
	MatterAlt_raw = cls;

	// override ALTREP methods
	R_set_altrep_Serialized_state_method(cls, MatterAlt::Serialized_state);
	R_set_altrep_Unserialize_method(cls, MatterAlt::Unserialize);
	R_set_altrep_Length_method(cls, MatterAlt::Length);
	R_set_altrep_Inspect_method(cls, MatterAlt::Inspect);

	// override ALTVEC methods
	R_set_altvec_Dataptr_method(cls, MatterAlt::Dataptr);
	R_set_altvec_Dataptr_or_null_method(cls, MatterAlt::Dataptr_or_null);

	// overrid ALTRAW methods
	R_set_altraw_Elt_method(cls, MatterAlt::raw_Elt);
	R_set_altraw_Get_region_method(cls, MatterAlt::raw_Get_region);
}

// Logical

static void init_MatterAlt_logical(DllInfo * info)
{
	R_altrep_class_t cls = R_make_altlogical_class("MatterAlt_logical", MATTER_PKG, info);
	MatterAlt_logical = cls;

	// override ALTREP methods
	R_set_altrep_Serialized_state_method(cls, MatterAlt::Serialized_state);
	R_set_altrep_Unserialize_method(cls, MatterAlt::Unserialize);
	R_set_altrep_Length_method(cls, MatterAlt::Length);
	R_set_altrep_Inspect_method(cls, MatterAlt::Inspect);

	// override ALTVEC methods
	R_set_altvec_Dataptr_method(cls, MatterAlt::Dataptr);
	R_set_altvec_Dataptr_or_null_method(cls, MatterAlt::Dataptr_or_null);

	// overrid ALTLOGICAL methods
	R_set_altlogical_Elt_method(cls, MatterAlt::logical_Elt);
	R_set_altlogical_Get_region_method(cls, MatterAlt::logical_Get_region);
}

// Integer

static void init_MatterAlt_integer(DllInfo * info)
{
	R_altrep_class_t cls = R_make_altinteger_class("MatterAlt_integer", MATTER_PKG, info);
	MatterAlt_integer = cls;

	// override ALTREP methods
	R_set_altrep_Serialized_state_method(cls, MatterAlt::Serialized_state);
	R_set_altrep_Unserialize_method(cls, MatterAlt::Unserialize);
	R_set_altrep_Length_method(cls, MatterAlt::Length);
	R_set_altrep_Inspect_method(cls, MatterAlt::Inspect);

	// override ALTVEC methods
	R_set_altvec_Dataptr_method(cls, MatterAlt::Dataptr);
	R_set_altvec_Dataptr_or_null_method(cls, MatterAlt::Dataptr_or_null);

	// overrid ALTINTEGER methods
	R_set_altinteger_Elt_method(cls, MatterAlt::integer_Elt);
	R_set_altinteger_Get_region_method(cls, MatterAlt::integer_Get_region);
}

// Real

static void init_MatterAlt_real(DllInfo * info)
{
	R_altrep_class_t cls = R_make_altreal_class("MatterAlt_real", MATTER_PKG, info);
	MatterAlt_real = cls;

	// override ALTREP methods
	R_set_altrep_Serialized_state_method(cls, MatterAlt::Serialized_state);
	R_set_altrep_Unserialize_method(cls, MatterAlt::Unserialize);
	R_set_altrep_Length_method(cls, MatterAlt::Length);
	R_set_altrep_Inspect_method(cls, MatterAlt::Inspect);

	// override ALTVEC methods
	R_set_altvec_Dataptr_method(cls, MatterAlt::Dataptr);
	R_set_altvec_Dataptr_or_null_method(cls, MatterAlt::Dataptr_or_null);

	// overrid ALTREAL methods
	R_set_altreal_Elt_method(cls, MatterAlt::real_Elt);
	R_set_altreal_Get_region_method(cls, MatterAlt::real_Get_region);
}

#endif
