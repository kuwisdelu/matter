
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

static SEXP makeAltrep(SEXP x, SEXP attr, SEXP nm, SEXP dm, SEXP dnm, SEXP wrap)
{
	SEXP ret;
	R_altrep_class_t cls;
	if ( Rf_isS4(x) )
	{
		if ( MAYBE_REFERENCED(x) )
			x = Rf_duplicate(x);
		PROTECT(x);
		Matter mVec(x);
		if ( mVec.S4class() != MATTER_VEC && mVec.S4class() != MATTER_STR )
	    	Rf_error("ALTREP not supported for this matter object");
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
	        case R_CHARACTER:
	            cls = MatterAlt_string;
	            break;
	        default:
	            Rf_error("ALTREP not supported for this matter datamode");
	    }
	    ret = R_new_altrep(cls, x, R_NilValue);
	    MARK_NOT_MUTABLE(ret);
	}
	else if ( Rf_isVector(x) )
	{
		if ( MAYBE_REFERENCED(x) )
		{
			if ( Rf_isVectorList(x) )
				x = Rf_shallow_duplicate(x);
			else
				x = Rf_duplicate(x);
		}
		PROTECT(x);
		ret = x;
	}
	else
	{
		Rf_error("SEXP type not supported");
	}
    Rboolean has_attr = FALSE, has_special = FALSE; 
    if ( attr != R_NilValue && XLENGTH(attr) > 0 )
    	has_attr = TRUE;
    if ( nm != R_NilValue || dm != R_NilValue || dnm != R_NilValue )
    	has_special = TRUE;
    if ( has_attr || has_special || Rf_asLogical(wrap) )
    	ret = R_tryWrap(ret);
    PROTECT(ret);
    if ( nm != R_NilValue )
    	Rf_setAttrib(ret, R_NamesSymbol, nm);
    if ( dm != R_NilValue )
    	Rf_setAttrib(ret, R_DimSymbol, dm);
    if ( dnm != R_NilValue )
    	Rf_setAttrib(ret, R_DimNamesSymbol, dnm);
    if ( has_attr )
    {
    	SEXP tags = Rf_getAttrib(attr, R_NamesSymbol);
    	for ( int i = 0; i < XLENGTH(attr); i++ )
    	{
    		SEXP ikey = STRING_ELT(tags, i);
    		SEXP ival = VECTOR_ELT(attr, i);
    		Rf_setAttrib(ret, Rf_install(CHAR(ikey)), ival);
    	}
    }
    UNPROTECT(2);
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

// String

static void init_MatterAlt_string(DllInfo * info)
{
	R_altrep_class_t cls = R_make_altstring_class("MatterAlt_string", MATTER_PKG, info);
	MatterAlt_string = cls;

	// override ALTREP methods
	R_set_altrep_Serialized_state_method(cls, MatterAlt::Serialized_state);
	R_set_altrep_Unserialize_method(cls, MatterAlt::Unserialize);
	R_set_altrep_Length_method(cls, MatterAlt::Length);
	R_set_altrep_Inspect_method(cls, MatterAlt::Inspect);

	// override ALTVEC methods
	R_set_altvec_Dataptr_method(cls, MatterAlt::Dataptr);
	R_set_altvec_Dataptr_or_null_method(cls, MatterAlt::Dataptr_or_null);

	// overrid ALTREAL methods
	R_set_altstring_Elt_method(cls, MatterAlt::string_Elt);
}

#endif
