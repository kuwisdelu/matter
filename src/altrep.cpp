
#include "matterDefines.h"
#include "matter.h"
#include "altrep.h"

extern "C" {

SEXP newMatterAltrep(SEXP x, SEXP attr,
	SEXP nm, SEXP dm, SEXP dnm, SEXP wrap)
{
	SEXP ans;
	R_altrep_class_t cls;
	PROTECT(x);
	if ( Rf_inherits(x, "matter") )
	{
		if ( MAYBE_REFERENCED(x) )
			x = Rf_duplicate(x);
		if ( Rf_inherits(x, "matter_arr"))
		{
			int type = Rf_asInteger(R_do_slot(x, Rf_install("type")));
			switch(type) {
				case R_RAW:
					cls = matter_altraw;
					break;
				case R_LOGICAL:
					cls = matter_altlogical;
					break;
				case R_INTEGER:
					cls = matter_altinteger;
					break;
				case R_DOUBLE:
					cls = matter_altreal;
					break;
				default:
					Rf_error("unsupported data type");
			}
		}
		else if ( Rf_inherits(x, "matter_str"))
			cls = matter_altstring;
		else
			Rf_error("ALTREP not supported for this matter class");
		PROTECT(ans = R_new_altrep(cls, x, R_NilValue));
		MARK_NOT_MUTABLE(ans);
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
		PROTECT(ans = x);
	}
	else {
		Rf_error("ALTREP not supported for this class");
	}
	Rboolean has_attr = FALSE, has_special = FALSE; 
	if ( attr != R_NilValue && XLENGTH(attr) > 0 )
		has_attr = TRUE;
	if ( nm != R_NilValue || dm != R_NilValue || dnm != R_NilValue )
		has_special = TRUE;
	if ( has_attr || has_special || Rf_asLogical(wrap) )
		ans = R_tryWrap(ans);
	if ( nm != R_NilValue )
		Rf_setAttrib(ans, R_NamesSymbol, nm);
	if ( dm != R_NilValue )
		Rf_setAttrib(ans, R_DimSymbol, dm);
	if ( dnm != R_NilValue )
		Rf_setAttrib(ans, R_DimNamesSymbol, dnm);
	if ( has_attr )
	{
		SEXP tags = Rf_getAttrib(attr, R_NamesSymbol);
		for ( int i = 0; i < XLENGTH(attr); i++ )
		{
			SEXP ikey = STRING_ELT(tags, i);
			SEXP ival = VECTOR_ELT(attr, i);
			Rf_setAttrib(ans, Rf_install(CHAR(ikey)), ival);
		}
	}
	UNPROTECT(2);
	return ans;
}

//// Matter ALTARRAY classes
//----------------------------

static Rboolean matter_altarray_Inspect(SEXP x, int pre, int deep, int pvec,
	void (*inspect_subtree)(SEXP, int, int, int))
{
	MatterArray mx(R_altrep_data1(x));
	int mem = Rf_isNull(R_altrep_data2(x));
	Rprintf("matter array (mode=%d, len=%d, mem=%d)\n", mx.type(), mx.length(), mem);
	return TRUE;
}

static R_xlen_t matter_altarray_Length(SEXP x)
{
	MatterArray mx(R_altrep_data1(x));
	return mx.length();
}

static SEXP matter_altarray_Realize(SEXP x)
{
	MTDEBUG0("matter: materializing data payload...\n");
	if ( R_altrep_data2(x) == R_NilValue )
	{
		SEXP data;
		MatterArray mx(R_altrep_data1(x));
		PROTECT(data = mx.getElements(R_NilValue));
		R_set_altrep_data2(x, data);
		UNPROTECT(1);
	}
	return R_altrep_data2(x);
}

static void * matter_altarray_Dataptr(SEXP x, Rboolean writeable)
{
	MTDEBUG0("matter: Dataptr() access\n");
	return DATAPTR(matter_altarray_Realize(x));
}

static const void * matter_altarray_Dataptr_or_null(SEXP x)
{
	MTDEBUG0("matter: Dataptr_or_null() access\n");
	if ( Rf_isNull(R_altrep_data2(x)) )
		return NULL;
	else
		return DATAPTR(R_altrep_data2(x));
}

static SEXP matter_altarray_Extract_subset(SEXP x, SEXP indx, SEXP call)
{
	MTDEBUG0("matter: raw_Extract_subset() access\n");
	MatterArray mx(x);
	return mx.getElements(indx);
}

// ALTRAW

static Rbyte matter_altraw_Elt(SEXP x, R_xlen_t i)
{
	MTDEBUG1("matter: raw_Elt(%d) access\n", i);
	MatterArray mx(x);
	Rbyte ans;
	mx.getRegion(i, 1, &ans);
	return ans;
}

static R_xlen_t matter_altraw_Get_region(SEXP x,
	R_xlen_t i, R_xlen_t size, Rbyte * buffer)
{
	MTDEBUG2("matter: raw_Get_region(%d, %d) access\n", i, size);
	MatterArray mx(x);
	return mx.getRegion(i, size, buffer);
}

// ALTLOGICAL

static int matter_altlogical_Elt(SEXP x, R_xlen_t i)
{
	MTDEBUG1("matter: logical_Elt(%d) access\n", i);
	MatterArray mx(x);
	int ans;
	mx.getRegion(i, 1, &ans);
	return ans;
}

static R_xlen_t matter_altlogical_Get_region(SEXP x,
	R_xlen_t i, R_xlen_t size, int * buffer)
{
	MTDEBUG2("matter: logical_Get_region(%d, %d) access\n", i, size);
	MatterArray mx(x);
	return mx.getRegion(i, size, buffer);
}

// ALTINTEGER

static int matter_altinteger_Elt(SEXP x, R_xlen_t i)
{
	MTDEBUG1("matter: integer_Elt(%d) access\n", i);
	MatterArray mx(x);
	int ans;
	mx.getRegion(i, 1, &ans);
	return ans;
}

static R_xlen_t matter_altinteger_Get_region(SEXP x,
	R_xlen_t i, R_xlen_t size, int * buffer)
{
	MTDEBUG2("matter: integer_Get_region(%d, %d) access\n", i, size);
	MatterArray mx(x);
	return mx.getRegion(i, size, buffer);
}

// ALTREAL

static double matter_altreal_Elt(SEXP x, R_xlen_t i)
{
	MTDEBUG1("matter: real_Elt(%d) access\n", i);
	MatterArray mx(x);
	double ans;
	mx.getRegion(i, 1, &ans);
	return ans;
}

static R_xlen_t matter_altreal_Get_region(SEXP x,
	R_xlen_t i, R_xlen_t size, double * buffer)
{
	MTDEBUG2("matter: real_Get_region(%d, %d) access\n", i, size);
	MatterArray mx(x);
	return mx.getRegion(i, size, buffer);
}

//// Matter ALTSTRING class
//--------------------------

static Rboolean matter_altstring_Inspect(SEXP x, int pre, int deep, int pvec,
	void (*inspect_subtree)(SEXP, int, int, int))
{
	MatterStringList mx(R_altrep_data1(x));
	int mem = Rf_isNull(R_altrep_data2(x));
	Rprintf("matter strings (mode=%d, len=%d, mem=%d)\n", mx.type(), mx.length(), mem);
	return TRUE;
}

static R_xlen_t matter_altstring_Length(SEXP x)
{
	MatterStringList mx(R_altrep_data1(x));
	return mx.length();
}

static SEXP matter_altstring_Realize(SEXP x)
{
	MTDEBUG0("matter: materializing data payload...\n");
	if ( R_altrep_data2(x) == R_NilValue )
	{
		SEXP data;
		MatterStringList mx(R_altrep_data1(x));
		PROTECT(data = mx.getStrings(R_NilValue));
		R_set_altrep_data2(x, data);
		UNPROTECT(1);
	}
	return R_altrep_data2(x);
}

static void * matter_altstring_Dataptr(SEXP x, Rboolean writeable)
{
	MTDEBUG0("matter: Dataptr() access\n");
	return DATAPTR(matter_altstring_Realize(x));
}

static const void * matter_altstring_Dataptr_or_null(SEXP x)
{
	MTDEBUG0("matter: Dataptr_or_null() access\n");
	if ( Rf_isNull(R_altrep_data2(x)) )
		return NULL;
	else
		return DATAPTR(R_altrep_data2(x));
}

static SEXP matter_altstring_Extract_subset(SEXP x, SEXP indx, SEXP call)
{
	MTDEBUG0("matter: raw_Extract_subset() access\n");
	MatterStringList mx(x);
	return mx.getStrings(indx);
}

static SEXP matter_altstring_Elt(SEXP x, R_xlen_t i)
{
	MTDEBUG1("matter: string_Elt(%d) access\n", i);
	MatterStringList mx(x);
	return mx.getChar(i);
}

//// Initialize Matter ALTREP classes
//------------------------------------

void init_matter_altraw(DllInfo * info)
{
	R_altrep_class_t cls = R_make_altraw_class("matter_altraw", MATTER_PKG, info);
	matter_altraw = cls;

	// override ALTREP methods
	R_set_altrep_Serialized_state_method(cls, matter_altrep_Serialized_state);
	R_set_altrep_Unserialize_method(cls, matter_altrep_Unserialize);
	
	R_set_altrep_Inspect_method(cls, matter_altarray_Inspect);
	R_set_altrep_Length_method(cls, matter_altarray_Length);

	// override ALTVEC methods
	R_set_altvec_Dataptr_method(cls, matter_altarray_Dataptr);
	R_set_altvec_Dataptr_or_null_method(cls, matter_altarray_Dataptr_or_null);
	R_set_altvec_Extract_subset_method(cls, matter_altarray_Extract_subset);

	// overrid ALTRAW methods
	R_set_altraw_Elt_method(cls, matter_altraw_Elt);
	R_set_altraw_Get_region_method(cls, matter_altraw_Get_region);
}

void init_matter_altlogical(DllInfo * info)
{
	R_altrep_class_t cls = R_make_altlogical_class("matter_altlogical", MATTER_PKG, info);
	matter_altlogical = cls;

	// override ALTREP methods
	R_set_altrep_Serialized_state_method(cls, matter_altrep_Serialized_state);
	R_set_altrep_Unserialize_method(cls, matter_altrep_Unserialize);
	
	R_set_altrep_Inspect_method(cls, matter_altarray_Inspect);
	R_set_altrep_Length_method(cls, matter_altarray_Length);

	// override ALTVEC methods
	R_set_altvec_Dataptr_method(cls, matter_altarray_Dataptr);
	R_set_altvec_Dataptr_or_null_method(cls, matter_altarray_Dataptr_or_null);
	R_set_altvec_Extract_subset_method(cls, matter_altarray_Extract_subset);

	// overrid ALTLOGICAL methods
	R_set_altlogical_Elt_method(cls, matter_altlogical_Elt);
	R_set_altlogical_Get_region_method(cls, matter_altlogical_Get_region);
}

void init_matter_altinteger(DllInfo * info)
{
	R_altrep_class_t cls = R_make_altinteger_class("matter_altinteger", MATTER_PKG, info);
	matter_altinteger = cls;

	// override ALTREP methods
	R_set_altrep_Serialized_state_method(cls, matter_altrep_Serialized_state);
	R_set_altrep_Unserialize_method(cls, matter_altrep_Unserialize);
	
	R_set_altrep_Inspect_method(cls, matter_altarray_Inspect);
	R_set_altrep_Length_method(cls, matter_altarray_Length);

	// override ALTVEC methods
	R_set_altvec_Dataptr_method(cls, matter_altarray_Dataptr);
	R_set_altvec_Dataptr_or_null_method(cls, matter_altarray_Dataptr_or_null);
	R_set_altvec_Extract_subset_method(cls, matter_altarray_Extract_subset);

	// overrid ALTINTEGER methods
	R_set_altinteger_Elt_method(cls, matter_altinteger_Elt);
	R_set_altinteger_Get_region_method(cls, matter_altinteger_Get_region);
}

void init_matter_altreal(DllInfo * info)
{
	R_altrep_class_t cls = R_make_altreal_class("matter_altreal", MATTER_PKG, info);
	matter_altreal = cls;

	// override ALTREP methods
	R_set_altrep_Serialized_state_method(cls, matter_altrep_Serialized_state);
	R_set_altrep_Unserialize_method(cls, matter_altrep_Unserialize);
	
	R_set_altrep_Inspect_method(cls, matter_altarray_Inspect);
	R_set_altrep_Length_method(cls, matter_altarray_Length);

	// override ALTVEC methods
	R_set_altvec_Dataptr_method(cls, matter_altarray_Dataptr);
	R_set_altvec_Dataptr_or_null_method(cls, matter_altarray_Dataptr_or_null);
	R_set_altvec_Extract_subset_method(cls, matter_altarray_Extract_subset);

	// overrid ALTREAL methods
	R_set_altreal_Elt_method(cls, matter_altreal_Elt);
	R_set_altreal_Get_region_method(cls, matter_altreal_Get_region);
}

void init_matter_altstring(DllInfo * info)
{
	R_altrep_class_t cls = R_make_altstring_class("matter_altstring", MATTER_PKG, info);
	matter_altstring = cls;

	// override ALTREP methods
	R_set_altrep_Serialized_state_method(cls, matter_altrep_Serialized_state);
	R_set_altrep_Unserialize_method(cls, matter_altrep_Unserialize);
	
	R_set_altrep_Inspect_method(cls, matter_altstring_Inspect);
	R_set_altrep_Length_method(cls, matter_altstring_Length);

	// override ALTVEC methods
	R_set_altvec_Dataptr_method(cls, matter_altstring_Dataptr);
	R_set_altvec_Dataptr_or_null_method(cls, matter_altstring_Dataptr_or_null);
	R_set_altvec_Extract_subset_method(cls, matter_altstring_Extract_subset);

	// overrid ALTSTRING methods
	R_set_altstring_Elt_method(cls, matter_altstring_Elt);
}

} // extern "C"

