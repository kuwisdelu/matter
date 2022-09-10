
#ifndef MATTER_ALTREP
#define MATTER_ALTREP

#include <R_ext/Altrep.h>

#define MATTER_PKG "matter"

extern "C" {

//// ALTREP matter class
//-----------------------

static R_altrep_class_t MatterAlt_raw;
static R_altrep_class_t MatterAlt_logical;
static R_altrep_class_t MatterAlt_integer;
static R_altrep_class_t MatterAlt_real;
static R_altrep_class_t MatterAlt_string;

/* Matter-backed ALTREP objects
	---------------------------
	data1: the original matter object (as a SEXP)
	data2: either NULL or a SEXP of the in-memory R object
	---------------------------
*/

SEXP newMatterAltrep(SEXP x, SEXP attr, SEXP nm, SEXP dm, SEXP dnm, SEXP wrap);

void init_matter_altraw(DllInfo * info);
void init_matter_altlogical(DllInfo * info);
void init_matter_altinteger(DllInfo * info);
void init_matter_altreal(DllInfo * info);
void init_matter_altstring(DllInfo * info);

static SEXP matter_altrep_Serialized_state(SEXP x)
{
	return R_altrep_data1(x);
}

static SEXP matter_altrep_Unserialize(SEXP cls, SEXP state)
{
	return newMatterAltrep(state, R_NilValue, R_NilValue,
		R_NilValue, R_NilValue, Rf_ScalarLogical(FALSE));
}

//// Matter ALTARRAY classes
//---------------------------

static Rboolean matter_altarray_Inspect(SEXP x, int pre, int deep, int pvec,
	void (*inspect_subtree)(SEXP, int, int, int));
static R_xlen_t matter_altarray_Length(SEXP x);

static SEXP matter_altarray_Realize(SEXP x);

static void * matter_altarray_Dataptr(SEXP x, Rboolean writeable);
static const void * matter_altarray_Dataptr_or_null(SEXP x);
static SEXP matter_altarray_Extract_subset(SEXP x, SEXP indx, SEXP call);

static Rbyte matter_altraw_Elt(SEXP x, R_xlen_t i);
static R_xlen_t matter_altraw_Get_region(SEXP x, R_xlen_t i, R_xlen_t size, Rbyte * buffer);

static int matter_altlogical_Elt(SEXP x, R_xlen_t i);
static R_xlen_t matter_altlogical_Get_region(SEXP x, R_xlen_t i, R_xlen_t size, Rbyte * buffer);

static int matter_altinteger_Elt(SEXP x, R_xlen_t i);
static R_xlen_t matter_altinteger_Get_region(SEXP x, R_xlen_t i, R_xlen_t size, Rbyte * buffer);

static double matter_altreal_Elt(SEXP x, R_xlen_t i);
static R_xlen_t matter_altreal_Get_region(SEXP x, R_xlen_t i, R_xlen_t size, Rbyte * buffer);

//// Matter ALTSTRING class
//--------------------------

static SEXP matter_altstring_Realize(SEXP x);

static void * matter_altstring_Dataptr(SEXP x, Rboolean writeable);
static const void * matter_altstring_Dataptr_or_null(SEXP x);
static SEXP matter_altstring_Extract_subset(SEXP x, SEXP indx, SEXP call);

static SEXP matter_altstring_Elt(SEXP x, R_xlen_t i);
static R_xlen_t matter_altstring_Get_region(SEXP x,R_xlen_t i, R_xlen_t size, Rbyte * buffer);


} // extern "C"

#endif // MATTER_ALTREP
