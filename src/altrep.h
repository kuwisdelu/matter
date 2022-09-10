
#ifndef MATTER_ALTREP
#define MATTER_ALTREP

#include <R_ext/Altrep.h>

#define MATTER_PKG "matter"

extern "C" {

//// ALTREP matter class
//-----------------------

static R_altrep_class_t matter_altraw;
static R_altrep_class_t matter_altlogical;
static R_altrep_class_t matter_altinteger;
static R_altrep_class_t matter_altreal;
static R_altrep_class_t matter_altstring;

SEXP newMatterAltrep(SEXP x, SEXP attr,
	SEXP nm, SEXP dm, SEXP dnm, SEXP wrap);

void init_matter_altraw(DllInfo * info);
void init_matter_altlogical(DllInfo * info);
void init_matter_altinteger(DllInfo * info);
void init_matter_altreal(DllInfo * info);
void init_matter_altstring(DllInfo * info);

} // extern "C"

#endif // MATTER_ALTREP
