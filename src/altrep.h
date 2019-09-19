
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
		Rprintf("matter: vector (mode=%d, len=%d, mem=%d)\n", mVec.datamode(), mVec.length(), inmem);
		return TRUE;
	}

	// ALTVEC methods

	static void * Dataptr(SEXP x, Rboolean writeable)
	{
		#ifdef MATTER_DEBUG
			Rprintf("matter: Dataptr() access\n");
		#endif
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
		#ifdef MATTER_DEBUG
			Rprintf("matter: Dataptr_or_null() access\n");
		#endif
		if ( R_altrep_data2(x) == R_NilValue )
			return NULL;
		else
			return DATAPTR(R_altrep_data2(x));
	}

	// ALTRAW methods

	static Rbyte raw_Elt(SEXP x, R_xlen_t i)
	{
		#ifdef MATTER_DEBUG
			Rprintf("matter: raw_Elt(%d) access\n", i);
		#endif
		Matter mVec(R_altrep_data1(x));
		Rbyte ans;
		mVec.data().read<Rbyte>(&ans, i, 1);
		return ans;
	}

	static R_xlen_t raw_Get_region(SEXP x, R_xlen_t i, R_xlen_t size, Rbyte * buffer)
	{
		#ifdef MATTER_DEBUG
			Rprintf("matter: raw_Get_region(%d, %d) access\n", i, size);
		#endif
		Matter mVec(R_altrep_data1(x));
		return mVec.data().read<Rbyte>(buffer, i, size);
	}

	static SEXP raw_Extract_subset(SEXP x, SEXP indx, SEXP call)
	{
		#ifdef MATTER_DEBUG
			Rprintf("matter: raw_Extract_subset() access\n");
		#endif
		SEXP result = NULL;
		if ( R_altrep_data2(x) == R_NilValue )
		{
			Matter mVec(R_altrep_data1(x));
		    PROTECT(result = Rf_allocVector(RAWSXP, XLENGTH(indx)));
		    Rbyte * presult = RAW(result);
		    if ( TYPEOF(indx) == INTSXP )
		    {
		    	const int * pindx = INTEGER_RO(indx);
		    	mVec.data().read_indices<Rbyte>(presult, pindx, XLENGTH(indx), 1, 1);
		    }
		    else
		    {
		    	const double * pindx = REAL_RO(indx);
		    	mVec.data().read_indices<Rbyte>(presult, pindx, XLENGTH(indx), 1, 1);
		    }
		    UNPROTECT(1);
		}
	    return result;
	}

	// ALTLOGICAL methods

	static int logical_Elt(SEXP x, R_xlen_t i)
	{
		#ifdef MATTER_DEBUG
			Rprintf("matter: logical_Elt(%d) access\n", i);
		#endif
		Matter mVec(R_altrep_data1(x));
		int ans;
		mVec.data().read<int>(&ans, i, 1);
		return ans;
	}

	static R_xlen_t logical_Get_region(SEXP x, R_xlen_t i, R_xlen_t size, int * buffer)
	{
		#ifdef MATTER_DEBUG
			Rprintf("matter: logical_Get_region(%d, %d) access\n", i, size);
		#endif
		Matter mVec(R_altrep_data1(x));
		return mVec.data().read<int>(buffer, i, size);
	}

	static SEXP logical_Extract_subset(SEXP x, SEXP indx, SEXP call)
	{
		#ifdef MATTER_DEBUG
			Rprintf("matter: logical_Extract_subset() access\n");
		#endif
		SEXP result = NULL;
		if ( R_altrep_data2(x) == R_NilValue )
		{
			Matter mVec(R_altrep_data1(x));
		    PROTECT(result = Rf_allocVector(LGLSXP, XLENGTH(indx)));
		    int * presult = LOGICAL(result);
		    if ( TYPEOF(indx) == INTSXP )
		    {
		    	const int * pindx = INTEGER_RO(indx);
		    	mVec.data().read_indices<int>(presult, pindx, XLENGTH(indx), 1, 1);
		    }
		    else
		    {
		    	const double * pindx = REAL_RO(indx);
		    	mVec.data().read_indices<int>(presult, pindx, XLENGTH(indx), 1, 1);
		    }
		    UNPROTECT(1);
		}
	    return result;
	}

	// ALTINTEGER methods

	static int integer_Elt(SEXP x, R_xlen_t i)
	{
		#ifdef MATTER_DEBUG
			Rprintf("matter: integer_Elt(%d) access\n", i);
		#endif
		Matter mVec(R_altrep_data1(x));
		int ans;
		mVec.data().read<int>(&ans, i, 1);
		return ans;
	}

	static R_xlen_t integer_Get_region(SEXP x, R_xlen_t i, R_xlen_t size, int * buffer)
	{
		#ifdef MATTER_DEBUG
			Rprintf("matter: integer_Get_region(%d, %d) access\n", i, size);
		#endif
		Matter mVec(R_altrep_data1(x));
		return mVec.data().read<int>(buffer, i, size);
	}

	static SEXP integer_Extract_subset(SEXP x, SEXP indx, SEXP call)
	{
		#ifdef MATTER_DEBUG
			Rprintf("matter: integer_Extract_subset() access\n");
		#endif
		SEXP result = NULL;
		if ( R_altrep_data2(x) == R_NilValue )
		{
			Matter mVec(R_altrep_data1(x));
		    PROTECT(result = Rf_allocVector(INTSXP, XLENGTH(indx)));
		    int * presult = INTEGER(result);
		    if ( TYPEOF(indx) == INTSXP )
		    {
		    	const int * pindx = INTEGER_RO(indx);
		    	mVec.data().read_indices<int>(presult, pindx, XLENGTH(indx), 1, 1);
		    }
		    else
		    {
		    	const double * pindx = REAL_RO(indx);
		    	mVec.data().read_indices<int>(presult, pindx, XLENGTH(indx), 1, 1);
		    }
		    UNPROTECT(1);
		}
	    return result;
	}

	// ALTREAL methods

	static double real_Elt(SEXP x, R_xlen_t i)
	{
		#ifdef MATTER_DEBUG
			Rprintf("matter: real_Elt(%d) access\n", i);
		#endif
		Matter mVec(R_altrep_data1(x));
		double ans;
		mVec.data().read<double>(&ans, i, 1);
		return ans;
	}

	static R_xlen_t real_Get_region(SEXP x, R_xlen_t i, R_xlen_t size, double * buffer)
	{
		#ifdef MATTER_DEBUG
			Rprintf("matter: real_Get_region(%d, %d) access\n", i, size);
		#endif
		Matter mVec(R_altrep_data1(x));
		return mVec.data().read<double>(buffer, i, size);
	}

	static SEXP real_Extract_subset(SEXP x, SEXP indx, SEXP call)
	{
		#ifdef MATTER_DEBUG
			Rprintf("matter: real_Extract_subset() access\n");
		#endif
		SEXP result = NULL;
		if ( R_altrep_data2(x) == R_NilValue )
		{
			Matter mVec(R_altrep_data1(x));
		    PROTECT(result = Rf_allocVector(REALSXP, XLENGTH(indx)));
		    double * presult = REAL(result);
		    if ( TYPEOF(indx) == INTSXP )
		    {
		    	const int * pindx = INTEGER_RO(indx);
		    	mVec.data().read_indices<double>(presult, pindx, XLENGTH(indx), 1, 1);
		    }
		    else
		    {
		    	const double * pindx = REAL_RO(indx);
		    	mVec.data().read_indices<double>(presult, pindx, XLENGTH(indx), 1, 1);
		    }
		    UNPROTECT(1);
		}
	    return result;
	}

	// ALTSTRING methods

	static SEXP string_Elt(SEXP x, R_xlen_t i)
	{
		#ifdef MATTER_DEBUG
			Rprintf("matter: string_Elt(%d) access\n", i);
		#endif
		SEXP tmp, result = NULL;
		if ( R_altrep_data2(x) == R_NilValue )
		{
			Matter mVec(R_altrep_data1(x));
	        PROTECT(result = Rf_allocVector(STRSXP, 1));
			if ( i == NA_INTEGER )
				SET_STRING_ELT(result, 0, NA_STRING);
			else
			{
	    		PROTECT(tmp = Rf_allocVector(RAWSXP, mVec.dim(i)));
	    		mVec.data().set_group(i);
		    	mVec.data().read<Rbyte>(RAW(tmp), 0, mVec.dim(i));
		    	SET_STRING_ELT(result, 0, raw_to_char(tmp));
		    	UNPROTECT(1);
			}
	        UNPROTECT(1);
	    }
		return result;
	}

	static SEXP string_Extract_subset(SEXP x, SEXP indx, SEXP call)
	{
		#ifdef MATTER_DEBUG
			Rprintf("matter: string_Extract_subset() access\n");
		#endif
		SEXP tmp, result = NULL;
		if ( R_altrep_data2(x) == R_NilValue )
		{
			Matter mVec(R_altrep_data1(x));
	        PROTECT(result = Rf_allocVector(STRSXP, XLENGTH(indx)));
	    	if ( TYPEOF(indx) == INTSXP )
		    {
		    	const int * pindx = INTEGER_RO(indx);
		    	for ( int i = 0; i < XLENGTH(indx); i++ )
		    	{
		    		if ( is_R_NA(pindx[i]) )
		    			SET_STRING_ELT(result, i, NA_STRING);
		    		else
		    		{
		    			index_t ii = static_cast<index_t>(pindx[i] - 1);
			    		PROTECT(tmp = Rf_allocVector(RAWSXP, mVec.dim(ii)));
			    		mVec.data().set_group(ii);
				    	mVec.data().read<Rbyte>(RAW(tmp), 0, mVec.dim(ii));
				    	SET_STRING_ELT(result, i, raw_to_char(tmp));
				    	UNPROTECT(1);
		    		}
		    	}
		    }
		    else
		    {
		    	const double * pindx = REAL_RO(indx);
		    	for ( int i = 0; i < XLENGTH(indx); i++ )
		    	{
		    		if ( is_R_NA(pindx[i]) )
		    			SET_STRING_ELT(result, i, NA_STRING);
		    		else
		    		{
		    			index_t ii = static_cast<index_t>(pindx[i] - 1);
			    		PROTECT(tmp = Rf_allocVector(RAWSXP, mVec.dim(ii)));
			    		mVec.data().set_group(ii);
				    	mVec.data().read<Rbyte>(RAW(tmp), 0, mVec.dim(ii));
				    	SET_STRING_ELT(result, i, raw_to_char(tmp));
				    	UNPROTECT(1);
		    		}
		    	}
		    }
	        UNPROTECT(1);
		}
		return result;
	}

};

#endif
