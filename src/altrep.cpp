
#include "altrep.h"

extern "C" {

	SEXP makeAltrep(SEXP x, SEXP attr, SEXP nm, SEXP dm, SEXP dnm, SEXP wrap)
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

}

extern "C" {

	void init_MatterAlt_raw(DllInfo * info)
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
		R_set_altvec_Extract_subset_method(cls, MatterAlt::raw_Extract_subset);
	}

	// Logical

	void init_MatterAlt_logical(DllInfo * info)
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
		R_set_altvec_Extract_subset_method(cls, MatterAlt::logical_Extract_subset);
	}

	// Integer

	void init_MatterAlt_integer(DllInfo * info)
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
		R_set_altvec_Extract_subset_method(cls, MatterAlt::integer_Extract_subset);
	}

	// Real

	void init_MatterAlt_real(DllInfo * info)
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
		R_set_altvec_Extract_subset_method(cls, MatterAlt::real_Extract_subset);
	}

	// String

	void init_MatterAlt_string(DllInfo * info)
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
		R_set_altvec_Extract_subset_method(cls, MatterAlt::string_Extract_subset);
	}

}

