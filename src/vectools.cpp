
#include "vectools.h"

//// Local extrema
//-------------------

template<typename T>
SEXP local_maxima(T * x, int halfWindow, int length) {
	SEXP isLocMax;
	PROTECT(isLocMax = Rf_allocVector(LGLSXP, length));
	int * locmax = LOGICAL(isLocMax);
	for ( int i = 0; i < length; i++ )
		locmax[i] = false;
	for ( int i = halfWindow; i < length - halfWindow; i++ )
	{
		locmax[i] = true;
		for ( int j = i - halfWindow; j <= i + halfWindow; j++ )
		{
			if ( j < i )
			{
				if ( x[j] >= x[i] )
				{
					locmax[i] = false;
					break;
				}
			}
			if ( j > i )
			{
				if ( x[j] > x[i] )
				{
					locmax[i] = false;
					break;
				}
			}
		}
	}
	UNPROTECT(1);
	return isLocMax;
}

template<typename T>
SEXP region_maxima(T * x, int * m, int halfWindow, int length, int nmax) {
	SEXP lower, upper, ret;
	PROTECT(lower = Rf_allocVector(INTSXP, nmax));
	PROTECT(upper = Rf_allocVector(INTSXP, nmax));
	int * pLower, * pUpper;
	pLower = INTEGER(lower);
	pUpper = INTEGER(upper);
	for ( int i = 0; i < nmax; i++ )
	{
		pLower[i] = m[i];
		for ( int j = m[i] - 1; j > 0; j-- ) {
			if ( x[j - 1] < x[j] )
				pLower[i] = j;
            else if ( x[j - 1] <= x[j] && m[i] - j < halfWindow )
                pLower[i] = j;
			else
				break;
		}
		if ( m[i] - pLower[i] < halfWindow ) {
			for ( int j = pLower[i] - 2; j > 0; j-- ) {
				if ( x[j - 1] < x[j] && x[j - 1] <= x[m[i] - 1] )
					pLower[i] = j;
				else
					break;
			}	
		}
		pUpper[i] = m[i];
		for ( int j = m[i] - 1; j < length - 1; j++ ) {
			if ( x[j + 1] < x[j] )
				pUpper[i] = j + 2;
            if ( x[j + 1] <= x[j] && (j + 2) - m[i] < halfWindow )
                pUpper[i] = j + 2;
			else
				break;
		}
		if ( pUpper[i] - m[i] < halfWindow ) {
			for ( int j = pUpper[i]; j < length - 1; j++ ) {
				if ( x[j + 1] < x[j] && x[j + 1] <= x[m[i] - 1] )
					pUpper[i] = j + 2;
				else
					break;
			}
		}
	}
	PROTECT(ret = Rf_allocVector(VECSXP, 2));
	SET_VECTOR_ELT(ret, 0, lower);
	SET_VECTOR_ELT(ret, 1, upper);
	UNPROTECT(3);
	return ret;
}

//// Binning
//------------

template<typename T>
SEXP bin_means(T * x, int * lower, int * upper, int length, int nbin)
{
    SEXP ret;
    PROTECT(ret = Rf_allocVector(REALSXP, nbin));
    double * pRet = REAL(ret);
    for ( int i = 0; i < nbin; i++ ) {
    	pRet[i] = 0;
    	if ( lower[i] < 1 || upper[i] < 1 )
    		Rf_error("bin limits must be positive");
        for ( int j = lower[i] - 1; j < upper[i] && j < length; j++ )
        	pRet[i] += x[j];
        int n = (upper[i] - lower[i]) + 1;
        pRet[i] /= n;
    }
    UNPROTECT(1);
    return ret;
}

template<typename T>
SEXP bin_sums(T * x, int * lower, int * upper, int length, int nbin)
{
    SEXP ret;
    PROTECT(ret = Rf_allocVector(REALSXP, nbin));
    double * pRet = REAL(ret);
    for ( int i = 0; i < nbin; i++ ) {
    	pRet[i] = 0;
    	if ( lower[i] < 1 || upper[i] < 1 )
    		Rf_error("bin limits must be positive");
        for ( int j = lower[i] - 1; j < upper[i] && j < length; j++ )
        	pRet[i] += x[j];
    }
    UNPROTECT(1);
    return ret;
}

template<typename T>
SEXP bin_mins(T * x, int * lower, int * upper, int length, int nbin)
{
    SEXP ret;
    PROTECT(ret = Rf_allocVector(REALSXP, nbin));
    double * pRet = REAL(ret);
    for ( int i = 0; i < nbin; i++ ) {
    	pRet[i] = R_DOUBLE_MAX;
    	if ( lower[i] < 1 || upper[i] < 1 )
    		Rf_error("bin limits must be positive");
        for ( int j = lower[i] - 1; j < upper[i] && j < length; j++ )
        	if ( x[j] < pRet[i] )
	        	pRet[i] = x[j];
    }
    UNPROTECT(1);
    return ret;
}

template<typename T>
SEXP bin_maxs(T * x, int * lower, int * upper, int length, int nbin)
{
    SEXP ret;
    PROTECT(ret = Rf_allocVector(REALSXP, nbin));
    double * pRet = REAL(ret);
    for ( int i = 0; i < nbin; i++ ) {
    	pRet[i] = R_DOUBLE_MIN;
    	if ( lower[i] < 1 || upper[i] < 1 )
    		Rf_error("bin limits must be positive");
        for ( int j = lower[i] - 1; j < upper[i] && j < length; j++ )
        	if ( x[j] > pRet[i] )
	        	pRet[i] = x[j];
    }
    UNPROTECT(1);
    return ret;
}

//// Grouped summaries
//---------------------

template<typename T>
SEXP group_means(T * x, int * group, int ngroup, int length, double init)
{
    SEXP ret;
    PROTECT(ret = Rf_allocVector(REALSXP, ngroup));
    double * pRet = REAL(ret);
    int * n = (int *) Calloc(ngroup, int);
    for ( int g_i = 0; g_i < ngroup; g_i++ ) {
        n[g_i] = 0;
        pRet[g_i] = 0;
    }
    for ( int i = 0; i < length; i++ ) {
        if ( group[i] != NA_INTEGER ) {
            if ( group[i] <= 0 || group[i] > ngroup )
                Rf_error("unexpected group id");
            n[group[i] - 1]++;
            pRet[group[i] - 1] += x[i];
        }
    }
    for ( int g_i = 0; g_i < ngroup; g_i++ ) {
        if ( n[g_i] != 0 )
            pRet[g_i] /= n[g_i];
        else
            pRet[g_i] = init;
    }
    Free(n);
    UNPROTECT(1);
    return ret;
}

template<typename T>
SEXP group_sums(T * x, int * group, int ngroup, int length, double init)
{
    SEXP ret;
    PROTECT(ret = Rf_allocVector(REALSXP, ngroup));
    double * pRet = REAL(ret);
    int * n = (int *) Calloc(ngroup, int);
    for ( int g_i = 0; g_i < ngroup; g_i++ ) {
        n[g_i] = 0;
        pRet[g_i] = 0;
    }
    for ( int i = 0; i < length; i++ ) {
        if ( group[i] != NA_INTEGER ) {
            if ( group[i] <= 0 || group[i] > ngroup )
                Rf_error("unexpected group id");
            n[group[i] - 1]++;
            pRet[group[i] - 1] += x[i];
        }
    }
    for ( int g_i = 0; g_i < ngroup; g_i++ )
        if ( n[g_i] == 0 )
            pRet[g_i] = init;
    Free(n);
    UNPROTECT(1);
    return ret;
}

template<typename T>
SEXP group_mins(T * x, int * group, int ngroup, int length, double init)
{
    SEXP ret;
    PROTECT(ret = Rf_allocVector(REALSXP, ngroup));
    double * pRet = REAL(ret);
    int * n = (int *) Calloc(ngroup, int);
    for ( int g_i = 0; g_i < ngroup; g_i++ ) {
        n[g_i] = 0;
        pRet[g_i] = R_DOUBLE_MAX;
    }
    for ( int i = 0; i < length; i++ ) {
        if ( group[i] != NA_INTEGER ) {
            if ( group[i] <= 0 || group[i] > ngroup )
                Rf_error("unexpected group id");
            n[group[i] - 1]++;
            if ( x[i] < pRet[group[i] - 1] )
                pRet[group[i] - 1] = x[i];
        }
    }
    for ( int g_i = 0; g_i < ngroup; g_i++ )
        if ( n[g_i] == 0 )
            pRet[g_i] = init;
    Free(n);
    UNPROTECT(1);
    return ret;
}

template<typename T>
SEXP group_maxs(T * x, int * group, int ngroup, int length, double init)
{
    SEXP ret;
    PROTECT(ret = Rf_allocVector(REALSXP, ngroup));
    double * pRet = REAL(ret);
    int * n = (int *) Calloc(ngroup, int);
    for ( int g_i = 0; g_i < ngroup; g_i++ ) {
        n[g_i] = 0;
        pRet[g_i] = R_DOUBLE_MIN;
    }
    for ( int i = 0; i < length; i++ ) {
        if ( group[i] != NA_INTEGER ) {
            if ( group[i] <= 0 || group[i] > ngroup )
                Rf_error("unexpected group id");
            n[group[i] - 1]++;
            if ( x[i] > pRet[group[i] - 1] )
                pRet[group[i] - 1] = x[i];
        }
    }
    for ( int g_i = 0; g_i < ngroup; g_i++ )
        if ( n[g_i] == 0 )
            pRet[g_i] = init;
    Free(n);
    UNPROTECT(1);
    return ret;
}

extern "C" {

	SEXP localMaxima(SEXP x, SEXP halfWindow)
	{
		switch(TYPEOF(x)) {
			case INTSXP:
				return local_maxima<int>(INTEGER(x),
					Rf_asInteger(halfWindow), LENGTH(x));
			case REALSXP:
				return local_maxima<double>(REAL(x),
					Rf_asInteger(halfWindow), LENGTH(x));
		}
		Rf_error("supported types are 'integer' or 'numeric'");
	}

	SEXP regionMaxima(SEXP x, SEXP m, SEXP halfWindow)
	{
		switch(TYPEOF(x)) {
			case INTSXP:
				return region_maxima<int>(INTEGER(x), INTEGER(m),
					Rf_asInteger(halfWindow), LENGTH(x), LENGTH(m));
			case REALSXP:
				return region_maxima<double>(REAL(x), INTEGER(m),
					Rf_asInteger(halfWindow), LENGTH(x), LENGTH(m));
		}
		Rf_error("supported types are 'integer' or 'numeric'");
	}

	SEXP binMeans(SEXP x, SEXP lower, SEXP upper)
    {
        switch(TYPEOF(x)) {
            case INTSXP:
                return bin_means<int>(INTEGER(x), INTEGER(lower),
                    INTEGER(upper), LENGTH(x), LENGTH(lower));
            case REALSXP:
                return bin_means<double>(REAL(x), INTEGER(lower),
                    INTEGER(upper), LENGTH(x), LENGTH(lower));
        }
        Rf_error("supported types are 'integer' or 'numeric'");
    }

    SEXP binSums(SEXP x, SEXP lower, SEXP upper)
    {
        switch(TYPEOF(x)) {
            case INTSXP:
                return bin_sums<int>(INTEGER(x), INTEGER(lower),
                    INTEGER(upper), LENGTH(x), LENGTH(lower));
            case REALSXP:
                return bin_sums<double>(REAL(x), INTEGER(lower),
                    INTEGER(upper), LENGTH(x), LENGTH(lower));
        }
        Rf_error("supported types are 'integer' or 'numeric'");
    }

    SEXP binMins(SEXP x, SEXP lower, SEXP upper)
    {
        switch(TYPEOF(x)) {
            case INTSXP:
                return bin_mins<int>(INTEGER(x), INTEGER(lower),
                    INTEGER(upper), LENGTH(x), LENGTH(lower));
            case REALSXP:
                return bin_mins<double>(REAL(x), INTEGER(lower),
                    INTEGER(upper), LENGTH(x), LENGTH(lower));
        }
        Rf_error("supported types are 'integer' or 'numeric'");
    }

    SEXP binMaxs(SEXP x, SEXP lower, SEXP upper)
    {
        switch(TYPEOF(x)) {
            case INTSXP:
                return bin_maxs<int>(INTEGER(x), INTEGER(lower),
                    INTEGER(upper), LENGTH(x), LENGTH(lower));
            case REALSXP:
                return bin_maxs<double>(REAL(x), INTEGER(lower),
                    INTEGER(upper), LENGTH(x), LENGTH(lower));
        }
        Rf_error("supported types are 'integer' or 'numeric'");
    }

    SEXP groupMeans(SEXP x, SEXP group, SEXP ngroup, SEXP init)
    {
        switch(TYPEOF(x)) {
            case INTSXP:
                return group_means<int>(INTEGER(x), INTEGER(group),
                    Rf_asInteger(ngroup), LENGTH(x), Rf_asReal(init));
            case REALSXP:
                return group_means<double>(REAL(x), INTEGER(group),
                    Rf_asInteger(ngroup), LENGTH(x), Rf_asReal(init));
        }
        Rf_error("supported types are 'integer' or 'numeric'");
    }

    SEXP groupSums(SEXP x, SEXP group, SEXP ngroup, SEXP init)
    {
        switch(TYPEOF(x)) {
            case INTSXP:
                return group_sums<int>(INTEGER(x), INTEGER(group),
                    Rf_asInteger(ngroup), LENGTH(x), Rf_asReal(init));
            case REALSXP:
                return group_sums<double>(REAL(x), INTEGER(group),
                    Rf_asInteger(ngroup), LENGTH(x), Rf_asReal(init));
        }
        Rf_error("supported types are 'integer' or 'numeric'");
    }

    SEXP groupMins(SEXP x, SEXP group, SEXP ngroup, SEXP init)
    {
        switch(TYPEOF(x)) {
            case INTSXP:
                return group_mins<int>(INTEGER(x), INTEGER(group),
                    Rf_asInteger(ngroup), LENGTH(x), Rf_asReal(init));
            case REALSXP:
                return group_mins<double>(REAL(x), INTEGER(group),
                    Rf_asInteger(ngroup), LENGTH(x), Rf_asReal(init));
        }
        Rf_error("supported types are 'integer' or 'numeric'");
    }

    SEXP groupMaxs(SEXP x, SEXP group, SEXP ngroup, SEXP init)
    {
        switch(TYPEOF(x)) {
            case INTSXP:
                return group_maxs<int>(INTEGER(x), INTEGER(group),
                    Rf_asInteger(ngroup), LENGTH(x), Rf_asReal(init));
            case REALSXP:
                return group_maxs<double>(REAL(x), INTEGER(group),
                    Rf_asInteger(ngroup), LENGTH(x), Rf_asReal(init));
        }
        Rf_error("supported types are 'integer' or 'numeric'");
    }

}



