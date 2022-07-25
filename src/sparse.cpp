
#include "sparse.h"

extern "C" {

	SEXP getSparseVector(SEXP x, SEXP i) {
		SparseVector sVec(x);
		switch(sVec.datamode()) {
			case R_INTEGER:
				switch(TYPEOF(sVec.keys())) {
					case INTSXP:
						return sVec.getElements<int,int,INTSXP>(i);
					case REALSXP:
						return sVec.getElements<double,int,INTSXP>(i);
				}
			case R_NUMERIC:
				switch(TYPEOF(sVec.keys())) {
					case INTSXP:
						return sVec.getElements<int,double,REALSXP>(i);
					case REALSXP:
						return sVec.getElements<double,double,REALSXP>(i);
				}
		}
		return R_NilValue;
	}

}

