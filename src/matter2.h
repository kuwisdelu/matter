#ifndef MATTER2
#define MATTER2

#include "Rutils.h"
#include "atoms.h"

//// Matter class
//----------------

class Matter2 {

	public:

		Matter2(SEXP x) : _data(x)
		{
			_type = R_do_slot(x, Rf_install("type"));
			_dim = R_do_slot(x, Rf_install("dim"));
			_names = R_do_slot(x, Rf_install("names"));
			_dimnames = R_do_slot(x, Rf_install("dimnames"));
		}

		~Matter2() {
			_data.self_destruct();
		}

		void self_destruct() {
			_data.self_destruct();
		}

		Atoms * data() {
			return &_data;
		}

		int type() {
			return INTEGER_ELT(_type, 0);
		}

		int type(int i) {
			if ( XLENGTH(_type) > i )
				return INTEGER_ELT(_type, i);
			else
				return NA_INTEGER;
		}

		R_xlen_t dim(int i) {
			if ( XLENGTH(_dim) > i )
				return IndexElt(_dim, i);
			else
				return NA_INTEGER;
		}

		int rank() {
			return LENGTH(_dim);
		}

		SEXP names() {
			return _names;
		}

		SEXP names(int i) {
			if ( XLENGTH(_names) > i )
				return STRING_ELT(_names, i);
			else
				return NA_STRING;
		}

		SEXP dimnames() {
			return _dimnames;
		}

		SEXP dimnames(int i) {
			if ( XLENGTH(_dimnames) > i )
				return VECTOR_ELT(_dimnames, i);
			else
				return R_NilValue;
		}

	protected:

		Atoms2 _data;
		SEXP _type;
		SEXP _dim;
		SEXP _names;
		SEXP _dimnames;

};

class Matter2Array {

	public:

		Matter2Array(SEXP x) : Matter2(x)
		{
			_ops = R_do_slot(x, Rf_install("ops"));
			_lastMaj = Rf_asLogical(R_do_slot(x, Rf_install("lastMaj")));
		}

		bool last_dim_major() {
			return _lastMaj
		}

		template<typename Tind, typename Tval>
		Tval get(index_t i);

		template<typename Tind, typename Tval>
		size_t getRegion(index_t i, size_t size, Tval * buffer);

		SEXP getRegion(index_t i, size_t size);

		template<typename Tind, typename Tval>
		size_t getElements(SEXP indx, Tval * buffer);

		SEXP getElements(SEXP indx);

	protected:

		SEXP _ops;
		bool _lastMaj;

};

#endif // MATTER2
