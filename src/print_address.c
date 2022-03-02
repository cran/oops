// Get the address of a object. Taken with love from inspect.c in the R sources and modified.
// Source: Dirk Eddelbuettel

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

SEXP get_address(SEXP v) {
    char buf[32];
#ifdef _WIN64
    snprintf(buf, 31, "0x%p", v);
#else
    snprintf(buf, 31, "0x%lx", (long) v);
#endif
    SEXP result;
    PROTECT(result = allocVector(STRSXP, 1));
    SET_STRING_ELT(result, 0, mkChar(buf));
    UNPROTECT(1);
    return result;
}

void R_init_oops(DllInfo *info) {
    R_RegisterCCallable("oops", "get_address", (DL_FUNC) &get_address);

    /* tools::package_native_routine_registration_skeleton() reports empty set */
    R_registerRoutines(info,
                       NULL,            /* slot for .C */
                       NULL,            /* slot for .Call */
                       NULL,            /* slot for .Fortran */
                       NULL);           /* slot for .External */
    R_useDynamicSymbols(info, TRUE);    /* controls visibility */
}

