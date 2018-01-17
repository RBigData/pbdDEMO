/* Automatically generated. Do not edit by hand. */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdlib.h>

extern SEXP pbddemo_linecount(SEXP file);

static const R_CallMethodDef CallEntries[] = {
  {"pbddemo_linecount", (DL_FUNC) &pbddemo_linecount, 1},
  {NULL, NULL, 0}
};

void R_init_pbdDEMO(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
