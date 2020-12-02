#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void cyst_generator_initmod_desolve(void *);
extern void cyst_generator_output_dde(void *);
extern void cyst_generator_rhs_dde(void *);
extern void cyst_generator_rhs_desolve(void *);

/* .Call calls */
extern SEXP cyst_generator_contents(SEXP);
extern SEXP cyst_generator_create(SEXP);
extern SEXP cyst_generator_initial_conditions(SEXP, SEXP);
extern SEXP cyst_generator_metadata(SEXP);
extern SEXP cyst_generator_rhs_r(SEXP, SEXP, SEXP);
extern SEXP cyst_generator_set_initial(SEXP, SEXP, SEXP, SEXP);
extern SEXP cyst_generator_set_user(SEXP, SEXP);

static const R_CMethodDef CEntries[] = {
    {"cyst_generator_initmod_desolve", (DL_FUNC) &cyst_generator_initmod_desolve, 1},
    {"cyst_generator_output_dde",      (DL_FUNC) &cyst_generator_output_dde,      1},
    {"cyst_generator_rhs_dde",         (DL_FUNC) &cyst_generator_rhs_dde,         1},
    {"cyst_generator_rhs_desolve",     (DL_FUNC) &cyst_generator_rhs_desolve,     1},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"cyst_generator_contents",           (DL_FUNC) &cyst_generator_contents,           1},
    {"cyst_generator_create",             (DL_FUNC) &cyst_generator_create,             1},
    {"cyst_generator_initial_conditions", (DL_FUNC) &cyst_generator_initial_conditions, 2},
    {"cyst_generator_metadata",           (DL_FUNC) &cyst_generator_metadata,           1},
    {"cyst_generator_rhs_r",              (DL_FUNC) &cyst_generator_rhs_r,              3},
    {"cyst_generator_set_initial",        (DL_FUNC) &cyst_generator_set_initial,        4},
    {"cyst_generator_set_user",           (DL_FUNC) &cyst_generator_set_user,           2},
    {NULL, NULL, 0}
};

void R_init_epicyst(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
