// This file was automatically generated by odin.
// Do not edit by hand as changes will be lost.
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdbool.h>

// Collect together all the parameters and transient memory
// required to run the model in a struct.
typedef struct cyst_generator_pars {
  int odin_use_dde;
  double initial_RP;
  double RR;
  double E0;
  double IP0;
  double SP0;
  double tau;
  double SHC0;
  double IHC0;
  double SH0;
  double IH0;
  double LEH;
  double LEP;
  double HPS;
  double PPS;
  double dH;
  double dP;
  double AEL;
  double dE;
  double delta;
  double TPrev;
  double CPrev;
  double PTPrev;
  double phi;
  double ATL;
  double ADI;
  double alpha;
  double eta;
  double pil;
  double chil;
  double pih;
  double chih;
  double epsilon;
  double initial_SH;
  double initial_IH;
  double initial_SHC;
  double initial_IHC;
  double initial_E;
  double initial_SP;
  double initial_IPL;
  double initial_IPH;
  double bH;
  double bP;
  double theta;
} cyst_generator_pars;
cyst_generator_pars* cyst_generator_get_pointer(SEXP cyst_generator_ptr, int closed_error);
SEXP cyst_generator_set_user(cyst_generator_pars *cyst_generator_p, SEXP user);

SEXP get_ds_pars();
double get_user_double(SEXP user, const char *name, double default_value);
SEXP get_list_element(SEXP list, const char *name);

// Create the pointer; this will establish the struct, allocate
// memory for things that are constant size, and initialize
// constant variables
static void cyst_generator_finalize(SEXP cyst_generator_ptr);
SEXP cyst_generator_create(SEXP user, SEXP odin_use_dde) {
  cyst_generator_pars *cyst_generator_p = (cyst_generator_pars*) Calloc(1, cyst_generator_pars);
  cyst_generator_p->RR = 1;
  cyst_generator_p->E0 = NA_REAL;
  cyst_generator_p->IP0 = NA_REAL;
  cyst_generator_p->SP0 = NA_REAL;
  cyst_generator_p->tau = NA_REAL;
  cyst_generator_p->SHC0 = NA_REAL;
  cyst_generator_p->IHC0 = NA_REAL;
  cyst_generator_p->SH0 = NA_REAL;
  cyst_generator_p->IH0 = NA_REAL;
  cyst_generator_p->LEH = NA_REAL;
  cyst_generator_p->LEP = NA_REAL;
  cyst_generator_p->HPS = NA_REAL;
  cyst_generator_p->PPS = NA_REAL;
  cyst_generator_p->dH = NA_REAL;
  cyst_generator_p->dP = NA_REAL;
  cyst_generator_p->AEL = NA_REAL;
  cyst_generator_p->dE = NA_REAL;
  cyst_generator_p->delta = NA_REAL;
  cyst_generator_p->TPrev = NA_REAL;
  cyst_generator_p->CPrev = NA_REAL;
  cyst_generator_p->PTPrev = NA_REAL;
  cyst_generator_p->phi = NA_REAL;
  cyst_generator_p->ATL = NA_REAL;
  cyst_generator_p->ADI = NA_REAL;
  cyst_generator_p->alpha = NA_REAL;
  cyst_generator_p->eta = NA_REAL;
  cyst_generator_p->pil = NA_REAL;
  cyst_generator_p->chil = NA_REAL;
  cyst_generator_p->pih = NA_REAL;
  cyst_generator_p->chih = NA_REAL;
  cyst_generator_p->epsilon = 0.01;
  SEXP cyst_generator_ptr = PROTECT(R_MakeExternalPtr(cyst_generator_p, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(cyst_generator_ptr, cyst_generator_finalize);
  cyst_generator_set_user(cyst_generator_p, user);
  cyst_generator_p->odin_use_dde = INTEGER(odin_use_dde)[0];
  UNPROTECT(1);
  return cyst_generator_ptr;
}

// Set user-supplied parameter values.
SEXP cyst_generator_set_user(cyst_generator_pars *cyst_generator_p, SEXP user) {
  cyst_generator_p->E0 = get_user_double(user, "E0", cyst_generator_p->E0);
  cyst_generator_p->IP0 = get_user_double(user, "IP0", cyst_generator_p->IP0);
  cyst_generator_p->SP0 = get_user_double(user, "SP0", cyst_generator_p->SP0);
  cyst_generator_p->tau = get_user_double(user, "tau", cyst_generator_p->tau);
  cyst_generator_p->SHC0 = get_user_double(user, "SHC0", cyst_generator_p->SHC0);
  cyst_generator_p->IHC0 = get_user_double(user, "IHC0", cyst_generator_p->IHC0);
  cyst_generator_p->SH0 = get_user_double(user, "SH0", cyst_generator_p->SH0);
  cyst_generator_p->IH0 = get_user_double(user, "IH0", cyst_generator_p->IH0);
  cyst_generator_p->LEH = get_user_double(user, "LEH", cyst_generator_p->LEH);
  cyst_generator_p->LEP = get_user_double(user, "LEP", cyst_generator_p->LEP);
  cyst_generator_p->HPS = get_user_double(user, "HPS", cyst_generator_p->HPS);
  cyst_generator_p->PPS = get_user_double(user, "PPS", cyst_generator_p->PPS);
  cyst_generator_p->dH = get_user_double(user, "dH", cyst_generator_p->dH);
  cyst_generator_p->dP = get_user_double(user, "dP", cyst_generator_p->dP);
  cyst_generator_p->AEL = get_user_double(user, "AEL", cyst_generator_p->AEL);
  cyst_generator_p->dE = get_user_double(user, "dE", cyst_generator_p->dE);
  cyst_generator_p->delta = get_user_double(user, "delta", cyst_generator_p->delta);
  cyst_generator_p->TPrev = get_user_double(user, "TPrev", cyst_generator_p->TPrev);
  cyst_generator_p->CPrev = get_user_double(user, "CPrev", cyst_generator_p->CPrev);
  cyst_generator_p->PTPrev = get_user_double(user, "PTPrev", cyst_generator_p->PTPrev);
  cyst_generator_p->phi = get_user_double(user, "phi", cyst_generator_p->phi);
  cyst_generator_p->ATL = get_user_double(user, "ATL", cyst_generator_p->ATL);
  cyst_generator_p->ADI = get_user_double(user, "ADI", cyst_generator_p->ADI);
  cyst_generator_p->alpha = get_user_double(user, "alpha", cyst_generator_p->alpha);
  cyst_generator_p->eta = get_user_double(user, "eta", cyst_generator_p->eta);
  cyst_generator_p->pil = get_user_double(user, "pil", cyst_generator_p->pil);
  cyst_generator_p->chil = get_user_double(user, "chil", cyst_generator_p->chil);
  cyst_generator_p->pih = get_user_double(user, "pih", cyst_generator_p->pih);
  cyst_generator_p->chih = get_user_double(user, "chih", cyst_generator_p->chih);
  cyst_generator_p->epsilon = get_user_double(user, "epsilon", cyst_generator_p->epsilon);
  cyst_generator_p->bH = cyst_generator_p->HPS * cyst_generator_p->dH;
  cyst_generator_p->bP = cyst_generator_p->PPS * cyst_generator_p->dP;
  cyst_generator_p->theta = (cyst_generator_p->bH + cyst_generator_p->eta * (cyst_generator_p->SHC0 + cyst_generator_p->IHC0) - cyst_generator_p->dH * (cyst_generator_p->SH0 + cyst_generator_p->IH0)) / ((cyst_generator_p->SH0 * cyst_generator_p->E0) + ((1 + cyst_generator_p->RR) * cyst_generator_p->IH0 * cyst_generator_p->E0));
  return R_NilValue;
}
// Wrapper around this for use from R.
SEXP r_cyst_generator_set_user(SEXP cyst_generator_ptr, SEXP user) {
  cyst_generator_pars *cyst_generator_p = cyst_generator_get_pointer(cyst_generator_ptr, 1);
  cyst_generator_set_user(cyst_generator_p, user);
  return R_NilValue;
}

// Arrange to free all memory we have allocated
// This is called by R automatically when the pointer is
// garbage collected (i.e., when all objects holding the pointer
// go out of scope
void cyst_generator_finalize(SEXP cyst_generator_ptr) {
  cyst_generator_pars *cyst_generator_p = cyst_generator_get_pointer(cyst_generator_ptr, 0);
  if (cyst_generator_ptr) {
    Free(cyst_generator_p);
    R_ClearExternalPtr(cyst_generator_ptr);
  }
}

SEXP cyst_generator_initialise(SEXP cyst_generator_ptr, SEXP t_ptr) {
  cyst_generator_pars *cyst_generator_p = cyst_generator_get_pointer(cyst_generator_ptr, 1);
  cyst_generator_p->initial_RP = 0;
  cyst_generator_p->initial_SH = cyst_generator_p->SH0;
  cyst_generator_p->initial_IH = cyst_generator_p->IH0;
  cyst_generator_p->initial_SHC = cyst_generator_p->SHC0;
  cyst_generator_p->initial_IHC = cyst_generator_p->IHC0;
  cyst_generator_p->initial_E = cyst_generator_p->E0;
  cyst_generator_p->initial_SP = cyst_generator_p->SP0;
  cyst_generator_p->initial_IPL = cyst_generator_p->IP0 * cyst_generator_p->phi;
  cyst_generator_p->initial_IPH = cyst_generator_p->IP0 * (1 - cyst_generator_p->phi);
  SEXP state = PROTECT(allocVector(REALSXP, 9));
  REAL(state)[0] = cyst_generator_p->initial_SH;
  REAL(state)[1] = cyst_generator_p->initial_IH;
  REAL(state)[2] = cyst_generator_p->initial_SHC;
  REAL(state)[3] = cyst_generator_p->initial_IHC;
  REAL(state)[4] = cyst_generator_p->initial_E;
  REAL(state)[5] = cyst_generator_p->initial_SP;
  REAL(state)[6] = cyst_generator_p->initial_IPL;
  REAL(state)[7] = cyst_generator_p->initial_IPH;
  REAL(state)[8] = cyst_generator_p->initial_RP;
  UNPROTECT(1);
  return state;
}

SEXP cyst_generator_set_initial(SEXP cyst_generator_ptr, SEXP t_ptr, SEXP state_ptr) {
  return R_NilValue;
}

void cyst_generator_deriv(cyst_generator_pars *cyst_generator_p, double t, double *state, double *dstatedt, double *output) {
  double SH = state[0];
  double IH = state[1];
  double SHC = state[2];
  double IHC = state[3];
  double E = state[4];
  double SP = state[5];
  double IPL = state[6];
  double IPH = state[7];
  double RP = state[8];
  dstatedt[4] = cyst_generator_p->delta * IH + cyst_generator_p->delta * IHC - cyst_generator_p->dE * E;
  dstatedt[6] = cyst_generator_p->phi * cyst_generator_p->tau * SP * E - cyst_generator_p->dP * IPL;
  dstatedt[7] = (1 - cyst_generator_p->phi) * cyst_generator_p->tau * SP * E - cyst_generator_p->dP * IPH;
  dstatedt[8] = -cyst_generator_p->epsilon * RP - cyst_generator_p->dP * RP;
  dstatedt[5] = cyst_generator_p->bP + cyst_generator_p->epsilon * RP - cyst_generator_p->phi * cyst_generator_p->tau * SP * E - (1 - cyst_generator_p->phi) * cyst_generator_p->tau * SP * E - cyst_generator_p->dP * SP;
  dstatedt[0] = cyst_generator_p->bH + cyst_generator_p->alpha * IH + cyst_generator_p->eta * SHC + cyst_generator_p->eta * IHC - (cyst_generator_p->pil * cyst_generator_p->chil) * SH * IPL / cyst_generator_p->PPS - (cyst_generator_p->pih * cyst_generator_p->chih) * SH * IPH / cyst_generator_p->PPS - cyst_generator_p->theta * SH * E - cyst_generator_p->dH * SH;
  dstatedt[1] = (cyst_generator_p->pil * cyst_generator_p->chil) * SH * IPL / cyst_generator_p->PPS + (cyst_generator_p->pih * cyst_generator_p->chih) * SH * IPH / cyst_generator_p->PPS - cyst_generator_p->alpha * IH - cyst_generator_p->theta * (1 + cyst_generator_p->RR) * IH * E - cyst_generator_p->dH * IH;
  dstatedt[2] = cyst_generator_p->theta * SH * E + cyst_generator_p->alpha * IHC - (cyst_generator_p->pil * cyst_generator_p->chil) * SHC * IPL / cyst_generator_p->PPS - (cyst_generator_p->pih * cyst_generator_p->chih) * SHC * IPH / cyst_generator_p->PPS - cyst_generator_p->eta * SHC - cyst_generator_p->dH * SHC;
  dstatedt[3] = (cyst_generator_p->pil * cyst_generator_p->chil) * SHC * IPL / cyst_generator_p->PPS + (cyst_generator_p->pih * cyst_generator_p->chih) * SHC * IPH / cyst_generator_p->PPS + cyst_generator_p->theta * (1 + cyst_generator_p->RR) * IH * E - cyst_generator_p->alpha * IHC - cyst_generator_p->eta * IHC - cyst_generator_p->dH * IHC;
}

// deSolve interface
// Global variable set on initmod, as per deSolve design
static cyst_generator_pars *cyst_generator_p;
void cyst_generator_initmod_ds(void(* odeparms) (int *, double *)) {
  DL_FUNC get_deSolve_gparms = R_GetCCallable("deSolve", "get_deSolve_gparms");
  cyst_generator_p = cyst_generator_get_pointer(get_deSolve_gparms(), 1);
}
void cyst_generator_deriv_ds(int *neq, double *t, double *state,
                             double *dstatedt, double *output, int *np) {
  cyst_generator_deriv(cyst_generator_p, *t, state, dstatedt, output);
}

// dde interface
void cyst_generator_deriv_dde(size_t n_eq, double t, double *state,
                               double *dstatedt, void *cyst_generator_p) {
  cyst_generator_deriv((cyst_generator_pars*)cyst_generator_p, t, state, dstatedt, NULL);
}

SEXP cyst_generator_deriv_r(SEXP cyst_generator_ptr, SEXP t, SEXP state) {
  SEXP dstatedt = PROTECT(allocVector(REALSXP, LENGTH(state)));
  cyst_generator_pars *cyst_generator_p = cyst_generator_get_pointer(cyst_generator_ptr, 1);
  double *output = NULL;
  cyst_generator_deriv(cyst_generator_p, REAL(t)[0], REAL(state), REAL(dstatedt), output);
  UNPROTECT(1);
  return dstatedt;
}

// Translate all elements in the struct back to R
// This will mostly be useful for debugging.
SEXP cyst_generator_contents(SEXP cyst_generator_ptr) {
  cyst_generator_pars *cyst_generator_p = cyst_generator_get_pointer(cyst_generator_ptr, 1);
  SEXP state = PROTECT(allocVector(VECSXP, 44));
  SET_VECTOR_ELT(state, 0, ScalarInteger(cyst_generator_p->odin_use_dde));
  SET_VECTOR_ELT(state, 1, ScalarReal(cyst_generator_p->initial_RP));
  SET_VECTOR_ELT(state, 2, ScalarReal(cyst_generator_p->RR));
  SET_VECTOR_ELT(state, 3, ScalarReal(cyst_generator_p->E0));
  SET_VECTOR_ELT(state, 4, ScalarReal(cyst_generator_p->IP0));
  SET_VECTOR_ELT(state, 5, ScalarReal(cyst_generator_p->SP0));
  SET_VECTOR_ELT(state, 6, ScalarReal(cyst_generator_p->tau));
  SET_VECTOR_ELT(state, 7, ScalarReal(cyst_generator_p->SHC0));
  SET_VECTOR_ELT(state, 8, ScalarReal(cyst_generator_p->IHC0));
  SET_VECTOR_ELT(state, 9, ScalarReal(cyst_generator_p->SH0));
  SET_VECTOR_ELT(state, 10, ScalarReal(cyst_generator_p->IH0));
  SET_VECTOR_ELT(state, 11, ScalarReal(cyst_generator_p->LEH));
  SET_VECTOR_ELT(state, 12, ScalarReal(cyst_generator_p->LEP));
  SET_VECTOR_ELT(state, 13, ScalarReal(cyst_generator_p->HPS));
  SET_VECTOR_ELT(state, 14, ScalarReal(cyst_generator_p->PPS));
  SET_VECTOR_ELT(state, 15, ScalarReal(cyst_generator_p->dH));
  SET_VECTOR_ELT(state, 16, ScalarReal(cyst_generator_p->dP));
  SET_VECTOR_ELT(state, 17, ScalarReal(cyst_generator_p->AEL));
  SET_VECTOR_ELT(state, 18, ScalarReal(cyst_generator_p->dE));
  SET_VECTOR_ELT(state, 19, ScalarReal(cyst_generator_p->delta));
  SET_VECTOR_ELT(state, 20, ScalarReal(cyst_generator_p->TPrev));
  SET_VECTOR_ELT(state, 21, ScalarReal(cyst_generator_p->CPrev));
  SET_VECTOR_ELT(state, 22, ScalarReal(cyst_generator_p->PTPrev));
  SET_VECTOR_ELT(state, 23, ScalarReal(cyst_generator_p->phi));
  SET_VECTOR_ELT(state, 24, ScalarReal(cyst_generator_p->ATL));
  SET_VECTOR_ELT(state, 25, ScalarReal(cyst_generator_p->ADI));
  SET_VECTOR_ELT(state, 26, ScalarReal(cyst_generator_p->alpha));
  SET_VECTOR_ELT(state, 27, ScalarReal(cyst_generator_p->eta));
  SET_VECTOR_ELT(state, 28, ScalarReal(cyst_generator_p->pil));
  SET_VECTOR_ELT(state, 29, ScalarReal(cyst_generator_p->chil));
  SET_VECTOR_ELT(state, 30, ScalarReal(cyst_generator_p->pih));
  SET_VECTOR_ELT(state, 31, ScalarReal(cyst_generator_p->chih));
  SET_VECTOR_ELT(state, 32, ScalarReal(cyst_generator_p->epsilon));
  SET_VECTOR_ELT(state, 33, ScalarReal(cyst_generator_p->initial_SH));
  SET_VECTOR_ELT(state, 34, ScalarReal(cyst_generator_p->initial_IH));
  SET_VECTOR_ELT(state, 35, ScalarReal(cyst_generator_p->initial_SHC));
  SET_VECTOR_ELT(state, 36, ScalarReal(cyst_generator_p->initial_IHC));
  SET_VECTOR_ELT(state, 37, ScalarReal(cyst_generator_p->initial_E));
  SET_VECTOR_ELT(state, 38, ScalarReal(cyst_generator_p->initial_SP));
  SET_VECTOR_ELT(state, 39, ScalarReal(cyst_generator_p->initial_IPL));
  SET_VECTOR_ELT(state, 40, ScalarReal(cyst_generator_p->initial_IPH));
  SET_VECTOR_ELT(state, 41, ScalarReal(cyst_generator_p->bH));
  SET_VECTOR_ELT(state, 42, ScalarReal(cyst_generator_p->bP));
  SET_VECTOR_ELT(state, 43, ScalarReal(cyst_generator_p->theta));
  SEXP state_names = PROTECT(allocVector(STRSXP, 44));
  SET_STRING_ELT(state_names, 0, mkChar("odin_use_dde"));
  SET_STRING_ELT(state_names, 1, mkChar("initial_RP"));
  SET_STRING_ELT(state_names, 2, mkChar("RR"));
  SET_STRING_ELT(state_names, 3, mkChar("E0"));
  SET_STRING_ELT(state_names, 4, mkChar("IP0"));
  SET_STRING_ELT(state_names, 5, mkChar("SP0"));
  SET_STRING_ELT(state_names, 6, mkChar("tau"));
  SET_STRING_ELT(state_names, 7, mkChar("SHC0"));
  SET_STRING_ELT(state_names, 8, mkChar("IHC0"));
  SET_STRING_ELT(state_names, 9, mkChar("SH0"));
  SET_STRING_ELT(state_names, 10, mkChar("IH0"));
  SET_STRING_ELT(state_names, 11, mkChar("LEH"));
  SET_STRING_ELT(state_names, 12, mkChar("LEP"));
  SET_STRING_ELT(state_names, 13, mkChar("HPS"));
  SET_STRING_ELT(state_names, 14, mkChar("PPS"));
  SET_STRING_ELT(state_names, 15, mkChar("dH"));
  SET_STRING_ELT(state_names, 16, mkChar("dP"));
  SET_STRING_ELT(state_names, 17, mkChar("AEL"));
  SET_STRING_ELT(state_names, 18, mkChar("dE"));
  SET_STRING_ELT(state_names, 19, mkChar("delta"));
  SET_STRING_ELT(state_names, 20, mkChar("TPrev"));
  SET_STRING_ELT(state_names, 21, mkChar("CPrev"));
  SET_STRING_ELT(state_names, 22, mkChar("PTPrev"));
  SET_STRING_ELT(state_names, 23, mkChar("phi"));
  SET_STRING_ELT(state_names, 24, mkChar("ATL"));
  SET_STRING_ELT(state_names, 25, mkChar("ADI"));
  SET_STRING_ELT(state_names, 26, mkChar("alpha"));
  SET_STRING_ELT(state_names, 27, mkChar("eta"));
  SET_STRING_ELT(state_names, 28, mkChar("pil"));
  SET_STRING_ELT(state_names, 29, mkChar("chil"));
  SET_STRING_ELT(state_names, 30, mkChar("pih"));
  SET_STRING_ELT(state_names, 31, mkChar("chih"));
  SET_STRING_ELT(state_names, 32, mkChar("epsilon"));
  SET_STRING_ELT(state_names, 33, mkChar("initial_SH"));
  SET_STRING_ELT(state_names, 34, mkChar("initial_IH"));
  SET_STRING_ELT(state_names, 35, mkChar("initial_SHC"));
  SET_STRING_ELT(state_names, 36, mkChar("initial_IHC"));
  SET_STRING_ELT(state_names, 37, mkChar("initial_E"));
  SET_STRING_ELT(state_names, 38, mkChar("initial_SP"));
  SET_STRING_ELT(state_names, 39, mkChar("initial_IPL"));
  SET_STRING_ELT(state_names, 40, mkChar("initial_IPH"));
  SET_STRING_ELT(state_names, 41, mkChar("bH"));
  SET_STRING_ELT(state_names, 42, mkChar("bP"));
  SET_STRING_ELT(state_names, 43, mkChar("theta"));
  setAttrib(state, R_NamesSymbol, state_names);
  UNPROTECT(2);
  return state;
}

// Report back to R information on variable ordering
// The reported information includes position and length of each
// variable, from which offset, etc, can be worked out.
SEXP cyst_generator_variable_order(SEXP cyst_generator_ptr) {
  SEXP state_len = PROTECT(allocVector(VECSXP, 9));
  SEXP state_names = PROTECT(allocVector(STRSXP, 9));
  SET_VECTOR_ELT(state_len, 0, R_NilValue);
  SET_STRING_ELT(state_names, 0, mkChar("SH"));
  SET_VECTOR_ELT(state_len, 1, R_NilValue);
  SET_STRING_ELT(state_names, 1, mkChar("IH"));
  SET_VECTOR_ELT(state_len, 2, R_NilValue);
  SET_STRING_ELT(state_names, 2, mkChar("SHC"));
  SET_VECTOR_ELT(state_len, 3, R_NilValue);
  SET_STRING_ELT(state_names, 3, mkChar("IHC"));
  SET_VECTOR_ELT(state_len, 4, R_NilValue);
  SET_STRING_ELT(state_names, 4, mkChar("E"));
  SET_VECTOR_ELT(state_len, 5, R_NilValue);
  SET_STRING_ELT(state_names, 5, mkChar("SP"));
  SET_VECTOR_ELT(state_len, 6, R_NilValue);
  SET_STRING_ELT(state_names, 6, mkChar("IPL"));
  SET_VECTOR_ELT(state_len, 7, R_NilValue);
  SET_STRING_ELT(state_names, 7, mkChar("IPH"));
  SET_VECTOR_ELT(state_len, 8, R_NilValue);
  SET_STRING_ELT(state_names, 8, mkChar("RP"));
  setAttrib(state_len, R_NamesSymbol, state_names);
  UNPROTECT(2);
  return state_len;
}

cyst_generator_pars* cyst_generator_get_pointer(SEXP cyst_generator_ptr, int closed_error) {
  cyst_generator_pars *cyst_generator_p = NULL;
  if (TYPEOF(cyst_generator_ptr) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  cyst_generator_p = (cyst_generator_pars*) R_ExternalPtrAddr(cyst_generator_ptr);
  if (!cyst_generator_p && closed_error) {
    Rf_error("Pointer has been invalidated");
  }
  return cyst_generator_p;
}

SEXP get_ds_pars() {
  static DL_FUNC get_deSolve_gparms = NULL;
  if (get_deSolve_gparms == NULL) {
    get_deSolve_gparms = R_GetCCallable("deSolve", "get_deSolve_gparms");
  }
  return get_deSolve_gparms();
}
double get_user_double(SEXP user, const char *name, double default_value) {
  double ret = default_value;
  SEXP el = get_list_element(user, name);
  if (el != R_NilValue) {
    if (length(el) != 1) {
      Rf_error("Expected scalar numeric for %s", name);
    }
    if (TYPEOF(el) == REALSXP) {
      ret = REAL(el)[0];
    } else if (TYPEOF(el) == INTSXP) {
      ret = INTEGER(el)[0];
    } else {
      Rf_error("Expected a numeric value for %s", name);
    }
  }
  if (ISNA(ret)) {
    Rf_error("Expected value for %s", name);
  }
  return ret;
}
SEXP get_list_element(SEXP list, const char *name) {
  SEXP ret = R_NilValue, names = getAttrib(list, R_NamesSymbol);
  for (int i = 0; i < length(list); ++i) {
    if(strcmp(CHAR(STRING_ELT(names, i)), name) == 0) {
      ret = VECTOR_ELT(list, i);
      break;
    }
  }
  return ret;
}
