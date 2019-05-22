// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// SRM_RCPP_COLSUMS
Rcpp::NumericVector SRM_RCPP_COLSUMS(Rcpp::NumericMatrix x);
RcppExport SEXP _srm_SRM_RCPP_COLSUMS(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(SRM_RCPP_COLSUMS(x));
    return rcpp_result_gen;
END_RCPP
}
// SRM_RCPP_ROWSUMS
Rcpp::NumericVector SRM_RCPP_ROWSUMS(Rcpp::NumericMatrix x);
RcppExport SEXP _srm_SRM_RCPP_ROWSUMS(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(SRM_RCPP_ROWSUMS(x));
    return rcpp_result_gen;
END_RCPP
}
// SRM_ARBSRM_TRACE_PRODUCT_MATRIX
double SRM_ARBSRM_TRACE_PRODUCT_MATRIX(Rcpp::NumericMatrix x, Rcpp::NumericMatrix y);
RcppExport SEXP _srm_SRM_ARBSRM_TRACE_PRODUCT_MATRIX(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(SRM_ARBSRM_TRACE_PRODUCT_MATRIX(x, y));
    return rcpp_result_gen;
END_RCPP
}
// SRM_ARBSRM_TRACE_PRODUCT_MATRIX_TRANSPOSE
double SRM_ARBSRM_TRACE_PRODUCT_MATRIX_TRANSPOSE(Rcpp::NumericMatrix x, Rcpp::NumericMatrix y);
RcppExport SEXP _srm_SRM_ARBSRM_TRACE_PRODUCT_MATRIX_TRANSPOSE(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(SRM_ARBSRM_TRACE_PRODUCT_MATRIX_TRANSPOSE(x, y));
    return rcpp_result_gen;
END_RCPP
}
// SRM_RCPP_SRM_ARBSRM_ONE_GROUP_ESTIMATE
Rcpp::List SRM_RCPP_SRM_ARBSRM_ONE_GROUP_ESTIMATE(Rcpp::NumericMatrix data, Rcpp::LogicalMatrix data_resp, bool bivariate);
RcppExport SEXP _srm_SRM_RCPP_SRM_ARBSRM_ONE_GROUP_ESTIMATE(SEXP dataSEXP, SEXP data_respSEXP, SEXP bivariateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalMatrix >::type data_resp(data_respSEXP);
    Rcpp::traits::input_parameter< bool >::type bivariate(bivariateSEXP);
    rcpp_result_gen = Rcpp::wrap(SRM_RCPP_SRM_ARBSRM_ONE_GROUP_ESTIMATE(data, data_resp, bivariate));
    return rcpp_result_gen;
END_RCPP
}
// SRM_RCPP_MATRIX_TRACE_PRODUCT
double SRM_RCPP_MATRIX_TRACE_PRODUCT(Rcpp::NumericMatrix x, Rcpp::NumericMatrix y);
RcppExport SEXP _srm_SRM_RCPP_MATRIX_TRACE_PRODUCT(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(SRM_RCPP_MATRIX_TRACE_PRODUCT(x, y));
    return rcpp_result_gen;
END_RCPP
}
// SRM_RCPP_SRM_MATRIX_MULT_LOGICAL
Rcpp::NumericMatrix SRM_RCPP_SRM_MATRIX_MULT_LOGICAL(Rcpp::NumericMatrix x, Rcpp::LogicalMatrix y);
RcppExport SEXP _srm_SRM_RCPP_SRM_MATRIX_MULT_LOGICAL(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalMatrix >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(SRM_RCPP_SRM_MATRIX_MULT_LOGICAL(x, y));
    return rcpp_result_gen;
END_RCPP
}
// SRM_RCPP_SRM_ARBSRM_SE_CREATE_CWU
Rcpp::NumericMatrix SRM_RCPP_SRM_ARBSRM_SE_CREATE_CWU(int NF);
RcppExport SEXP _srm_SRM_RCPP_SRM_ARBSRM_SE_CREATE_CWU(SEXP NFSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type NF(NFSEXP);
    rcpp_result_gen = Rcpp::wrap(SRM_RCPP_SRM_ARBSRM_SE_CREATE_CWU(NF));
    return rcpp_result_gen;
END_RCPP
}
// SRM_RCPP_SRM_COMPUTE_HESSIAN_RR
Rcpp::NumericMatrix SRM_RCPP_SRM_COMPUTE_HESSIAN_RR(Rcpp::List hess_list, Rcpp::List mu_y_der_list, Rcpp::List mu_y_der_bool_list, Rcpp::NumericMatrix SIGMA_Y_inv, int npar);
RcppExport SEXP _srm_SRM_RCPP_SRM_COMPUTE_HESSIAN_RR(SEXP hess_listSEXP, SEXP mu_y_der_listSEXP, SEXP mu_y_der_bool_listSEXP, SEXP SIGMA_Y_invSEXP, SEXP nparSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type hess_list(hess_listSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type mu_y_der_list(mu_y_der_listSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type mu_y_der_bool_list(mu_y_der_bool_listSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type SIGMA_Y_inv(SIGMA_Y_invSEXP);
    Rcpp::traits::input_parameter< int >::type npar(nparSEXP);
    rcpp_result_gen = Rcpp::wrap(SRM_RCPP_SRM_COMPUTE_HESSIAN_RR(hess_list, mu_y_der_list, mu_y_der_bool_list, SIGMA_Y_inv, npar));
    return rcpp_result_gen;
END_RCPP
}
// SRM_RCPP_SRM_COMPUTE_LOG_LIKELIHOOD_GRADIENT_W0
Rcpp::NumericMatrix SRM_RCPP_SRM_COMPUTE_LOG_LIKELIHOOD_GRADIENT_W0(Rcpp::NumericMatrix sigma_y_inv, Rcpp::NumericMatrix sigma_y_der, Rcpp::LogicalMatrix der_bool);
RcppExport SEXP _srm_SRM_RCPP_SRM_COMPUTE_LOG_LIKELIHOOD_GRADIENT_W0(SEXP sigma_y_invSEXP, SEXP sigma_y_derSEXP, SEXP der_boolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type sigma_y_inv(sigma_y_invSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type sigma_y_der(sigma_y_derSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalMatrix >::type der_bool(der_boolSEXP);
    rcpp_result_gen = Rcpp::wrap(SRM_RCPP_SRM_COMPUTE_LOG_LIKELIHOOD_GRADIENT_W0(sigma_y_inv, sigma_y_der, der_bool));
    return rcpp_result_gen;
END_RCPP
}
// SRM_RCPP_SRM_COMPUTE_NONZERO_GRADIENT
Rcpp::LogicalMatrix SRM_RCPP_SRM_COMPUTE_NONZERO_GRADIENT(Rcpp::NumericMatrix sigma_y_der, double eps);
RcppExport SEXP _srm_SRM_RCPP_SRM_COMPUTE_NONZERO_GRADIENT(SEXP sigma_y_derSEXP, SEXP epsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type sigma_y_der(sigma_y_derSEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    rcpp_result_gen = Rcpp::wrap(SRM_RCPP_SRM_COMPUTE_NONZERO_GRADIENT(sigma_y_der, eps));
    return rcpp_result_gen;
END_RCPP
}
// SRM_RCPP_SRM_DATA_LIST_CREATE_INSERTION_MATRIX
Rcpp::IntegerMatrix SRM_RCPP_SRM_DATA_LIST_CREATE_INSERTION_MATRIX(Rcpp::IntegerMatrix x);
RcppExport SEXP _srm_SRM_RCPP_SRM_DATA_LIST_CREATE_INSERTION_MATRIX(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(SRM_RCPP_SRM_DATA_LIST_CREATE_INSERTION_MATRIX(x));
    return rcpp_result_gen;
END_RCPP
}
// SRM_RCPP_SRM_INSERT_ELEMENTS
Rcpp::NumericMatrix SRM_RCPP_SRM_INSERT_ELEMENTS(Rcpp::NumericMatrix sigma_y0, Rcpp::IntegerMatrix Zis, Rcpp::NumericMatrix sigma_u);
RcppExport SEXP _srm_SRM_RCPP_SRM_INSERT_ELEMENTS(SEXP sigma_y0SEXP, SEXP ZisSEXP, SEXP sigma_uSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type sigma_y0(sigma_y0SEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type Zis(ZisSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type sigma_u(sigma_uSEXP);
    rcpp_result_gen = Rcpp::wrap(SRM_RCPP_SRM_INSERT_ELEMENTS(sigma_y0, Zis, sigma_u));
    return rcpp_result_gen;
END_RCPP
}
// SRM_RCPP_SRM_ULS_GRADIENT_SIGMA_PART
double SRM_RCPP_SRM_ULS_GRADIENT_SIGMA_PART(Rcpp::NumericMatrix cov_resid, Rcpp::NumericMatrix SIGMA_Y_der, Rcpp::LogicalMatrix der_bool);
RcppExport SEXP _srm_SRM_RCPP_SRM_ULS_GRADIENT_SIGMA_PART(SEXP cov_residSEXP, SEXP SIGMA_Y_derSEXP, SEXP der_boolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type cov_resid(cov_residSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type SIGMA_Y_der(SIGMA_Y_derSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalMatrix >::type der_bool(der_boolSEXP);
    rcpp_result_gen = Rcpp::wrap(SRM_RCPP_SRM_ULS_GRADIENT_SIGMA_PART(cov_resid, SIGMA_Y_der, der_bool));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_srm_SRM_RCPP_COLSUMS", (DL_FUNC) &_srm_SRM_RCPP_COLSUMS, 1},
    {"_srm_SRM_RCPP_ROWSUMS", (DL_FUNC) &_srm_SRM_RCPP_ROWSUMS, 1},
    {"_srm_SRM_ARBSRM_TRACE_PRODUCT_MATRIX", (DL_FUNC) &_srm_SRM_ARBSRM_TRACE_PRODUCT_MATRIX, 2},
    {"_srm_SRM_ARBSRM_TRACE_PRODUCT_MATRIX_TRANSPOSE", (DL_FUNC) &_srm_SRM_ARBSRM_TRACE_PRODUCT_MATRIX_TRANSPOSE, 2},
    {"_srm_SRM_RCPP_SRM_ARBSRM_ONE_GROUP_ESTIMATE", (DL_FUNC) &_srm_SRM_RCPP_SRM_ARBSRM_ONE_GROUP_ESTIMATE, 3},
    {"_srm_SRM_RCPP_MATRIX_TRACE_PRODUCT", (DL_FUNC) &_srm_SRM_RCPP_MATRIX_TRACE_PRODUCT, 2},
    {"_srm_SRM_RCPP_SRM_MATRIX_MULT_LOGICAL", (DL_FUNC) &_srm_SRM_RCPP_SRM_MATRIX_MULT_LOGICAL, 2},
    {"_srm_SRM_RCPP_SRM_ARBSRM_SE_CREATE_CWU", (DL_FUNC) &_srm_SRM_RCPP_SRM_ARBSRM_SE_CREATE_CWU, 1},
    {"_srm_SRM_RCPP_SRM_COMPUTE_HESSIAN_RR", (DL_FUNC) &_srm_SRM_RCPP_SRM_COMPUTE_HESSIAN_RR, 5},
    {"_srm_SRM_RCPP_SRM_COMPUTE_LOG_LIKELIHOOD_GRADIENT_W0", (DL_FUNC) &_srm_SRM_RCPP_SRM_COMPUTE_LOG_LIKELIHOOD_GRADIENT_W0, 3},
    {"_srm_SRM_RCPP_SRM_COMPUTE_NONZERO_GRADIENT", (DL_FUNC) &_srm_SRM_RCPP_SRM_COMPUTE_NONZERO_GRADIENT, 2},
    {"_srm_SRM_RCPP_SRM_DATA_LIST_CREATE_INSERTION_MATRIX", (DL_FUNC) &_srm_SRM_RCPP_SRM_DATA_LIST_CREATE_INSERTION_MATRIX, 1},
    {"_srm_SRM_RCPP_SRM_INSERT_ELEMENTS", (DL_FUNC) &_srm_SRM_RCPP_SRM_INSERT_ELEMENTS, 3},
    {"_srm_SRM_RCPP_SRM_ULS_GRADIENT_SIGMA_PART", (DL_FUNC) &_srm_SRM_RCPP_SRM_ULS_GRADIENT_SIGMA_PART, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_srm(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
