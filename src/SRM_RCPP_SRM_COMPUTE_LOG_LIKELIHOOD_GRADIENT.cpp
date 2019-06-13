//// File Name: SRM_RCPP_SRM_COMPUTE_LOG_LIKELIHOOD_GRADIENT.cpp
//// File Version: 0.193



// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
// #include <Rcpp.h>

using namespace Rcpp;
using namespace arma;

// [[RcppNOinterfaces(r, cpp)]]



///********************************************************************
///** SRM_RCPP_SRM_COMPUTE_LOG_LIKELIHOOD_GRADIENT_W0
// [[Rcpp::export]]
Rcpp::NumericMatrix SRM_RCPP_SRM_COMPUTE_LOG_LIKELIHOOD_GRADIENT_W0(
        Rcpp::NumericMatrix sigma_y_inv, Rcpp::NumericMatrix sigma_y_der,
        Rcpp::LogicalMatrix der_bool )
{
    // matrix multiplication
    int N = sigma_y_der.nrow();
    Rcpp::NumericMatrix w0(N,N);
    w0.fill(0);
    for (int ii=0; ii<N; ii++){
        for (int jj=0; jj<N; jj++){
            for (int hh=0; hh<N; hh++){
                if (der_bool(hh,jj)){
                    w0(ii,jj) += sigma_y_inv(ii,hh)*sigma_y_der(hh,jj);
                }
            }
        }
    }
    //--- OUTPUT
    return w0;
}
///********************************************************************


///********************************************************************
///** SRM_RCPP_SRM_COMPUTE_LOG_LIKELIHOOD_GRADIENT_W1
// [[Rcpp::export]]
arma::mat SRM_RCPP_SRM_COMPUTE_LOG_LIKELIHOOD_GRADIENT_W1(
        arma::mat sigma_y_inv, arma::mat sigma_y_der )
{
    arma::mat w0 = arma::mat(sigma_y_inv * sigma_y_der);
    //--- OUTPUT
    return w0;
}
///********************************************************************


///********************************************************************
///** SRM_RCPP_SRM_COMPUTE_LOG_LIKELIHOOD_GRADIENT_W2
// [[Rcpp::export]]
Rcpp::NumericMatrix SRM_RCPP_SRM_COMPUTE_LOG_LIKELIHOOD_GRADIENT_W2(
        Rcpp::NumericMatrix sigma_y_inv, Rcpp::NumericMatrix sigma_y_der,
        Rcpp::IntegerMatrix der_bool )
{
    int N = sigma_y_der.nrow();
    Rcpp::NumericMatrix w0(N,N);
    int NZ = der_bool.nrow();
    int rr=0;
    int cc=0;
    for (int hh=0; hh<N; hh++){
        for (int zz=0; zz<NZ; zz++){
            rr = der_bool(zz,0);
            cc = der_bool(zz,1);
            w0(hh,cc) += sigma_y_inv(hh,rr)*sigma_y_der(rr,cc);
        }
    }

    //--- OUTPUT
    return w0;
}
///********************************************************************
