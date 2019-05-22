//// File Name: SRM_RCPP_SRM_COMPUTE_LOG_LIKELIHOOD_GRADIENT.cpp
//// File Version: 0.12



// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

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

