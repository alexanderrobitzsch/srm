//// File Name: SRM_RCPP_SRM_COMPUTE_NONZERO_GRADIENT.cpp
//// File Version: 0.02



// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// [[RcppNOinterfaces(r, cpp)]]



///********************************************************************
///** SRM_RCPP_SRM_COMPUTE_NONZERO_GRADIENT
// [[Rcpp::export]]
Rcpp::LogicalMatrix SRM_RCPP_SRM_COMPUTE_NONZERO_GRADIENT( 
        Rcpp::NumericMatrix sigma_y_der, double eps)
{
    //-- detect places in sigma_y_der with non-zero entries
    int N = sigma_y_der.nrow();
    Rcpp::LogicalMatrix der_bool(N,N);
    der_bool.fill(TRUE);
    for (int ii=0; ii<N; ii++){
        for (int jj=ii; jj<N; jj++){
            if ( std::abs(sigma_y_der(ii,jj) ) < eps ){
                der_bool(ii,jj) = FALSE;
                der_bool(jj,ii) = FALSE;
            }                        
        }                
    }
    //--- OUTPUT
    return der_bool;
}
///********************************************************************

