//// File Name: SRM_RCPP_SRM_COMPUTE_NONZERO_GRADIENT.cpp
//// File Version: 0.07



// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// [[RcppNOinterfaces(r, cpp)]]


///********************************************************************
///** SRM_RCPP_SRM_COMPUTE_NONZERO_GRADIENT_INDICES
// [[Rcpp::export]]
Rcpp::IntegerMatrix SRM_RCPP_SRM_COMPUTE_NONZERO_GRADIENT_INDICES(
        Rcpp::NumericMatrix sigma_y_der, double eps)
{
    //-- detect places in sigma_y_der with non-zero entries
    int N = sigma_y_der.nrow();
    int N2 = N*N;
    Rcpp::IntegerMatrix der_bool(N2,2);
    int hh=0;
    for (int ii=0; ii<N; ii++){
        for (int jj=ii; jj<N; jj++){
            if ( std::abs(sigma_y_der(ii,jj) ) >= eps ){
                der_bool(hh,0) = ii;
                der_bool(hh,1) = jj;
                hh ++;
                if (jj>ii){
                    der_bool(hh,0) = jj;
                    der_bool(hh,1) = ii;
                    hh ++;
                }
            }
        }
    }
    if (hh>0){
        hh = hh-1;
    }
    der_bool = der_bool( Rcpp::Range(0,hh), Rcpp::Range(0,1));

    //--- OUTPUT
    return der_bool;
}
///********************************************************************
