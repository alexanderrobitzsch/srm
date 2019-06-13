//// File Name: SRM_RCPP_SRM_ULS_GRADIENT.cpp
//// File Version: 0.06



// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// [[RcppNOinterfaces(r, cpp)]]




///********************************************************************
///** SRM_RCPP_SRM_ULS_GRADIENT_SIGMA_PART
// [[Rcpp::export]]
double SRM_RCPP_SRM_ULS_GRADIENT_SIGMA_PART(Rcpp::NumericMatrix cov_resid,
        Rcpp::NumericMatrix SIGMA_Y_der, Rcpp::LogicalMatrix der_bool)
{
    int ny = cov_resid.nrow();
    double val=0;
    double temp=0;
    // gr2a <- -2*sum(cov_resid*SIGMA_Y_der*der_bool)
    for (int rr=0; rr<ny; rr++){
        for (int cc=rr; cc<ny; cc++){
            if (der_bool(rr,cc)){
                if (rr==cc){
                    temp = cov_resid(rr,cc)*SIGMA_Y_der(rr,cc);
                } else {
                    temp = 2*cov_resid(rr,cc)*SIGMA_Y_der(rr,cc);
                }
                val -= temp;
            }
        }
    }
    val = 2*val;
    //--- OUTPUT
    return val;
}
///********************************************************************
