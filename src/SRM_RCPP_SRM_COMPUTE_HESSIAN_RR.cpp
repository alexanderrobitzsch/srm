//// File Name: SRM_RCPP_SRM_COMPUTE_HESSIAN_RR.cpp
//// File Version: 0.245



// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// [[RcppNOinterfaces(r, cpp)]]




///********************************************************************
///** SRM_RCPP_SRM_COMPUTE_HESSIAN_RR_SIGMA_CON
// [[RcppNOexport]]
double SRM_RCPP_SRM_COMPUTE_HESSIAN_RR_SIGMA_CON(Rcpp::NumericMatrix hess_ii,
        Rcpp::NumericMatrix hess_jj, double eps)
{
    int ny = hess_ii.ncol();
    double val=0;
    // hessian_rr[ii,jj] <- .5*sum( hess_list[[ii]] * t(hess_list[[jj]]) )
    for (int rr=0; rr<ny; rr++){
        for (int cc=0; cc<ny; cc++){
            if ( std::abs(hess_ii(rr,cc)) > eps ){
                if ( std::abs(hess_jj(cc,rr)) > eps ){
                    val += hess_ii(rr,cc)*hess_jj(cc,rr);
                }
            }
        } // end cc
    }  // end rr
    val = .5*val;
    //--- OUTPUT
    return val;
}
///********************************************************************

///********************************************************************
///** SRM_RCPP_SRM_COMPUTE_HESSIAN_RR_MU_CON0
// [[RcppNOexport]]
Rcpp::NumericVector SRM_RCPP_SRM_COMPUTE_HESSIAN_RR_MU_CON0(
        Rcpp::NumericVector mu_y_der_ii, Rcpp::NumericMatrix SIGMA_Y_inv,
        double eps)
{
    int ny = mu_y_der_ii.size();
    Rcpp::NumericVector mu_ii(ny);
    //    if (mu_y_der_bool_list[[ii]]){
    //        mu_ii <- crossprod(mu_y_der_list[[ii]], SIGMA_Y_inv)
    //    }
    for (int vv=0; vv<ny; vv++){
        for (int hh=0; hh<ny; hh++){
            if ( std::abs(mu_y_der_ii[hh]) > eps ){
                mu_ii[vv] += mu_y_der_ii[hh] * SIGMA_Y_inv(hh,vv);
            }
        }
    }
    //--- OUTPUT
    return mu_ii;
}
///********************************************************************

///********************************************************************
///** SRM_RCPP_SRM_COMPUTE_HESSIAN_RR_MU_CON
// [[RcppNOexport]]
double SRM_RCPP_SRM_COMPUTE_HESSIAN_RR_MU_CON(
        Rcpp::NumericVector mu_ii, Rcpp::NumericVector mu_y_der_jj,
        double eps)
{
    int ny = mu_y_der_jj.size();
    double val = 0;
    //        if (mu_y_der_bool_list[[ii]] & mu_y_der_bool_list[[jj]]){
    //            # contribution is of the form a'Va
    //            mu_contrib <- mu_ii %*% mu_y_der_list[[jj]]
    for (int hh=0; hh<ny; hh++){
        if ( std::abs(mu_y_der_jj[hh]) > eps ){
            val += mu_ii[hh] * mu_y_der_jj[hh];
        }
    }
    //--- OUTPUT
    return val;
}
///********************************************************************

///********************************************************************
///** SRM_RCPP_SRM_COMPUTE_HESSIAN_RR
// [[Rcpp::export]]
Rcpp::NumericMatrix SRM_RCPP_SRM_COMPUTE_HESSIAN_RR(Rcpp::List hess_list,
        Rcpp::List mu_y_der_list, Rcpp::List mu_y_der_bool_list,
        Rcpp::NumericMatrix SIGMA_Y_inv, int npar)
{
    double eps = 1e-14;
    int ny = SIGMA_Y_inv.ncol();
    double val, mu_contrib;
    Rcpp::NumericMatrix hess_ii, hess_jj;
    Rcpp::NumericVector mu_y_der_ii, mu_y_der_jj;
    bool mu_y_der_bool_ii, mu_y_der_bool_jj;
    Rcpp::NumericMatrix hessian_rr(npar, npar);
    Rcpp::NumericVector mu_ii(ny);

    for (int ii=0; ii<npar; ii++){
        hess_ii = Rcpp::as<Rcpp::NumericMatrix>(hess_list[ii]);
        mu_y_der_bool_ii = Rcpp::as<bool>(mu_y_der_bool_list[ii]);
        mu_y_der_ii = Rcpp::as<Rcpp::NumericVector>(mu_y_der_list[ii]);
        if (mu_y_der_bool_ii){
            mu_ii = SRM_RCPP_SRM_COMPUTE_HESSIAN_RR_MU_CON0( mu_y_der_ii,
                            SIGMA_Y_inv, eps);
        }
        for (int jj=ii; jj<npar; jj++){
            hess_jj = Rcpp::as<Rcpp::NumericMatrix>(hess_list[jj]);
            val = SRM_RCPP_SRM_COMPUTE_HESSIAN_RR_SIGMA_CON(hess_ii, hess_jj, eps);
            hessian_rr(ii,jj) = val;
            mu_y_der_bool_jj = Rcpp::as<bool>(mu_y_der_bool_list[jj]);
            mu_y_der_jj = Rcpp::as<Rcpp::NumericVector>(mu_y_der_list[jj]);
            if(mu_y_der_bool_ii & mu_y_der_bool_jj){
                mu_contrib = SRM_RCPP_SRM_COMPUTE_HESSIAN_RR_MU_CON(mu_ii, mu_y_der_jj, eps);
                hessian_rr(ii,jj) += mu_contrib;
            }
            hessian_rr(ii,jj) = -hessian_rr(ii,jj);
            hessian_rr(jj,ii) = hessian_rr(ii,jj);
        }
    }

    //--- OUTPUT
    return hessian_rr;
}
///********************************************************************

