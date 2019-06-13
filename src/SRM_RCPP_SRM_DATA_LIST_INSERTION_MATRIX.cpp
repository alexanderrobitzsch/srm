//// File Name: SRM_RCPP_SRM_DATA_LIST_INSERTION_MATRIX.cpp
//// File Version: 0.15



// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// [[RcppNOinterfaces(r, cpp)]]



///********************************************************************
///** SRM_RCPP_SRM_DATA_LIST_CREATE_INSERTION_MATRIX
// [[Rcpp::export]]
Rcpp::IntegerMatrix SRM_RCPP_SRM_DATA_LIST_CREATE_INSERTION_MATRIX(
        Rcpp::IntegerMatrix x)
{
    int N=x.nrow();
    int N2=N*(N+1)/2;
    Rcpp::IntegerMatrix y(N2,5);
    int hh=0;
    for (int ii=0; ii<N; ii++){
        for (int jj=ii; jj<N; jj++){
            y(hh,0) = x(ii,0);
            y(hh,1) = x(ii,1);
            y(hh,2) = x(jj,1);
            y(hh,3) = x(ii,2);
            y(hh,4) = x(jj,2);
            hh++;
        }
    }
    //--- OUTPUT
    return y;
}
///********************************************************************


///********************************************************************
///** SRM_RCPP_SRM_INSERT_ELEMENTS
// [[Rcpp::export]]
Rcpp::NumericMatrix SRM_RCPP_SRM_INSERT_ELEMENTS(
        Rcpp::NumericMatrix sigma_y0, Rcpp::IntegerMatrix Zis,
        Rcpp::NumericMatrix sigma_u )
{
    Rcpp::NumericMatrix sigma_y = Rcpp::clone(sigma_y0);
    int NH=Zis.nrow();
    int ind1, ind2;

    for (int hh=0; hh<NH; hh++){
        ind1 = Zis(hh,1);
        ind2 = Zis(hh,2);
        sigma_y(ind1, ind2) += sigma_u( Zis(hh,3), Zis(hh,4) );
        sigma_y(ind2, ind1) = sigma_y(ind1, ind2);
    }

    //--- OUTPUT
    return sigma_y;
}
///********************************************************************
