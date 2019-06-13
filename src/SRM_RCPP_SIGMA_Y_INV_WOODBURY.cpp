//// File Name: SRM_RCPP_SIGMA_Y_INV_WOODBURY.cpp
//// File Version: 0.242



// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// [[RcppNOinterfaces(r, cpp)]]



///********************************************************************
///** SRM_RCPP_SIGMA_Y_INV_WOODBURY_PHI_INV
// [[Rcpp::export]]
Rcpp::NumericMatrix SRM_RCPP_SIGMA_Y_INV_WOODBURY_PHI_INV( Rcpp::NumericMatrix SIGMA_U_INV,
    int NI )
{
    int n1 = SIGMA_U_INV.ncol();
    int n2 = n1*NI;
    Rcpp::NumericMatrix PHI_INV(n2,n2);

    for (int ii=0; ii<NI; ii++){
        for (int hh=0; hh<n1; hh++){
            for (int jj=0; jj<n1; jj++){
                PHI_INV(ii*n1+hh, ii*n1+jj)    = SIGMA_U_INV(hh,jj);
            }
        }
    }

    //--- OUTPUT
    return PHI_INV;
}
///********************************************************************

///********************************************************************
///** SRM_RCPP_SIGMA_Y_INV_WOODBURY_TMAT
// [[Rcpp::export]]
Rcpp::NumericMatrix SRM_RCPP_SIGMA_Y_INV_WOODBURY_TMAT( Rcpp::NumericMatrix A_inv,
    Rcpp::IntegerMatrix Z_ind, Rcpp::NumericMatrix Phi_inv )
{
    Rcpp::NumericMatrix ZAZ = Rcpp::clone(Phi_inv);
    int NZ = Phi_inv.nrow();
    int n2 = Z_ind.nrow();
    int cc=0;
    int dd=0;
    for (int zz=0; zz<n2; zz++){
        for (int yy=zz; yy<n2; yy++){
            cc=Z_ind(zz,1);
            dd=Z_ind(yy,1);
            ZAZ(cc,dd) += A_inv( Z_ind(zz,0), Z_ind(yy,0) );
        }
    }
    for (int cc=0; cc<NZ; cc++){
        for (int dd=cc+1; dd<NZ; dd++){
                ZAZ(dd,cc) = ZAZ(cc,dd);
        }
    }

    //--- OUTPUT
    return ZAZ;
}
///********************************************************************

///********************************************************************
///** SRM_RCPP_SIGMA_Y_INV_WOODBURY_ZA
// [[Rcpp::export]]
Rcpp::NumericMatrix SRM_RCPP_SIGMA_Y_INV_WOODBURY_ZA( Rcpp::IntegerMatrix Z_ind,
    Rcpp::NumericMatrix A_inv, int NZ)
{
    int NA1 = A_inv.ncol();
    Rcpp::NumericMatrix ZA(NZ, NA1);
    int n2 = Z_ind.nrow();
    for (int cc=0; cc<NA1; cc++){
        for (int zz=0; zz<n2; zz++){
            ZA(Z_ind(zz,1),cc) += A_inv(Z_ind(zz,0),cc);
        }
    }
    //--- OUTPUT
    return ZA;
}
///********************************************************************


///********************************************************************
///** SRM_RCPP_SIGMA_Y_INV_WOODBURY_Y_INV
// [[Rcpp::export]]
Rcpp::NumericMatrix SRM_RCPP_SIGMA_Y_INV_WOODBURY_Y_INV( Rcpp::NumericMatrix ZA,
    Rcpp::NumericMatrix T_inv, Rcpp::NumericMatrix A_inv)
{
    int NZ = ZA.ncol();
    Rcpp::NumericMatrix Emat(NZ,NZ);
    int NT = T_inv.ncol();
    int xx=0;
    int yy=0;

    for (int cc=0; cc<NZ; cc++){
        for (int dd=cc; dd<NZ; dd++){
            Emat(cc,dd) = A_inv(cc,dd);
            for (xx=0; xx<NT; xx++){
                for (yy=0; yy<NT; yy++){
                    Emat(cc,dd) -= ZA(xx,cc)*ZA(yy,dd)*T_inv(xx,yy);
                }
            }
            Emat(dd,cc) = Emat(cc,dd);
        }
    }

    //--- OUTPUT
    return Emat;
}
///********************************************************************

