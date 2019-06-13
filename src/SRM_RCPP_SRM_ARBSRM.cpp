//// File Name: SRM_RCPP_SRM_ARBSRM.cpp
//// File Version: 0.434


// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
using namespace arma;


// user includes


///********************************************************************
///** SRM_RCPP_COLSUMS
// [[Rcpp::export]]
Rcpp::NumericVector SRM_RCPP_COLSUMS(Rcpp::NumericMatrix x)
{
    int n = x.nrow();
    int p = x.ncol();
    Rcpp::NumericVector y(p);
    for (int pp=0; pp<p; pp++){
        for (int hh=0; hh<n; hh++){
            y[pp] += x(hh,pp);
        }
    }
    //--- OUTPUT:
    return y;
}
///********************************************************************

///********************************************************************
///** SRM_RCPP_ROWSUMS
// [[Rcpp::export]]
Rcpp::NumericVector SRM_RCPP_ROWSUMS(Rcpp::NumericMatrix x)
{
    int n = x.nrow();
    int p = x.ncol();
    Rcpp::NumericVector y(n);
    for (int pp=0; pp<p; pp++){
        for (int hh=0; hh<n; hh++){
            y[pp] += x(pp,hh);
        }
    }
    //--- OUTPUT:
    return y;
}
///********************************************************************

///********************************************************************
///** SRM_ARBSRM_TRACE_PRODUCT_MATRIX
// [[Rcpp::export]]
double SRM_ARBSRM_TRACE_PRODUCT_MATRIX(Rcpp::NumericMatrix x, Rcpp::NumericMatrix y)
{
    double z = 0;
    int nr = x.nrow();
    int nc = x.ncol();
    for (int ii=0; ii<nr; ii++){
        for (int hh=0; hh<nc; hh++){
            // if ( ( std::abs( x(ii,hh)) > eps ) | ( std::abs(y(hh,ii)) > eps ) ){
                z += x(ii,hh)*y(hh,ii);
            // }
        }
    }
    //--- OUTPUT:
    return z;
}
///********************************************************************

///********************************************************************
///** SRM_ARBSRM_TRACE_PRODUCT_MATRIX_TRANSPOSE
// [[Rcpp::export]]
double SRM_ARBSRM_TRACE_PRODUCT_MATRIX_TRANSPOSE(Rcpp::NumericMatrix x, Rcpp::NumericMatrix y)
{
    double z = 0;
    int nr = x.nrow();
    int nc = x.ncol();
    for (int ii=0; ii<nr; ii++){
        for (int hh=0; hh<nc; hh++){
            if ( ( x(ii,hh)!=0 ) | (y(ii,hh)!= 0 ) ){
                z += x(ii,hh)*y(ii,hh);
            }
        }
    }
    for (int ii=0; ii<nr; ii++){
        for (int hh=0; hh<nc; hh++){
            if ( ( x(hh,ii)!=0 ) | (y(hh,ii)!= 0 ) ){
                z += x(hh,ii)*y(hh,ii);
            }
        }
    }
    z = 0.5*z;
    //--- OUTPUT:
    return z;
}
///********************************************************************


///********************************************************************
///** SRM_RCPP_SRM_ARBSRM_ONE_GROUP_ESTIMATE
// [[Rcpp::export]]
Rcpp::List SRM_RCPP_SRM_ARBSRM_ONE_GROUP_ESTIMATE( Rcpp::NumericMatrix data,
    Rcpp::LogicalMatrix data_resp, bool bivariate )
{
    int nact = data.nrow();
    int npar = data.ncol();
    int ntot = Rcpp::sum(data_resp);

    // indicator matrices dyads
    Rcpp::IntegerMatrix inde(ntot/2,2);
    Rcpp::NumericVector dv(ntot);
    int k = 0;
    int l = nact/2+1;
    Rcpp::NumericMatrix bata(nact,npar);
    for (int i=0; i<nact; i++){
        for (int j=0; j<npar; j++) {
            if ( data_resp(i,j) ){
                dv[k] = data(i,j);
                bata(i,j) = 1;
                if (i+1<l) {
                    inde(k,0) = i;
                    inde(k,1) = j;
                }
                k++;
            }
        }
    }

    // indicator matrices actor and partner effects
    ntot = k/2;
    nact = nact/2;
    Rcpp::NumericMatrix ac(ntot, nact);
    Rcpp::NumericMatrix pa(ntot, npar);
    for (int k=0; k<ntot; k++){
        ac(k, inde(k,0)) = 1;
        pa(k, inde(k,1)) = 1;
    }
    Rcpp::NumericVector apar = SRM_RCPP_COLSUMS(ac);
    Rcpp::NumericVector pact = SRM_RCPP_COLSUMS(pa);

    Rcpp::NumericVector mirr(nact);
    Rcpp::NumericMatrix recip(nact,npar);
    int ntot2 = 2*ntot;
    int ntot3 = 3*ntot;
    int ntot4 = 4*ntot;
    int ntot5 = 5*ntot;
    int ntot6 = 6*ntot;
    Rcpp::NumericMatrix aa(ntot,ntot6);
    Rcpp::NumericMatrix vv(ntot,ntot6);
    Rcpp::NumericMatrix uvv(ntot,ntot);
    int n1 = 6*ntot-1;
    Rcpp::IntegerVector j0 = Rcpp::seq(0, n1);
    for (int ii=2*ntot; ii<3*ntot; ii++){
        j0[ii] = ii+ntot;
        j0[ii+ntot] = ii;
    }

    int ii = 0;
    for (int i=0; i<ntot; i++){
        aa(i,i) = 1;
        vv(i,i) = 1;
        ii = ntot+i;
        aa(i,ii) = 1;
        vv(i,ii) = 1;
        ii = ntot4+i;
        aa(i,ii) = 1;
        vv(i,ii) = 1;
    }

    double ntot0 = 1.0/( (double)ntot );
    for (int i=0; i<ntot; i++){
        for (int j=4*ntot; j<5*ntot; j++){
            aa(i,j) = aa(i,j) - ntot0;
        }
    }

    int jj=0;
    double pact1 = 0;
    double apar0 = 0;
    for (int i=0; i<ntot; i++){
        pact1 = -1/pact[inde(i,1)];
        apar0 = -1/apar[inde(i,0)];
        aa(i,i) = 1 + pact1;
        aa(i,(ntot+i)) = 1 + apar0;
        for (int j=0; j<ntot; j++){
            if ((inde(i,0)!=inde(j,0)) | (inde(i,1)!=inde(j,1))) {
                if ((inde(i,0)==inde(j,1)) & (inde(i,1)==inde(j,0))){
                    recip(inde(i,0),inde(i,1)) = 1;
                    aa(i,(ntot5+i)) = 1;
                    aa(i,(ntot5+j)) = -1;
                    vv(i,(ntot2+j)) = 1;
                    vv(i,(ntot3+j)) = 1;
                    vv(i,(ntot5+j)) = 1;
                }
                jj = j+ntot;
                if (inde(i,0)==inde(j,0)){
                    aa(i,jj) = apar0;
                    vv(i,j) = 1;
                }
                if (inde(i,1)==inde(j,1)){
                    vv(i,jj) = 1;
                    aa(i,j) = pact1;
                }
                if (inde(i,0)==inde(j,1)){
                    vv(i,(ntot2+j)) = 1;
                }
                if (inde(i,1)==inde(j,0)){
                    vv(i,(ntot3+j)) = 1;
                }
            }
        }
    }

    mirr = SRM_RCPP_ROWSUMS(recip);
    double mirr_i0a = 0;
    double mirr_i0b = 0;
    Rcpp::NumericVector mirr_j1(ntot);
    Rcpp::NumericVector mirr_j2(ntot);
    for (int j=0; j<ntot; j++){
        mirr_j1[j] = (mirr(inde(j,0))-1)/mirr(inde(j,0));
        mirr_j2[j] = -1/mirr(inde(j,0));
    }
    for (int i=0; i<ntot; i++){
        mirr_i0a = -1/mirr(inde(i,0));
        mirr_i0b = (mirr(inde(i,0))-1)/mirr(inde(i,0));
        for (int j=0; j<ntot; j++){
            if ((recip(inde(i,0),inde(i,1))!=0) & (recip(inde(j,0),inde(j,1))!=0)){
                if ((inde(i,0)==inde(j,1)) & (inde(i,1)==inde(j,0))){
                    aa(i,(ntot3+j)) = mirr_i0b;
                    aa(i,(ntot2+j)) = mirr_j1[j];
                }
                if ((inde(i,0)!=inde(j,1)) & (inde(i,1)==inde(j,0))){
                    aa(i,(ntot2+j)) = mirr_j2[j];
                }
                if ((inde(i,0)==inde(j,1)) & (inde(i,1)!=inde(j,0))){
                    aa(i,(ntot3+j)) = mirr_i0a;
                }
            }
        }
    }

    // uvv <- vv[,(2*ntot+1):(3*ntot)]+vv[,(3*ntot+1):(4*ntot)]
    for (int hh=0; hh<ntot; hh++){
        uvv(_,hh) = vv(_,hh+ntot2) + vv(_,hh+ntot3);
    }

    arma::mat ss = arma::zeros(6,3);
    arma::mat c = arma::zeros(6,6);

    Rcpp::NumericMatrix zz(ntot,ntot);
    Rcpp::NumericMatrix uu(ntot,ntot);
    Rcpp::NumericMatrix aa1(ntot,ntot);
    Rcpp::NumericMatrix vv1(ntot,ntot);
    int ttt=0;
    int uuu=0;
    for (int i=0; i<6; i++){
        for (int j=0; j<6; j++){
            ttt = i*ntot;
            uuu = j*ntot;
            for (int cc=0; cc<ntot; cc++){
                aa1(_,cc) = aa(_,cc+ttt);
                vv1(_,cc) = vv(_,cc+uuu);
            }
            if ((i<2) | (i>3)){
                c(i,j) = SRM_ARBSRM_TRACE_PRODUCT_MATRIX(aa1,vv1);
            }
            if ((i==2) | (i==3)){
                c(i,j) = SRM_ARBSRM_TRACE_PRODUCT_MATRIX_TRANSPOSE(aa1, vv1);
            }
        }
    }

    arma::mat cin = arma::pinv(c);
    double tmp1 = 0;
    for (int i=0; i<6; i++){
        ttt = i*ntot;
        for (int hh=0; hh<ntot; hh++){
            for (int kk=0; kk<ntot; kk++){
                tmp1 = aa(hh,kk+ttt);
                if (tmp1!=0){
                    // ss[i,1] <- t(dv[1:ntot])%*%aa[,dumi]%*%dv[1:ntot]
                    ss(i,0) += dv[hh]*tmp1*dv[kk];
                    if (bivariate){
                        // ss[i,2] <- t(dv[(ntot+1):(2*ntot)])%*%aa[,dumi]%*%dv[(ntot+1):(2*ntot)]
                        ss(i,1) += dv[hh+ntot]*tmp1*dv[kk+ntot];
                        // ss[i,3]<-t(dv[1:ntot])%*%aa[,dumi]%*%dv[(ntot+1):(2*ntot)]
                        ss(i,2) += dv[hh]*tmp1*dv[kk+ntot];
                    }
                }
            }
        }
    }

    //- point estimate
    arma::mat est = arma::mat(cin*ss);

    //--- OUTPUT:
    return Rcpp::List::create(
            Rcpp::Named("dv") = dv,
            Rcpp::Named("bata") = bata,
            Rcpp::Named("inde") = inde,
            Rcpp::Named("ac") = ac,
            Rcpp::Named("pa") = pa,
            Rcpp::Named("apar") = apar,
            Rcpp::Named("pact") = pact,
            Rcpp::Named("j0") = j0,
            Rcpp::Named("aa") = aa,
            Rcpp::Named("vv") = vv,
            Rcpp::Named("recip") = recip,
            Rcpp::Named("mirr") = mirr,
            Rcpp::Named("uvv") = uvv,
            Rcpp::Named("c") = c,
            Rcpp::Named("cin") = cin,
            Rcpp::Named("ss") = ss,
            Rcpp::Named("est") = est,
            Rcpp::Named("ntot") = ntot,
            Rcpp::Named("npar") = npar
        );
}
///********************************************************************

///********************************************************************
///** SRM_RCPP_MATRIX_TRACE_PRODUCT
// [[Rcpp::export]]
double SRM_RCPP_MATRIX_TRACE_PRODUCT(Rcpp::NumericMatrix x, Rcpp::NumericMatrix y)
{
    double tot=0;
    int n=x.nrow();
    int p=y.ncol();
    for (int nn=0; nn<n; nn++){
        for (int pp=0; pp<p; pp++){
            tot += x(nn,pp)*y(pp,nn);
        }
    }
    //--- OUTPUT:
    return tot;
}
///********************************************************************

///********************************************************************
///** SRM_RCPP_SRM_MATRIX_MULT_LOGICAL
// [[Rcpp::export]]
Rcpp::NumericMatrix SRM_RCPP_SRM_MATRIX_MULT_LOGICAL(Rcpp::NumericMatrix x, Rcpp::LogicalMatrix y)
{
    int xr = x.nrow();
    int xc = x.ncol();
    int yc = y.ncol();
    Rcpp::NumericMatrix z(xr,yc);

    for (int hh=0; hh<xc; hh++){
        for (int cc=0; cc<yc; cc++){
            if (y(hh,cc)){
                for (int rr=0; rr<xr; rr++){
                    z(rr,cc) += x(rr,hh);
                }
            }
        }
    }
    //--- OUTPUT:
    return z;
}
///********************************************************************


///********************************************************************
///** SRM_RCPP_SRM_ARBSRM_SE_CREATE_CWU
// [[Rcpp::export]]
Rcpp::NumericMatrix SRM_RCPP_SRM_ARBSRM_SE_CREATE_CWU(int NF)
{
    Rcpp::NumericMatrix cwu(6,6);
    Rcpp::NumericVector foo(6);
    int j = 0;
    int nh=0;
    for (int h=0; h<4; h++){
        if (h==0){
            // foo = {0,6,12,18,24,30};
            nh = 6;
            foo[0] = 0;
            foo[1] = 6;
            foo[2] = 12;
            foo[3] = 18;
            foo[4] = 24;
            foo[5] = 30;
        }
        if (h==1){
            // foo = {36,42,48,54,60};
            nh = 5;
            foo[0] = 36;
            foo[1] = 42;
            foo[2] = 48;
            foo[3] = 54;
            foo[4] = 60;
        }
        if (h==2){
            // foo = {66,72,78,84};
            nh = 4;
            foo[0] = 66;
            foo[1] = 72;
            foo[2] = 78;
            foo[3] = 84;
        }
        if (h==3){
            // foo = {90,96,102};
            nh = 3;
            foo[0] = 90;
            foo[1] = 96;
            foo[2] = 102;
        }
        j = h;
        for (int i=0; i<nh; i++){
            cwu(h,j) = foo[i];
            cwu(j,h) = foo[i];
            j++;
        }
    }
    cwu(4,4) = 108;
    cwu(5,4) = 114;
    cwu(4,5) = 114;
    cwu(5,5) = 120;

    //--- OUTPUT:
    return cwu;
}
///********************************************************************


// Rcpp::Rcout << "j1=" << j1 << " j2=" << j2 << " w1=" << w1 << std::endl;
