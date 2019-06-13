//// File Name: SRM_RCPP_SRM_MAKE_DATA_LIST.cpp
//// File Version: 0.243



// [[Rcpp::depends(RcppArmadillo)]]

// #include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// [[RcppNOinterfaces(r, cpp)]]


///********************************************************************
/// copied from https://stackoverflow.com/questions/21609934/ordering-permutation-in-rcpp-i-e-baseorder/26977061
///** SRM_RCPP_ORDER
// [[Rcpp::export]]
Rcpp::IntegerVector SRM_RCPP_ORDER(Rcpp::NumericVector x)
{
    Rcpp::NumericVector sorted = Rcpp::clone(x).sort();
    Rcpp::IntegerVector matched = Rcpp::match(sorted, x);
    return matched;
}

///********************************************************************
///** SRM_RCPP_SRM_MAKE_DATA_MATRIX_PERSON_ONE_PERSON
// [[Rcpp::export]]
Rcpp::NumericMatrix SRM_RCPP_SRM_MAKE_DATA_MATRIX_PERSON_ONE_PERSON( Rcpp::NumericMatrix tmp_data3,
    int no_person, int no_vars, int rr, int person, int pid)
{
    int N=no_person*no_vars*2;
    int NZ=tmp_data3.nrow();
    Rcpp::NumericMatrix dfr(N,4);
    int index=0;
    int var=0;

    int hh=0;
    for (int m=1; m<no_vars+1; m++){
        for (int zz=0; zz<NZ; zz++){
            var = tmp_data3(zz,2);
            if (tmp_data3(zz,0)==person){
                if (var==m){
                    dfr(hh,0) = rr;
                    dfr(hh,1) = pid;
                    dfr(hh,2) = zz+1;
                    dfr(hh,3) = m;
                    hh ++;
                }
            }
        }
    }
    for (int m=1; m<no_vars+1; m++){
        for (int zz=0; zz<NZ; zz++){
            var = tmp_data3(zz,2);
            if (tmp_data3(zz,1)==person){
                if (var==m){
                    dfr(hh,0) = rr;
                    dfr(hh,1) = pid;
                    dfr(hh,2) = zz+1;
                    dfr(hh,3) = m+no_vars;
                    hh ++;
                }
            }
        }
    }


    index = hh-1;
    dfr = dfr( Rcpp::Range(0,index), Rcpp::Range(0,3) );

    //--- OUTPUT
    return dfr;
}
///********************************************************************

///********************************************************************
///** SRM_RCPP_SRM_MAKE_DATA_MATRIX_PERSON
// [[Rcpp::export]]
Rcpp::NumericMatrix SRM_RCPP_SRM_MAKE_DATA_MATRIX_PERSON( Rcpp::NumericMatrix tmp_data3,
    int no_person, int no_vars, int rr, Rcpp::IntegerVector persons)
{
    int N=no_person*no_person*no_vars*2;
    int nc=4;
    Rcpp::NumericMatrix out(N,nc);
    int index=0;
    int pid=0;
    int n1=0;
    for (int pp=0; pp<no_person; pp++){
        pid=pp+1;
        Rcpp::NumericMatrix res0 = SRM_RCPP_SRM_MAKE_DATA_MATRIX_PERSON_ONE_PERSON(
                    tmp_data3, no_person, no_vars, rr, persons[pp], pid);
        n1 = res0.nrow();
        for (int nn=0; nn<n1; nn++){
            out(index+nn,_) = res0(nn,_);
        }
        index += n1;
    }
    out = out( Rcpp::Range(0, index-1), Rcpp::Range(0,nc-1));

    //--- OUTPUT
    return out;
}
///********************************************************************

///********************************************************************
///** SRM_RCPP_SRM_MAKE_DATA_MATRIX_DYAD_ONE_DYAD
// [[Rcpp::export]]
Rcpp::NumericMatrix SRM_RCPP_SRM_MAKE_DATA_MATRIX_DYAD_ONE_DYAD( Rcpp::NumericMatrix tmp_data3,
    int no_vars, int rr, int dyad, int did)
{
    int N=no_vars*2;
    int NZ=tmp_data3.nrow();
    Rcpp::NumericMatrix dfr(N,4);
    int index=0;
    int var=0;
    int fac=0;

    for (int zz=0; zz<NZ; zz++){
        if (tmp_data3(zz,0)==dyad){
            dfr(index,0) = rr;
            dfr(index,1) = did;
            var=tmp_data3(zz,2);
            dfr(index,2) = zz+1;
            if (tmp_data3(zz,1)==1){ fac = 1; }
            if (tmp_data3(zz,1)==2){ fac = 2; }
            dfr(index,3) = 2*(var-1)+fac;
            index++;
        }
    }

    dfr = dfr( Rcpp::Range(0, index-1), Rcpp::Range(0,3));

    //--- OUTPUT
    return dfr;
}
///********************************************************************

///********************************************************************
///** SRM_RCPP_SRM_MAKE_DATA_MATRIX_DYAD
// [[Rcpp::export]]
Rcpp::NumericMatrix SRM_RCPP_SRM_MAKE_DATA_MATRIX_DYAD( Rcpp::NumericMatrix tmp_data3,
    int no_vars, int rr, int no_dyads, Rcpp::NumericVector dyads)
{
    int N=no_dyads*no_vars*2;
    int nc=4;
    Rcpp::NumericMatrix out(N,nc);
    int index=0;
    int did=0;
    int n1=0;
    for (int dd=0; dd<no_dyads; dd++){
        did = dd+1;
        Rcpp::NumericMatrix res0 =SRM_RCPP_SRM_MAKE_DATA_MATRIX_DYAD_ONE_DYAD( tmp_data3,
                        no_vars, rr, dyads[dd], did);
        n1 = res0.nrow();
        for (int nn=0; nn<n1; nn++){
            out(index+nn,_) = res0(nn,_);
        }
        index += n1;
    }
    out = out( Rcpp::Range(0, index-1), Rcpp::Range(0,nc-1));
    //--- OUTPUT
    return out;
}
///********************************************************************
