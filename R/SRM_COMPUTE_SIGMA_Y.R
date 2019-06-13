## File Name: SRM_COMPUTE_SIGMA_Y.R
## File Version: 0.222

SRM_COMPUTE_SIGMA_Y <- function(Zis, Wds, NI, ND, SIGMA_U,
        SIGMA_D, NY, use_rcpp=TRUE, Zis_rcpp=NULL, Wds_rcpp=NULL, Z_ind=NULL )
{
    SIGMA_Y0 <- matrix(0 , nrow=NY, ncol=NY)
    if (use_rcpp){
        Zis <- Zis_rcpp
        Wds <- Wds_rcpp
    }

    SIGMA_Y <- SRM_ADD_SIGMA_PARTS(SIGMA_Y=SIGMA_Y0, SIGMA_U=SIGMA_U, Z=Zis,
                    NI=NI, use_rcpp=use_rcpp )
    SIGMA_Y <- SRM_ADD_SIGMA_PARTS(SIGMA_Y=SIGMA_Y, SIGMA_U=SIGMA_D, Z=Wds, NI=ND,
                        use_rcpp=use_rcpp)
    return(SIGMA_Y)
}
