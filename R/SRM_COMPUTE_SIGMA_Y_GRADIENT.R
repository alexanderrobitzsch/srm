## File Name: SRM_COMPUTE_SIGMA_Y_GRADIENT.R
## File Version: 0.04

SRM_COMPUTE_SIGMA_Y_GRADIENT <- function(SIGMA_der, parm_table_free, nn, rr, 
    Zis, Wds, NY, NI, ND, use_rcpp=TRUE, Zis_rcpp=NULL, Wds_rcpp=NULL)
{
    if (use_rcpp){
        Zis <- Zis_rcpp
        Wds <- Wds_rcpp
    }
    SIGMA_Y0 <- matrix(0 , nrow=NY, ncol=NY)
    parm_level <- parm_table_free[nn, "level"]
    if (parm_level=="U"){
        NI1 <- NI[rr,2]
        Z1 <- Zis
    }
    if (parm_level=="D"){
        NI1 <- ND[rr,2]
        Z1 <- Wds
    }    
    res <- SRM_ADD_SIGMA_PARTS(SIGMA_Y=SIGMA_Y0, SIGMA_U=SIGMA_der[[nn]], 
                    Z=Z1, NI=NI1, use_rcpp=use_rcpp )    
    return(res)
}
