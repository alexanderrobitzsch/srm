## File Name: SRM_ADD_SIGMA_PARTS.R
## File Version: 0.08


#--- construct 'large' covariance matrices
SRM_ADD_SIGMA_PARTS <- function(SIGMA_Y, SIGMA_U, Z, NI, use_rcpp=FALSE)
{ 
    #** implementation with design matrices
    if ( ! use_rcpp){
        for (ii in 1:NI){
            Zis_ii <- Z[[ii]]
            SIGMA_Y <- SIGMA_Y + Zis_ii %*% tcrossprod( SIGMA_U, Zis_ii )
        }
    }
    #** implemention with insertion operation
    if ( use_rcpp ){
        SIGMA_Y <- SRM_RCPP_SRM_INSERT_ELEMENTS( sigma_y0=SIGMA_Y, 
                        Zis=Z, sigma_u=SIGMA_U )
    }    
    return(SIGMA_Y)
}
