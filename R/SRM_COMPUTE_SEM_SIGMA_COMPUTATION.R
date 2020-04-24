## File Name: SRM_COMPUTE_SEM_SIGMA_COMPUTATION.R
## File Version: 0.05


SRM_COMPUTE_SEM_SIGMA_COMPUTATION <- function(B, LAM, PHI, PSI)
{
    if ( ! is.null(B) ){
        IB <- diag(1, nrow(B) )
        W <- solve( IB - B )
        LAM_W <- LAM %*% W
    } else {
        LAM_W <- LAM
    }
    SIGMA <- LAM_W %*% PHI %*% t(LAM_W) + PSI
    return(SIGMA)
}
