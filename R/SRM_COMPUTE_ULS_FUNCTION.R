## File Name: SRM_COMPUTE_ULS_FUNCTION.R
## File Version: 0.11


SRM_COMPUTE_ULS_FUNCTION <- function(args_eval, inv_type=NULL)
{
    ey <- args_eval$ey   # y - mu_y
    SIGMA_Y <- args_eval$SIGMA_Y
    muy <- args_eval$muy
    ny <- length(ey)
    f1 <- sum(ey^2) / ny
    
    # cross-product matrix
    cpm <- ey * matrix(ey, nrow=ny, ncol=ny, byrow=TRUE)
    cov_resid <- cpm - SIGMA_Y  # observed minus expected
    # least squares function for covariance
    f2 <- sum(cov_resid^2) / ny
    ll <- f1 + f2
    #-- output
    res <- list(ll=-ll, cov_resid=cov_resid)
    return(res)
}
