## File Name: SRM_COMPUTE_ULS_GRADIENT.R
## File Version: 0.03


SRM_COMPUTE_ULS_GRADIENT <- function(args_grad)
{
    MU_Y_der <- args_grad$MU_Y_der
    ey <- args_grad$ey        
    SIGMA_Y_der <- args_grad$SIGMA_Y_der
    cov_resid <- args_grad$cov_resid            
    use_rcpp <- args_grad$use_rcpp
    # f = ( y - p)^2
    # df/dt = -2*(y-p)*dp/dt

    # gradient part due to mu
    ny <- length(ey)
    gr1 <- -2*sum(ey*MU_Y_der) / ny
    # covariance part
    if (! use_rcpp){
        gr2 <- -2*sum(cov_resid*SIGMA_Y_der)
    } else {
        der_bool <- args_grad$der_bool    
        gr2 <- SRM_RCPP_SRM_ULS_GRADIENT_SIGMA_PART(cov_resid=cov_resid, 
                    SIGMA_Y_der=SIGMA_Y_der, der_bool=der_bool)
    }
    ll_grad_pos <- -gr1 - gr2/ny
    #-- output
    res <- list(ll_grad_pos=ll_grad_pos)
    return(res)
}
