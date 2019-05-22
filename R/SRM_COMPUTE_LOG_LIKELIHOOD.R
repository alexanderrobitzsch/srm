## File Name: SRM_COMPUTE_LOG_LIKELIHOOD.R
## File Version: 0.44

SRM_COMPUTE_LOG_LIKELIHOOD <- function(y, muy, ey, SIGMA_Y, use_rcpp=TRUE, 
        calculate_full=TRUE, res_ll=NULL, inv_type="pinv")
{    
    if (calculate_full){
        # generalized inverse
        if (inv_type=="ginv"){
            res <- SRM_GINV(x=SIGMA_Y, output_det=TRUE)
        }        
        if (inv_type=="pinv"){
            res <- SRM_PINV(x=SIGMA_Y, output_det=TRUE)
        }
        SIGMA_Y_inv <- res$inv
        log_det_sigma <- res$log_det
    } else {
        SIGMA_Y <- res_ll$SIGMA_Y
        SIGMA_Y_inv <- res_ll$SIGMA_Y_inv
        log_det_sigma <- res_ll$log_det_sigma
    }
    
    #--- compute likelihood
    ll <- SRM_DMVNORM(x=y, mean=muy, sigma=SIGMA_Y, log=TRUE, sigma_inv=SIGMA_Y_inv,
                log_det_sigma=log_det_sigma)
        
    #--- output
    res <- list(SIGMA_Y=SIGMA_Y, SIGMA_Y_inv=SIGMA_Y_inv, ll=ll, 
                    log_det_sigma=log_det_sigma, cov_resid=NULL)
    return(res)
}
