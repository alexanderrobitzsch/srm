## File Name: SRM_DMVNORM.R
## File Version: 0.07

SRM_DMVNORM <- function(x, mean, sigma, log=FALSE, sigma_inv=NULL,
        log_det_sigma=NULL)
{
    if (is.null(sigma_inv)){
        sigma_inv <- SRM_GINV(x=sigma)
    }
    ex <- x - mean
    p <- length(x)
    if ( is.null(log_det_sigma) ){
        log_det_sigma <- log(det(sigma))
    }
    ll1 <- -0.5*p*log(2*pi) - 0.5*log_det_sigma
    ll <- ll1 - 0.5*crossprod(ex, sigma_inv) %*% ex
    ll <- ll[1,1]
    return(ll)
}

