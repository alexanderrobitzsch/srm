## File Name: SRM_UPDATE_PARAMETER.R
## File Version: 0.04

SRM_UPDATE_PARAMETER <- function(parm_old, grad, hessian)
{
    eps <- 1e-8    
    infomat1 <- infomat <- SRM_GINV(hessian)    
    # stabilize information matrix 
    diag(infomat1) <- diag(infomat1) + eps
    incr <- infomat1 %*% grad
    incr <- incr[,1]
    parm_new <- parm_old - incr
    parm_change <- max(abs(incr))
    #-- output
    res <- list(infomat=infomat, parm_new=parm_new, incr=incr,
                    parm_change=parm_change)
    return(res)
}
