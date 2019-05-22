## File Name: SRM_GINV.R
## File Version: 0.08


SRM_GINV <- function(x, eps=1e-14, output_det=FALSE)
{
    sx <- svd(x=x)
    p <- ncol(x)
    ind <- which( sx$d > eps )
    dx <- sx$d[ind]
    ux <- sx$u[1:p, ind, drop=FALSE]
    dx1 <- 1/dx
    w <- ux %*% diag(dx1) %*% t(ux)        
    if (output_det){
        log_det <- -sum(log(dx1) )
        w <- list( inv=w, log_det=log_det )
    }        
    return(w)
}
