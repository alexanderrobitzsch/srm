## File Name: SRM_PINV.R
## File Version: 0.07

SRM_PINV <- function(x, output_det=FALSE)
{
    is_ginv <- FALSE
    e1 <- try( chol2inv(chol(x)), silent=TRUE)
    if (class(e1)=="try-error"){
        e1 <- try( solve(x), silent=TRUE)
    }
    if (class(e1)=="try-error"){
        res <- SRM_GINV(x=x, output_det=output_det)
        is_ginv <- TRUE
        output_det <- FALSE
    }
    if (! is_ginv){
        res <- list(inv=e1)
    }
    if (output_det){
        res$log_det <- log(det(x))
    }
    return(res)
}
