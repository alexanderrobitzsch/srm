## File Name: SRM_OPTIMIZER_INTERNAL_COMPUTE_INCREMENT.R
## File Version: 0.04

SRM_OPTIMIZER_INTERNAL_COMPUTE_INCREMENT <- function(grad, infomat,
    diagonal=FALSE)
{
    if (!diagonal){
        I0 <- SRM_GINV(x=infomat)
        incr <- as.vector(I0 %*% grad)
    } else {
        incr <- grad / diag(infomat)
    }
    return(incr)
}
