## File Name: SRM_LOG_DET.R
## File Version: 0.02

SRM_LOG_DET <- function(x)
{
    res <- determinant(x=x, logarithm=TRUE)$modulus
    attr(res, "logarithm") <- NULL
    return(res)
}
