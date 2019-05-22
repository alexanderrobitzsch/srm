## File Name: SRM_MATRIX_TRIPLE_PRODUCT.R
## File Version: 0.05

SRM_MATRIX_TRIPLE_PRODUCT <- function(x,y)
{
    z <- x %*% tcrossprod(y,x)
    return(z)
}
