## File Name: SRM_TRACE_PRODUCT.R
## File Version: 0.14

SRM_TRACE_PRODUCT <- function(x, y, use_rcpp=TRUE)
{
    if ( ! use_rcpp ){
        z <- sum(x*t(y))
    } else {
        z <- SRM_RCPP_MATRIX_TRACE_PRODUCT(x=x, y=y)
    }
    return(z)
}
