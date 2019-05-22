## File Name: SRM_OUTER.R
## File Version: 0.03

SRM_OUTER <- function(x,y)
{
    n <- length(x)
    m <- length(y)
    XX <- matrix(x, nrow=n, ncol=m, byrow=FALSE)
    YY <- matrix(y, nrow=n, ncol=m, byrow=TRUE)
    z <- XX*YY
    return(z)
}
