## File Name: SRM_COVFUN.R
## File Version: 0.06

SRM_COVFUN <- function(LAM, B, PHI, PSI, IB)
{
    W <- solve(IB - B)
    Wt <- t(W)
    val <- LAM %*% W %*% PHI %*% Wt %*% t(LAM)
    return(val)
}
