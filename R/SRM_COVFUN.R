## File Name: SRM_COVFUN.R
## File Version: 0.05

SRM_COVFUN <- function(LAM, B, PHI, PSI, IB)
{
    W <- SRM_GINV(IB - B)
    Wt <- t(W)
    val <- LAM %*% W %*% PHI %*% Wt %*% t(LAM)
    return(val)
}
