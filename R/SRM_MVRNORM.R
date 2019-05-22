## File Name: SRM_MVRNORM.R
## File Version: 0.02

SRM_MVRNORM <- function(n, mu, Sigma)
{        
    svd1 <- svd(Sigma)    
    L <- svd1$u %*% diag(sqrt(svd1$d))
    ny <- ncol(Sigma)
    r <- matrix( stats::rnorm(n*ny), nrow=n, ncol=ny)
    muM <- matrix(mu, nrow=n, ncol=ny, byrow=TRUE)
    y <- muM + tcrossprod(r, L)
    return(y)    
}
