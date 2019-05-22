## File Name: SRM_ARBSRM_ONE_GROUP_SE.R
## File Version: 0.06

SRM_ARBSRM_ONE_GROUP_SE <- function(vv, aa, uvv, ntot, j0, cin, est, bivariate=TRUE)
{
    #- se part 1
    res <- SRM_ARBSRM_ONE_GROUP_SE_PART1( vv=vv, aa=aa, uvv=uvv, ntot=ntot, 
                j0=j0, bivariate=bivariate )
    w <- res$w
    uw <- res$uw
    #- se part 2
    res <- SRM_ARBSRM_ONE_GROUP_SE_PART2( cin=cin, w=w, uw=uw, est=est )
    #--- output
    return(res)
}
