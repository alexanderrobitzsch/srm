## File Name: SRM_COMPUTE_SEM_SIGMA.R
## File Version: 0.07


# compute sigma in a SEM model
SRM_COMPUTE_SEM_SIGMA <- function( parm_list, level, group=1 )
{
    #-- collect arguments
    parm_list1 <- parm_list[[ paste0("parm_list_", level) ]][[group]]
    LAM <- parm_list1[[ paste0("LAM_", level) ]]
    PHI <- parm_list1[[ paste0("PHI_", level) ]]
    PSI <- parm_list1[[ paste0("PSI_", level) ]]
    B <- parm_list1[[ paste0("G_", level) ]]
    #-- computations
    if ( ! is.null(B) ){
        IB <- diag(1, nrow(B) )
        W <- SRM_GINV( IB - B )
        LAM_W <- LAM %*% W
    } else {
        LAM_W <- LAM
    }
    SIGMA <- LAM_W %*% PHI %*% t(LAM_W) + PSI
    return(SIGMA)
}
