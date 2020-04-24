## File Name: SRM_COMPUTE_SEM_SIGMA.R
## File Version: 0.12


# compute sigma in a SEM model
SRM_COMPUTE_SEM_SIGMA <- function( parm_list, level, group=1 )
{
    #-- collect arguments
    parm_list1 <- parm_list[[ paste0("parm_list_", level) ]][[group]]
    LAM <- parm_list1[[ paste0("LAM_", level) ]]
    PHI <- parm_list1[[ paste0("PHI_", level) ]]
    PSI <- parm_list1[[ paste0("PSI_", level) ]]
    B <- parm_list1[[ paste0("B_", level) ]]
    #-- computations
    SIGMA <- SRM_COMPUTE_SEM_SIGMA_COMPUTATION(B=B, LAM=LAM, PHI=PHI, PSI=PSI)    
    return(SIGMA)
}
