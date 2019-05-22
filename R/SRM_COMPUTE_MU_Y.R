## File Name: SRM_COMPUTE_MU_Y.R
## File Version: 0.02

SRM_COMPUTE_MU_Y <- function(parm_list, y, Xs, group)
{
    parm_list_gg <- parm_list[[group]][[1]]
    BETA <- parm_list_gg$BETA

    # include structure for latent means
    muy <- as.vector(Xs %*% BETA)
    ey <- y - muy
    
    #-- output
    res <- list(muy=muy, ey=ey)
    return(res)
}
