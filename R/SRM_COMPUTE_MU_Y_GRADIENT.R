## File Name: SRM_COMPUTE_MU_Y_GRADIENT.R
## File Version: 0.05


SRM_COMPUTE_MU_Y_GRADIENT <- function(y, Xs, parm_list, parm_table_free, nn, group)
{
    parm_table_free_nn <- parm_table_free[nn, ]
    parm_list_gg <- parm_list[[group]]
    BETA <- parm_list[[group]]

    der_in_mu <- FALSE
    MU_Y_der <- rep(0, length(y))

    if (parm_table_free_nn$mat == "BETA"){
        BETA_row <- parm_table_free_nn$row
        MU_Y_der <- MU_Y_der + as.vector(Xs[, BETA_row])
        der_in_mu <- TRUE
    }

    #---
    res <- list( der_in_mu=der_in_mu, MU_Y_der=MU_Y_der)
    return(res)
}
