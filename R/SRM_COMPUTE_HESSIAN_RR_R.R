## File Name: SRM_COMPUTE_HESSIAN_RR_R.R
## File Version: 0.283

SRM_COMPUTE_HESSIAN_RR_R <- function(hess_list, mu_y_der_list, mu_y_der_bool_list,
        SIGMA_Y_inv, npar)
{
    hessian_rr <- matrix(0, nrow=npar, ncol=npar)
    for (ii in 1:npar){
        if (mu_y_der_bool_list[[ii]]){
            mu_ii <- crossprod(mu_y_der_list[[ii]], SIGMA_Y_inv)
        }
        for (jj in ii:npar){
            hessian_rr[ii,jj] <- .5*sum( hess_list[[ii]] * t(hess_list[[jj]]) )
            # hessian_rr[ii,jj] <- .5*sum( diag( hess_list[[ii]] %*% hess_list[[jj]] ) )
            if (mu_y_der_bool_list[[ii]] & mu_y_der_bool_list[[jj]]){
                # contribution is of the form a'Va
                mu_contrib <- mu_ii %*% mu_y_der_list[[jj]]
                hessian_rr[ii,jj] <- hessian_rr[ii,jj] + as.numeric(mu_contrib)
            }
            hessian_rr[jj,ii] <- hessian_rr[ii,jj]
        }
    }
    return(-hessian_rr)
}
