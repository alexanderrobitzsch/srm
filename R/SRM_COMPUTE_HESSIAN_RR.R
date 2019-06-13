## File Name: SRM_COMPUTE_HESSIAN_RR.R
## File Version: 0.43


SRM_COMPUTE_HESSIAN_RR <- function(hess_list, mu_y_der_list, mu_y_der_bool_list,
        SIGMA_Y_inv, npar, use_rcpp)
{
    args <- list(hess_list=hess_list, mu_y_der_list=mu_y_der_list,
                mu_y_der_bool_list=mu_y_der_bool_list, SIGMA_Y_inv=SIGMA_Y_inv,
                npar=npar)
    if (!use_rcpp){
        hessian_fct <- SRM_COMPUTE_HESSIAN_RR_R
    } else {
        hessian_fct <- SRM_RCPP_SRM_COMPUTE_HESSIAN_RR
    }
    hessian_rr <- do.call(what=hessian_fct, args=args)
    return(hessian_rr)
}
