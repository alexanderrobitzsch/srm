## File Name: SRM_COMPUTE_LOG_LIKELIHOOD_GRADIENT.R
## File Version: 0.384


SRM_COMPUTE_LOG_LIKELIHOOD_GRADIENT <- function(y, muy, ey, SIGMA_Y_inv,
        SIGMA_Y_der, MU_Y_der, der_in_mu=FALSE, use_rcpp=TRUE, t1b=NULL, calculate_full=TRUE,
        res_saved=NULL, der_bool=NULL, cov_resid=NULL)
{
vv0 <- Sys.time()
    if (calculate_full){
        if (is.null(der_bool)){
            use_rcpp <- FALSE
        }
        # w0 <- SIGMA_Y_inv %*% SIGMA_Y_der
        w0 <- SRM_RCPP_SRM_COMPUTE_LOG_LIKELIHOOD_GRADIENT_W2(
                sigma_y_inv=SIGMA_Y_inv, sigma_y_der=SIGMA_Y_der, der_bool=der_bool)
        # w0 <- SRM_RCPP_SRM_COMPUTE_LOG_LIKELIHOOD_GRADIENT_W1(
        #        sigma_y_inv=SIGMA_Y_inv, sigma_y_der=SIGMA_Y_der)
        t1 <- -0.5 * sum(diag(w0))
    } else {
        w0 <- res_saved$ll_matrix
        t1 <- res_saved$t1
    }

    if (is.null(t1b)){
        t1b <- SIGMA_Y_inv %*% ey
    }
    # t(ey) * w0 * SIGMA^-1 * ey, where w0 = SIGMA^-1 * (d SIGMA)/(d theta)
    t2 <- 0.5 * ( crossprod(ey, w0) %*% t1b )
    ll_grad_pos <- t1 + t2[1,1]

    #- add gradient for mean structure
    if (der_in_mu){
        grad_mu <- crossprod(ey, SIGMA_Y_inv) %*% MU_Y_der
        ll_grad_pos <- ll_grad_pos + grad_mu[1,1]
    }

    #--- output
    res <- list( ll_matrix = w0, ll_grad_pos = ll_grad_pos, t1b=t1b, t1=t1)
    return(res)
}

# cat("   +++  compute A") ; vv1 <- Sys.time(); print(vv1-vv0) ; vv0 <- vv1
