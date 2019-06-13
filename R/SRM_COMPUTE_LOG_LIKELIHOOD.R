## File Name: SRM_COMPUTE_LOG_LIKELIHOOD.R
## File Version: 0.479

SRM_COMPUTE_LOG_LIKELIHOOD <- function(y, muy, ey, SIGMA_Y, use_rcpp=TRUE,
        calculate_full=TRUE, res_ll=NULL, inv_type="pinv",
        Zis_rcpp=NULL, Wds_rcpp=NULL, SIGMA_U=NULL, SIGMA_D=NULL, Z_ind=NULL,
        NI=NULL, ND=NULL, use_woodbury=FALSE)
{
    if (calculate_full){
        if (use_woodbury){  # use Woodbury identity
            NY <- length(y)
            SIGMA_Y0 <- matrix(0 , nrow=NY, ncol=NY)
            SIGMA_D_inv <- SRM_PINV(x=SIGMA_D)$inv
            A_inv <- SRM_ADD_SIGMA_PARTS(SIGMA_Y=SIGMA_Y0, SIGMA_U=SIGMA_D_inv, Z=Wds_rcpp, NI=ND,
                                use_rcpp=use_rcpp)
            #-- use block-diagonal form of SIGMA_U
            SIGMA_U0 <- SRM_PINV(x=SIGMA_U)$inv
            n1 <- ncol(SIGMA_U)
            NZ <- NI*n1
            Phi_inv <- SRM_RCPP_SIGMA_Y_INV_WOODBURY_PHI_INV( SIGMA_U_INV=SIGMA_U0, NI=NI )
            Tmat <- SRM_RCPP_SIGMA_Y_INV_WOODBURY_TMAT( A_inv=A_inv, Z_ind=Z_ind, Phi_inv=Phi_inv )
            Tmat_inv <- SRM_PINV(x=Tmat)$inv
            # ZA <- t(Z) %*% A_inv
            ZA <- SRM_RCPP_SIGMA_Y_INV_WOODBURY_ZA( Z_ind=Z_ind, A_inv=A_inv, NZ=NZ)
            # SIGMA_Y_inv <- SRM_RCPP_SIGMA_Y_INV_WOODBURY_Y_INV(ZA=ZA, T_inv=Tmat_inv, A_inv=A_inv)
            # SIGMA_Y_inv <- A_inv - t(ZA) %*% Tmat_inv %*% ZA
            SIGMA_Y_inv <- A_inv - crossprod(ZA, Tmat_inv) %*% ZA
            # SIGMA_Y_det <- det(A)*det(SIGMA_U)^NI*det(Tmat)
            if (ncol(A_inv)==ND*ncol(SIGMA_D)){
                log_det_A <- SRM_LOG_DET(x=SIGMA_D)*ND
            } else {
                A <- SRM_ADD_SIGMA_PARTS(SIGMA_Y=SIGMA_Y0, SIGMA_U=SIGMA_D, Z=Wds_rcpp,
                                NI=ND, use_rcpp=use_rcpp)
                log_det_A <- SRM_LOG_DET(x=A)
            }
            log_det_sigma <- log_det_A + NI*SRM_LOG_DET(x=SIGMA_U) + SRM_LOG_DET(x=Tmat)
            res <- list(inv=SIGMA_Y_inv, log_det=log_det_sigma)
        } else {
            if (inv_type=="ginv"){
                res <- SRM_GINV(x=SIGMA_Y, output_det=TRUE)
            }
            if (inv_type=="pinv"){
                res <- SRM_PINV(x=SIGMA_Y, output_det=TRUE)
            }
        }
        SIGMA_Y_inv <- res$inv
        log_det_sigma <- res$log_det
    } else {
        SIGMA_Y <- res_ll$SIGMA_Y
        SIGMA_Y_inv <- res_ll$SIGMA_Y_inv
        log_det_sigma <- res_ll$log_det_sigma
    }

    #--- compute likelihood
    ll <- SRM_DMVNORM(x=y, mean=muy, sigma=SIGMA_Y, log=TRUE, sigma_inv=SIGMA_Y_inv,
                log_det_sigma=log_det_sigma)
    #--- output
    res <- list(SIGMA_Y=SIGMA_Y, SIGMA_Y_inv=SIGMA_Y_inv, ll=ll,
                    log_det_sigma=log_det_sigma, cov_resid=NULL)
    return(res)
}

# cat("   ###  compute A") ; vv1 <- Sys.time(); print(vv1-vv0) ; vv0 <- vv1
