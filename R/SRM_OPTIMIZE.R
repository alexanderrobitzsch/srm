## File Name: SRM_OPTIMIZE.R
## File Version: 0.253

SRM_OPTIMIZE <- function(x, parm.table, parm_list, symm_matrices, npar,
    data_list, NOP, parm_table_free, use_rcpp=TRUE, shortcut=TRUE, 
    compute_gradient=TRUE, compute_hessian=FALSE, method="ml", inv_type="pinv")
{
    if (!compute_gradient){
        compute_hessian <- FALSE
    }
    #* include parameters in parameter table and parameter list
    parm.table <- SRM_INCLUDE_PARAMETERS_PARM_TABLE( parm=x, parm.table=parm.table)        
    parm_list <- SRM_INCLUDE_PARAMETERS_PARM_LIST( parm.table=parm.table, 
            parm_list=parm_list, symm_matrices=symm_matrices, include_fixed=FALSE ) 
    ll_new <- 0
    grad <- rep(0, npar)
    hessian <- matrix(0, nrow=npar, ncol=npar)    
    G <- length(data_list)

    for (gg in seq_len(G) ){
        data_list_gg <- data_list[[gg]]    
        calculate_gg <- (! shortcut ) | data_list_gg$calculate_gg
        
        # compute covariance matrix at individual level
        SIGMA_U <- SRM_COMPUTE_SEM_SIGMA( parm_list=parm_list, level="U", group=gg )
        # compute covariance matrix at dyad level
        SIGMA_D <- SRM_COMPUTE_SEM_SIGMA( parm_list=parm_list, level="D", group=gg )    

        nrr <- data_list_gg$nrr
            
        NI <- data_list_gg$NI
        ND <- data_list_gg$ND    

        null_list <- as.list( seq_len(NOP) )
        SIGMA_der <- null_list
        
        if (compute_gradient){
            for (nn in 1:NOP){
                parm_type <- parm_table_free[nn, "mat"]    
                parm_pos <- c( parm_table_free[nn,"row"], parm_table_free[nn,"col"])
                parm_level <- parm_table_free[nn, "level"]    
                SIGMA_der[[nn]] <- SRM_COMPUTE_SEM_SIGMA_GRADIENT( parm_list=parm_list, 
                        parm_type=parm_type, parm_pos=parm_pos, parm_level=parm_level, group=gg ) 
            }    
        }
        
        #* loop over round robin groups rr
        hess_list <- as.list( seq_len(npar) )
        mu_y_der_list <- as.list( seq_len(npar) )        
        mu_y_der_bool_list <- as.list( seq_len(npar) )        
        sigma_y_der_list <- as.list( seq_len(npar) )        
        Zis_rcpp <- Wds_rcpp <- NULL
        res_ll <- NULL
        
        SIGMA_Y_saved <- as.list( seq_len(nrr) )
        SIGMA_Y_der_saved <- null_list
        res_log_like_grad_saved <- null_list
        der_bool_saved <- null_list
                
        for (rr in seq_len(nrr) ){
            calculate_gg_rr <- calculate_gg[rr]
            y <- data_list_gg$y_rr[[rr]]
            Xs <- data_list_gg$Xs_rr[[rr]]
            Zis <- data_list_gg$Zis_rr[[rr]]        
            Wds <- data_list_gg$Wds_rr[[rr]]
            if (use_rcpp){
                Zis_rcpp <- data_list_gg$Zis_rr_rcpp[[rr]]
                Wds_rcpp <- data_list_gg$Wds_rr_rcpp[[rr]]
            }
            NI_rr <- NI[rr,2]
            ND_rr <- ND[rr,2]        
            NY <- length(y)
            hessian_rr <- matrix(0, nrow=npar, ncol=npar)
            M0 <- matrix( 0, nrow=NY, ncol=NY)        
            mu0 <- rep(0, NY)        
            for (pp in 1:npar){
                hess_list[[pp]] <- M0
                mu_y_der_list[[pp]] <- mu0
                mu_y_der_bool_list[[pp]] <- FALSE
                sigma_y_der_list[[pp]] <- M0
            }
            # compute SIGMA_Y
            if (calculate_gg_rr){    
                SIGMA_Y <- SRM_COMPUTE_SIGMA_Y( Zis=Zis, Wds=Wds, NI=NI_rr, ND=ND_rr, 
                                SIGMA_U=SIGMA_U, SIGMA_D=SIGMA_D, NY=NY, use_rcpp=use_rcpp, 
                                Zis_rcpp=Zis_rcpp, Wds_rcpp=Wds_rcpp ) 
                SIGMA_Y_saved[[rr]] <- SIGMA_Y
            } else {
                SIGMA_Y <- SIGMA_Y_saved[[rr]]
            }

            # compute muy and ey
            res <- SRM_COMPUTE_MU_Y(parm_list=parm_list, y=y, Xs=Xs, group=gg)        
            muy <- res$muy
            ey <- res$ey

            # compute log likelihood    
            args_eval <- list( y=y, muy=muy, ey=ey, SIGMA_Y=SIGMA_Y, use_rcpp=use_rcpp, 
                                    calculate_full=calculate_gg_rr, res_ll=res_ll,
                                    inv_type=inv_type)                 
            res_ll <- SRM_COMPUTE_OPTIM_FUNCTION_EVAL(args_eval=args_eval, method=method)                            
            SIGMA_Y <- res_ll$SIGMA_Y        
            SIGMA_Y_inv <- res_ll$SIGMA_Y_inv
            cov_resid <- res_ll$cov_resid
            ll_new <- ll_new + res_ll$ll            
            #-- compute gradient with respect to all parameters    
            if (compute_gradient){
                t1b <- NULL
                for (nn in 1:NOP){
                    index_nn <- parm_table_free[nn,"index"]        
                    #- derivative with respect to SIGMA_Y
                    if (calculate_gg_rr){
                        SIGMA_Y_der <- SRM_COMPUTE_SIGMA_Y_GRADIENT( SIGMA_der=SIGMA_der, 
                            parm_table_free=parm_table_free, nn=nn, rr=rr, Zis=Zis, Wds=Wds, NY=NY, NI=NI, ND=ND, 
                            use_rcpp=use_rcpp, Zis_rcpp=Zis_rcpp, Wds_rcpp=Wds_rcpp )            
                        SIGMA_Y_der_saved[[nn]] <- SIGMA_Y_der
                    } else {
                        SIGMA_Y_der <- SIGMA_Y_der_saved[[nn]]
                    }
                    sigma_y_der_list[[index_nn]] <- sigma_y_der_list[[index_nn]] + SIGMA_Y_der             
                    #- derivative with respect to mu
                    res <- SRM_COMPUTE_MU_Y_GRADIENT(y=y, Xs=Xs, parm_list=parm_list, 
                                    parm_table_free=parm_table_free, nn=nn, group=gg)
                    der_in_mu <- res$der_in_mu
                    MU_Y_der <- res$MU_Y_der

                    #-- non-zero gradient in SIGMA_Y_der
                    if (calculate_gg_rr & use_rcpp){
                        der_bool <- SRM_RCPP_SRM_COMPUTE_NONZERO_GRADIENT( sigma_y_der=SIGMA_Y_der, eps=1e-12)
                        der_bool_saved[[nn]] <- der_bool
                    } else {
                        der_bool <- der_bool_saved[[nn]]
                    }

                    #-- derivative log-likelihood and Hessian
                    args_grad <- list( y=y, muy=muy, ey=ey, 
                                SIGMA_Y_inv=SIGMA_Y_inv, SIGMA_Y_der=SIGMA_Y_der, 
                                MU_Y_der=MU_Y_der, der_in_mu=der_in_mu, use_rcpp=use_rcpp, t1b=t1b, 
                                calculate_full=calculate_gg_rr, res_saved=res_log_like_grad_saved[[nn]], 
                                der_bool=der_bool, cov_resid=cov_resid)
                    res <- SRM_COMPUTE_OPTIM_FUNCTION_GRAD(args_grad=args_grad, method=method)                    
                    res_log_like_grad_saved[[nn]] <- res                            
                    t1b <- res$t1b                            
                    grad[index_nn] <- grad[index_nn] + res$ll_grad_pos
                    if (compute_hessian){
                        hess_list[[index_nn]] <- hess_list[[index_nn]] + res$ll_matrix
                    }
                    mu_y_der_list[[index_nn]] <- mu_y_der_list[[index_nn]] + MU_Y_der
                    mu_y_der_bool_list[[index_nn]] <- mu_y_der_bool_list[[index_nn]] | der_in_mu
                }  # end nn ... free parameters
            } # end compute_gradient
            #-- compute Hessian
            if (compute_hessian){
                calc_hess_rr <- calculate_gg_rr
                if (calc_hess_rr){
                    # compute Hessian for round robin group rr
                    hessian_rr <- SRM_COMPUTE_HESSIAN_RR(hess_list=hess_list, mu_y_der_list=mu_y_der_list, 
                                    mu_y_der_bool_list=mu_y_der_bool_list, SIGMA_Y_inv=SIGMA_Y_inv, 
                                    npar=npar, use_rcpp=use_rcpp)
                    hessian_rr_temp <- hessian_rr    
                } else {
                    hessian_rr <- hessian_rr_temp
                }
                # add to total Hessian
                hessian <- hessian + hessian_rr
            }  # end compute_hessian            
        } # end rr    
    } ## end gg
    #--- output
    if (! compute_hessian){
        hessian <- NULL
    }
    if (! compute_gradient){
        grad <- NULL
    }    
    res <- list(ll_new=ll_new, grad=grad, expected_infomat=hessian, parm.table=parm.table,
                parm_list=parm_list, G=G )
    return(res)
}
