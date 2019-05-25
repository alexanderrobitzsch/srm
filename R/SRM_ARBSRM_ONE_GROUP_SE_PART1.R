## File Name: SRM_ARBSRM_ONE_GROUP_SE_PART1.R
## File Version: 0.12


SRM_ARBSRM_ONE_GROUP_SE_PART1 <- function(vv, aa, uvv, ntot, j0, bivariate=TRUE)
{
    w <- matrix(0, nrow=6, ncol=276)
    uw <- matrix(0, nrow=6, ncol=126)
    cwu <- SRM_RCPP_SRM_ARBSRM_SE_CREATE_CWU(NF=6)
    v1 <- as.list(1:6)
    garmat1_list <- list(v1, v1, v1, v1, v1, v1) 
    garmat2_list <- garmat1_list
    garmat3_list <- garmat1_list
    tmp13_list <- tmp23_list <- as.list(1:6)
    vvv <- vv == 1
    vv_list <- list()
    dum_list <- list()
    for (i1 in 1:6){
        dum_list[[i1]] <- (ntot*(i1-1)+1):(i1*ntot)
        vv_list[[i1]] <- vv[, dum_list[[i1]] ]
    }

    for (i1 in 1:6){
        dumi1 <- dum_list[[i1]]
        aa1 <- aa[,dumi1]
        aa2 <- aa[,j0[dumi1]]
        tmp23_list[[i1]] <- aa1 %*% uvv
        tmp13_list[[i1]] <- aa2 %*% uvv
        germat1_wide <- SRM_RCPP_SRM_MATRIX_MULT_LOGICAL( x=aa1, y=vvv )
        if (bivariate){
            germat2_wide <- SRM_RCPP_SRM_MATRIX_MULT_LOGICAL( x=aa2, y=vvv )
        } else {
            germat2_wide <- germat1_wide
        }
        for (j1 in 1:6){
            dumj1 <- dum_list[[j1]]
            vvv1 <- vvv[,dumj1]
            vvv2 <- vvv[,j0[dumj1]]
            garmat1_list[[i1]][[j1]] <- germat1_wide[,j0[dumj1]]
            garmat2_list[[i1]][[j1]] <- germat2_wide[,dumj1]
            garmat3_list[[i1]][[j1]] <- germat1_wide[,dumj1]
        }
    }
    garmat1_trace <- array(NA, dim=rep(6,4))
    garmat2_trace <- garmat1_trace
    garmat3_trace <- garmat1_trace
    garmat23_trace <- garmat1_trace
    
    # Here comes the time-consuming computation
    for (i1 in 1:6){ 
        dumi1 <- dum_list[[i1]]
        w1=0
        for (j1 in 1:6){
            startw <- w1
            dumj1 <- dum_list[[j1]]
            garmat1 <- garmat1_list[[i1]][[j1]]    
            garmat2 <- garmat2_list[[i1]][[j1]]
            garmat3 <- garmat3_list[[i1]][[j1]]
            for (i2 in 1:6){
                dumi2 <- dum_list[[i2]]
                w1 <- startw+i2-6
                for (j2 in j1:6){
                    compute <- TRUE
                    w1 <- w1+6
                    dumj2 <- dum_list[[j2]]
                    cons=.5
                    if (j2!=j1){ cons=1 }
                    #--- garmat1
                    if (i2<=i1){
                        tmp <- garmat1_trace[i2,j2,i1,j1]
                        if (!is.na(tmp)){ compute <- FALSE }
                    }
                    if (compute){
                        tmp <- SRM_TRACE_PRODUCT(garmat1, garmat1_list[[i2]][[j2]])
                    }
                    garmat1_trace[i1,j1,i2,j2] <- tmp
                    w[i1,w1] <- w[i1,w1]+cons*tmp
                    #--- garmat2
                    if (i2<=i1){
                        tmp <- garmat2_trace[i2,j2,i1,j1]
                        if (!is.na(tmp)){ compute <- FALSE }
                    }
                    if (compute & bivariate){
                        tmp <- SRM_TRACE_PRODUCT(garmat2, garmat2_list[[i2]][[j2]])
                    }
                    garmat2_trace[i1,j1,i2,j2] <- tmp
                    w[i1,w1] <- w[i1,w1]+cons*tmp
                }
                for (j2 in 1:6){ # setting up var-cov
                    compute <- TRUE
                    jj2 <- j2 # matrix for SS & SCP
                    if ((j2==3) | (j2==4)){ jj2 <- 7-j2 }
                    dumj2 <- dum_list[[jj2]]
                    w2 <- cwu[j1,j2]+i2
                    #- garmat3
                    if (i2<=i1){
                        tmp <- garmat3_trace[i2,jj2,i1,j1]
                        if (!is.na(tmp)){ compute <- FALSE }
                    }
                    if (compute & bivariate){
                        tmp <- SRM_TRACE_PRODUCT(garmat3, garmat3_list[[i2]][[jj2]])
                    }
                    garmat3_trace[i1,j1,i2,jj2] <- tmp
                    uw[i1,w2] <- uw[i1,w2]+2*tmp
                }
            } # end i2 loop
        } # end j1 loop

        tmp13 <- tmp13_list[[i1]]
        uvv_value <- FALSE
        for (i2 in 1:6){
            dumi2 <- dum_list[[i2]]
            tmp23 <- tmp23_list[[i2]]
            w1 <- i2+120
            for (j1 in 1:5) {
                jj1 <- j1
                if (j1>3){ jj1 <- j1+1 }
                uu <- vv_list[[jj1]]
                if (j1==3){
                    tmp1 <- tmp13
                    uvv_value <- TRUE
                } else {
                    tmp1 <- garmat2_list[[i1]][[jj1]]
                }
                for (j2 in 1:5) {
                    compute <- TRUE
                    w1 <- w1+6
                    jj2 <- j2
                    if (jj2>3){ jj2 <- j2+1 }
                    zz <- vv_list[[jj2]]
                    if (j2==3){
                        tmp2 <- tmp23
                        uvv_value <- TRUE
                    } else {
                        tmp2 <- garmat3_list[[i2]][[jj2]]
                    }
                    if (uvv_value){
                        temp <- SRM_TRACE_PRODUCT(tmp1, tmp2)
                    } else {
                        if (i2<=i1){
                            temp <- garmat23_trace[i2,jj2,i1,jj1]
                            if (!is.na(temp)){ compute <- FALSE }
                        }
                        if (compute){
                            temp <- SRM_TRACE_PRODUCT(tmp1, tmp2)
                        }
                    }
                    garmat23_trace[i1,jj1,i2,jj2] <- temp
                    w[i1,w1] <- w[i1,w1]+temp
                } # end j2
            }  # end loop j1
        }  # end i2
            
    } # End i2 and i1 loops and end time-consuming computation
    
    #--- output
    res <- list(w=w, cwu=cwu, uw=uw)
    return(res)
}

