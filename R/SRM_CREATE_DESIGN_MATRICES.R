## File Name: SRM_CREATE_DESIGN_MATRICES.R
## File Version: 0.319

SRM_CREATE_DESIGN_MATRICES <- function( data_list, ngroups, use_rcpp=TRUE)
{
    # data_list$ngroups <- ngroups
    comp_design <- TRUE
    for (gg in 1L:ngroups){
        NI <- data_list[[gg]]$NI
        ND <- data_list[[gg]]$ND
        NR <- nrow(NI)
        y_rr <- list()
        Zis_rr <- list()
        Zis_rr_rcpp <- list()
        Z_ind_rr_rcpp <- list()
        Xs_rr <- list()
        Wds_rr <- list()
        Wds_rr_rcpp <- list()
        groups <- unique( data_list[[gg]]$y$Group )
        data_list[[1]]$nrr <- NR
        y_gg <- data_list[[gg]]$y
        Zis_gg <- data_list[[gg]]$Zis
        Wds_gg <- data_list[[gg]]$Wds
        Xs_gg <- data_list[[gg]]$Xs
        calculate_gg <- rep(TRUE,NR)
        for (rr in seq_len(NR) ){
            # y
            ind_rr <- which(y_gg$Group == groups[rr])
            y_temp <- y_gg[ ind_rr, ]
            y_rr[[rr]] <- y_temp$y

            # X
            Xs_rr[[rr]] <- Xs_gg[ ind_rr, , drop=FALSE]

            # Z
            NC <- max(Zis_gg[,"cols"])
            NZ <- nrow(y_temp)
            Z0 <- matrix(0, nrow=NZ, ncol=NC)
            Zis2 <- Zis_gg[ Zis_gg[,"rrgroup"] == rr, ]
            Zis2 <- Zis2[ order(Zis2[,"rows"]), ]
            n_rr <- NI[rr,"NI"]
            Zis_temp <- list()
            Zis_temp_rcpp <- NULL
            Z <- matrix(0, nrow=NZ, ncol=n_rr*NC)
            for (pp in 1:n_rr){
                Z1 <- Z0
                Zis2_pp <- Zis2[ Zis2[,"pid"] == pp, ]
                Z1[ Zis2_pp[, c("rows","cols")] ] <- 1
                Zis_temp[[pp]] <- Z1
                ind <- NC*(pp-1)+1:NC
                Z[ , ind] <- Z1
                if(comp_design){
                    Zis2_mm <- Zis2_pp[,2:4] - 1
                    Z2 <- SRM_RCPP_SRM_DATA_LIST_CREATE_INSERTION_MATRIX(x=Zis2_mm)
                    Zis_temp_rcpp <- rbind( Zis_temp_rcpp, Z2)
                }
            }
            Zis_rr[[rr]] <- Zis_temp
            Zis_rr_rcpp[[rr]] <- Zis_temp_rcpp
            ZR <- nrow(Z)
            ZC <- ncol(Z)
            Z_ind <- cbind( rep(0:(ZR-1),ZC), rep(0:(ZC-1), each=ZR)  )
            Z_ind <- Z_ind[ as.vector(Z) == 1 , ]
            Z_ind_rr_rcpp[[rr]] <- Z_ind

            # W
            Wds2 <- Wds_gg[ Wds_gg[,"rrgroup"] == rr, ]
            Wds2 <- Wds2[ order(Wds2[,"rows"]), ]
            n_rr <- ND[rr,"ND"]
            Wds_temp <- list()
            Wds_temp_rcpp <- NULL
            for (pp in 1:n_rr){
                Z1 <- Z0
                Wds2_pp <- Wds2[ Wds2[,"did"] == pp,,drop=FALSE]
                Z1[ Wds2_pp[, c("rows","cols")] ] <- 1
                Wds_temp[[pp]] <- Z1
                if (comp_design){
                    Wds2_mm <- Wds2_pp
                    Zis2_mm <- Wds2_mm[,2:4,drop=FALSE] - 1
                    Z2 <- SRM_RCPP_SRM_DATA_LIST_CREATE_INSERTION_MATRIX(x=Zis2_mm)
                    Wds_temp_rcpp <- rbind( Wds_temp_rcpp, Z2)
                }
            }
            Wds_rr[[rr]] <- Wds_temp
            Wds_rr_rcpp[[rr]] <- Wds_temp_rcpp

            if (rr>1){
                diff_z <- SRM_EQUAL_MATRICES(x1=Zis_rr_rcpp[[rr]], x2=Zis_rr_rcpp[[rr-1]], elim=c(1))
                diff_w <- SRM_EQUAL_MATRICES(x1=Wds_rr_rcpp[[rr]], x2=Wds_rr_rcpp[[rr-1]], elim=c(1))
                diff_all <- diff_z & diff_w
                calculate_gg[[rr]] <- ! diff_all
            }

        }  # end rr

        data_list[[gg]]$y_rr <- y_rr
        data_list[[gg]]$Xs_rr <- Xs_rr
        data_list[[gg]]$Zis_rr <- Zis_rr
        data_list[[gg]]$Zis_rr_rcpp <- Zis_rr_rcpp
        data_list[[gg]]$Z_ind_rr_rcpp <- Z_ind_rr_rcpp
        data_list[[gg]]$Wds_rr <- Wds_rr
        data_list[[gg]]$Wds_rr_rcpp <- Wds_rr_rcpp
        data_list[[gg]]$calculate_gg <- calculate_gg
    }
    return(data_list)
}
