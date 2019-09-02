## File Name: SRM_PARTABLE_MATRIX.R
## File Version: 0.11

## functions to compute the matrix representation
## overall function that is called in SRM_AUFRUF

SRM_PARTABLE_TO_MATRIX <- function(srmpartable, name = "EMPTY",
                                   add.attributes = TRUE,
                                   as.a.data.frame = TRUE,
                                   ngroups  = 1L) {

    # get model matrices
    if(name == "Person") {
        REP <- SRM_PARTABLE_TO_MATRIX_PERSON(srmpartable,
                                             target = NULL,
                                             extra = add.attributes,
                                             ngroups = ngroups)
    } else if (name == "Dyad") {
        REP <- SRM_PARTABLE_TO_MATRIX_DYAD(srmpartable,
                                           target = NULL,
                                           extra = add.attributes,
                                           ngroups = ngroups)
    }

    srmpartable$mat <- REP$mat
    srmpartable$row <- REP$row
    srmpartable$col <- REP$col

    if(as.a.data.frame) {
        srmpartable <- as.data.frame(srmpartable, stringsAsFactors=FALSE)
    }

    if(add.attributes) {
        attr(srmpartable, "mmNames")  <- attr(REP, "mmNames")
        attr(srmpartable, "mmRows")   <- attr(REP, "mmRows")
        attr(srmpartable, "mmCols")   <- attr(REP, "mmCols")
        attr(srmpartable, "mmSymmetric") <- attr(REP, "mmSymmetric")
    }

    srmpartable
}

## specific function

SRM_PARTABLE_TO_MATRIX_PERSON <- function(srmpartable = NULL,
                                          target   = NULL,
                                          extra    = FALSE,
                                          ngroups  = 1L) {


    # prepare target list
    if(is.null(target)) target <- srmpartable

    # prepare output
    N <- length(target$lhs)
    tmp.mat <- character(N)
    tmp.row <- integer(N)
    tmp.col <- integer(N)

    # global settings
    # meanstructure <- any(srmpartable$op == "~1")
    # categorical   <- any(partable$op == "|")

    if(any(srmpartable$op == "~" & grepl("@E",srmpartable$rhs))) {
      Gamma <- TRUE
    } else { Gamma <- FALSE }

    if(extra) {
        REP.mmNames     <- vector("list", ngroups)
        REP.mmRows      <- vector("list", ngroups)
        REP.mmCols      <- vector("list", ngroups)
        REP.mmSymmetric <- vector("list", ngroups)
    }

    for(g in 1:ngroups) {

        # ++++++++++++++++++++++++++++++++
        #   get information per group
        # +++++++++++++++++++++++++++++++

        tmp.srmpartable <- subset(srmpartable,srmpartable$group == g)

        # ++++++++++++++++++++++++++++++++
        #  info from user model per block
        # ++++++++++++++++++++++++++++++++

        # IMPOTRANT: Because srmpartable contains defaults values for
        #            single-indicator rs, the results are different for
        #            srmpartable compared to PARLIST

        # latent actor- and partner-rrs
        rr.lv.names.a <- SRM_PARTABLE_VNAMES_PERSON(tmp.srmpartable, type="rr.lv.a")
        rr.lv.names.p <- SRM_PARTABLE_VNAMES_PERSON(tmp.srmpartable, type="rr.lv.p")
        #rr.latents <- sort(c(rr.lv.names.a,rr.lv.names.p))
        rr.latents <- c(rr.lv.names.a,rr.lv.names.p)
        rr.nfac = length(rr.latents)

        # observed rrs that are indicators of the rr-lvs
        rr.ov.ind.names.a <- SRM_PARTABLE_VNAMES_PERSON(tmp.srmpartable, type="rr.ov.ind.a")
        rr.ov.ind.names.p <- SRM_PARTABLE_VNAMES_PERSON(tmp.srmpartable, type="rr.ov.ind.p")
        #rr.ov.inds <- sort(c(rr.ov.ind.names.a,rr.ov.ind.names.p))
        rr.ov.inds <- c(rr.ov.ind.names.a,rr.ov.ind.names.p)
        rr.true.ov.inds <- setdiff(rr.ov.inds,rr.latents) # exclude false ovs

        # if there is only one indicator per factor, then this is a special kind of
        # latent variable
        rr.true.latents <- setdiff(rr.latents,rr.ov.inds)

        # true exogenuous covariates
        sv.eqs.x <- SRM_PARTABLE_VNAMES_PERSON(tmp.srmpartable, type="sv.eqs.x")
        #sv.eqs.y <- SRM_PARTABLE_VNAMES_PERSON(PARLIST, type="sv.eqs.y")

        # ++++++++++++++++++++++++++++++++++
        #    now we build the matrices
        # ++++++++++++++++++++++++++++++++++

        # 1. Factor loading matrix, i.e., "=~" regular indicators
        idx <- which(target$group == g &
                     target$op == "=~")
        tmp.mat[idx] <- "LAM_U"
        tmp.row[idx] <- match(target$rhs[idx],rr.ov.inds)
        tmp.col[idx] <- match(target$lhs[idx],rr.latents)
        ncol_LAM_U <- rr.nfac
        nrow_LAM_U <- length(rr.ov.inds)

        # 2. "~" regressions by rr-variables and exogenous covariates
        if(Gamma) {
            # GAM_U
            idx <- which(target$group == g  &
                         target$rhs %in% sv.eqs.x &
                         target$op == "~")
            tmp.mat[idx] <- "G_U"
            tmp.row[idx] <- match(target$lhs[idx],rr.latents)
            tmp.col[idx] <- match(target$rhs[idx],sv.eqs.x)
            ncol_G_U <- length(sv.eqs.x)
            nrow_G_U <- rr.nfac

            # B_U
            idx <- which(!(target$rhs %in% sv.eqs.x) &
                           target$group == g &
                           target$op == "~")
            tmp.mat[idx] <- "B_U"
            tmp.row[idx] <- match(target$lhs[idx],rr.latents)
            tmp.col[idx] <- match(target$rhs[idx],rr.latents)
            ncol_B_U <- rr.nfac
            nrow_B_U <- rr.nfac

        } else {
            # just B_U
            idx <- which(target$group == g &
                         target$op == "~" )
            tmp.mat[idx] <- "B_U"
            tmp.row[idx] <- match(target$lhs[idx],rr.latents)
            tmp.col[idx] <- match(target$rhs[idx],rr.latents)
            ncol_B_U <- rr.nfac
            nrow_B_U <- rr.nfac

            ncol_G_U <- 0
            nrow_G_U <- 0
        }

        # 3.  variance-covariance matrices
        # 3a. "~~" observed variables
        idx <- which(target$group == g &
                     target$op == "~~"  &
                     target$lhs %in% rr.true.ov.inds &
                     target$rhs %in% rr.true.ov.inds )
        tmp.mat[idx] <- "PSI_U"
        tmp.row[idx] <- match(target$lhs[idx],rr.true.ov.inds)
        tmp.col[idx] <- match(target$rhs[idx],rr.true.ov.inds)
        ncol_PSI_U <- length(rr.ov.inds)
        nrow_PSI_U <- length(rr.ov.inds)

        # 3b. "~~" latent variables --> PHI_U
        idx <- which(target$group == g &
                     target$op == "~~"  &
                     target$lhs %in% rr.latents &
                     target$rhs %in% rr.latents)
        tmp.mat[idx] <- "PHI_U"
        tmp.row[idx] <- match(target$lhs[idx], rr.latents)
        tmp.col[idx] <- match(target$rhs[idx], rr.latents)
        ncol_PHI_U <- length(rr.latents)
        nrow_PHI_U <- length(rr.latents)

        # 4. means and Intercepts
        # 4a. "~1" ov
        idx <- which(target$group == g &
                     target$op == "~1" &
                     !(target$lhs %in% rr.true.latents))

        tmp.mat[idx] <- "BETA"
        tmp.row[idx] <- match(target$lhs[idx], rr.ov.ind.names.a)
        tmp.col[idx] <- 1L
        nrow_BETA <- length(idx)

        # 4b. "~1" lv
        idx <- which(target$group == g &
                     target$op == "~1" &
                     target$lhs %in% rr.true.latents)
        tmp.mat[idx] <- "MU_U"
        tmp.row[idx] <- match(target$lhs[idx], rr.latents)
        tmp.col[idx] <- 1L
        nrow_MU_U <- length(idx)

        if(extra) {
            # mRows
            mmRows <- list(LAM_U = nrow_LAM_U,
                           G_U = nrow_G_U,
                           B_U   = nrow_B_U,
                           PHI_U = nrow_PHI_U,
                           PSI_U = nrow_PSI_U,
                           MU_U = nrow_MU_U,
                           BETA = nrow_BETA)

            # mCols
            mmCols <- list(LAM_U = ncol_LAM_U,
                           G_U = ncol_G_U,
                           B_U   = ncol_B_U,
                           PHI_U = ncol_PHI_U,
                           PSI_U = ncol_PSI_U,
                           MU_U = 1L,
                           BETA = 1L)

            # isSymmetric
            mmSymmetric <- list(LAM_U = FALSE,
                           G_U = FALSE,
                           B_U   = TRUE,
                           PHI_U = TRUE,
                           PSI_U = TRUE,
                           MU_U = FALSE,
                           BETA = FALSE)

            IDX <- which(target$group == g)
            mmNames <- character()

            if("LAM_U" %in% tmp.mat[IDX]) {
                mmNames <- c(mmNames, "LAM_U")
            }
            if("G_U" %in% tmp.mat[IDX]) {
                mmNames <- c(mmNames, "G_U")
            }
            if("B_U" %in% tmp.mat[IDX]) {
                mmNames <- c(mmNames, "B_U")
            }
            if("PHI_U" %in% tmp.mat[IDX]) {
                mmNames <- c(mmNames, "PHI_U")
            }
            if("PSI_U" %in% tmp.mat[IDX]) {
                mmNames <- c(mmNames, "PSI_U")
            }
            if("MU_U" %in% tmp.mat[IDX]) {
                mmNames <- c(mmNames, "MU_U")
            }
            if("BETA" %in% tmp.mat[IDX]) {
                mmNames <- c(mmNames, "BETA")
            }
            REP.mmNames[[g]]     <- mmNames
            REP.mmRows[[g]]      <- unlist(mmRows[ mmNames ])
            REP.mmCols[[g]]      <- unlist(mmCols[ mmNames ])
            REP.mmSymmetric[[g]] <- unlist(mmSymmetric[ mmNames ])
        } # extra
    } # ngroups

    REP <- list(mat = tmp.mat,
                row = tmp.row,
                col = tmp.col)

    if(extra) {
        attr(REP, "mmNames")     <- REP.mmNames
        attr(REP, "mmRows")      <- REP.mmRows
        attr(REP, "mmCols")      <- REP.mmCols
        attr(REP, "mmSymmetric") <- REP.mmSymmetric
    }

    return(REP)
}

SRM_PARTABLE_TO_MATRIX_DYAD <- function(srmpartable = NULL,
                                        target   = NULL,
                                        extra    = FALSE,
                                        ngroups  = 1L) {

    # prepare target list
    if(is.null(target)) target <- srmpartable

    # prepare output
    N <- length(target$lhs)
    tmp.mat <- character(N)
    tmp.row <- integer(N)
    tmp.col <- integer(N)

    # global settings
    #meanstructure <- any(srmpartable$op == "~1")

    if(any(srmpartable$op == "~" & grepl("@F",srmpartable$rhs))) { Gamma <- TRUE
    } else { Gamma <- FALSE }


    if(extra) {
        REP.mmNames     <- vector("list", ngroups)
        REP.mmRows      <- vector("list", ngroups)
        REP.mmCols      <- vector("list", ngroups)
        REP.mmSymmetric <- vector("list", ngroups)
    }

    for(g in 1:ngroups) {

        # ++++++++++++++++++++++++++++++++
        #   get information per group
        # +++++++++++++++++++++++++++++++

        tmp.srmpartable <- subset(srmpartable,srmpartable$group == g)
        tmp.srmpartable$group = 1

        # ++++++++++++++++++++++++++++++++
        #  info from user model per block
        # ++++++++++++++++++++++++++++++++

        # IMPOTRANT: Because srmpartable contains defaults values for
        #            single-indicator rs, the results are different for
        #            srmpartable compared to PARLIST

        # latent actor- and partner-rrs
        rr.lv.names.ij <- SRM_PARTABLE_VNAMES_DYAD(tmp.srmpartable, type="rr.lv.ij")
        rr.lv.names.ji <- SRM_PARTABLE_VNAMES_DYAD(tmp.srmpartable, type="rr.lv.ji")
        rr.latents <- sort(c(rr.lv.names.ij,rr.lv.names.ji))
        rr.nfac = length(rr.latents)

        # observed rrs that are indicators of the rr-lvs
        rr.ov.ind.names.ij <- SRM_PARTABLE_VNAMES_DYAD(tmp.srmpartable, type="rr.ov.ind.ij")
        rr.ov.ind.names.ji <- SRM_PARTABLE_VNAMES_DYAD(tmp.srmpartable, type="rr.ov.ind.ji")
        rr.ov.inds <- sort(c(rr.ov.ind.names.ij,rr.ov.ind.names.ji))
        rr.true.ov.inds <- setdiff(rr.ov.inds,rr.latents) # exclude false ovs

        # true exogenuous covariates
        sv.eqs.x <- SRM_PARTABLE_VNAMES_DYAD(tmp.srmpartable, type="sv.eqs.x")
        #sv.eqs.y <- SRM_PARTABLE_VNAMES_PERSON(PARLIST, type="sv.eqs.y")

        # ++++++++++++++++++++++++++++++++++
        #    now we build the matrices
        # ++++++++++++++++++++++++++++++++++

        # 1. Factor loading matrix, i.e., "=~" regular indicators
        idx <- which(target$group == g &
                     target$op == "=~")
        tmp.mat[idx] <- "LAM_D"
        tmp.row[idx] <- match(target$rhs[idx],rr.ov.inds)
        tmp.col[idx] <- match(target$lhs[idx],rr.latents)
        ncol_LAM_D <- rr.nfac
        nrow_LAM_D <- length(rr.ov.inds)

        # 2. "~" regressions by rr-variables and exogenous covariates
        if(Gamma) {
            # GAM_D
            idx <- which(target$group == g  &
                         target$rhs %in% sv.eqs.x &
                         target$op == "~")
            tmp.mat[idx] <- "G_D"
            tmp.row[idx] <- match(target$lhs[idx],rr.latents)
            tmp.col[idx] <- match(target$rhs[idx],sv.eqs.x)
            ncol_G_D <- length(sv.eqs.x)
            nrow_G_D <- rr.nfac

            # B_D
            idx <- which(!(target$rhs %in% sv.eqs.x) &
                           target$group == g &
                           target$op == "~")
            tmp.mat[idx] <- "B_D"
            tmp.row[idx] <- match(target$lhs[idx],rr.latents)
            tmp.col[idx] <- match(target$rhs[idx],rr.latents)
            ncol_B_D <- rr.nfac
            nrow_B_D <- rr.nfac

        } else {
            # just B_D
            idx <- which(target$group == g &
                         target$op == "~" )
            tmp.mat[idx] <- "B_U"
            tmp.row[idx] <- match(target$lhs[idx],rr.latents)
            tmp.col[idx] <- match(target$rhs[idx],rr.latents)
            ncol_B_D <- rr.nfac
            nrow_B_D <- rr.nfac

            ncol_G_D <- 0
            nrow_G_D <- 0
        }

        # 3.  variance-covariance matrices
        # 3a. "~~" observed variables
        idx <- which(target$group == g &
                     target$op == "~~"  &
                     target$lhs %in% rr.true.ov.inds &
                     target$rhs %in% rr.true.ov.inds )
        tmp.mat[idx] <- "PSI_D"
        tmp.row[idx] <- match(target$lhs[idx],rr.true.ov.inds)
        tmp.col[idx] <- match(target$rhs[idx],rr.true.ov.inds)
        ncol_PSI_D <- length(rr.ov.inds)
        nrow_PSI_D <- length(rr.ov.inds)

        # 3b. "~~" latent variables --> PHI_U
        idx <- which(target$group == g &
                     target$op == "~~"  &
                     target$lhs %in% rr.latents &
                     target$rhs %in% rr.latents)
        tmp.mat[idx] <- "PHI_D"
        tmp.row[idx] <- match(target$lhs[idx], rr.latents)
        tmp.col[idx] <- match(target$rhs[idx], rr.latents)
        ncol_PHI_D <- length(rr.latents)
        nrow_PHI_D <- length(rr.latents)

        # 4. means and Intercepts
        # 4a. "~1" ov
        #idx <- which(target$block == g &
        #             target$op == "~1" & !(target$lhs %in% lv.names))
        #tmp.mat[idx] <- "BETA_U"
        #tmp.row[idx] <- match(target$lhs[idx], ov.names)
        #tmp.col[idx] <- 1L

        # 4b. "~1" lv
        #idx <- which(target$block == g &
        #             target$op == "~1" & target$lhs %in% lv.names)
        #tmp.mat[idx] <- "MU_U"
        #tmp.row[idx] <- match(target$lhs[idx], lv.names)
        #tmp.col[idx] <- 1L

        if(extra) {
            # mRows
            mmRows <- list(LAM_D = nrow_LAM_D,
                           G_D = nrow_G_D,
                           B_D   = nrow_B_D,
                           PHI_D = nrow_PHI_D,
                           PSI_D = nrow_PSI_D,
                           MU_D = 1L,
                           BETA_D = 1L)

            # mCols
            mmCols <- list(LAM_D = ncol_LAM_D,
                           G_D = ncol_G_D,
                           B_D   = ncol_B_D,
                           PHI_D = ncol_PHI_D,
                           PSI_D = ncol_PSI_D,
                           MU_D = 1L,
                           BETA_D = 1L)

            # isSymmetric
            mmSymmetric <- list(LAM_D = FALSE,
                           G_D = FALSE,
                           B_D   = TRUE,
                           PHI_D = TRUE,
                           PSI_D = TRUE,
                           MU_D = FALSE,
                           BETA_D = FALSE)

            # which mm's do we need? (always include lambda, theta and psi)
            # new: 0.6 this block only!!
            IDX <- which(target$group == g)
            mmNames <- character()

            if("LAM_D" %in% tmp.mat[IDX]) {
                mmNames <- c(mmNames, "LAM_D")
            }
            if("G_D" %in% tmp.mat[IDX]) {
                mmNames <- c(mmNames, "G_D")
            }
            if("B_D" %in% tmp.mat[IDX]) {
                mmNames <- c(mmNames, "B_D")
            }
            if("PHI_D" %in% tmp.mat[IDX]) {
                mmNames <- c(mmNames, "PHI_D")
            }
            if("PSI_D" %in% tmp.mat[IDX]) {
                mmNames <- c(mmNames, "PSI_D")
            }
            if("MU_D" %in% tmp.mat[IDX]) {
                mmNames <- c(mmNames, "MU_D")
            }
            if("BETA_D" %in% tmp.mat[IDX]) {
                mmNames <- c(mmNames, "BETA_D")
            }
            REP.mmNames[[g]]     <- mmNames
            REP.mmRows[[g]]      <- unlist(mmRows[ mmNames ])
            REP.mmCols[[g]]      <- unlist(mmCols[ mmNames ])
            REP.mmSymmetric[[g]] <- unlist(mmSymmetric[ mmNames ])
        } # extra
    } # nblocks

    REP <- list(mat = tmp.mat,
                row = tmp.row,
                col = tmp.col)

    if(extra) {
        attr(REP, "mmNames")     <- REP.mmNames
        attr(REP, "mmRows")      <- REP.mmRows
        attr(REP, "mmCols")      <- REP.mmCols
        attr(REP, "mmSymmetric") <- REP.mmSymmetric
    }

    REP
}
