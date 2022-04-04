## File Name: SRM_COMPUTE_SEM_SIGMA_GRADIENT.R
## File Version: 0.172


SRM_COMPUTE_SEM_SIGMA_GRADIENT <- function( parm_list, parm_type, parm_pos,
    parm_level, group )
{
    parm_list1 <- parm_list[[ paste0("parm_list_", parm_level) ]][[group]]
    #-- collect arguments
    LAM <- parm_list1[[ paste0("LAM_", parm_level) ]]
    PHI <- parm_list1[[ paste0("PHI_", parm_level) ]]
    PSI <- parm_list1[[ paste0("PSI_", parm_level) ]]
    B <- parm_list1[[ paste0("B_", parm_level) ]]

    #-- start computations
    res <- diag(0, ncol(PSI) )
    if ( is.null(B) ){
        n1 <- ncol(PHI)
        B <- diag(0, n1)
    }
    IB <- diag(1, nrow(B) )
    levels_sub <- paste0("_", c("U","D") )
    #--------------------------------------
    # differentiation with respect to LAM
    if (parm_type %in% paste0("LAM", levels_sub) ){
        W <- solve(IB - B)
        # H1 <- LAM %*% W %*% PHI
        H1 <- LAM %*% W %*% PHI %*% t(W)
        H2 <- t(H1)
        ONE_H1 <- matrix(0, nrow=ncol(LAM), ncol=nrow(LAM))
        ONE_H1 <- SRM_REPLACE_VALUES(x=ONE_H1, val=1, pos=parm_pos[c(2,1)], symm=FALSE)
        ONE_H2 <- matrix(0, nrow=nrow(LAM), ncol=ncol(LAM))
        ONE_H2 <- SRM_REPLACE_VALUES(x=ONE_H2, val=1, pos=parm_pos, symm=FALSE)
        t1 <- H1 %*% ONE_H1
        t2 <- ONE_H2 %*% H2
        res <- t1 + t2
    }
    #--------------------------------------
    # differentiation with respect to PHI
    if (parm_type %in% paste0("PHI", levels_sub ) ){
        W <- solve(IB - B)
        H1 <- LAM %*% W
        x <- matrix(0, nrow=nrow(PHI), ncol=ncol(PHI) )
        x <- SRM_REPLACE_VALUES(x=x, val=1, pos=parm_pos, symm=TRUE)
        res <- H1 %*% tcrossprod( x=x, y=H1)
    }
    #--------------------------------------
    # differentiation with respect to PSI
    if (parm_type %in% paste0("PSI", levels_sub ) ){
        res <- matrix(0, nrow=nrow(PSI), ncol=ncol(PSI) )
        res <- SRM_REPLACE_VALUES(x=res, val=1, pos=parm_pos, symm=TRUE)
    }
    #--------------------------------------
    # differentiation with respect to B
    if (parm_type %in% paste0("B", levels_sub) ){
        numdiff <- FALSE
        #- do not use these derivatives anymore
        if (! numdiff){
            W <- solve(IB - B)
            Wt <- t(W)
            x0 <- matrix(0, nrow=nrow(B), ncol=ncol(B))
            x <- SRM_REPLACE_VALUES(x=x0, val=1, pos=parm_pos, symm=FALSE)
            Wt_LAM <- tcrossprod( x=Wt, y=LAM)
            LAM_W <- LAM %*% W
            t1a <- LAM_W %*% x %*% W
            t1 <- t1a %*% PHI %*% Wt_LAM
            t2a <- LAM_W %*% PHI
            x <- SRM_REPLACE_VALUES(x=x0, val=1, pos=parm_pos[c(2,1)], symm=FALSE)
            t2 <- t2a %*% Wt %*% x %*% Wt_LAM
            res <- t1 + t2
        } else {
            # numerical derivatives
            h <- 1e-4
            B1 <- B
            B1[ parm_pos[1], parm_pos[2] ] <- B1[ parm_pos[1], parm_pos[2] ] - h
            cov1 <- SRM_COVFUN(LAM=LAM, B=B1, PHI=PHI, PSI=PSI, IB=IB)
            B1 <- B
            B1[ parm_pos[1], parm_pos[2] ] <- B1[ parm_pos[1], parm_pos[2] ] + h
            cov2 <- SRM_COVFUN(LAM=LAM, B=B1, PHI=PHI, PSI=PSI, IB=IB)
            res <- (cov2-cov1)/(2*h)
        }
    }

    return(res)
}
