## File Name: SRM_SIM_DATA.R
## File Version: 0.01

SRM_SIM_DATA <- function(n, R, parm_list, var_label="V")
{
    #-- create combinations
    combis <- t(utils::combn(n,2))
    combis <- cbind(1:nrow(combis),combis)
    ND <- nrow(combis)
    combis <- rbind( combis,combis[,c(1,3,2)])
    colnames(combis) <- c("dyad", "Actor", "Partner")
    combis <- as.data.frame(combis)
    
    dat <- NULL
    LAM_U <- parm_list$LAM_U
    PHI_U <- parm_list$PHI_U
    PSI_U <- parm_list$PSI_U
    LAM_D <- parm_list$LAM_D
    PHI_D <- parm_list$PHI_D
    PSI_D <- parm_list$PSI_D    
    NM <- nrow(LAM_U) / 2

    for (rr in 1:R){    
        mu0 <- rep(0,2*NM)
        # simulate person effects
        SIGMA_U <- LAM_U %*% PHI_U %*% t(LAM_U)  + PSI_U
        u <- SRM_MVRNORM(n=n, mu=mu0, Sigma=SIGMA_U)

        # simulate dyad effects
        SIGMA_D <- LAM_D %*% PHI_D %*% t(LAM_D)  + PSI_D
        r <- matrix( SRM_MVRNORM(n=ND, mu=mu0, Sigma=SIGMA_D), nrow=ND)
    
        # define effects
        NV <- nrow(LAM_U)/2
        dat0 <- matrix(0, nrow=2*ND, ncol=NV)
        ind1 <- 1:NM
        ind2 <- NM + ind1
        dat0 <- dat0 + u[ combis[,2] , ind1] + u[ combis[,3] , ind2]        
        ind3 <- seq(1,2*NM,2)
        ind4 <- seq(2,2*NM,2)    
        dat0 <- dat0 + rbind( r[,ind3, drop=FALSE], r[,ind4,drop=FALSE] )
        colnames(dat0) <- paste0(var_label,1:NM)
        dat00 <- data.frame( Group=rr, combis, dat0)
        dat00 <- dat00[ order(dat00$Actor), ]
        dat <- rbind( dat, dat00 )
    }
    return(dat)
}
