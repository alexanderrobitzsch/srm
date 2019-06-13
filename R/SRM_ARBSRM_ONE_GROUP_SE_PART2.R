## File Name: SRM_ARBSRM_ONE_GROUP_SE_PART2.R
## File Version: 0.19

SRM_ARBSRM_ONE_GROUP_SE_PART2 <- function(cin, w, uw, est)
{
    b <- matrix(0,46,46)
    var <- matrix(0,6,6)
    for (j1 in 1:46) {
        dum <- ((j1-1)*6+1):(j1*6)
        # Searle, Casella, & McCullough, Equation 21, page 176
        var <- SRM_MATRIX_TRIPLE_PRODUCT(x=cin, y=w[,dum])
        r1 <- 0
        for (i1 in 1:6){
            for (i2 in i1:6) {
                r1 <- r1+1
                b[r1,j1] <- var[i1,i2]
            }
        }
        if (j1<22){
            r2 <- 21
            var <- SRM_MATRIX_TRIPLE_PRODUCT(x=cin, y=uw[,dum])
            for (i1 in 1:5) {
                ii1 <- i1
                if (i1>3) ii1=i1+1
                for (i2 in 1:5){
                    ii2 <- i2
                    if (i2>3) ii2 <- i2+1 # Accommodating the
                    r2 <- r2+1 # asymmetrical SCPs
                    bare <- var[ii1,ii2]
                    if (i1==3) bare <- (var[3,ii2]+var[4,ii2])/2
                    if (i2==3) bare <- (var[ii1,3]+var[ii1,4])/2
                    if ((i1==3) & (i2==3))
                    bare <- (var[3,3]+var[3,4]+var[4,3]+var[4,4])/4
                    b[r2,j1] <- bare
                }
            } # b gives exact vars and covs    # among parameter estimates
        }
    }
    bi <- diag(46)
    bi <- b+bi
    bi <- SRM_GINV(x=bi)
    bb <- matrix(0,6,46)
    dn <- c(1,7,12,16,19,21)
    for (i in 1:6) {
        bb[i,] <- b[dn[i],]
    }
    bb <- bb %*% bi
    # Having set everything up, we now begin using the data
    oot <- matrix(0,6,3)
    kk <- 3
    estp <- rep(0,46)
    #Computing products of estimates
    ind <- c(1,2,3,5,6)
    xvec <- est[ind,1]
    yvec <- est[ind,2]
    for (kk in 1:3){
        xyvec<- c(est[1,kk],est[2,kk],est[3,kk],est[4,kk],est[5,kk],est[6,kk])
        matr<- SRM_OUTER(xyvec, xyvec)
        estp[1:21]<- c(matr[,1],matr[2:6,2],matr[3:6,3],matr[4:6,4],matr[5:6,5],matr[6,6])
        matrx <- SRM_OUTER(xvec, xvec)
        if (kk==2) matrx <- SRM_OUTER(yvec, yvec)
        if (kk==3) matrx <- SRM_OUTER(yvec, xvec)
        estp[22:46] <- c(matrx)
        oot[,kk] <- bb %*% estp
    }
    se <- oot^.5
    #-- output
    res <- list(se=se, var=oot)
    return(res)
}
