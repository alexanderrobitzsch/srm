## File Name: SRM_ARBSRM_ONE_GROUP.R
## File Version: 0.355


SRM_ARBSRM_ONE_GROUP <- function(data, serror=TRUE, bivariate=TRUE)
{
    CALL <- match.call()
    s1 <- Sys.time()
    data_resp <- ! is.na(data)
    data <- as.matrix(data)

    #--- parameter estimation
    res <- SRM_RCPP_SRM_ARBSRM_ONE_GROUP_ESTIMATE( data=data, data_resp=data_resp,
                bivariate=bivariate)
    est <- res$est
    j0 <- res$j0 + 1
    est <- res$est
    cin <- res$cin
    aa <- res$aa
    vv <- res$vv
    uvv <- res$uvv
    ntot <- res$ntot
    npar <- res$npar
    s2 <- Sys.time()

    #--- standard error estimation
    var <- se <- NULL
    if (serror){
        res <- SRM_ARBSRM_ONE_GROUP_SE( vv=vv, aa=aa, uvv=uvv, ntot=ntot, j0=j0,
                        cin=cin, est=est, bivariate=bivariate )
        se <- res$se
        var <- res$var
    }

    #--- output
    s3 <- Sys.time()
    time <- list(start=s1, end=s3, time_est=s2-s1, time_se=s3-s2)
    res <- list(est=est, se=se, serror=serror, time=time)
    return(res)
}
