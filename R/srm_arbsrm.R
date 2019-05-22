## File Name: srm_arbsrm.R
## File Version: 0.09

srm_arbsrm <- function(data, serror=TRUE, use_srm=TRUE)
{
    #- preliminaries
    CALL <- match.call()
    s1 <- Sys.time()
    bivariate <- TRUE   # bivariate estimation
    
    if (use_srm){
        #-- implementation in srm package
        res <- SRM_ARBSRM_ONE_GROUP(data=data, serror=serror, bivariate=bivariate)
    } else {
        #-- original implementation of Bond & Malloy (2018)
        # http://thomasemalloy.org/arbsrm-the-general-social-relations-model/
        # http://thomasemalloy.org/wp-content/uploads/2017/09/arbcodeR.pdf        
        res <- SRM_ARBSRM_BOND_MALLOY(data=data, serror=serror)
    }
    est <- res$est
    se <- res$se
    time <- res$time 
        
    #-- arrange output table
    res <- SRM_ARBSRM_PROC_SUMMARY_OUTPUT(est=est, se=se)    
    par_summary <- res$par_summary
    coef <- res$coef
    
    #--- output
    s2 <- Sys.time()
    time$start <- s1
    time$end <- s2
    res <- list(par_summary=par_summary, coef=coef, est=est, se=se, data=data, serror=serror, 
                    bivariate=bivariate, time=time, use_srm=use_srm, CALL=CALL)    
    class(res) <- "srm_arbsrm"
    return(res)
}
