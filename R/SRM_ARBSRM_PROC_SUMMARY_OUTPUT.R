## File Name: SRM_ARBSRM_PROC_SUMMARY_OUTPUT.R
## File Version: 0.04


SRM_ARBSRM_PROC_SUMMARY_OUTPUT <- function(est, se)
{
    #- parameter names
    par_mat <- matrix("", nrow=6, ncol=3)
    par_mat[1,1] <- "Var(AX)"
    par_mat[2,1] <- "Var(PX)"
    par_mat[3,1] <- "Cov(AX,PX)"
    par_mat[5,1] <- "Var(RX)"
    par_mat[6,1] <- "Cov(RX,R'X)"
    par_mat[1,2] <- "Var(AY)"
    par_mat[2,2] <- "Var(PY)"
    par_mat[3,2] <- "Cov(AY,PY)"
    par_mat[5,2] <- "Var(RY)"
    par_mat[6,2] <- "Cov(RY,R'Y)"
    par_mat[1,3] <- "Cov(AX,AY)"
    par_mat[2,3] <- "Cov(PX,PY)"
    par_mat[3,3] <- "Cov(AX,PY)"
    par_mat[4,3] <- "Cov(PX,AY)"
    par_mat[5,3] <- "Cov(RX,RY)"
    par_mat[6,3] <- "Cov(RX,R'Y)"
    #- arrange output table
    par_summary <- data.frame( par=as.vector(par_mat), est=as.vector(est),
                                    se=as.vector(se) )
    par_summary <- par_summary[ paste(par_summary$par) !="", ]
    par_summary$t <- par_summary$est / par_summary$se
    par_summary$p <- 2*stats::pnorm( - abs(par_summary$t) )
    #- coefficient vector
    coef <- par_summary$est
    names(coef) <- par_summary$par    
    #--- output
    res <- list(par_summary=par_summary, coef=coef)
    return(res)
}
