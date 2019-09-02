## File Name: SRM_PRINT_SUMMARY_LAYOUT2.R
## File Version: 0.01

SRM_PRINT_SUMMARY_LAYOUT2 <- function(object, digits)
{
    #- print call
    SRM_SUMMARY_PRINT_CALL(object=object)

    #* R session info
    SRM_SUMMARY_PACKAGE_VERSIONS(object=object)

    #* print informations about computation time
    parm.table <- object$parm.table
    cat("\nDate of analysis:", paste(object$time_start),"\n")
    cat("Time pre-processing: "); print(object$time$time_pre)
    cat("Time estimation: "); print(object$time$time_opt)
    cat("Time post-processing: "); print(object$time$time_post)

    #* information about estimation
    cat("\nINFORMATIONS ABOUT ESTIMATION\n\n")
    cat("Log-likelihood","=", round(object$loglike, digits),"\n")
    cat("Deviance","=", round(object$dev, digits),"\n")
    cat("Number of estimated parameters","=", attr(parm.table, "npar"), "\n")
    res_opt <- object$res_opt
    cat("Converged","=", res_opt$converged, "\n")
    cat("Number of iterations","=", res_opt$iter, "\n")
    cat("optimizer","=", res_opt$optimizer, "\n")
    cat("Optimization function","=", res_opt$opt_label, "\n")
    cat("Maximum absolute value of relative gradient", "=", object$grad_maxabs, "\n")

    #* display parameter table
    cat("\nESTIMATED PARAMETERS\n\n")
    # select columns
    sel <- c("index", "group", "lhs", "op", "rhs", "mat", "fixed", "est", "se", "lower")
    obji <- parm.table
    obji <- obji[, sel]
    round_vars <- c("est","se")
    obji[,round_vars] <- round( obji[,round_vars], digits)
    print(obji)
}
