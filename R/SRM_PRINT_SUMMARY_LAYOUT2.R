## File Name: SRM_PRINT_SUMMARY_LAYOUT2.R
## File Version: 0.08

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

    cat("\nINPUT DATA\n\n")
    cat("Number of groups","=", object$ngroups, "\n")
    cat("Number of Round-Robin groups","=", object$nrr, "\n")
    cat("Number of persons","=", object$npersons, "\n")
    cat("Number of dyads","=", object$ndyads, "\n")

    #* display parameter table
    cat("\nESTIMATED PARAMETERS\n\n")
    # select columns
    sel <- c("index", "group", "lhs", "op", "rhs", "mat", "row", "col",
                    "fixed", "est", "se", "lower")
    obji <- parm.table
    obji <- obji[, sel]
    round_vars <- c("est","se")
    obji[,round_vars] <- round( obji[,round_vars], digits)
    print(obji)
    
    #*** model implied covariance matrices
    cat("\nMODEL IMPLIED COVARIANCE MATRICES\n")    
    for (gg in 1:object$ngroups){
        cat(paste0("\nGroup ", gg, ", Person Level\n\n"))
        obji <- object$sigma[["U"]][[gg]]
        print(round(obji, digits))
        cat(paste0("\nGroup ", gg, ", Dyad Level\n\n"))
        obji <- object$sigma[["D"]][[gg]]
        print(round(obji, digits))        
    }
    
    
}
