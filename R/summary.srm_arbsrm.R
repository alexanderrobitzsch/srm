## File Name: summary.srm_arbsrm.R
## File Version: 0.07


summary.srm_arbsrm <- function(object, digits=3, file=NULL, ...)
{
    #-- open sink
    SRM_OPEN_SINK(file=file)

    #- print call
    SRM_SUMMARY_PRINT_CALL(object=object)

    #* R session info
    SRM_SUMMARY_PACKAGE_VERSIONS(object=object)

    #* print informations about computation time
    cat("\nDate of analysis:", paste(object$time$start),"\n\n")
    cat("Time elapsed total: "); print(object$time$end-object$time$start)
    cat("  Time elapsed estimation: "); print(object$time$time_est)
    cat("  Time elapsed standard errors: "); print(object$time$time_se)

    #* display parameter table
    cat("\nESTIMATED PARAMETERS\n\n")
    SRM_SUMMARY_PRINT_OUTPUT(obji=object$par_summary, round_from=2, digits=digits)

    #* close sink
    SRM_CLOSE_SINK(file=file)
}
