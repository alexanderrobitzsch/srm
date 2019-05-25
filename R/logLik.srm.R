## File Name: logLik.srm.R
## File Version: 0.04

logLik.srm <- function (object, ...)
{
    out <- object$loglike
    attr(out, "df") <- length(object$coef)
    attr(out, "nobs") <- NA
    class(out) <- "logLik"
    return(out)
}
