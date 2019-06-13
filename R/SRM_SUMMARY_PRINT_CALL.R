## File Name: SRM_SUMMARY_PRINT_CALL.R
## File Version: 0.03

SRM_SUMMARY_PRINT_CALL <- function(object)
{
    s3 <- paste0(object$CALL, collapse = " ")
    if (nchar(s3) < 1000) {
        cat("\nCall:\n", paste(deparse(object$CALL), sep = "\n", collapse = "\n"),
                    "\n\n", sep = "")
    }
}
