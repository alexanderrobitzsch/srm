## File Name: SRM_EQUAL_MATRICES.R
## File Version: 0.03

SRM_EQUAL_MATRICES <- function(x1, x2, elim=NULL)
{
    if ( ! is.null(elim) ){
        x1 <- x1[ , -elim, drop=FALSE]
        x2 <- x2[ , -elim, drop=FALSE]
    }
    if (nrow(x1) != nrow(x2) ){
        val <- FALSE
    } else {
        val <- ! any(x1 != x2)
    }
    return(val)
}
