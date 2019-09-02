## File Name: SRM_IDVARIABLE.R
## File Version: 0.25


SRM_IDVARIABLE <- function(TABLE, method="ml")
{

    index <- rep(NA,length(TABLE$lhs))
    if (method=="uls"){
        ind <- TABLE$mat == "BETA"
        TABLE[ ind, c("fixed","free") ] <- c(1,0)
    }

    # all free parameters that are no constrained get a single number
    idx <- which(is.na(TABLE$equal) & TABLE$free == 1)
    index[idx] <- seq(1,length(idx),1)
    last <- length(idx)

    # all free parameters that are constrained get a number
    idx <- which(!(is.na(TABLE$equal)) & TABLE$free == 1)
    name <- unique(TABLE$equal[idx])
    lauf <- last + 1
    for (i in 1:length(name)) {
        tmp <- which(TABLE$equal == name[i])
        index[tmp] <- lauf
        lauf <- lauf+1
    }

    TABLE$index <- index
    TABLE$equal <- NULL
    TABLE$mod.idx <- NULL
    TABLE_vars <- c("group","lhs","op","rhs","index","mat",
                     "row","col","fixed","starts","user",
                     "level")
    TABLE_vars <- intersect(colnames(TABLE), TABLE_vars)
    TABLE <- TABLE[TABLE_vars] # added by Steffen
    return(TABLE)

}
