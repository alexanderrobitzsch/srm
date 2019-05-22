## File Name: SRM_IDVARIABLE.R
## File Version: 0.11


SRM_IDVARIABLE <- function(TABLE) {

    index <- rep(NA,length(TABLE$lhs))
    
    # all free parameters that are no constrained get a single number
    idx <- which(is.na(TABLE$equal) & TABLE$free == 1)
    index[idx] <- seq(1,length(idx),1)
    last <- length(idx)
    
    # all free parameters that are constrainted get a number
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
    
    TABLE <- TABLE[c("group","lhs","op","rhs","index","mat",
                     "row","col","fixed","starts","user")]
    return(TABLE)
    
}
