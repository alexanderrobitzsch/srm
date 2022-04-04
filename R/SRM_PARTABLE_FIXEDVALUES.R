## File Name: SRM_PARTABLE_FIXEDVALUES.R
## File Version: 0.18

SRM_PARTABLE_FIXEDVALUES <- function(LIST, name="EMPTY", ngroups = 1L)
{

    equal <- rep(NA,length(LIST$lhs))
    if (name=="Person")  {

       for (g in 1:ngroups) {

       idx <- which(!(is.na(LIST$equal)) & LIST$group == g )
       equal[idx] <- LIST$equal[idx]

       }

    } else if (name == "Dyad") {

      lauf2 <- 0
      for (g in 1:ngroups) {

      # we start with the loadings and regressions
      idx <- which(LIST$op %in% c("=~","~") & !(is.na(LIST$equal)) & LIST$group == g)
      equal[idx] <- LIST$equal[idx]

      # now, we compute the constraints for the "~~" parameters
      idx.names <- which(LIST$op == "~~" & grepl("@AP",LIST$lhs) & LIST$group == g)
      var.names <- unique(gsub("@AP","",LIST$lhs[idx.names]))
      lauf <- 1 + lauf2
      for (i in 1:length(var.names)) {
          tmp.idx1 <- which(LIST$op == "~~" & LIST$group == g &
                            gsub("@AP","",LIST$lhs) == var.names[i] &
                            gsub("@AP","",LIST$rhs) == var.names[i]  )
          tmp.idx2 <- which(LIST$op == "~~" & LIST$group == g &
                            gsub("@PA","",LIST$lhs) == var.names[i] &
                            gsub("@PA","",LIST$rhs) == var.names[i]  )
          tmp.idx <- c(tmp.idx1,tmp.idx2)
          ## if user - constrained
          if(any(LIST$user[tmp.idx] == 1)) {
             k.idx = which(LIST$user[tmp.idx] == 1)
             equal[tmp.idx] <- LIST$equal[k.idx][1]
          } else {
             equal[tmp.idx] <- paste("vv",lauf,sep="")
             lauf <- lauf + 1
          }

      } # for-loop
      lauf2 <- lauf2 + lauf

      } # for-loop groups

        ind <- ( ! is.na(LIST$equal) ) & ( is.na(equal) )
        if (sum(ind)>0){
            equal[ind] <- LIST$equal[ind]
        }

    } # else - Dyad
    LIST$equal <- equal
    return(LIST)

}
