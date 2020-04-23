## File Name: SRM_PARTABLE_ORDER.R
## File Version: 0.11


SRM_PARTABLE_ORDER <- function(LIST, ngroups = 1L) {

    NEWLIST <- data.frame()

    for (g in 1:ngroups) {

      tmp <- subset(LIST,LIST$op=="=~" & LIST$group==g )
      if (nrow(tmp) > 0L) {
          tmp <- tmp[order(tmp$lhs,tmp$rhs),]
          NEWLIST <- rbind(NEWLIST,tmp)
      }

      tmp <- subset(LIST,LIST$op=="~" & LIST$group==g)
      if (nrow(tmp) > 0L) {
         tmp <- tmp[order(tmp$lhs,tmp$rhs),]
         NEWLIST <- rbind(NEWLIST,tmp)
      }

      tmp <- subset(LIST,LIST$op=="~~" & LIST$group==g)
      if (nrow(tmp) > 0L) {
         tmp <- tmp[order(tmp$lhs,tmp$rhs),]
         NEWLIST <- rbind(NEWLIST,tmp)
      }

      tmp <- subset(LIST,LIST$op=="~1" & LIST$group==g)
      if (nrow(tmp) > 0L) {
         tmp <- tmp[order(tmp$lhs,tmp$rhs),]
         NEWLIST <- rbind(NEWLIST,tmp)
      }

    }

    return(NEWLIST)

}
