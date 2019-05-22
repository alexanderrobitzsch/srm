## File Name: SRM_PARTABLE.R
## File Version: 0.06

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

SRM_PARTABLE_FIXEDVALUES <- function(LIST, name="EMPTY", ngroups = 1L) {

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
    
    } # else - Dyad
    
    LIST$equal <- equal
    return(LIST)

}
    
## the following two functions are very similar to 
## lavaan's lavaanify aka lav_partable functions
## SRM_PARTABLE_PERSON: lavaanify for persons
## SRM_PARTABLE_DYAD: lavaanify for dyads

SRM_PARTABLE_PERSON  <- function(PARLIST = NULL,
                                 as.a.data.frame=TRUE,
                                 ngroups = 1L) 

{
 
    ## for-loop for groups
    ## call SRM_PARTABLE_FLAT to add default elements 
    LIST <- SRM_PARTABLE_FLAT_PERSON(
                              PARLIST,
                              # definitions for default parameters
                              # 1. covariance actor-partner effects of one latent rr
                              auto.cov.lv.ap = TRUE,
                              # 2. covariance a-p effects of one observed rr
                              auto.cov.ov.ap = TRUE,
                              # 3. covariance a-p-effects of across latent rrs
                              auto.cov.lv.block = FALSE,
                              # 4. meanstructure
                              auto.int.ov = TRUE,
                              auto.int.lv = FALSE,
                              # definitions for fixed values
                              auto.fix.loa.first.ind.a=TRUE,
                              auto.fix.loa.first.ind.p=TRUE,
                              ngroups = ngroups )
                              
    ## now, we handling the modifiers fixed values and equality constraints
    if (as.a.data.frame) {
       LIST <- as.data.frame(LIST, stringsAsFactors = FALSE)
    }
    TABLE <- SRM_PARTABLE_ORDER(LIST,ngroups=ngroups) # sort the list
    TABLE <- SRM_PARTABLE_FIXEDVALUES(TABLE,name="Person",ngroups=ngroups)
    return(TABLE)
    
}       

SRM_PARTABLE_DYAD  <- function(PARLIST = NULL,
                               as.a.data.frame=TRUE,
                               ngroups = 1L) 

{
 
    ## here for-loop for groups?
    ## call SRM_PARTABLE_FLAT to add default elements 
    LIST <- SRM_PARTABLE_FLAT_DYAD(
                              PARLIST,       
                              # definitions for default parameters
                              # 1. covariance relationship effects of one latent rr
                              auto.cov.lv.dy = TRUE,
                              # 2. covariance relationship effects of one observed rr
                              auto.cov.ov.dy = TRUE,
                              # 3. covariance relationship-effects of across latent rrs
                              auto.cov.lv.block = FALSE,
                              # 4. meanstructure
                              auto.int.ov = FALSE,
                              auto.int.lv = FALSE,
                              # definitions for fixed values
                              auto.fix.loa.first.ind.ij=TRUE,
                              auto.fix.loa.first.ind.ji=TRUE,
                              auto.fix.loa.ind.ij.ji = TRUE,
                              auto.fix.int.first.ind.ij=FALSE,
                              auto.fix.int.first.ind.ji=FALSE,
                              ngroups = ngroups 
                              )
                              
    ## now, we handling the modifiers fixed values and equality constraints
    if (as.a.data.frame) {
       LIST <- as.data.frame(LIST, stringsAsFactors = FALSE)
    }
    TABLE <- SRM_PARTABLE_ORDER(LIST, ngroups = ngroups)
    TABLE <- SRM_PARTABLE_FIXEDVALUES(TABLE,name="Dyad",ngroups = ngroups)
    return(TABLE)

}             
