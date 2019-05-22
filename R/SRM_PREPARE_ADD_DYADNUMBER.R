## File Name: SRM_PREPARE_ADD_DYADNUMBER.R
## File Version: 0.12

SRM_PREPARE_ADD_DYADNUMBER <- function(data = NULL,
                                       person_names = NULL,
                                       rrgroup_name = NULL ) {

     ## here we compute a unique dyad number for 
     ## 1-2 and 2-1 etc. combinations in each round-robin group
     
     rrgroups = unique(data[,rrgroup_name])
     nrr = length( rrgroups )
     data$DyadNo_SRM = NA
     data$DyadNo_SRM_type = NA
     
     for (rr in 1:nrr) {
     
         ##-- get numbers 
         idx = which( data[,rrgroup_name] == rrgroups[rr] )
                 
         ##-- make the first dyadic identifier: ij and ji are given the same number
         data[idx,]$DyadNo_SRM = apply(data[idx,person_names],1,function(x) { as.numeric(paste(sort(x), collapse="")) })
     
         ##-- make a second identifier: is 1 for ij and 2 for ji
         data[idx,]$DyadNo_SRM_type = stats::ave(data[idx,person_names[1]],data[idx,"DyadNo_SRM"], FUN=seq_along)
     
     }
     
     return(data)

} 
