## File Name: SRM_INFO_INPUT_DATA.R
## File Version: 0.04

SRM_INFO_INPUT_DATA <- function(data_list)
{
    ngroups <- length(data_list)
    nrr <- unlist( lapply(data_list, FUN = function(ll){ nrow(ll$NI) } ) )
    npersons <- unlist( lapply(data_list, FUN = function(ll){ sum(ll$NI[,2]) } ) )
    ndyads <- unlist( lapply(data_list, FUN = function(ll){ sum(ll$ND[,2]) } ) )    
    #- output
    res <- list(ngroups=ngroups, nrr=nrr, npersons=npersons, ndyads=ndyads)
    return(res)
}
