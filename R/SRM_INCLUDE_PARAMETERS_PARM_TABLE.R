## File Name: SRM_INCLUDE_PARAMETERS_PARM_TABLE.R
## File Version: 0.02

SRM_INCLUDE_PARAMETERS_PARM_TABLE <- function( parm, parm.table)
{
    parm1 <- parm[ parm.table$index ]
    parm.table$est <- parm1
    ind <- ! is.na( parm.table$fixed )
    if (length(ind)>0){
        parm.table$est[ ind ] <- parm.table$fixed[ ind ]
    }
    return(parm.table)
}
