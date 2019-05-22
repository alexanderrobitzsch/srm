## File Name: SRM_INCLUDE_PARAMETERS_PARM_LIST.R
## File Version: 0.13

SRM_INCLUDE_PARAMETERS_PARM_LIST <- function( parm.table, parm_list,
    symm_matrices, include_fixed=FALSE )
{
    NPT <- nrow(parm.table)
    for (rr in seq_len(NPT) ){        
        val <- parm.table$est[rr]
        val_na <- is.na(val)
        if ( ( val_na & include_fixed ) |  ( ! val_na) ){    
            if ( is.na(val) ){
                val <- parm.table$fixed[rr]
            }
            if (parm.table$level[rr] == "U"){
                entr <- "parm_list_U"
            } else {
                entr <- "parm_list_D"
            }            
            mat_rr <- parm.table$mat[rr]
            r1 <- parm.table$row[rr]
            c1 <- parm.table$col[rr]
            group_rr <- parm.table$group[rr]
            parm_list[[ entr ]][[ group_rr ]][[ mat_rr ]][ r1, c1 ] <- val        
            if (mat_rr %in% symm_matrices){
                parm_list[[ entr ]][[ group_rr ]][[ mat_rr ]][c1, r1 ] <- val                        
            }            
        }                    
    }
    return(parm_list)
}
