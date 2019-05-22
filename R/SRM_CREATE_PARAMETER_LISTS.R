## File Name: SRM_CREATE_PARAMETER_LISTS.R
## File Version: 0.03

SRM_CREATE_PARAMETER_LISTS <- function(parm.table, ngroups)
{
    mmNames <- attr(parm.table, "mmNames")    
    parm_list_U <- as.list( seq_len(ngroups) )
    parm_list_D <- as.list( seq_len(ngroups) )    
    
    for (hh in 1:2){
        for (gg in seq_len(ngroups)){
            ii <- ngroups*(hh-1) + gg
            NM <- length(mmNames[[ii]])
            parm_list0 <- as.list( seq_len(NM) )                    
            for (mm in 1:NM){
                name_mm <- mmNames[mm]
                mat_mm <- matrix(0, nrow=attr(parm.table,"mmRows")[[ii]][mm],
                                    ncol=attr(parm.table,"mmCols")[[ii]][mm] )
                parm_list0[[mm]] <- mat_mm
            }
            names(parm_list0) <- mmNames[[ii]]
            if (hh==1){
                parm_list_U[[gg]] <- parm_list0
            }
            if (hh==2){
                parm_list_D[[gg]] <- parm_list0
            }    
        }    
    }
    
    #- output
    res <- list( parm_list_U = parm_list_U, parm_list_D = parm_list_D)
    return(res)
}
