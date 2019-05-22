## File Name: SRM_PARTABLE_EXTEND.R
## File Version: 0.04

SRM_PARTABLE_EXTEND <- function(parm.table, var_positive, optimizer)
{
    symm_matrices <- c("PHI_U", "PSI_U", "PHI_D", "PSI_D")
    npar <- attr(parm.table, "npar")        
    parm.table$est <- parm.table$starts
    parm.table$est[ is.na(parm.table$starts) ] <- parm.table$fixed[ is.na(parm.table$starts) ]
    parm.table$starts[ ! is.na(parm.table$fixed) ] <- parm.table$fixed[ ! is.na(parm.table$fixed) ]
    parm.table$est[ ! is.na(parm.table$fixed) ] <- parm.table$fixed[ ! is.na(parm.table$fixed) ]
    parm.table$lower <- -Inf
    if (var_positive>=0){
        ind1 <- union( grep("PHI", parm.table$mat), grep("PSI", parm.table$mat) )
        ind2 <- which(parm.table$row == parm.table$col)
        parm.table$lower[ intersect(ind1,ind2)] <- var_positive        
        if (optimizer=="srm"){
            optimizer <- "nlminb"
        }        
    }    
    parm_table_free <- parm.table[ ! is.na(parm.table$index), ]
    parm_table_free <- parm_table_free[ order(parm_table_free$index), ]
    # define lower bounds
    lower <- parm_table_free[ parm_table_free$unid > 0, 'lower']    
    NOP <- nrow(parm_table_free)

    #- available optimizers
    optim_avai <- c("srm", "nlminb", "spg")
    if (! (optimizer %in% optim_avai) ){
        stop(paste0("Choose among following optimizers:\n", 
                    paste0(optim_avai, collapse=" "), "\n" ))
    }
    
    #--- output
    res <- list(parm.table=parm.table, parm_table_free=parm_table_free,
                    lower=lower, upper=NULL, NOP=NOP, npar=npar, symm_matrices=symm_matrices,
                    optimizer=optimizer)
    return(res)
}
