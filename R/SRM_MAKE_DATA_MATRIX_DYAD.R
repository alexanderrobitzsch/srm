## File Name: SRM_MAKE_DATA_MATRIX_DYAD.R
## File Version: 0.267



SRM_MAKE_DATA_MATRIX_DYAD <- function(data = NULL,
                                      dyad_names = c("DyadNo_SRM","DyadNo_SRM_type"),
                                      rrgroup_name = NULL ) 

{

     ## IMPORTANT: the function presumes that dyads
     ## --- hier wird angenommen, dass pro Maß jeweils erst ij, dann ji, dann ij...
    
     ##-- how many round-robin groups
     rrgroups = unique(data[,rrgroup_name])
     nrr = length( rrgroups )
     
     ##-- how many variables?
     no_vars = length(unique(data$no_vars))
     res1 = NULL
     res2 = NULL
     
     for ( rr in 1:nrr ) {
     
         ##-- get numbers 
         idx = which( data[,rrgroup_name] == rrgroups[rr] )
         tmp.data = data[ idx, ]
    
         ##-- how many dyads?
         no_dyads =  length(unique(tmp.data[,dyad_names[1]]))
         dyads = unique(tmp.data[,dyad_names[1]])
    
         ##-- now we generate a list of data frame containing the position of 
         ##   the elements in the design matrix for each dyad
         tmp1 <- tmp.data[,dyad_names[2]]==1
         tmp2 <- tmp.data[,dyad_names[2]]==2
         # ind_dyads <- match(dyads, tmp.data[,dyad_names[1]])
        
        no_vars_ind <- list()
        for (m in 1:no_vars){
            no_vars_ind[[m]] <- (tmp.data[,"no_vars"]==m)
         }
         dyad_matrix_list = lapply(1:no_dyads, function(x) {

                              idx.rows = numeric()
                              idx.cols = numeric()
                             
                              pos.cols = c(1,2)   # new: 17122018
                            index1 <- (tmp.data[,dyad_names[1]]==dyads[x])                                 
                              for (m in 1:no_vars) {
                                  # index <- (tmp.data[,dyad_names[1]]==dyads[x]) & (tmp.data[,"no_vars"]==m)
                                  index <- index1 & no_vars_ind[[m]]

                                  tmp_ij <- which(index & tmp1)
                                  tmp_ji <- which(index & tmp2)
                                  idx.rows <- c(idx.rows, tmp_ij, tmp_ji)
                                  # idx.cols <- c(idx.cols, pos.cols)   # new: 17122018
                                  # pos.cols <- pos.cols + no_vars    # new: 17122018
                                  if (tmp_ij>0){
                                    idx.cols <- c(idx.cols, pos.cols[1])                                  
                                  }
                                  if (tmp_ji>0){
                                    idx.cols <- c(idx.cols, pos.cols[2])                                  
                                  }                                  
                                  pos.cols <- pos.cols + 2  # change ARb 2019-01-02
                              }
                              
                              out <- data.frame(rrgroup = rr, did = x, rows=idx.rows,cols=idx.cols)                              
                              out
          }) 
          res1 = rbind(res1, as.matrix( do.call(rbind, dyad_matrix_list )))
          res2 = rbind(res2, as.matrix( data.frame( rrgroup = rr, ND = no_dyads )) )
    }
    
    return( list( res1 = res1, res2 = res2 ))

}
