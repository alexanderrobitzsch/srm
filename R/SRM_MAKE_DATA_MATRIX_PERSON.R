## File Name: SRM_MAKE_DATA_MATRIX_PERSON.R
## File Version: 0.21


SRM_MAKE_DATA_MATRIX_PERSON <- function(data = NULL,
                                        person_names = NULL,
                                        rrgroup_name = NULL ) 

{

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
         
         ##-- how many persons?
         no_person = length(unique(tmp.data[,person_names[1]]))
         persons = unique(tmp.data[,person_names[1]])
     
         ##-- now we generate a list of data frame containing the position of 
         ##   the elements in the design matrix for each dyad
         person_matrix_list = lapply(1:no_person,function(x) {
                                 
                                 idx.rows = numeric()
                                 idx.cols = numeric()
                                 
                                 for (m in 1:no_vars) {
                                 
                                     tmp_actor <-  which(tmp.data[,person_names[1]]==persons[x] & tmp.data[,"no_vars"]==m)
                                     tmp_partner <-  which(tmp.data[,person_names[2]]==persons[x] & tmp.data[,"no_vars"]==m)
                                 
                                     idx.rows <- c(idx.rows,tmp_actor,tmp_partner)
                                     idx.cols <- c(idx.cols,rep(m,length(tmp_actor)),rep(m+no_vars,length(tmp_partner)))
                                  
                                 }
                                 
                                 out <- data.frame(rrgroup = rr, pid = x, rows = idx.rows, cols = idx.cols)
                                 out = out[order(out$cols),]
                                 out
         })
     
         res1 = rbind(res1, as.matrix( do.call ("rbind", person_matrix_list )))
         res2 = rbind(res2, as.matrix( data.frame( rrgroup = rr, NI = no_person )) )
     
     }
     
     return( list( res1 = res1, res2 = res2 ))

}
