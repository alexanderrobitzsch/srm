## File Name: SRM_MAKE_DATA_LIST.R
## File Version: 0.226

SRM_MAKE_DATA_LIST <- function( srm_data = NULL,
                                person_names = NULL,
                                rrgroup_name = NULL,
                                group.var = NULL,
                                fixed.groups = FALSE, var_names=NULL )

{

     ##-- some checks
     if( length(unique(srm_data[,person_names[1]])) != length(unique(srm_data[,person_names[2]])) )  {
       stop("SRM ERROR: Number of actors does not match number of partners!")
     }
   
     if (!(identical(sort(unique(srm_data[,person_names[1]])),sort(unique(srm_data[,person_names[2]]))))) {
       stop("SRM ERROR: Actor-IDs does not match Partner-IDs!")
     }
     
     ##-- if there is only one rr-group add a rrgroup_name variable
     if ( is.null(rrgroup_name) ) {
       srm_data$rrgroup = 1
       rrgroup_name = "rrgroup"
     }

     ##-- add a dyad number that is rrgroup specific
z0 <- Sys.time()     
     srm_data = SRM_PREPARE_ADD_DYADNUMBER(data = srm_data, person_names = person_names, rrgroup_name = rrgroup_name )
# cat(" *** SRM_PREPARE_ADD_DYADNUMBER") ; z1 <- Sys.time(); print(z1-z0) ; z0 <- z1    
     
     ##-- make a long data frame, sort the data
     srm_data = SRM_PREPARE_DATA( data = srm_data, person_names = person_names , 
                rrgroup_name = rrgroup_name, var_names = var_names )
# cat(" *** SRM_PREPARE_DATA") ; z1 <- Sys.time(); print(z1-z0) ; z0 <- z1    
                
     ##-- now, we generate a list of data frames for each SRM-SEM group
     if ( is.null(group.var) ) { srm_data$group.var = 1; group.var = "group.var" } 
     
     groups = unique(srm_data[,group.var])
     ngroups = length(groups)
     data_list = vector("list",ngroups)

# cat(" *** start group iteration") ; z1 <- Sys.time(); print(z1-z0) ; z0 <- z1         
     ##-- we iterate through the groups
     for (ng in 1:ngroups) {

         #- a temporary data-frame
         tmp_data = srm_data[srm_data[,group.var] == groups[ng],] 
         
         #- make the Xs matrix
         if ( fixed.groups ) {
              # generate variable combining rr_group and measure
              tmp_data$tmpX <- interaction( tmp_data[,rrgroup_name], tmp_data[,"no_vars"] )
              Xs = outer(tmp_data[,"tmpX"], unique(tmp_data[,"tmpX"]), '==')*1
         } else {
              Xs = outer(tmp_data[,"no_vars"], unique(tmp_data[,"no_vars"]), '==')*1
         }
         
         #- make the y-index matrix
         y_index <- tmp_data[,c(group.var,rrgroup_name,"Actor","Partner","DyadNo_SRM","no_vars","y")]
         
         #- make the matrix-lists
         pers_matrix_list = SRM_MAKE_DATA_MATRIX_PERSON( data = tmp_data, person_names = person_names, 
                                    rrgroup_name = rrgroup_name)
         dyad_matrix_list = SRM_MAKE_DATA_MATRIX_DYAD( data = tmp_data, rrgroup_name = rrgroup_name )

         #- add the lists to the overall list
         tmp_data_list = list( y = tmp_data[,c(rrgroup_name,"y")] , 
                               Xs = Xs,
                               Zis = pers_matrix_list$res1, 
                               Wds = dyad_matrix_list$res1 , 
                               NI = pers_matrix_list$res2, 
                               ND = dyad_matrix_list$res2,
                               y_index = y_index )        
         #- add to the overall list
         data_list[[ng]] <- tmp_data_list

     }
# cat(" *** end group iteration") ; z1 <- Sys.time(); print(z1-z0) ; z0 <- z1              

     return(data_list)

}
