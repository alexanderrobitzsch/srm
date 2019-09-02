## File Name: SRM_MAKE_DATA_LIST.R
## File Version: 0.264


SRM_MAKE_DATA_LIST <- function( srm_data = NULL,
                                person_names = NULL,
                                rrgroup_name = NULL,
                                group.var = NULL,
                                rrvar_names=NULL,
                                personcov_names = NULL,
                                dyadcov_names = NULL,
                                fixed.groups = FALSE,
                                use_rcpp=TRUE,
                                do_checks=FALSE )
{
    #use_rcpp <- FALSE

    ##-- some checks
    if (do_checks){

       if ( length(unique(srm_data[,person_names[1]])) != length(unique(srm_data[,person_names[2]])) )  {
          stop("SRM ERROR: Number of actors does not match number of partners!")
       }

       if (!(identical(sort(unique(srm_data[,person_names[1]])),sort(unique(srm_data[,person_names[2]]))))) {
          stop("SRM ERROR: Actor-IDs does not match Partner-IDs!")
       }

    }

    ##-- if there is only one participant group add a rrgroup_name variable
    if ( is.null(group.var) ) {
        srm_data$group.var <- 1
        group.var <- "group.var"
    }

    ##-- if there is only one rr-group add a rrgroup_name variable
    if ( is.null(rrgroup_name) ) {
       srm_data$rrgroup <- 1
       rrgroup_name <- "rrgroup"
    }


    ##-- add a dyad number that is rrgroup specific
    #z0 <- Sys.time()
    srm_data <- SRM_PREPARE_ADD_DYADNUMBER( data = srm_data,
                                            person_names = person_names,
                                            rrgroup_name = rrgroup_name )
    # cat(" --- SRM_PREPARE_ADD_DYADNUMBER") ; z1 <- Sys.time(); print(z1-z0) ; z0 <- z1

    ##-- make a long data frame, sort the data
    srm_data <- SRM_PREPARE_DATA( data = srm_data,
                                  group.var = group.var,
                                  person_names = person_names,
                                  rrgroup_name = rrgroup_name,
                                  rrvar_names = rrvar_names,
                                  personcov_names = personcov_names,
                                  dyadcov_names = dyadcov_names )
    rrdata <- srm_data[["rrdata"]]
    pedata <- srm_data[["pedata"]]
    dydata <- srm_data[["dydata"]]

    # cat(" --- SRM_PREPARE_DATA") ; z1 <- Sys.time(); print(z1-z0) ; z0 <- z1

    #groups <- unique(srm_data[,group.var])
    groups <- unique(rrdata[,group.var])
    ngroups <- length(groups)
    data_list <- vector("list",ngroups)
    allnames <- c(group.var, rrgroup_name, person_names, "DyadNo_SRM", "no_vars", "y" )

    # cat(" --- start group iteration") ; z1 <- Sys.time(); print(z1-z0) ; z0 <- z1
    ##-- we iterate through the groups
    for (ng in 1:ngroups) {

         #- temporary data frame fpr rr-variables
         tmp_data <- rrdata[rrdata[,group.var] == groups[ng],]

         #- make the Xs matrix
         if ( fixed.groups ) {

            #- generate variable combining rr_group and measure
            tmp_data$tmpX <- interaction( tmp_data[,rrgroup_name], tmp_data[,"no_vars"] )
            #Xs <- outer(tmp_data[,"tmpX"], unique(tmp_data[,"tmpX"]), '==')*1
            c1 <- unique(tmp_data[,"tmpX"])
            n2 <- length(c1)
            n1 <- nrow(tmp_data)
            m1 <- matrix(tmp_data[,"tmpX"], nrow=n1, ncol=n2)
            m2 <- matrix(c1, nrow=n1, ncol=n2, byrow=TRUE)
            Xs <- 1*(m1 == m2)

         } else {

            # Xs = outer(tmp_data[,"no_vars"], unique(tmp_data[,"no_vars"]), '==')*1
            c1 <- unique(tmp_data[,"no_vars"])
            n2 <- length(c1)
            n1 <- nrow(tmp_data)
            m1 <- matrix(tmp_data[,"no_vars"], nrow=n1, ncol=n2)
            m2 <- matrix(c1, nrow=n1, ncol=n2, byrow=TRUE)
            Xs <- 1*(m1 == m2)
         }

         #- make the y-index matrix
         y_index <- tmp_data[,allnames]

         #- make the personcov - variable matrices
         if( !(is.null(pedata)) ) {
             Xu <- as.matrix( pedata[pedata[,group.var] == groups[ng],] )
         } else { Xu <- NULL }

         #- make the personcov - variable matrices
         if( !(is.null(dydata)) ) {
             Xd <- as.matrix( dydata[dydata[,group.var] == groups[ng],] )
         } else { Xd <- NULL }

         #- make the matrix lists
         pers_matrix_list <- SRM_MAKE_DATA_MATRIX_PERSON( data = tmp_data,
                                                          person_names = person_names,
                                                          rrgroup_name = rrgroup_name,
                                                          use_rcpp = use_rcpp )

         dyad_matrix_list <- SRM_MAKE_DATA_MATRIX_DYAD( data = tmp_data,
                                                        rrgroup_name = rrgroup_name,
                                                        use_rcpp = use_rcpp)

         #- add the lists to the overall list
         tmp_data_list <- list( y = tmp_data[,c(rrgroup_name,"y")] ,
                                Xs = Xs,
                                Zis = pers_matrix_list$res1,
                                Wds = dyad_matrix_list$res1 ,
                                NI = pers_matrix_list$res2,
                                ND = dyad_matrix_list$res2,
                                y_index = y_index,
                                Xu = Xu,
                                Xd = Xd )

         #- add to the overall list
         data_list[[ng]] <- tmp_data_list
     }
     # cat(" --- end group iteration") ; z1 <- Sys.time(); print(z1-z0) ; z0 <- z1
    attr(data_list, "rrgroup_name") <- rrgroup_name
    return(data_list)
}
