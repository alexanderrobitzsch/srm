## File Name: SRM_MAKE_DATA_MATRIX_PERSON.R
## File Version: 0.312


SRM_MAKE_DATA_MATRIX_PERSON <- function( data = NULL,
                                         person_names = NULL,
                                         rrgroup_name = NULL,
                                         use_rcpp = TRUE )

{

     ##-- how many round-robin groups
     rrgroups <- unique(data[,rrgroup_name])
     nrr <- length(rrgroups)

     ##-- how many variables?
     no_vars <- length(unique(data$no_vars))
     res1 <- NULL
     res2 <- NULL

     for ( rr in 1:nrr ){

         ##-- get numbers
         idx <- which( data[,rrgroup_name] == rrgroups[rr] )
         tmp.data <- data[ idx, ]

         ##-- how many persons?
         persons <- sort(unique(c(tmp.data[,person_names[1]], tmp.data[,person_names[2]])))
         no_person <- length(persons)

         ##-- now we generate a list of data frame containing the position of
         ##   the elements in the design matrix for each dyad
         if (!use_rcpp){

             person_matrix_list <- lapply(1:no_person, function(x) {

                                     idx.rows = numeric()
                                     idx.cols = numeric()
                                     for (m in 1:no_vars) {

                                         tmp_actor <-  which(tmp.data[,person_names[1]]==persons[x] & tmp.data[,"no_vars"]==m)
                                         tmp_partner <- which(tmp.data[,person_names[2]]==persons[x] & tmp.data[,"no_vars"]==m)

                                         idx.rows <- c(idx.rows,tmp_actor,tmp_partner)
                                         idx.cols <- c(idx.cols,rep(m,length(tmp_actor)),rep(m+no_vars,length(tmp_partner)))

                                     }

                                     out <- data.frame(rrgroup = rr, pid = x, rows = idx.rows, cols = idx.cols)
                                     out = out[order(out$cols),]
                                     out

             })

            out1 <- as.matrix( do.call("rbind", person_matrix_list ))

        } else { # use Rcpp

            tmp_data3 <- as.matrix(tmp.data[, c(person_names, "no_vars")])
            out1 <- SRM_RCPP_SRM_MAKE_DATA_MATRIX_PERSON( tmp_data3 = tmp_data3,
                                                          no_person = no_person,
                                                          no_vars = no_vars,
                                                          rr = rr,
                                                          persons = persons )
            colnames(out1) <- c("rrgroup", "pid", "rows", "cols")
        }

        res1 <- rbind(res1, out1)
        res2 <- rbind(res2, as.matrix( data.frame( rrgroup = rr, NI = no_person )))

    }

    return( list( res1 = res1, res2 = res2 ))

}
