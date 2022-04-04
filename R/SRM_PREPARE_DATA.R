## File Name: SRM_PREPARE_DATA.R
## File Version: 0.303


## all functions assume that data is sorted in a specific way
## make this explicit!

SRM_PREPARE_DATA <- function( data = NULL,
                              group.var = NULL,
                              person_names = NULL,
                              rrgroup_name = NULL,
                              rrvar_names = NULL,
                              personcov_names = NULL,
                              dyadcov_names = NULL )

{

    #-- makes a data frame in long format for the round robin
    #   variables, the person covariates and the dyadic covariates

    #-- make a vector with default variables
    allnames <- c(group.var,rrgroup_name,person_names,"DyadNo_SRM","DyadNo_SRM_type")

    #-- the data frame for the rr-variables
    rrdata <- lapply(1:length(rrvar_names), function(x) {
                                            out <- data[,c(allnames,rrvar_names[x])];
                                            out$no_vars <- x;
                                            names(out)[which(names(out) == rrvar_names[x])] = "y";
                                            return(out)
    })
    rrdata <- do.call("rbind",rrdata)

    #-- exclude missings
    rrdata <- rrdata[ !is.na( rrdata$y ) , ]

    #-- add number of data points per rr-group
    rrdata$rrcount <- ave( rrdata$y,
                           rrdata[,c(group.var,rrgroup_name)],
                           FUN = length )

    #-- sort the data frame
    rrdata <- rrdata[order(  rrdata[,group.var],
                            -rrdata$rrcount,
                             rrdata[,rrgroup_name],
                             rrdata$no_vars,
                             rrdata[,person_names[1]],
                             rrdata[,person_names[2]] ),]

    #-- make person data frame
    if ( !identical( personcov_names, character(0) ) ) {
       pedata <- data[,c(allnames,personcov_names)]
       select <- !duplicated(pedata[c(group.var,rrgroup_name,person_names[1])])
       pedata <- pedata[select,]
       pedata[,person_names[2]] <- pedata[,person_names[1]]
       pedata <- pedata[,-which(names(pedata) %in% c("DyadNo_SRM","DyadNo_SRM_type"))]

       pedata <- pedata[order( pedata[,group.var],
                               pedata[,rrgroup_name],
                               pedata[,person_names[1]] ),]

    } else { pedata <- NULL }

    #-- make dyad data frame
    if ( !identical( dyadcov_names, character(0) ) ) {
       dydata <- data[,c(allnames,dyadcov_names)]
       dydata <- dydata[,-which(names(dydata) == "DyadNo_SRM_type")]
       dydata <- dydata[order( dydata[,group.var],
                               dydata[,rrgroup_name],
                               dydata[,person_names[1]],
                               dydata[,person_names[2]] ),]
    } else { dydata = NULL }

    res <- list( rrdata = rrdata, pedata = pedata, dydata = dydata )
    return( res )

}
