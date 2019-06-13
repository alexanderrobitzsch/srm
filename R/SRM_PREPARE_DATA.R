## File Name: SRM_PREPARE_DATA.R
## File Version: 0.281

## all functions assume that data is sorted in a specific way
## make this explicit!

SRM_PREPARE_DATA <- function(data = NULL,
                             person_names = NULL,
                             rrgroup_name = NULL,
                             var_names = NULL)

{

    #-- makes a data frame in long format (data for each variable
    #   is written below each other
    allnames = names(data)
    allnames = allnames[-which(allnames %in% var_names)]
    allnames <- setdiff(allnames, c("y"))
     #-- we generate a list of dataframes (one dataframe for one variable)
     new_data = lapply(1:length(var_names), function(x) {
                                            out = data[,c(allnames,var_names[x])];
                                            out$no_vars = x;
                                            names(out)[which(names(out) == var_names[x])] = "y";
                                            return(out)
     })
     #-- now we bind the data-frames together
     new_data = do.call("rbind",new_data)

    new_data <- new_data[ ! is.na(new_data$y) , ]

    #-- we sort the data
     new_data = new_data[order(new_data$no_vars,
                               new_data[,rrgroup_name],
                               new_data[,person_names[1]],
                               new_data[,person_names[2]]),]

     return(new_data)

}

