## File Name: SRM_PRINT_OPTINFOS.R
## File Version: 0.12

SRM_PRINT_OPTINFOS <- function( object = NULL,
                                digits = 3L )
{

     #- get part of result_object that containts optimizer infos
     srm_optimizer_info <- object$res_opt
     srm_yindex <- object$data_list[[1]]$y_index

     #- convergence info:
     conv_info <- "converged."
     if ( !srm_optimizer_info$converged ) { conv_info <- "NOT converged." }

     #- define the text stimuli
     texts <- c("The optimizer is ",
                "The estimator has ",
                "Number of iterations: ",
                "",
                "The number of estimated parameters is: ",
                "Log-likelihood: ",
                "Deviance: ",
                "",
                "Number of groups: ",
                "Number of round-robin groups: ",
                "Number of persons: ",
                "Number of dyads: "
                )

     values <- c(srm_optimizer_info$optimizer,
                 conv_info,
                 srm_optimizer_info$iter,
                 "",
                 length( srm_optimizer_info$par),
                 round( srm_optimizer_info$value, digits),
                 -2*round( srm_optimizer_info$value, digits),
                 "",
                 length( unique( srm_yindex$group.var ) ),
                 length( unique( srm_yindex$Group ) ),
                 object$npersons, object$ndyads     )

     #- print texts
     char.format <- paste("%3s%-", 14, "s", sep = "")
     cat("\nOPTIMIZATION\n\n")

     for ( i in 1:length( texts ) ) {
         tmp <- paste( sprintf( char.format, "", texts[i] ),
                       values[i],
                       sep = "" )
         #message(tmp)
         cat(tmp)
         cat("\n")
     }

     cat("\n")

}
