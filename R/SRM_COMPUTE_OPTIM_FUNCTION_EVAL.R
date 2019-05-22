## File Name: SRM_COMPUTE_OPTIM_FUNCTION_EVAL.R
## File Version: 0.07

SRM_COMPUTE_OPTIM_FUNCTION_EVAL <- function(args_eval, method)
{
    #- ML estimation
    if (method == "ml"){
        res_ll <- do.call(what=SRM_COMPUTE_LOG_LIKELIHOOD, args=args_eval)    
    }    
    #- ULS estimation
    if (method == "uls"){    
        res_ll <- SRM_COMPUTE_ULS_FUNCTION(args_eval=args_eval)    
    }
    return(res_ll)
}
