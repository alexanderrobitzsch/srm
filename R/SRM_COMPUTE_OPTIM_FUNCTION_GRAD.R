## File Name: SRM_COMPUTE_OPTIM_FUNCTION_GRAD.R
## File Version: 0.01

SRM_COMPUTE_OPTIM_FUNCTION_GRAD <- function(args_grad, method)
{
    #- ML estimation
    if (method == "ml"){
        res <- do.call(what=SRM_COMPUTE_LOG_LIKELIHOOD_GRADIENT, args=args_grad)    
    }    
    #- ULS estimation
    if (method == "uls"){    
        res <- SRM_COMPUTE_ULS_GRADIENT(args_grad=args_grad)        
    }

    return(res)
}
