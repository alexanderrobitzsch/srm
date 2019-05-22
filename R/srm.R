## File Name: srm.R
## File Version: 0.5891


srm <- function(model.syntax = NULL, 
                data = NULL,
                group.var = NULL,
                rrgroup_name = NULL,
                person_names = c("Actor","Partner"),
                fixed.groups=FALSE, var_positive=-1, optimizer="srm",
                maxiter=300, conv_dev=1e-8, conv_par=1e-6, 
                do_line_search=TRUE, line_search_iter_max=6, 
                verbose=TRUE, use_rcpp=TRUE, shortcut=TRUE)
{

    CALL <- match.call()
    v1 <- s1 <- Sys.time()
    method <- "ml"
    inv_type <- "pinv"
z0 <- Sys.time()    
    
    ## Step 0: Determine the number of groups
    if (is.null(group.var)) { 
        ngroups = 1L 
    } else { 
        ngroups = length(unique(data[,c(group.var)])) 
    }
    
    ## Step 1: use model syntax to generate a parameter table        
    parm.table <- SRM_PARTABLE_MAKE(model.syntax = model.syntax, ngroups = ngroups,
                        data_colnames = colnames(data) )
    var_names <- attr(parm.table, "var_names")
# cat(" *** make partable ") ; z1 <- Sys.time(); print(z1-z0) ; z0 <- z1

    #-- add more information to parameter table
    # parm.table <- SRM_PARTABLE_ADD_NOISE(parm.table=parm.table, sd=.1)
    res <- SRM_PARTABLE_EXTEND( parm.table=parm.table, var_positive=var_positive, 
                    optimizer=optimizer ) 
    parm.table <- res$parm.table
    parm_table_free <- res$parm_table_free
    lower <- res$lower
    upper <- res$upper
    NOP <- res$NOP
    npar <- res$npar
    symm_matrices <- res$symm_matrices
    optimizer <- res$optimizer    
# cat(" *** refine partable ") ; z1 <- Sys.time(); print(z1-z0) ; z0 <- z1    
    
    # create lists of model parameters
    parm_list <- SRM_CREATE_PARAMETER_LISTS(parm.table = parm.table, ngroups = ngroups )

    parm_list <- SRM_INCLUDE_PARAMETERS_PARM_LIST( parm.table=parm.table, 
                parm_list=parm_list, symm_matrices=symm_matrices, include_fixed=TRUE ) 
# cat(" *** create parameter lists") ; z1 <- Sys.time(); print(z1-z0) ; z0 <- z1

                    
    ## Step 2: make list with data
    data_list <- SRM_MAKE_DATA_LIST(srm_data = data, person_names = person_names ,
                            rrgroup_name = rrgroup_name, group.var = group.var,
                            fixed.groups = fixed.groups, var_names=var_names)
# cat(" *** make data list") ; z1 <- Sys.time(); print(z1-z0) ; z0 <- z1                                   
                               
    # make design matrices
    data_list <- SRM_CREATE_DESIGN_MATRICES( data_list = data_list, 
                        ngroups = ngroups, use_rcpp=use_rcpp)    
# cat(" *** create design matrices") ; z1 <- Sys.time(); print(z1-z0) ; z0 <- z1    
 
    ## Step 3: estimate the model                
    x0 <- parm.table[ attr(parm.table, "parm_extract"), "est"]
    parm_list <- SRM_INCLUDE_PARAMETERS_PARM_LIST( parm.table=parm.table, 
            parm_list=parm_list, symm_matrices=symm_matrices, include_fixed=TRUE )                         

    #- define optimization function for stats::nlminb()
    srm_optim_function <- function(x, parm.table, parm_list, symm_matrices,
        npar, data_list, NOP, parm_table_free, only_value=FALSE, only_gradient=FALSE)
    {
        compute_gradient <- ! only_value
        res <- SRM_OPTIMIZE(x=x, parm.table=parm.table, parm_list=parm_list, 
                    symm_matrices=symm_matrices, npar=npar,    data_list=data_list, 
                    NOP=NOP, parm_table_free=parm_table_free, use_rcpp=use_rcpp, 
                    shortcut=shortcut, compute_gradient=compute_gradient, 
                    compute_hessian=FALSE, method=method, inv_type=inv_type)
        if (only_value){
            val <- -res$ll_new
        } else {
            if (only_gradient){
                val <- -res$grad
            } else {
                val <- list( objective=-res$ll_new, gradient=-res$grad)            
            }
        }        
        return(val)    
    }
    
    #- optimization function for external optimizers
    srm_objective <- function(x, parm.table, parm_list, symm_matrices,
            npar, data_list, NOP, parm_table_free, method)
            {
                res <- srm_optim_function( x=x, parm.table=parm.table, parm_list=parm_list, 
                        symm_matrices=symm_matrices, npar=npar, data_list=data_list, NOP=NOP, 
                        parm_table_free=parm_table_free, only_value=TRUE, only_gradient=FALSE)
                return(res)
            }
    srm_gradient <- function(x, parm.table, parm_list, symm_matrices,
            npar, data_list, NOP, parm_table_free, method)
            {
                res <- srm_optim_function( x=x, parm.table=parm.table, parm_list=parm_list, 
                        symm_matrices=symm_matrices, npar=npar, data_list=data_list, NOP=NOP, 
                        parm_table_free=parm_table_free, only_value=FALSE, only_gradient=TRUE)            
                return(res)
            }            

    # collect optimization function arguments
    args_opt0 <-  list( parm.table=parm.table, parm_list=parm_list, 
                    symm_matrices=symm_matrices, npar=npar,    data_list=data_list, 
                    NOP=NOP, parm_table_free=parm_table_free)                        
    args_fs <- args_opt0
    args_fs$use_rcpp <- use_rcpp
    args_fs$shortcut <- shortcut    
    args_fs$compute_hessian <- TRUE                    
    s2 <- Sys.time()
    time_pre <- s2-s1
    
    # args_fs$method <- method
    # args_opt0$method <- method
    
    #--- optimization
    if (optimizer=="srm"){
        #*** internal optimizer: optimization of the log-likelihood with Fisher scoring
        res <- SRM_OPTIMIZER_INTERNAL(x0=x0, fn=SRM_OPTIMIZE, args_fs=args_fs, 
                    conv_dev=conv_dev, conv_par=conv_par, maxiter=maxiter, 
                    do_line_search=do_line_search, line_search_iter_max=line_search_iter_max, 
                    verbose=verbose)
    } else {
        #*** external optimizer: stats::nlminb
        res <- SRM_OPTIMIZER_EXTERNAL( optimizer=optimizer, x0=x0, fn=srm_objective, 
                    gr=srm_gradient, args_opt0=args_opt0, maxiter=maxiter, lower=lower, 
                    upper=upper, conv_dev=conv_dev, conv_par=conv_par, verbose=verbose )             
    }    
    res_opt <- res
    time_opt <- res$time_diff
    coef <- x0 <- res$par
    names(coef) <- attr(parm.table, "par_names")
    
    #- compute Hessian matrix
    s1 <- Sys.time()
    args_fs$x <- x0
    res <- do.call(what=SRM_OPTIMIZE, args=args_fs)
    parm.table <- as.data.frame(res$parm.table)
    loglike <- res$ll_new
    dev <- -2*loglike
    grad <- res$grad
    grad_maxabs <- max(abs(grad)) / abs(loglike)
    infomat <- res$expected_infomat
    vcov <- SRM_GINV(x=-infomat)
    rownames(vcov) <- colnames(vcov) <- names(coef)

    parm_list <- res$parm_list
    # compute standard errors
    se <- sqrt(diag(vcov))
    parm.table$se <- se[ parm.table$index ]    
        
    s2 <- Sys.time()
    time <- list(time_pre=time_pre, time_opt=time_opt, time_post=s2-s1)
    
    #--- output
    res <- list( coef=coef, vcov=vcov, grad=grad, se=se, loglike=loglike, dev=dev, 
                    res_opt=res_opt, parm.table=parm.table, parm_list=parm_list, 
                    data_list=data_list, ngroups=ngroups, grad_maxabs=grad_maxabs, 
                    CALL=CALL, time=time, time_start=v1 )
    class(res) <- 'srm'
    return(res)
}
