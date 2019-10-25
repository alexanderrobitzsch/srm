## File Name: SRM_OPTIMIZER_EXTERNAL.R
## File Version: 0.15

SRM_OPTIMIZER_EXTERNAL <- function(optimizer, x0, fn, gr, args_opt0, maxiter,
        lower, upper, conv_dev, conv_par, verbose)
{
    args_opt <- args_opt0
    s1 <- Sys.time()
    #-- nlminb
    if (optimizer=="nlminb"){
        if (verbose){
            args_opt$control$trace <- 1
        }
        args_opt$control$iter.max <- maxiter
        args_opt$control$rel.tol <- conv_dev
        args_opt$control$x.tol <- conv_par
        args_opt$start <- x0
        args_opt$objective <- fn
        args_opt$gradient <- gr
        args_opt$lower <- lower
        args_opt$upper <- upper
        opt_fct <- stats::nlminb
    }
    #-- BB::spg
    # if (optimizer=="spg"){
    if (FALSE){
        # requireNamespace("BB")
        if (verbose){
            args_opt$control$trace <- TRUE
            args_opt$control$triter <- 1
        }
        args_opt$control$maxit <- maxiter
        # args_opt$control$x.tol <- conv_par
        args_opt$control$M <- 10
        args_opt$control$checkGrad <- FALSE
        args_opt0$x <- x0
        val <- do.call(what=fn, args=args_opt0)
        args_opt$control$ftol <- conv_dev*abs(val)
        args_opt$par <- x0
        args_opt$fn <- fn
        args_opt$gr <- gr
        args_opt$lower <- lower
        args_opt$upper <- upper
        # opt_fct <- BB::spg
    }
    #- optimization
    res <- do.call( what=opt_fct, args=args_opt)
    s2 <- Sys.time()
    res$time_diff <- s2-s1
    res$optimizer <- optimizer
    res$converged <- ( res$convergence == 0 )
    res$value <- -res$objective

    #- post-processing
    if (optimizer=="nlminb"){
        res$opt_label <- 'stats:nlminb'
    }
    if (optimizer=="spg"){
        res$opt_label <- 'BB:spg'
    }
    #--- output
    return(res)
}
