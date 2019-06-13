## File Name: SRM_OPTIMIZER_INTERNAL.R
## File Version: 0.16

SRM_OPTIMIZER_INTERNAL <- function(x0, fn, args_fs, conv_dev, conv_par,
        maxiter, do_line_search=TRUE, line_search_iter_max=6,
        verbose=TRUE)
{
    x <- x0
    iter <- 1
    iterate <- TRUE
    conv <- conv_par
    change_digits_par <- round( abs( log10(conv_par) ) ) + 2
    change_digits_dev <- round( abs( log10(conv_dev) ) ) + 2
    ll <- -Inf
    ll_history <- rep(NA, maxiter)
    s1 <- Sys.time()

    if (do_line_search){
        args_fs_ls <- args_fs
        args_fs_ls$compute_gradient <- FALSE
    }

    while (iterate){
        xold <- x
        llold <- ll
        args_fs$x <- x
        res <- do.call(what=fn, args=args_fs)
        infomat <- res[[3]]             # res[[3]]  $expected_infomat
        grad <- res[[2]]                # res[[2]]  $grad
        ll <- res[[1]]                    # res[[1]]    $ll_new
        #- parameter update
        incr <- SRM_OPTIMIZER_INTERNAL_COMPUTE_INCREMENT(grad=grad, infomat=infomat)
        x <- xold + incr
        ll_diff <- ll-llold

        #- line search
        if (do_line_search){
            res <- SRM_OPTIMIZER_INTERNAL_LINE_SEARCH(xold=xold, incr=incr,
                        ll0=ll, fn=fn, args_fn=args_fs_ls,
                        line_search_iter_max=line_search_iter_max)
            ll <- res$ll
            x <- res$x
            incr <- res$incr
        }
        change_par <- max(abs(x-xold))
        change_ll <- abs(ll-llold)/abs(ll)
        if (verbose){
            cat("Iteration", iter, "| optim function", "=", round(ll,5) )
            cat(" | Parameter change","=", round(change_par, change_digits_par) )
            cat(" | Relative function change","=", round(change_ll, change_digits_dev), "\n" )
            utils::flush.console()
        }
        ll_history[iter] <- ll
        if (change_par < conv_par){ iterate <- FALSE }
        if (iter >= maxiter){ iterate <- FALSE }
        if (iterate){ iter <- iter + 1}
    }

    s2 <- Sys.time()
    time_diff <- s2 - s1
    converged <- ( iter < maxiter )
    ll_history <- ll_history[1:iter]
    opt_label <- 'srm:::SRM_OPTIMIZER_INTERNAL'
    #-- output
    res <- list(optimizer='srm', value=ll, par=x, grad=grad, infomat=infomat,
                    iter=iter, converged=converged, time_diff=time_diff,
                    ll_history=ll_history, opt_label=opt_label)
    return(res)
}
