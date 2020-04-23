## File Name: SRM_OPTIMIZER_INTERNAL_LINE_SEARCH.R
## File Version: 0.164

SRM_OPTIMIZER_INTERNAL_LINE_SEARCH <- function(xold, incr, ll0, fn, args_fn,
        line_search_iter_max=6, llold=Inf)
{
    line_search_iter <- 0
    iterate_line_search <- TRUE
    line_search_fctr <- 2
    ll <- ll0
    #- iterations
    while (iterate_line_search){    
        line_search_fctr <- line_search_fctr / 2
        incr <- incr * line_search_fctr
        x <- xold + incr
        args_fn$x <- x
        res <- do.call(what=fn, args=args_fn)
        if ( is.list(res) ){ ll <- res[[1]] } else { ll <- res }
        if (line_search_iter > line_search_iter_max){ iterate_line_search <- FALSE }
        if (ll > ll0){ iterate_line_search <- FALSE }
        line_search_iter <- line_search_iter + 1        
    }

    #-- output
    res <- list(ll=ll, x=x, incr=incr, line_search_iter=line_search_iter,
                line_search_fctr=line_search_fctr)
    return(res)
}
