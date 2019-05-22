## File Name: SRM_STARTVALUES.R
## File Version: 0.04

## this function define the starting values for the
## user-specified and default model paramters

## at present it is not defined for multiple groups

SRM_STARTVALUES <- function(method.startvalues = "default",
                            PARTABLE = NULL)

{

    # define start-vector
    starts <- numeric( length(PARTABLE$lhs) )
    # shortcut for 'simple'
    # be we should also take care of the 'fixed.x' issue
    if( method.startvalues == "default" ) {

        # start-values for loadings
        starts[ which(PARTABLE$op == "=~") ] <- 1.0
        # start-values for latent factors and residual variances
        ind <- which(PARTABLE$op == "~~")
        ind0 <- which(PARTABLE$lhs == PARTABLE$rhs)                
        ind1 <- intersect(ind, ind0)        
        ind0 <- which(PARTABLE$lhs != PARTABLE$rhs)
        ind2 <- intersect(ind, ind0)                
        starts[ ind1 ] <- 0.5
        starts[ ind2 ] <- 0.2
        # start-values for latent and exogenuous regressions
        starts[ which(PARTABLE$op == "~") ] <- 0
        # Intercepts and factor means
        ## ...
    }

    # now we substitute the user-specified starting values in the start value
    start.idx <- which(!is.na(PARTABLE$starts))
    starts[start.idx] <- PARTABLE$starts[start.idx]

    # delete the ustart and the starts-column in PARTABLE and
    # return the modified PARTABLE
    PARTABLE$starts <- starts
    return(PARTABLE)

}
