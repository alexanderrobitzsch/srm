## File Name: SRM_PARTABLE_ADD_NOISE.R
## File Version: 0.01

SRM_PARTABLE_ADD_NOISE <- function(parm.table, sd)
{
    parm.table0 <- parm.table[ ! is.na(parm.table$index), ]
    parm.table0 <- parm.table0[ ! duplicated(parm.table0$index) , ]
    parm.table0 <- parm.table0[ order( parm.table0$index ), ]
    npar <- max( parm.table0$index )
    val0 <- parm.table0$starts
    val <- round(val0 + rnorm( npar, mean=0, sd = sd ), 3 )
    dfr <- cbind(1:npar, val0, val)
    i2 <- which( ! is.na(parm.table$index) )
    i1 <- match(  parm.table$index[ i2 ], 1:npar )
    parm.table[  i2 , "starts" ] <- val[ i1 ]
    return(parm.table)
}
