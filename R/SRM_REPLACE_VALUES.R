## File Name: SRM_REPLACE_VALUES.R
## File Version: 0.03


SRM_REPLACE_VALUES <- function(x, val, pos, symm)
{
    x[ pos[1], pos[2] ] <- val
    if ( symm & ( pos[1] != pos[2] ) ){
        x[ pos[2], pos[1] ] <- val
    }
    return(x)
}
