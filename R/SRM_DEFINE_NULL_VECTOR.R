## File Name: SRM_DEFINE_NULL_VECTOR.R
## File Version: 0.01

SRM_DEFINE_NULL_VECTOR <- function(vec)
{
    if (length(vec)==0){
        vec <- NULL
    }
    return(vec)
}
