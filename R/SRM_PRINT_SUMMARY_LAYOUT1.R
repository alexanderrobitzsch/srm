## File Name: SRM_PRINT_SUMMARY_LAYOUT1.R
## File Version: 0.03


SRM_PRINT_SUMMARY_LAYOUT1 <- function( object = NULL, digits = 3L )
{
    SRM_PRINT_OPTINFOS( object = object, digits = digits )
    SRM_PRINT_PARAMETERESTIMATES( object = object, digits = digits )
}
