## File Name: summary.srm.R
## File Version: 0.199


summary.srm <- function(object, digits=3, file=NULL, layout=1, ...)
{
    #-- open sink
    SRM_OPEN_SINK(file=file)

    #-- print summaries
    if (layout==1){
        SRM_PRINT_SUMMARY_LAYOUT1(object=object, digits=digits)
    } else {
        SRM_PRINT_SUMMARY_LAYOUT2(object=object, digits=digits)
    }

    #-- close sink
    SRM_CLOSE_SINK(file=file)
}
