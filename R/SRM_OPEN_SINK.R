## File Name: SRM_OPEN_SINK.R
## File Version: 0.01

SRM_OPEN_SINK <- function(file, suffix="__SUMMARY.Rout")
{
    if (!is.null(file)){ 
        sink(paste0(file, suffix), split=TRUE)
    }
}
