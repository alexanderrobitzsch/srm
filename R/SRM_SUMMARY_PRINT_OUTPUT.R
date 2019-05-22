## File Name: SRM_SUMMARY_PRINT_OUTPUT.R
## File Version: 0.02

SRM_SUMMARY_PRINT_OUTPUT <- function(obji, round_from, round_to=NULL,
    digits=3)
{    
    if (is.null(round_to)){
        round_to <- ncol(obji)
    }
    round_vars <- seq(round_from, round_to)
    obji[,round_vars] <- round( obji[,round_vars], digits)
    print(obji)
}
