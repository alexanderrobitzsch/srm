## File Name: SRM_PRINT_PARAMETERESTIMATES.R
## File Version: 0.02


SRM_PRINT_PARAMETERESTIMATES <- function( object = NULL,
                                          digits = 3L)
{

    #- the following function is inspired by lavaans print function

    #- define character format
    char.format <- paste("%3s%-", 14, "s", sep = "")

    #- get parmtable from object
    parm.table <- object$parm.table

    #- add z- and p-values
    parm.table$z <- parm.table$est/parm.table$se
    parm.table$p <- 2 * pnorm( -abs( parm.table$z ) )# two sided value

    #- round to the defined number of digits
    parm.table[,c("est","se","z","p")] <- round( parm.table[,c("est","se","z","p")],
                                                 digits )

    #- we transform the level variable
    level <- c( "Person Level", "Dyad Level" )
    parm.table$level <- ifelse( parm.table$level == "U", level[1], level[2] )

    #- make parmtable smaller
    idx <- which( parm.table$fixed %in% c( 1, NA ) )
    parm.table <- parm.table[idx,]

    #- remove some columns
    y <- parm.table
    y$op <- y$rhs <- NULL
    y$group <- y$mat <- y$row <- y$col <- NULL
    y$starts <- y$user <- y$unid <- NULL
    y$level <- y$par_names <- y$lower <- NULL
    y$index <- y$fixed <- NULL

    #- how many groups?
    ngroups <- length( unique( parm.table$group ) )

    #- convert to character matrix
    m <- as.matrix( format.data.frame( y, na.encode = FALSE, justify = "right") )
    rownames( m ) <- rep( "", nrow(m) ) # make empty row names

    #- in case the se is NA we have to insert notthing...
    if( !is.null( parm.table$se ) ) {

        idx <- which( is.na( parm.table$se ) )
        if( length( idx ) > 0L ) {

            m[idx, "se"] <- ""
            m[idx, "z"] <- ""
            m[idx, "p"] <- ""

        }
    }

    # rename some column names
    colnames(m)[ colnames(m) ==    "lhs" ] <- ""
    colnames(m)[ colnames(m) ==    "est" ] <- "Estimate"
    colnames(m)[ colnames(m) ==     "se" ] <- "Std.Err"
    colnames(m)[ colnames(m) ==      "z" ] <- "z-value"
    colnames(m)[ colnames(m) ==      "p" ] <- "P(>|z|)"

    #- the four result section types
    sections <- c("Latent Variables",
                  "Regressions",
                  "Variances",
                  "Covariances",
                  "Intercepts")

    cat("PARAMETER ESTIMATES")

    # group-specific sections
    for( g in 1:ngroups ) {

        #- group counter:
        if(ngroups > 1L) {
            cat("Group ", g, "]:\n", sep="")
        }

        for( l in 1:2 ) { # Person and Dyad-Level

            #- level header
            cat( "\n\n" )
            cat( level[l],":\n", sep="" )

            #- person/dyad-specific sections

            for( s in sections ) {

                if( s == "Latent Variables" ) {

                    row.idx <- which( parm.table$op == "=~" &
                                      parm.table$level == level[l] )
                    if( length(row.idx ) == 0L) next
                    tmp <- rep( "", length( row.idx ) )
                    m[row.idx,1] <- sprintf(char.format, tmp, parm.table$rhs[row.idx] )

                } else if( s == "Regressions" ) {

                    row.idx <- which( parm.table$op == "~" &
                                      parm.table$level == level[l] )
                    if(length(row.idx) == 0L) next
                    tmp <- rep( "", length( row.idx ) )
                    m[row.idx,1] <- sprintf(char.format, tmp, parm.table$rhs[row.idx] )

                } else if( s == "Covariances" ) {

                    row.idx <- which( parm.table$op == "~~" &
                                      parm.table$lhs != parm.table$rhs &
                                      parm.table$level == level[l] )
                    if( length(row.idx) == 0L ) next
                    tmp <- rep( "", length( row.idx ) )
                    m[row.idx,1] <- sprintf(char.format, tmp, parm.table$rhs[row.idx] )

                } else if(s == "Variances") {

                    row.idx <- which(parm.table$op == "~~" &
                                     parm.table$lhs == parm.table$rhs &
                                     parm.table$level == level[l])

                    if(length(row.idx) == 0L) next
                    tmp <- rep( "", length( row.idx ) )
                    m[row.idx,1] <- sprintf(char.format, tmp, parm.table$rhs[row.idx] )

                } else if(s == "Intercepts") {

                    row.idx <- which( parm.table$op == "~1" &
                                      parm.table$level == level[l] )
                    if(length(row.idx) == 0L) next
                    tmp <- rep( "", length( row.idx ) )
                    m[row.idx,1] <- sprintf(char.format, tmp, parm.table$rhs[row.idx] )

                }  else { row.idx <- integer(0L) }

                #- we delete repeated lhs names in case of latent vars....

                if( s %in% c("Latent Variables", "Regressions", "Covariances") )
                {

                    nel <- length(row.idx)
                    M <- matrix("", nrow = nel*2, ncol = ncol(m))
                    colnames(M) <- colnames(m)
                    rownames(M) <- rep("", NROW(M))

                    LHS <- paste( parm.table$lhs[row.idx], parm.table$op[row.idx])
                    lhs.idx <- seq(1, nel*2, 2L)
                    rhs.idx <- lhs.idx + 1

                    tmp <- rep("", length(LHS))
                    M[lhs.idx,  1 ] <- sprintf("%1s%-15s", tmp, LHS)
                    M[rhs.idx, ] <- m[row.idx,]

                    idx <- duplicated(M[,1])
                    #M[,1] <- ifelse( idx, "", M[,1])
                    M <- rbind( M[!idx, ] )

                    cat("\n", s, ":\n", sep = "")
                    print(M, quote = FALSE)

                # Regular
                } else {

                   M <- m[row.idx,,drop=FALSE]
                   colnames(M) <- colnames(m)
                   rownames(M) <- rep("", NROW(M))

                   cat("\n", s, ":\n", sep = "")
                   print(M, quote = FALSE)

                }

            } # sections

        } # person and dyads

    } # groups

    cat("\n")
    invisible( m )
}
