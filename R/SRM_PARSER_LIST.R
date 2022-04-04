## File Name: SRM_PARSER_LIST.R
## File Version: 0.22



SRM_PARSER_LIST <- function( model, ngroups = 1L, name = NULL ) {

    SRM.type <- character(0)
    SRM.lhs <- character(0)
    SRM.op  <- character(0)
    SRM.rhs <- character(0)
    SRM.group  <- integer(0)
    SRM.ptype1 <- character(0) # actor/partner lhs
    SRM.ptype2 <- character(0) # actor/partner rhs
    SRM.fixed  <- integer(0) # fixed parameter value
    SRM.starts <- integer(0) # for user-defined starting values
    SRM.equal <- character(0) # equality constraints
    SRM.mod.idx <- integer(0) # modified things
    SRM.free <- integer(0)    # estimated or not?
    SRM.lauf <- 0

    for (i in 1:length(model))  {

       # line to analyse
       x <- model[i]
       # Step 1: which operator is used?
       x.simple <- gsub("\\\".[^\\\"]*\\\"", "LABEL", x)
       # "=~" operator?
       if(grepl("=~", x.simple, fixed=TRUE)) {
          op <- "=~"
       # "~~" operator?
       } else if(grepl("~~", x.simple, fixed=TRUE)) {
          op <- "~~"
       # "~" operator?
       } else if(grepl("~", x.simple, fixed=TRUE)) {
          op <- "~"
       } else {
          stop("SRM PACKAGE ERROR: unknown operator in ", model[i])
       }

       # Step 2: split by operator
       op.idx <- regexpr(op, x)
       lhs <- substr(x, 1L, op.idx-1L)
       rhs <- substr(x, op.idx+attr(op.idx, "match.length"), nchar(x))

       # Step 3: Get list of values for the lhs formula
       lhs.s <- stats::as.formula(paste("~",lhs)) # for functionality formula trans
       out.lhs <- SRM_PARSER_LHS(lhs.s,op=op)

       # Step 4: Get list of values for the rhs formula
       #         (including fixed and start values)
       rhs.s <- stats::as.formula(paste("~",rhs))
       out.rhs <- SRM_PARSER_RHS(rhs.s[[2L]],op=op,name=name)

       # Step 5: save all things in a list
       for (j in 1:length(out.lhs)) {

           for (k in 1:length(out.rhs)) {

               for (n in 1:ngroups) {

                   SRM.lauf <- SRM.lauf + 1
                   SRM.type[SRM.lauf] <- name
                   SRM.lhs[SRM.lauf] <- names(out.lhs)[j]
                   SRM.group[SRM.lauf]  <- n
                   SRM.ptype1[SRM.lauf] <- out.lhs[[j]]$ptype1
                   SRM.fixed[SRM.lauf]  <- as.numeric(NA)
                   SRM.starts[SRM.lauf] <- as.numeric(NA)
                   SRM.equal[SRM.lauf] <- as.numeric(NA)
                   SRM.mod.idx[SRM.lauf] <- 0
                   SRM.free[SRM.lauf] <- 1L

                   # if equation concerns an intercept:
                   if (out.rhs[[k]]$ptype2 == "I") {
                      SRM.op[SRM.lauf]  <- "~1"
                      SRM.rhs[SRM.lauf] <- names(out.lhs)[j]
                      SRM.ptype2[SRM.lauf] <- out.lhs[[j]]$ptype1

                   } else {
                      SRM.op[SRM.lauf]  <- op
                      SRM.rhs[SRM.lauf] <- names(out.rhs)[k]
                      SRM.ptype2[SRM.lauf] <- out.rhs[[k]]$ptype2
                   }

                   if(length(out.rhs[[k]]$fixed) > 0L) {
                      SRM.fixed[SRM.lauf] <- out.rhs[[k]]$fixed[n]
                      SRM.free[SRM.lauf] <- 0L
                      SRM.mod.idx[SRM.lauf] <- 1
                   }

                   if(length(out.rhs[[k]]$starts) > 0L) {
                      SRM.starts[SRM.lauf] <- out.rhs[[k]]$starts[n]
                      SRM.mod.idx[SRM.lauf] <- 1
                   }

                   if(length(out.rhs[[k]]$equal) > 0L) {
                      SRM.equal[SRM.lauf] <- out.rhs[[k]]$equal[n]
                      SRM.mod.idx[SRM.lauf] <- 1
                   }

               }  # for - loop groups

           } # for-loop rhs

       } # for-loop lhs

    } # for-loop lines


    SRM.PARSE <- list(type=SRM.type,lhs=SRM.lhs, op=SRM.op, rhs=SRM.rhs,
                      group=SRM.group, ptype1=SRM.ptype1, ptype2=SRM.ptype2,
                      fixed=SRM.fixed, starts=SRM.starts, equal=SRM.equal,
                      mod.idx=SRM.mod.idx,free=SRM.free)

    # now we check whether intercepts of ovs have been defined using @P
    SRM.PARSE <- SRM_PARSER_LIST_PERSON_CHANGE_INTERCEPT(SRM.PARSE)
    # before we return SRM.PARSE, we resort all P ~~ A entries to A ~~ P
    SRM.PARSE <- SRM_PARSER_SORT_PTYPE(SRM.PARSE,name=name)
    # now we match the lhs-ptype1 and rhs-ptype2 columns
    SRM.PARSE <- SRM_PARSER_ADD_PTYPE_EQS(SRM.PARSE)
    return(SRM.PARSE)

}
