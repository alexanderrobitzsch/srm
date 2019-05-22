## File Name: SRM_PARSER_FUNKTIONEN.R
## File Version: 0.09

SRM_PARSER_PREPARE <- function(model.syntax='') {

    # check for empty syntax
    if(length(model.syntax) == 0) {
        stop("SRM PACKAGE ERROR: no model defined")
    }              
                      
    # substitute all comments with newline
    model.syntax <- gsub("[#!].*(?=\n)","", model.syntax, perl=TRUE)                  
    # remove whitespace prior to split
    model.syntax <- gsub("[ \t]+", "", model.syntax, perl=TRUE)                  
    # break up in lines 
    model <- unlist( strsplit(model.syntax, "\n") )                 
    
    # bestimme die Positionen in der Liste, in denen Formeln stehen
    # und merge multiline formulas; lösche dann alle Nullzeilen
    start.idx <- grep("[~=@%]", model)
    end.idx <- c( start.idx[-1]-1, length(model) )
    model.orig    <- model
    model <- character( length(start.idx) )
    for(i in 1:length(start.idx)) {
        model[i] <- paste(model.orig[start.idx[i]:end.idx[i]], collapse="")
    }

    return(model)

}

SRM_PARSER_SPLIT <- function(model.syntax='') {

    # positionen of person code and dyad code start
    person.idx <- grep("%Person|%person",model.syntax)
    dyad.idx <- grep("%Dyad|%dyad",model.syntax) 

    # check for empty person syntax
    if(length(person.idx) == 0L) {
        stop("SRM PACKAGE ERROR: no model for person defined")
    }
    
    # check for empty dyad syntax
    if(length(dyad.idx) == 0L) {
        stop("SRM PACKAGE ERROR: no model for dyads defined")
    }
    
    # two cases: person formulas are prior to dyad formulas or 
    # it is vice versa
    if (person.idx < dyad.idx) {
        model.p <- character( dyad.idx-2 )
        for(i in 1:length(model.p)) {
              model.p[i] <- model.syntax[i+1]
        }
        model.d <- character( length(model.syntax) - dyad.idx)
        for(i in 1:length(model.d)) {
              model.d[i] <- model.syntax[i+dyad.idx]
        } 
    } else {
        model.d <- character( person.idx - 2)
        for(i in 1:length(model.d)) {
              model.d[i] <- model.syntax[i+1]
        }
        model.p <- character( length(model.syntax) - person.idx )
        for(i in 1:length(model.p)) {
              model.p[i] <- model.syntax[i+person.idx]
        }     
    }

    models <- list(model.p,model.d)
    return(models)

}

SRM_PARSER_LHS <- function(lhs.s,op="EMPTY") {

    out <- list()
    NAMES <- all.vars(lhs.s[[2L]])     
    if ( length(NAMES) == 2 ) {
       out <- c(vector("list", 1L), out)
       names(out)[1L] <- NAMES[1]
       out[[1L]]$ptype1 <- NAMES[2]
    } else {
       stop("SRM PACKAGE ERROR: only one lhs variable allowed")
    }

    return(out)

}

SRM_PARSER_RHS <- function(rhs.s,op="EMPTY") {

    # Parser for rhs-FORMULAS
    out <- list()
    repeat {
         
       if( (length(all.names(rhs.s)) == 3L) & (all.names(rhs.s)[1L]=="@") ) { # rr variable without modifier
            
          out <- c(vector("list", 1L), out)
          NAMES <- all.vars(rhs.s)
          names(out)[1L] <- NAMES[1]
          out[[1L]]$ptype2 <- NAMES[2]
          break
         
       }  else if ( length(all.names(rhs.s)) == 1L & op == "~" & all.names(rhs.s)[1L] != "*" ) { # exogenous covariate without modifier
       
          out <- c(vector("list", 1L), out)
          NAMES <- all.vars(rhs.s)
          names(out)[1L] <- NAMES[1]
          out[[1L]]$ptype2 <- "E"
          break
       
       } else if ( is.numeric(rhs.s) & op == "~") { # an intercept without modifier
       
          out <- c(vector("list", 1L), out)
          NAMES <- "1"
          names(out)[1L] <- NAMES[1]
          out[[1L]]$ptype2 <- "I"
          break
       
       } else if(rhs.s[[1L]] == "*" & length(all.names(rhs.s[[3L]])) == 3L ) { # rr variable with modifier
            
          out <- c(vector("list", 1L), out)
          NAMES <- all.vars(rhs.s[[3L]])
          names(out)[1L] <- NAMES[1]
          out[[1L]]$ptype2 <- NAMES[2]
            
          # get the modifier
          mod <- SRM_GET_MODIFIER(rhs.s[[2L]])
          
          # two possibilities: start values or constraint
          if( names(mod) == "fixed" ) { ## if for fixed parameter values
              out[[1L]]$fixed <- mod[[1L]]  
          } else if ( names(mod) == "equal") {
              out[[1L]]$equal <- mod[[1L]]
          } else if ( names(mod) == "start" ) {              
              out[[1L]]$starts <- mod[[1L]]
          } else {
              stop("SRM PACKAGE ERROR: problem with", rhs.s)
          }
          break
         
       } else if(rhs.s[[1L]] == "*" & length(all.names(rhs.s[[3L]])) == 1L) { # exogenous variable with modifier
            
          out <- c(vector("list", 1L), out)
          NAMES <- all.vars(rhs.s[[3L]])
          names(out)[1L] <- NAMES[1]
          out[[1L]]$ptype2 <- "E"
            
          # get the modifier
          mod <- SRM_GET_MODIFIER(rhs.s[[2L]])
          
          # two possibilities: start values or constraint
          if( names(mod) == "fixed" ) { ## if for fixed parameter values
              out[[1L]]$fixed <- mod[[1L]]  
          } else if ( names(mod) == "equal") {
              out[[1L]]$equal <- mod[[1L]]
          } else if ( names(mod) == "start" ) {              
              out[[1L]]$starts <- mod[[1L]]
          } else {
              stop("SRM PACKAGE ERROR: problem with", rhs.s)
          }
          break
         
       }  else if( rhs.s[[1L]] == "*" & is.numeric(rhs.s[[3L]]) ) { # intercept/mean with modifier
            
          out <- c(vector("list", 1L), out)
          NAMES <- "1"
          names(out)[1L] <- NAMES[1]
          out[[1L]]$ptype2 <- "I"
            
          # get the modifier
          mod <- SRM_GET_MODIFIER(rhs.s[[2L]])
          
          # two possibilities: start values or constraint
          if( names(mod) == "fixed" ) { ## if for fixed parameter values
              out[[1L]]$fixed <- mod[[1L]]  
          } else if ( names(mod) == "equal") {
              out[[1L]]$equal <- mod[[1L]]
          } else if ( names(mod) == "start" ) {              
              out[[1L]]$starts <- mod[[1L]]
          } else {
              stop("SRM PACKAGE ERROR: problem with", rhs.s)
          }
          break
          
       } else if(rhs.s[[1L]] == "+") { # not last one!
            
          i.var <- all.vars(rhs.s[[3L]], unique=FALSE)
          n.var <- length(i.var)
          rhs3.names <- all.names(rhs.s[[3L]])
          
          # Again, there are four cases: (1) rr variable without modifier,
          # (2) exo variable without modifier, (3) rr variable with modifier
          # (4) exo variable with modifier 
          
          out <- c(vector("list", 1L), out)

          # here, we can consider intercepts, but for now we do not allow them     
          if(length(i.var) >= 2L) {  ## rr variable without modifier
              names(out)[1L] <- i.var[n.var-1]
              out[[1L]]$ptype2 <- i.var[n.var]
          } 
          
          if(length(i.var) == 1L) { ## exo variable without modifier
              names(out)[1L] <- i.var[n.var]
              out[[1L]]$ptype2 <- "E"
          }

          # are there modifiers prior to the variable?
          if(length(rhs.s[[3L]]) == 3L && rhs3.names[1L] == "*") {
                
              # get the modifier
              mod <- SRM_GET_MODIFIER(rhs.s[[3L]][[2L]])
          
              # two possibilities: start values or constraint
              if( names(mod) == "fixed" ) { ## if for fixed parameter values
                  out[[1L]]$fixed <- mod[[1L]]  
              } else if ( names(mod) == "equal") {
                  out[[1L]]$equal <- mod[[1L]]
              } else if ( names(mod) == "start" ) {              
                  out[[1L]]$starts <- mod[[1L]]
              } else {
                  stop("SRM PACKAGE ERROR: problem with", rhs.s)
              }
            
          }
          # next element
          rhs.s <- rhs.s[[2L]]

       } else {    
           stop("SRM PACKAGE ERROR: problem with ", rhs.s)
       } 
          
    }
    
    return(out)
    
}

SRM_GET_MODIFIER <- function(modifier) {

     if(length(modifier) == 1L) {
       
       if( is.numeric(modifier) )  {
            return( list( fixed = modifier) )
        } else if ( is.symbol(modifier)) {
            return( list(equal = as.character(modifier)) )
        } else {
            stop("SRM ERROR: can not parse modifier:", modifier, "\n")
        }
    
    } else if(modifier[[1L]] == "start") { ## starting values
    
        cof <- unlist(lapply(as.list(modifier)[-1], eval, envir=NULL, enclos=NULL))
        return( list(start=cof) )
    
    } else if(modifier[[1L]] == "c") {
    
        # make modifier to list
        tmp = as.list(modifier)[-1]
        # elements of tmp can be symbols or numbers
        if ( is.symbol(tmp[[1L]]) ) {
            cof <- unlist(lapply(as.character(tmp), eval, envir=NULL, enclos=NULL))
            return( list(equal = cof) )
        } else if ( is.numeric(tmp[[1L]]) ) {
            cof <- unlist(lapply(tmp, eval, envir=NULL, enclos=NULL))
            return( list(fixed = cof) )
        } else {
            stop("SRM ERROR: can not parse modifier:", modifier, "\n")
        }
    
    } else {
        
        stop("SRM ERROR: can not parse modifier:", modifier, "\n")
    
    }
    
}

SRM_PARSER_SORT_PTYPE <- function(PARLIST, name="EMPTY") {

   
   if (name=="Person") {
   
      tmp.idx <- which(PARLIST$op == "~~" & 
                       PARLIST$ptype1 == "P" & 
                       PARLIST$ptype2 == "A")
      for (i in 1:length(tmp.idx)) { 
          PARLIST$ptype1[tmp.idx] <- "A"
          PARLIST$ptype2[tmp.idx] <- "P"
      }
   
   } else if (name=="Dyad") {
   
      tmp.idx <- which(PARLIST$op == "~~" & 
                       PARLIST$ptype1 == "PA" & 
                       PARLIST$ptype2 == "AP")
      for (i in 1:length(tmp.idx)) { 
          PARLIST$ptype1[tmp.idx] <- "AP"
          PARLIST$ptype2[tmp.idx] <- "PA"
      }
   
   }

   return(PARLIST)
   
}

SRM_PARSER_ADD_PTYPE_EQS <- function(PARLIST) {

   
   lhs <- character(0)
   rhs <- character(0)
   lhs <- paste(PARLIST$lhs,"@",PARLIST$ptype1,sep="")
   rhs <- paste(PARLIST$rhs,"@",PARLIST$ptype2,sep="")
   PARLIST$lhs <- lhs
   PARLIST$rhs <- rhs
   PARLIST$ptype1=NULL
   PARLIST$ptype2=NULL
   
   return(PARLIST)

}
   
SRM_PARSER_LIST <- function( model, ngroups = 1L, name="EMPTY" ) {

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
       out.rhs <- SRM_PARSER_RHS(rhs.s[[2L]],op=op)       
    
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
                      group=SRM.group,
                      ptype1=SRM.ptype1,ptype2=SRM.ptype2,
                      fixed=SRM.fixed, starts=SRM.starts,equal=SRM.equal,
                      mod.idx=SRM.mod.idx,free=SRM.free)
                      
    # now we check whether intercepts of ovs have been defined using @P
    SRM.PARSE <- SRM_PARSER_LIST_PERSON_CHANGE_INTERCEPT (SRM.PARSE)
    # before we return SRM.PARSE, we resort all P ~~ A entries to A ~~ P
    SRM.PARSE <- SRM_PARSER_SORT_PTYPE(SRM.PARSE,name=name)
    # now we match the lhs-ptype1 and rhs-ptype2 columns
    SRM.PARSE <- SRM_PARSER_ADD_PTYPE_EQS(SRM.PARSE)    
    return(SRM.PARSE)
    
}

SRM_PARSER_LIST_ADD_DYAD_FACTORS <- function( parlist ) {

    # ergänzt in dyad-Liste AP-Faktoren bzw. PA-Faktoren
    # Achtung: single-indicator Faktoren werden nicht ergänzt
    # das mache ich später beim erstellen der default-parliste
    
    idx.lv.ap <- which(parlist$op == "=~" & grepl("@AP",parlist$lhs))
    idx.lv.pa <- which(parlist$op == "=~" & grepl("@PA",parlist$lhs))
    lv.ap <- sort(unique(parlist$lhs[idx.lv.ap]))
    lv.pa <- sort(unique(parlist$lhs[idx.lv.pa]))
    vec.lv.ap <- gsub("@AP","",lv.ap)
    vec.lv.pa <- gsub("@PA","",lv.pa)
    
    # durchlaufe lv.aps und prüfe, ob sie in pa sind, wenn nicht, dann ergänze 
    # das entsprechende Faktormodell
    
    add.type <- character(0)
    add.lhs  <- character(0)
    add.op   <- character(0)
    add.rhs  <- character(0)
    add.group <- integer(0)
    add.equal <- character(0)
    add.starts <- integer(0)
    
    if (length(lv.ap) > 0L) {
    
       for (i in 1:length(lv.ap)) {
           
           if( !(vec.lv.ap[i] %in% vec.lv.pa)) {
           
              idx.ind <- which(parlist$op == "=~" & parlist$lhs == lv.ap[i]) 
              add.ind <- gsub("@AP","@PA",parlist$rhs[idx.ind])
              add.equ <- parlist$equal[idx.ind]
              add.sta <- parlist$starts[idx.ind]
              add.gro <- parlist$group[idx.ind] 
              
              add.type <- c(add.type,rep("Dyad",length(add.ind)))
              add.lhs <- c(add.lhs,gsub("@AP","@PA",parlist$lhs[idx.ind]))
              add.op  <- c(add.op,rep("=~",length(add.ind)))
              add.rhs <- c(add.rhs,add.ind)
              add.equal <- c(add.equal,add.equ)
              add.starts <- c(add.starts,add.sta)
              add.group <- c(add.group,add.gro)
           
           } 
       
       }
    
    }
    
    if (length(lv.pa) > 0L) {
    
       for (i in 1:length(lv.pa)) {
           
           if( !(vec.lv.pa[i] %in% vec.lv.ap)) {
           
              idx.ind <- which(parlist$op == "=~" & parlist$lhs == lv.pa[i]) 
              add.ind <- gsub("@PA","@AP",parlist$rhs[idx.ind])
              add.equ <- parlist$equal[idx.ind]
              add.sta <- parlist$starts[idx.ind]
              add.gro <- parlist$group[idx.ind]
              
              add.type <- c(add.type,rep("Dyad",length(add.ind)))
              add.lhs <- c(add.lhs,gsub("@PA","@AP",parlist$lhs[idx.ind]))
              add.op  <- c(add.op,rep("=~",length(add.ind)))
              add.rhs <- c(add.rhs,add.ind)
              add.equal <- c(add.equal,add.equ)
              add.starts <- c(add.starts,add.sta)
              add.group <- c(add.group,add.gro)
           
           } 
       
       }
    
    }
    
    parlist$type <- c(parlist$type,add.type)
    parlist$lhs <- c(parlist$lhs,add.lhs)
    parlist$op <- c(parlist$op,add.op)
    parlist$rhs <- c(parlist$rhs,add.rhs)
    parlist$group <- c(parlist$group,add.group)
    parlist$fixed <- c(parlist$fixed,rep(NA,length(add.lhs)))
    parlist$starts <- c(parlist$starts,add.starts)
    parlist$equal <- c(parlist$equal,add.equal)
    parlist$mod.idx <- c(parlist$mod.idx,rep(0,length(add.lhs)))
    parlist$free <- c(parlist$free,rep(1,length(add.lhs)))
        
    return(parlist)
    
}

SRM_PARSER_LIST_PERSON_CHANGE_INTERCEPT <- function( parlist ) {

    # if a user has defined the mean of an obsverved variable with 
    # @P we change this to @A
    # IMPORTANT: this change does not apply to the means of the latent
    #            factors

    tmp.idx <- which(  parlist$op == "~1" & 
                       parlist$ptype1 == "P" &
                     !(parlist$op == "=~") )
    
    for (i in 1:length(tmp.idx)) { 
         parlist$ptype1[tmp.idx] <- "A"
         parlist$ptype2[tmp.idx] <- "A"
    }

    return(parlist)
}
