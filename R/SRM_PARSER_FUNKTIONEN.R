## File Name: SRM_PARSER_FUNKTIONEN.R
## File Version: 0.171


SRM_PARSER_PREPARE <- function( model.syntax = '' ) {

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
    # und merge multiline formulas; loesche dann alle Nullzeilen
    start.idx <- grep("[~=@%]", model)
    end.idx <- c( start.idx[-1]-1, length(model) )
    model.orig    <- model
    model <- character( length(start.idx) )
    for(i in 1:length(start.idx)) {
        model[i] <- paste(model.orig[start.idx[i]:end.idx[i]], collapse="")
    }

    return(model)

}

SRM_PARSER_SPLIT <- function( model.syntax ='' ) {

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

SRM_PARSER_RHS <- function(rhs.s,op=NULL,name=NULL) {

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
          if ( name == "Person" ) { out[[1L]]$ptype2 <- "E" }
          else if ( name == "Dyad") { out[[1L]]$ptype2 <- "F" }

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
          if ( name == "Person" ) { out[[1L]]$ptype2 <- "E" }
          else if ( name == "Dyad") { out[[1L]]$ptype2 <- "F" }

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
              if ( name == "Person" ) { out[[1L]]$ptype2 <- "E" }
              else if ( name == "Dyad") { out[[1L]]$ptype2 <- "F" }
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

# new version: 26.02.2021
SRM_PARSER_SORT_PTYPE <- function(PARLIST, name="EMPTY") 
{
   if (name=="Person") {
      tmp.idx <- which(PARLIST$op == "~~" &
                       PARLIST$ptype1 == "P" &
                       PARLIST$ptype2 == "A")
      for (i in 1:length(tmp.idx)) {
          old.lhs <- PARLIST$lhs[tmp.idx]
          old.rhs <- PARLIST$rhs[tmp.idx]
          PARLIST$lhs[tmp.idx] <- old.rhs
          PARLIST$rhs[tmp.idx] <- old.lhs
          PARLIST$ptype1[tmp.idx] <- "A"
          PARLIST$ptype2[tmp.idx] <- "P"
      }
   } else if (name=="Dyad") {
      tmp.idx <- which(PARLIST$op == "~~" &
                       PARLIST$ptype1 == "PA" &
                       PARLIST$ptype2 == "AP")
      for (i in 1:length(tmp.idx)) {
          old.lhs <- PARLIST$lhs[tmp.idx]
          old.rhs <- PARLIST$rhs[tmp.idx]
          PARLIST$lhs[tmp.idx] <- old.rhs
          PARLIST$rhs[tmp.idx] <- old.lhs
          PARLIST$ptype1[tmp.idx] <- "AP"
          PARLIST$ptype2[tmp.idx] <- "PA"
      }
   }
   return(PARLIST)
}

SRM_PARSER_ADD_PTYPE_EQS <- function( parlist = NULL ) 
{
   lhs <- character(0)
   rhs <- character(0)
   lhs <- paste(parlist$lhs,"@",parlist$ptype1,sep="")
   rhs <- paste(parlist$rhs,"@",parlist$ptype2,sep="")
   parlist$lhs <- lhs
   parlist$rhs <- rhs
   parlist$ptype1=NULL
   parlist$ptype2=NULL
   return(parlist)
}

SRM_PARSER_LIST_ADD_DYAD_FACTORS <- function( parlist = NULL ) 
{
    # ergaenzt in dyad-Liste AP-Faktoren bzw. PA-Faktoren
    # Achtung: single-indicator Faktoren werden nicht ergaenzt
    # das mache ich spaeter beim erstellen der default-parliste
    idx.lv.ap <- which(parlist$op == "=~" & grepl("@AP",parlist$lhs))
    idx.lv.pa <- which(parlist$op == "=~" & grepl("@PA",parlist$lhs))
    lv.ap <- sort(unique(parlist$lhs[idx.lv.ap]))
    lv.pa <- sort(unique(parlist$lhs[idx.lv.pa]))
    vec.lv.ap <- gsub("@AP","",lv.ap)
    vec.lv.pa <- gsub("@PA","",lv.pa)
    # durchlaufe lv.aps und pruefe, ob sie in pa sind, wenn nicht, dann ergaenze
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

SRM_PARSER_LIST_ADD_DYAD_EXOCOV <- function( parlist = NULL ) 
{
  #- get indices:
  idx.reg <- which( parlist$op == "~" )
  #- iterate:
  if ( length(idx.reg) > 0L ) {
    for ( ii in 1:length( idx.reg ) ) {
      #- get index:
      idx <- idx.reg[ii]
      #- let's go: within the second if-else we check whether the
      #  covariate is already within the list 
      if ( grepl("@AP", parlist$lhs[ idx ] ) ) {
        tmp.lhs <- gsub("@AP","@PA",parlist$lhs[ idx ])
        if ( any( parlist$op == "~" & parlist$lhs == tmp.lhs & parlist$rhs == parlist$rhs[ idx ] ) ) 
          break
      } else if ( grepl("@PA", parlist$lhs[ idx ] ) ) {
        tmp.lhs <- gsub("@PA","@AP",parlist$lhs[ idx ])
        if ( any( parlist$op == "~" & parlist$lhs == tmp.lhs & parlist$rhs == parlist$rhs[ idx ] ) )
          break
      }
      #- else:
      parlist$type <- c( parlist$type, parlist$type[idx])
      parlist$lhs <- c( parlist$lhs,tmp.lhs)
      parlist$op <- c( parlist$op,parlist$op[idx])
      parlist$rhs <- c( parlist$rhs,parlist$rhs[idx])
      parlist$group <- c( parlist$group,parlist$group[idx])
      parlist$fixed <- c( parlist$fixed,parlist$fixed[idx])
      parlist$starts <- c( parlist$starts,parlist$starts[idx])
      parlist$mod.idx <- c( parlist$mod.idx,parlist$mod.idx[idx])
      parlist$free <- c( parlist$free,parlist$free[idx])
      #- set the parms to the same value:
      parlist$equal[idx] <- paste( "rr", ii, sep = "")
      parlist$equal <- c( parlist$equal,parlist$equal[idx])      
    }
  }
  return( parlist )
}

SRM_PARSER_LIST_PERSON_CHANGE_INTERCEPT <- function( parlist  = NULL ) 
{
    # if a user has defined the mean of an obsverved variable with
    # @P we change this to @A
    # IMPORTANT: this change does not apply to the means of the latent
    #            factors
    tmp.idx <- which(  parlist$op == "~1" &
                       parlist$ptype1 == "P" &
                     !(parlist$op == "=~") )
    for (ii in 1:length(tmp.idx)) {
      idx <- tmp.idx[ii]
      parlist$ptype1[idx] <- "A"
      parlist$ptype2[idx] <- "A"
    }
    return(parlist)
}
