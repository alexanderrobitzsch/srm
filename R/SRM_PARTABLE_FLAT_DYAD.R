## File Name: SRM_PARTABLE_FLAT_DYAD.R
## File Version: 0.11



## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
##         Function for the Dyad
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SRM_PARTABLE_FLAT_DYAD  <- function(PARLIST,
                              # definitions for default parameters
                              # 1. covariance relationship effects of one latent rr
                              auto.cov.lv.dy = TRUE,
                              # 2. covariance relationship effects of one observed rr
                              auto.cov.ov.dy = TRUE,
                              # 3. covariance relationship-effects of across latent rrs
                              auto.cov.lv.block = FALSE,
                              # 4. meanstructure
                              auto.int.ov = FALSE,
                              auto.int.lv = FALSE,
                              # definitions for fixed values
                              auto.fix.loa.first.ind.ij=TRUE,
                              auto.fix.loa.first.ind.ji=TRUE,
                              auto.fix.loa.ind.ij.ji = TRUE, # rel-loadings are set to the equal value
                              auto.fix.int.first.ind.ij=FALSE,
                              auto.fix.int.first.ind.ji=FALSE,
                              ngroups = 1L )

{

   # Step 1: extract `names' of various types of variables:
   # there are a number of possibilities
   # IMPORTANT: We make this selection for one group only!!!

   idx = which(PARLIST$group == 1)
   TMP.PARLIST = lapply(PARLIST,function(x) x[idx])

   # 1. regular latent round robin variable (defined by =~)
   # 2. observed round robin variables that are used to lv-rr vars (in =~)
   # 3. observed round robin variables that are not 1. or 2. but that are
   #    used as predictors or outcomes
   # 4. true exogenuous variables used to predict latent rr variables
   # 5. true exogenuous variables used to predict observed rr vars

   # the regular rr-lvs
   rr.lv.regular.names.ij <- SRM_PARTABLE_VNAMES_DYAD(TMP.PARLIST, type="rr.lv.ij")
   rr.lv.regular.names.ji <- SRM_PARTABLE_VNAMES_DYAD(TMP.PARLIST, type="rr.lv.ji")
   # regular rr-lvs that are used as predictors or are the outcomes
   rr.lv.names.y.ij <- SRM_PARTABLE_VNAMES_DYAD(TMP.PARLIST, type="rr.lv.y.ij") # dependent rr-lv actors
   rr.lv.names.y.ji <- SRM_PARTABLE_VNAMES_DYAD(TMP.PARLIST, type="rr.lv.y.ji") # dependent rr-lv partners
   rr.lv.names.x.ij <- SRM_PARTABLE_VNAMES_DYAD(TMP.PARLIST, type="rr.lv.x.ij") # independent rr-lv actors
   rr.lv.names.x.ji <- SRM_PARTABLE_VNAMES_DYAD(TMP.PARLIST, type="rr.lv.x.ji") # independent rr-lv partners

   # observed rrs that are the indicators of the regular rr-lvs
   rr.ov.ind.names.ij <- SRM_PARTABLE_VNAMES_DYAD(TMP.PARLIST, type="rr.ov.ind.ij")
   rr.ov.ind.names.ji <- SRM_PARTABLE_VNAMES_DYAD(TMP.PARLIST, type="rr.ov.ind.ji")
   # observed rrs that are used as predictors or are the outcomes
   rr.ov.names.y.ij <- SRM_PARTABLE_VNAMES_DYAD(TMP.PARLIST, type="rr.ov.y.ij") # dependent rr-ov actors
   rr.ov.names.y.ji <- SRM_PARTABLE_VNAMES_DYAD(TMP.PARLIST, type="rr.ov.y.ji") # dependent rr-ov partners
   rr.ov.names.x.ij <- SRM_PARTABLE_VNAMES_DYAD(TMP.PARLIST, type="rr.ov.x.ij") # independent rr-ov actors
   rr.ov.names.x.ji <- SRM_PARTABLE_VNAMES_DYAD(TMP.PARLIST, type="rr.ov.x.ji") # independent rr-ov partners
   rr.cov.names.ij <- SRM_PARTABLE_VNAMES_DYAD(TMP.PARLIST, type="rr.cov.ij") # covariance ij
   rr.cov.names.ji <- SRM_PARTABLE_VNAMES_DYAD(TMP.PARLIST, type="rr.cov.ji") # covariance ji

   # some computations with these variables:
   # ov-rrs that are not defined as regular lv-rrs (they are outcomes or they
   # are predictors, but they are not allowed to be indicators)
   rr.ov.notind.names.ij <- setdiff(Reduce(union, list(rr.ov.names.y.ij, rr.ov.names.x.ij, rr.cov.names.ij)),rr.ov.ind.names.ij)
   rr.ov.notind.names.ji <- setdiff(Reduce(union, list(rr.ov.names.y.ji, rr.ov.names.x.ji, rr.cov.names.ji)),rr.ov.ind.names.ji)

   # it's possible that the a-part or p-part was defined as the ov-rr so that we
   # we have to expand the respective other vector
   if ( length(rr.ov.notind.names.ij) > 0L ) {

      tmp.ij <- gsub("@AP","",rr.ov.notind.names.ij,perl=TRUE)
      tmp.ji <- gsub("@PA","",rr.ov.notind.names.ji,perl=TRUE)
      if (!(tmp.ij %in% tmp.ji)) { # elements in .p are missing in .a
         tmp <- setdiff(tmp.ij,tmp.ji)
         rr.ov.notind.names.ji <- c(rr.ov.notind.names.ji,paste(tmp,"@PA",sep=""))
      }
   }

  if ( length(rr.ov.notind.names.ji) > 0L ) {

      tmp.ij <- gsub("@AP","",rr.ov.notind.names.ij,perl=TRUE)
      tmp.ji <- gsub("@PA","",rr.ov.notind.names.ji,perl=TRUE)
      if (!(tmp.ji %in% tmp.ij)) { # elements in .p are missing in .a
         tmp <- setdiff(tmp.ji,tmp.ij)
         rr.ov.notind.names.ij <- c(rr.ov.notind.names.ij,paste(tmp,"@AP",sep=""))
      }

   }
   # save all rrs (latents and observed)
   rr.all.lv.names.ij <- c(rr.lv.regular.names.ij,rr.ov.notind.names.ij)
   rr.all.lv.names.ji <- c(rr.lv.regular.names.ji,rr.ov.notind.names.ji)

   # true exogenouos covariates
   sv.eqs.x <- SRM_PARTABLE_VNAMES_DYAD(TMP.PARLIST, type="sv.eqs.x")
   sv.eqs.y <- SRM_PARTABLE_VNAMES_DYAD(TMP.PARLIST, type="sv.eqs.y")

  ## +++++++++++++++++++++++++++++++++++++++++++
  ##   2. We construct a default parameter table
  ## +++++++++++++++++++++++++++++++++++++++++++

  lhs <- rhs <- character(0)
  mod.idx <- integer(0)
  #equal <- character(0)

  ## 2.1 ALWAYS: variances of latent actor and partner effects
  ##             and residual variances of actor and partner effects

  lhs <- c(lhs, rr.lv.regular.names.ij, rr.lv.regular.names.ji, rr.ov.ind.names.ij, rr.ov.ind.names.ji, rr.ov.notind.names.ij, rr.ov.notind.names.ji )
  rhs <- c(rhs, rr.lv.regular.names.ij, rr.lv.regular.names.ji, rr.ov.ind.names.ij, rr.ov.ind.names.ji, rr.ov.notind.names.ij, rr.ov.notind.names.ji )


  ## 2.3 Default covariance parameters:
  ## per Default, we always include the covariance between the a-part and the
  ## p-part of ONE rr-variable

  if ( auto.cov.lv.dy & length(rr.all.lv.names.ij) > 0L & length(rr.all.lv.names.ji) > 0L) {

     lhs <- c(lhs, sort(rr.all.lv.names.ij))
     rhs <- c(rhs, sort(rr.all.lv.names.ji))
     #equal <- c(equal,rep(as.numeric(NA),length(rr.all.lv.names.ij)))

  }

  if ( auto.cov.ov.dy & length(rr.ov.ind.names.ij) > 0L & length(rr.ov.ind.names.ji) > 0L) {

     lhs <- c(lhs, sort(rr.ov.ind.names.ij))
     rhs <- c(rhs, sort(rr.ov.ind.names.ji))
     #equal <- c(equal,rep(as.numeric(NA),length(rr.ov.ind.names.ij)))

  }

  ## Covariance block in PHI_U

  ## These covariances are added for those rr-lvs elements, that are not part
  ## of a regression model; when there is thus a regression of f1@A~f2@A, we have
  ## to delete the respective variable --> THIS HAS TO BE DONE

  if ( auto.cov.lv.block & length(rr.all.lv.names.ij) > 1L & length(rr.all.lv.names.ji) > 1L ) {

     tmp <- utils::combn(c(rr.all.lv.names.ij,rr.all.lv.names.ji), 2)
     tmp <- SRM_PARTABLE_DELETE_SAME(tmp) # delete all same elements
     lhs <- c(lhs, tmp[1,])
     rhs <- c(rhs, tmp[2,])
     #equal <- c(equal,rep(as.numeric(NA),length(tmp)))

  }

  op <- rep("~~", length(lhs))
  mod.idx <- rep(0,length(lhs))

  ## 2.2 If there are rr-ovs that are not used to define rr-lvs, we treat them
  ##     as single-indicator lvs that have a factor loading of one
  if (length(rr.ov.notind.names.ij) != 0L) {
     lhs <- c(lhs, rr.ov.notind.names.ij, rr.ov.notind.names.ji)
     rhs <- c(rhs, rr.ov.notind.names.ij, rr.ov.notind.names.ji)
     op <- c(op,rep("=~",length(c(rr.ov.notind.names.ij,rr.ov.notind.names.ji))))
     #equal <- c(equal,rep(as.numeric(NA),length(c(rr.ov.notind.names.ij,rr.ov.notind.names.ji))))
     mod.idx <- rep(0,length(lhs))
  }

  ## ADD EXOGENOUS COVARIATES HERE?

  ## 2.3 Default Observed Variable Intercepts
  #if(auto.int.ov && length(rr.ov.names.a) > 0L && length(rr.ov.names.p) > 0L) {
  #   ## Achtung, muss intersect tatsaechlich sein?
  #   tmp <- Reduce(union, list(rr.ov.names.a,rr.ov.names.p))
  #   lhs <- c(lhs, tmp)
  #   rhs <- c(rhs, tmp)
  #  op  <- c(op,  rep("~1", length(tmp)))
  #}

  ## 2.4 Default Lv intercepts --> only those that are predicted
  #if(auto.int.lv && length(rr.lv.names.y.a) > 0L && length(rr.lv.names.y.p) > 0L) {

  #   tmp <- c(rr.lv.names.y.a,rr.lv.names.y.p)
  #   lhs <- c(lhs, tmp)
  #   rhs <- c(rhs, tmp)
  #   op  <- c(op,  rep("~1", length(tmp)))
  #}

  DEFAULT <- data.frame(lhs=lhs, op=op, rhs=rhs,
                        mod.idx=mod.idx,
                        #equal=equal,
                        stringsAsFactors=FALSE)

  ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##  3. We construct the user parameter table and compare with the default table
  ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  #  USER table
  lhs <- TMP.PARLIST$lhs
  op  <- TMP.PARLIST$op
  rhs <- TMP.PARLIST$rhs
  mod.idx <- TMP.PARLIST$mod.idx
  group <- TMP.PARLIST$group
  fixed <- TMP.PARLIST$fixed
  starts <- TMP.PARLIST$starts
  equal <- TMP.PARLIST$equal
  free <- TMP.PARLIST$free

  USER <- data.frame(lhs=lhs, op=op, rhs=rhs,
                     mod.idx=mod.idx,
                     group=group,fixed=fixed,starts=starts,equal=equal,
                     free=free, stringsAsFactors=FALSE)

  # check for duplicated elements in USER
  TMP <- USER[,1:3]
  idx <- which(duplicated(TMP))
  if(length(idx) > 0L) {
    warning("There are duplicated elements in model syntax.
             They have been ignored.")
    USER <- USER[-idx,]
  }

  # We combine USER and DEFAULT and check for duplicated elements
  # These elements are then deleted from DEFAULT
  TMP <- rbind(DEFAULT[,1:3], USER[,1:3])
  idx <- which(duplicated(TMP, fromLast=TRUE)) # idx should be in DEFAULT
  if(length(idx)) { DEFAULT <- DEFAULT[-idx,] }

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##   4. We construct the final parameter table
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  lhs    <- c(USER$lhs, DEFAULT$lhs)
  op     <- c(USER$op,  DEFAULT$op)
  rhs    <- c(USER$rhs, DEFAULT$rhs)
  user   <- c(rep(1L, length(USER$lhs)),
              rep(0L, length(DEFAULT$lhs)))    # user-specified or not
  fixed   <- c(USER$fixed,rep(as.numeric(NA),length(DEFAULT$lhs)))
  starts <- c(USER$starts,rep(as.numeric(NA),length(DEFAULT$lhs))) # user svs
  #equal <- c(USER$equal,DEFAULT$equal)
  equal <- c(USER$equal,rep(as.numeric(NA),length(DEFAULT$lhs)))
  free  <- c(USER$free,rep(1,length(DEFAULT$lhs)))
  mod.idx <- c(USER$mod.idx, DEFAULT$mod.idx)  # modified or not

  #label   <- rep(character(1), length(lhs))
  #exo     <- rep(0L, length(lhs))

  ## some additional definitions
  ## fix first loading of latent actor factor indicator to one
  if(auto.fix.loa.first.ind.ij) {

    # fix metric by fixing the loading of the first indicator
    mm.idx <- which(op == "=~" & grepl("@AP",lhs))
    first.idx <- mm.idx[which(!duplicated(lhs[mm.idx]))]
    fixed[first.idx] <- 1.0
    free[first.idx] <- 0L

  }

  if(auto.fix.loa.first.ind.ji) {

    # fix metric by fixing the loading of the first indicator
    mm.idx <- which(op == "=~" & grepl("@PA",lhs))
    first.idx <- mm.idx[which(!duplicated(lhs[mm.idx]))]
    fixed[first.idx] <- 1.0
    free[first.idx] <- 0L

  }

  if (auto.fix.loa.ind.ij.ji) {

    # we have to constrain the factor laodings of the AP and the PA vector to the same value
    mm.idx.ap <- which(op == "=~" & grepl("@AP",lhs))
    mm.idx.pa <- which(op == "=~" & grepl("@PA",lhs))

    all.idx.ap <- mm.idx.ap[which(duplicated(lhs[mm.idx.ap]))]
    all.idx.pa <- mm.idx.pa[which(duplicated(lhs[mm.idx.pa]))]

    # check
    zz <- paste("eqload",rep(1:length(all.idx.ap)),sep="")
    if ( length( all.idx.ap ) != 0 ) {
       for ( i in 1:length( all.idx.ap ) ) {

           if ( is.na(equal[all.idx.ap[i]] == equal[all.idx.pa[i]]) ) {
              equal[all.idx.ap[i]] = zz[i]
              equal[all.idx.pa[i]] = zz[i]
           } else if ( equal[all.idx.ap[i]] != equal[all.idx.pa[i]] ) {
              warning("There is an error in the definition of the model syntax.
                       Syntax has been corrected in terms of the defaults.")
              equal[all.idx.ap[i]] = zz[i]
              equal[all.idx.pa[i]] = zz[i]
           }
      }
    }
  }
  ## Now, we have the Parameter table for one group; we now expand it for the case
  ## of multiple groups:
  group <- rep(1L, length(lhs))
  if(ngroups > 1) {

        group   <- rep(1:ngroups, each=length(lhs))
        user    <- rep(user,    times=ngroups)
        lhs     <- rep(lhs,     times=ngroups)
        op      <- rep(op,      times=ngroups)
        rhs     <- rep(rhs,     times=ngroups)
        fixed   <- rep(fixed,   times=ngroups)
        free    <- rep(free,    times=ngroups)
        starts  <- rep(starts,  times=ngroups)
        equal   <- rep(equal,   times=ngroups)
        mod.idx <- rep(mod.idx, times=ngroups)

        ## consider group specifcic defaults?
        for (g in 2:ngroups) {

        ###

        }

  }

  # Handling of exogenous variables?
  LIST <- list( lhs  = lhs,
                op   = op,
                rhs  = rhs,
                user = user,
                group = group)

  # other columns
  LIST2 <- list(fixed = fixed,
                starts = starts,
                equal = equal,
                mod.idx = mod.idx,
                free  = free)

  LIST <- c(LIST, LIST2)
  return(LIST)

}
