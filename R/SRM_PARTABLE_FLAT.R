## File Name: SRM_PARTABLE_FLAT.R
## File Version: 0.09

SRM_PARTABLE_DELETE_SAME <- function(x) {

        # util - function within SRM_PARTABLE_FLAT

        idx <- apply(x,2,function(t) { unlist(strsplit(t[1],"@"))[1] != unlist(strsplit(t[2],"@"))[1] })
        x <- x[,idx]
        return(x)

}

SRM_PARTABLE_FLAT_PERSON  <- function(PARLIST = NULL,
                              # definitions for default parameters
                              # 1. covariance actor-partner effects of one latent rr
                              auto.cov.lv.ap = FALSE,
                              # 2. covariance actor-partner effects of one observed rr
                              auto.cov.ov.ap = FALSE,
                              # 3. covariance a-p-effects of across latent rrs
                              auto.cov.lv.block = FALSE,
                              # 4. meanstructure
                              auto.int.ov = FALSE,
                              auto.int.lv = FALSE,
                              # definitions for fixed values
                              auto.fix.loa.first.ind.a=FALSE,
                              auto.fix.loa.first.ind.p=FALSE,
                              ngroups = 1L)

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
   # 6. latent or observed variables for which a mean is defined

   # the regular rr-lvs
   rr.lv.regular.names.a <- SRM_PARTABLE_VNAMES_PERSON(TMP.PARLIST, type="rr.lv.a")  # rr-lv actors
   rr.lv.regular.names.p <- SRM_PARTABLE_VNAMES_PERSON(TMP.PARLIST, type="rr.lv.p")  # rr-lv partners
   # regular rr-lvs that are used as predictors or are the outcomes
   rr.lv.names.y.a <- SRM_PARTABLE_VNAMES_PERSON(TMP.PARLIST, type="rr.lv.y.a") # dependent rr-lv actors
   rr.lv.names.y.p <- SRM_PARTABLE_VNAMES_PERSON(TMP.PARLIST, type="rr.lv.y.p") # dependent rr-lv partners
   rr.lv.names.x.a <- SRM_PARTABLE_VNAMES_PERSON(TMP.PARLIST, type="rr.lv.x.a") # independent rr-lv actors
   rr.lv.names.x.p <- SRM_PARTABLE_VNAMES_PERSON(TMP.PARLIST, type="rr.lv.x.p") # independent rr-lv partners

   # observed rrs that are the indicators of the regular rr-lvs
   rr.ov.ind.names.a <- SRM_PARTABLE_VNAMES_PERSON(TMP.PARLIST, type="rr.ov.ind.a")
   rr.ov.ind.names.p <- SRM_PARTABLE_VNAMES_PERSON(TMP.PARLIST, type="rr.ov.ind.p")
   # observed rrs that are used as predictors or are the outcomes
   rr.ov.names.y.a <- SRM_PARTABLE_VNAMES_PERSON(TMP.PARLIST, type="rr.ov.y.a") # dependent rr-ov actors
   rr.ov.names.y.p <- SRM_PARTABLE_VNAMES_PERSON(TMP.PARLIST, type="rr.ov.y.p") # dependent rr-ov partners
   rr.ov.names.x.a <- SRM_PARTABLE_VNAMES_PERSON(TMP.PARLIST, type="rr.ov.x.a") # independent rr-ov actors
   rr.ov.names.x.p <- SRM_PARTABLE_VNAMES_PERSON(TMP.PARLIST, type="rr.ov.x.p") # independent rr-ov partners
   # are there covariances in the object?
   rr.cov.names.aa <- SRM_PARTABLE_VNAMES_PERSON(TMP.PARLIST, type="rr.cov.aa")
   rr.cov.names.pp <- SRM_PARTABLE_VNAMES_PERSON(TMP.PARLIST, type="rr.cov.pp")

   # means
   rr.lv.names.a.mean <- SRM_PARTABLE_VNAMES_PERSON(TMP.PARLIST, type="rr.lv.a.mean")
   rr.lv.names.p.mean <- SRM_PARTABLE_VNAMES_PERSON(TMP.PARLIST, type="rr.lv.p.mean")
   # there can only be @A-means in case of observed variables
   rr.ov.names.mean <- SRM_PARTABLE_VNAMES_PERSON(TMP.PARLIST, type="rr.ov.mean")

   # some computations with these variables:
   # ov-rrs that are not defined as regular lv-rrs (they are outcomes or they
   # are predictors, but they are not allowed to be indicators)
   rr.ov.notind.names.a <- setdiff(Reduce(union, list(rr.ov.names.y.a, rr.ov.names.x.a, rr.cov.names.aa)),rr.ov.ind.names.a)
   rr.ov.notind.names.p <- setdiff(Reduce(union, list(rr.ov.names.y.p, rr.ov.names.x.p, rr.cov.names.pp)),rr.ov.ind.names.p)

   # it's possible that the a-part or p-part was defined as the ov-rr so that we
   # we have to expand the respective other vector
   if ( length(rr.ov.notind.names.a) > 0L ) {

      tmp.a <- gsub("@A","",rr.ov.notind.names.a,perl=TRUE)
      tmp.p <- gsub("@P","",rr.ov.notind.names.p,perl=TRUE)
      if (!(tmp.a %in% tmp.p)) { # elements in .p are missing in .a
         tmp <- setdiff(tmp.a,tmp.p)
         rr.ov.notind.names.p <- c(rr.ov.notind.names.p,paste(tmp,"@P",sep=""))
      }
   }

  if ( length(rr.ov.notind.names.p) > 0L ) {

      tmp.a <- gsub("@A","",rr.ov.notind.names.a,perl=TRUE)
      tmp.p <- gsub("@P","",rr.ov.notind.names.p,perl=TRUE)
      if (!(tmp.p %in% tmp.a)) { # elements in .p are missing in .a
         tmp <- setdiff(tmp.p,tmp.a)
         rr.ov.notind.names.a <- c(rr.ov.notind.names.a,paste(tmp,"@A",sep=""))
      }

   }
   # save all rrs (latents and observed)
   rr.all.lv.names.a <- c(rr.lv.regular.names.a,rr.ov.notind.names.a)
   rr.all.lv.names.p <- c(rr.lv.regular.names.p,rr.ov.notind.names.p)

   # true exogenouos covariates
   sv.eqs.x <- SRM_PARTABLE_VNAMES_PERSON(TMP.PARLIST, type="sv.eqs.x")
   sv.eqs.y <- SRM_PARTABLE_VNAMES_PERSON(TMP.PARLIST, type="sv.eqs.y")

  ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##   2. We construct a default parameter table for a single group
  ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  lhs <- rhs <- character(0)
  mod.idx <- integer(0)

  ## 2.1 ALWAYS: variances of latent actor and partner effects
  ##             and residual variances of actor and partner effects

  lhs <- c(lhs, rr.lv.regular.names.a, rr.lv.regular.names.p, rr.ov.ind.names.a, rr.ov.ind.names.p, rr.ov.notind.names.a, rr.ov.notind.names.p )
  rhs <- c(rhs, rr.lv.regular.names.a, rr.lv.regular.names.p, rr.ov.ind.names.a, rr.ov.ind.names.p, rr.ov.notind.names.a, rr.ov.notind.names.p )

  ## 2.3 Default covariance parameters:
  ## per Default, we always include the covariance between the a-part and the
  ## p-part of ONE rr-variable

  if ( auto.cov.lv.ap & length(rr.all.lv.names.a) > 0L & length(rr.all.lv.names.p) > 0L) {

     lhs <- c(lhs, sort(rr.all.lv.names.a))
     rhs <- c(rhs, sort(rr.all.lv.names.p))

  }

  if ( auto.cov.ov.ap & length(rr.ov.ind.names.a) > 0L & length(rr.ov.ind.names.p) > 0L) {

     lhs <- c(lhs, sort(rr.ov.ind.names.a))
     rhs <- c(rhs, sort(rr.ov.ind.names.p))

  }

  ## Covariance block in PHI_U

  ## These covariances are added for those rr-lvs elements, that are not part
  ## of a regression model; when there is thus a regression of f1@A~f2@A, we have
  ## to delete the respective variable --> THIS HAS TO BE DONE

  if ( auto.cov.lv.block & length(rr.all.lv.names.a) > 1L & length(rr.all.lv.names.p) > 1L ) {

     tmp <- utils::combn(c(rr.all.lv.names.a,rr.all.lv.names.a), 2)
     tmp <- SRM_PARTABLE_DELETE_SAME(tmp) # delete all same elements
     lhs <- c(lhs, tmp[1,])
     rhs <- c(rhs, tmp[2,])
  }

  op <- rep("~~", length(lhs))
  mod.idx <- rep(0,length(lhs))

  ## 2.2 If there are rr-ovs that are not used to define rr-lvs, we treat them
  ##     as single-indicator lvs that have a factor loading of one
  if (length(rr.ov.notind.names.a) != 0L) {
     lhs <- c(lhs, rr.ov.notind.names.a, rr.ov.notind.names.p)
     rhs <- c(rhs, rr.ov.notind.names.a, rr.ov.notind.names.p)
     op <- c(op,rep("=~",length(c(rr.ov.notind.names.a,rr.ov.notind.names.p))))
     mod.idx <- c(mod.idx,rep(0,length(c(rr.ov.notind.names.a,rr.ov.notind.names.p))))
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

  ## 2.4 Per default, we estimate the intercepts of the observed variables and set the
  ##     means of the latent factors to zero, in the definition below, we use the
  ##     actor version of the ovs only as the mean of the ovs is not
  ##     dependent upon whether it is actor or partner
  ##     if a user has defined @P in the model definition, we will change this
  ##     later
  if( auto.int.ov && auto.int.lv == FALSE ) {

      tmp <- c(rr.ov.ind.names.a,rr.ov.notind.names.a,rr.lv.regular.names.a, rr.lv.regular.names.p)
      lhs <- c(lhs, tmp)
      rhs <- c(rhs, tmp)
      op  <- c(op,  rep("~1", length(tmp)))
      mod.idx <- c(mod.idx,rep(0,length(tmp)))

  }

  DEFAULT <- data.frame(lhs=lhs, op=op, rhs=rhs,
                        mod.idx=mod.idx,
                        stringsAsFactors=FALSE)

  ## Up to now, DEFAULT contains the DEFAULT parameters for a single group;
  ## We now add the USER-TABLE for the first group to this

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##  3. We construct the user parameter table and compare with the default table for a single group
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
  ##   4. We construct the final parameter table for a single group
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  lhs    <- c(USER$lhs, DEFAULT$lhs)
  op     <- c(USER$op,  DEFAULT$op)
  rhs    <- c(USER$rhs, DEFAULT$rhs)
  user   <- c(rep(1L, length(USER$lhs)),
              rep(0L, length(DEFAULT$lhs)))    # user-specified or not
  fixed   <- c(USER$fixed,rep(as.numeric(NA),length(DEFAULT$lhs)))
  starts <- c(USER$starts,rep(as.numeric(NA),length(DEFAULT$lhs))) # user svs
  equal <- c(USER$equal,rep(as.numeric(NA),length(DEFAULT$lhs)))
  free  <- c(USER$free,rep(1,length(DEFAULT$lhs)))
  mod.idx <- c(USER$mod.idx, DEFAULT$mod.idx)  # modified or not

  #label   <- rep(character(1), length(lhs))
  #exo     <- rep(0L, length(lhs))

  ## some additional definitions
  ## fix first loading of latent actor factor indicator to one
  if(auto.fix.loa.first.ind.a) {

    # fix metric by fixing the loading of the first indicator
    mm.idx <- which(op == "=~" & grepl("@A",lhs))
    first.idx <- mm.idx[which(!duplicated(lhs[mm.idx]))]
    fixed[first.idx] <- 1.0
    free[first.idx] <- 0L

  }

  if(auto.fix.loa.first.ind.p) {

    # fix metric by fixing the loading of the first indicator
    mm.idx <- which(op == "=~" & grepl("@P",lhs))
    first.idx <- mm.idx[which(!duplicated(lhs[mm.idx]))]
    fixed[first.idx] <- 1.0
    free[first.idx] <- 0L

  }

  ## if the user has defined a factor mean, we have to fix the intercept of
  ## the first indicator to zero
  if( length(c(rr.lv.names.a.mean,rr.lv.names.p.mean)) != 0L) {

    # get the first indicator of the factors for which a mean is defined
    mm.idx <- which( lhs %in% c(rr.lv.names.a.mean,rr.lv.names.p.mean) & op == "=~" )
    first.idx <- mm.idx[which(!duplicated(lhs[mm.idx]))]
    # search the positions of the intercept of this indicator
    ov.idx <- which( rhs == rhs[first.idx] & op == "~1" )
    fixed[ov.idx] <- 0.0
    free[ov.idx] <- 0L

  }

  ## the latent factor means of unspecified factors have to be fixed to zero
  if( auto.int.lv == FALSE ) {

    # get the first indicator of the factors for which a mean is defined
    tmp.idx <- which( op == "~1" &
                      lhs %in% c(rr.all.lv.names.a,rr.all.lv.names.p) &
                     !(lhs %in% c(rr.lv.names.a.mean,rr.lv.names.p.mean)) &
                     !(lhs %in% c(rr.ov.notind.names.a,rr.ov.notind.names.p)) )

    fixed[tmp.idx] <- 0.0
    free[tmp.idx] <- 0L

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
