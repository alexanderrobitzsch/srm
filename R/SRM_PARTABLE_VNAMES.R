## File Name: SRM_PARTABLE_VNAMES.R
## File Version: 0.11



## this functions determines the names of the latent rr - vars for actors and
## partners, observed rr-vars for actors and partners and so on

SRM_PARTABLE_VNAMES_PERSON <- function(parlist, type = NULL) ## Add group here
{
   # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   #  at present, this function allows for observed exogenous covariates only
   # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   # Check for empty list
   if(length(parlist$lhs) == 0) return(character(0L))

   ngroups <- length(unique(parlist$group))
   OUT <- vector("list", length = ngroups)

   # we determine the following variables per group and type
   # latent rr-variables
   OUT$rr.lv.a <- vector("list", length = ngroups)    # latent actor
   OUT$rr.lv.p <- vector("list", length = ngroups)    # latent partner
   OUT$rr.lv.y.a <- vector("list", length = ngroups)  # dependent latent actor
   OUT$rr.lv.y.p <- vector("list", length = ngroups)  # dependent latent partner
   OUT$rr.lv.x.a <- vector("list", length = ngroups)  # dependent latent actor
   OUT$rr.lv.x.p <- vector("list", length = ngroups)  # dependent latent partner
   # observed variables
   OUT$rr.ov.ind.a <- vector("list", length = ngroups)
   OUT$rr.ov.ind.p <- vector("list", length = ngroups)
   OUT$rr.ov.y.a <- vector("list", length = ngroups)
   OUT$rr.ov.y.p <- vector("list", length = ngroups)
   OUT$rr.ov.x.a <- vector("list", length = ngroups)
   OUT$rr.ov.x.p <- vector("list", length = ngroups)
   OUT$rr.cov.aa <- vector("list", length = ngroups) # covariances ovs .a
   OUT$rr.cov.pp <- vector("list", length = ngroups) # covariances ovs .p
   # observed rr-variables that are outcomes, true covariates
   OUT$rr.eqs.y.actor   <- vector("list", length = ngroups)
   OUT$rr.eqs.y.partner <- vector("list", length = ngroups)
   OUT$rr.eqs.x.actor   <- vector("list", length = ngroups)
   OUT$rr.eqs.x.partner <- vector("list", length = ngroups)
   OUT$sv.eqs.x <- vector("list", length = ngroups)
   OUT$sv.eqs.y <- vector("list", length = ngroups)
   # intercepts and latent means
   OUT$rr.ov.mean <- vector("list", length = ngroups)
   OUT$rr.lv.a.mean <- vector("list", length = ngroups)
   OUT$rr.lv.p.mean <- vector("list", length = ngroups)

   # Determine elements per group
   g.index = unique(parlist$group) # because there is only one element
   for (g in g.index:g.index) {

        ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##     Names of latent rr variables (is always computed)
        ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        ## 1. lv.names for rr-variables actors
        rr.lv.names.actor <- unique( parlist$lhs[ parlist$group == g &
                                                  parlist$op == "=~" &
                                                  grepl("@A",parlist$lhs) &
                                                  grepl("@A",parlist$rhs) ] )
        ## store rr-lvs for actors
        if("rr.lv.a" %in% type) {  OUT$rr.lv.a[[g]] <- rr.lv.names.actor  }

        ## 2. lv.names for rr-variables partners
        rr.lv.names.partner <- unique( parlist$lhs[ parlist$group == g &
                                                    parlist$op == "=~" &
                                                    grepl("@P",parlist$lhs) &
                                                    grepl("@P",parlist$rhs) ] )
        ## store lvs actors
        if("rr.lv.p" %in% type) {  OUT$rr.lv.p[[g]] <- rr.lv.names.partner  }

        ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##     Names of indicators of latent rr variables
        ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        ## 3. rr.ind.actor --> indicators of lvs actors
        if(!(length(type) == 1L && type %in% c("rr.lv.a"))) {
            rr.ind.actor <- unique( parlist$rhs[ parlist$group == g  &
                                                 parlist$op == "=~"  &
                                                 grepl("@A",parlist$lhs) &
                                                 grepl("@A",parlist$rhs) ] )
        }
        if("rr.ov.ind.a" %in% type) {  OUT$rr.ov.ind.a[[g]] <- rr.ind.actor }

        ## 4. v.ind.partner --> indicators of lvs partners
        if(!(length(type) == 1L && type %in% c("rr.lv.p"))) {
            rr.ind.partner <- unique( parlist$rhs[ parlist$group == g  &
                                                   parlist$op == "=~"  &
                                                   grepl("@P",parlist$lhs) &
                                                   grepl("@P",parlist$rhs) ] )
        }
        if("rr.ov.ind.p" %in% type) {  OUT$rr.ov.ind.p[[g]] <- rr.ind.partner }

        ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##    Names of observed variables that are in covariance formulas
        ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        if(!(length(type) == 1L && type %in% c("rr.lv.a"))) {
            rr.cov.actor <- unique( parlist$rhs[ parlist$group == g  &
                                                 parlist$op == "~~"  &
                                                 grepl("@A",parlist$lhs) &
                                                 grepl("@A",parlist$rhs) ] )
        }
        if("rr.cov.aa" %in% type) {  OUT$rr.cov.aa[[g]] <- rr.cov.actor[!(rr.cov.actor %in% rr.lv.names.actor)]}

        if(!(length(type) == 1L && type %in% c("rr.lv.p"))) {
            rr.cov.partner <- unique( parlist$rhs[ parlist$group == g  &
                                                   parlist$op == "~~"  &
                                                   grepl("@P",parlist$lhs) &
                                                   grepl("@P",parlist$rhs) ] )
        }
        if("rr.cov.pp" %in% type) {  OUT$rr.cov.pp[[g]] <- rr.cov.partner[!(rr.cov.partner %in% rr.lv.names.partner)]}

        ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##    Now the regressions (op = "~" in parlist are left
        ##    possibilitites:
        ##       1. latent rr variable is outcome and/or predictor
        ##       2. observed rr variable is outcome and/or predictor
        ##       3. true actor or partner variable is predictor
        ##       4. mean of latent rr variable
        ##       5. mean of observed rr variable
        ##       6. anything left?
        ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        ## 4. we start with the lhs - regressions and save all latent or
        ##    observed rr - vars in eqs.y.actor (eqs.y.partner)
        ##    that appear in lhs formulas

        if(!(length(type) == 1L && type %in% c("rr.lv.a"))) {
            rr.eqs.y.actor <- unique( parlist$lhs[ parlist$group == g &
                                                   parlist$op == "~"  &
                                                   grepl("@A",parlist$lhs) ] ) # change here
        }
        # store in rr.eqs.y.a
        if("rr.eqs.y.a" %in% type) { OUT$rr.eqs.y.a[[g]] <- rr.eqs.y.actor }

        if(!(length(type) == 1L && type %in% c("rr.lv.p"))) {
            rr.eqs.y.partner <- unique( parlist$lhs[ parlist$group == g &
                                                     parlist$op == "~"  &
                                                     grepl("@P",parlist$lhs) ] )
        }
        # store rr.eqs.y.partner
        if("rr.eqs.y.p" %in% type) { OUT$rr.eqs.y.p[[g]] <- rr.eqs.y.partner }

        # now, we save that lhs - variables that are true covariates
        if(!(length(type) == 1L && type %in% c("rr.lv.a"))) {
            sv.eqs.y <- unique( parlist$lhs[ parlist$group == g &
                                             parlist$op == "~"  &
                                             grepl("@E",parlist$lhs) ] )
        }
        # store in sv.eqs.y
        if("sv.eqs.y" %in% type) { OUT$sv.eqs.y[[g]] <- sv.eqs.y }

        # now, we save the rr-variables that are latent and that are observed
        if("rr.lv.y.a" %in% type) {
           OUT$rr.lv.y.a[[g]] <-
               rr.eqs.y.actor[rr.eqs.y.actor %in% rr.lv.names.actor]
        }
        if("rr.ov.y.a" %in% type) {
           OUT$rr.ov.y.a[[g]] <-
               rr.eqs.y.actor[!(rr.eqs.y.actor %in% rr.lv.names.actor )]
        }
        if("rr.lv.y.p" %in% type) {
           OUT$rr.lv.y.p[[g]] <-
               rr.eqs.y.partner[rr.eqs.y.partner %in% rr.lv.names.partner]
        }
        if("rr.ov.y.p" %in% type) {
           OUT$rr.ov.y.p[[g]] <-
               rr.eqs.y.partner[!(rr.eqs.y.partner %in% rr.lv.names.partner )]
        }

        ## 5. we start with the rhs - regressions and save all latent or
        ##    observed rr - vars in eqs.x.actor (eqs.x.partner)
        ##    that appear in rhs formulas

        if(!(length(type) == 1L && type %in% c("rr.lv.a"))) {
            rr.eqs.x.actor <- unique( parlist$rhs[ parlist$group == g &
                                                   parlist$op == "~"  &
                                                   grepl("@A",parlist$rhs) ] )
        }
        # store in rr.eqs.x.a
        if("rr.eqs.x.a" %in% type) { OUT$rr.eqs.x.a[[g]] <- rr.eqs.x.actor }

        if(!(length(type) == 1L && type %in% c("rr.lv.p"))) {
            rr.eqs.x.partner <- unique( parlist$rhs[ parlist$group == g &
                                                     parlist$op == "~"  &
                                                     grepl("@P",parlist$rhs) ] )
        }
        # store rr.eqs.x.partner
        if("rr.eqs.x.p" %in% type) { OUT$rr.eqs.x.p[[g]] <- rr.eqs.x.partner }

        # now, we save that rhs - variables that are true covariates
        if(!(length(type) == 1L && type %in% c("rr.lv.a"))) {
            sv.eqs.x <- unique( parlist$rhs[ parlist$group == g &
                                             parlist$op == "~"  &
                                             grepl("@E",parlist$rhs) ] )
        }
        # store in sv.eqs.x
        if("sv.eqs.x" %in% type) { OUT$sv.eqs.x[[g]] <- sv.eqs.x }

        # now, we save the rr-variables that are latent and that are observed
        if("rr.lv.x.a" %in% type) {
           OUT$rr.lv.x.a[[g]] <-
               rr.eqs.x.actor[rr.eqs.x.actor %in% rr.lv.names.actor]
        }
        if("rr.ov.x.a" %in% type) {
           OUT$rr.ov.x.a[[g]] <-
               rr.eqs.x.actor[!(rr.eqs.x.actor %in% rr.lv.names.actor )]
        }
        if("rr.lv.x.p" %in% type) {
           OUT$rr.lv.x.p[[g]] <-
               rr.eqs.x.partner[rr.eqs.x.partner %in% rr.lv.names.partner]
        }
        if("rr.ov.x.p" %in% type) {
           OUT$rr.ov.x.p[[g]] <-
               rr.eqs.x.partner[!(rr.eqs.x.partner %in% rr.lv.names.partner )]
        }

        ## 5. we look for all latent actor and partner variables where
        ##    a latent mean is defined

        if(!(length(type) == 1L && type %in% c("rr.lv.a"))) {
            lv.actor.mean <- unique( parlist$lhs[ parlist$group == g  &
                                                  parlist$op == "~1"  &
                                                  grepl("@A",parlist$lhs) ] )
        }
        if("rr.lv.a.mean" %in% type) {  OUT$rr.lv.a.mean[[g]] <-  lv.actor.mean[lv.actor.mean %in% rr.lv.names.actor] }

        if(!(length(type) == 1L && type %in% c("rr.lv.p"))) {
            lv.partner.mean <- unique( parlist$lhs[ parlist$group == g  &
                                                    parlist$op == "~1"  &
                                                    grepl("@P",parlist$lhs) ] )
        }
        if("rr.lv.p.mean" %in% type) {  OUT$rr.lv.p.mean[[g]] <-  lv.partner.mean[lv.partner.mean %in% rr.lv.names.partner] }

        ## 6. we look for all observed actor and partner rr variables where
        ##    a mean is defined (REMEMBER all @P for ovs were changed in the Parser-part)

        if(!(length(type) == 1L && type %in% c("rr.lv.a"))) {
            ov.mean <- unique( parlist$lhs[ parlist$group == g  &
                                            parlist$op == "~1"  &
                                            grepl("@A",parlist$lhs) ] )
        }
        if("rr.ov.mean" %in% type) {  OUT$rr.ov.mean[[g]] <-  ov.mean[!(ov.mean %in% rr.lv.names.actor)] }

   }

   if(length(type) == 1L) {
       OUT <- unlist(OUT[[type]])
   } else {
       OUT <- OUT[type]
   }

   OUT

}

SRM_PARTABLE_VNAMES_DYAD <- function(parlist, type = NULL) ## Add group here
{
   # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   #  at present, this function allows for observed exogenous covariates only
   # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   # Check for empty list
   if(length(parlist$lhs) == 0) return(character(0L))

   ngroups <- length(unique(parlist$group))
   OUT <- vector("list", length = ngroups)

   # we determine the following variables per group and type
   # latent rr-variables
   OUT$rr.lv.ij <- vector("list", length = ngroups)
   OUT$rr.lv.ji <- vector("list", length = ngroups)
   OUT$rr.lv.y.ij <- vector("list", length = ngroups)
   OUT$rr.lv.y.ji <- vector("list", length = ngroups)
   OUT$rr.lv.x.ij <- vector("list", length = ngroups)
   OUT$rr.lv.x.ji <- vector("list", length = ngroups)
   # observed variables
   OUT$rr.ov.ind.ij <- vector("list", length = ngroups)
   OUT$rr.ov.ind.ji <- vector("list", length = ngroups)
   OUT$rr.ov.y.ij <- vector("list", length = ngroups)
   OUT$rr.ov.y.ji <- vector("list", length = ngroups)
   OUT$rr.ov.x.ij <- vector("list", length = ngroups)
   OUT$rr.ov.x.ji <- vector("list", length = ngroups)
   OUT$rr.cov.ij <- vector("list", length = ngroups) # covariances ovs .ij
   OUT$rr.cov.ji <- vector("list", length = ngroups) # covariances ovs .ji
   # observed rr-variables that are outcomes, true covariates
   OUT$rr.eqs.y.ij <- vector("list", length = ngroups)
   OUT$rr.eqs.y.ji <- vector("list", length = ngroups)
   OUT$rr.eqs.x.ij <- vector("list", length = ngroups)
   OUT$rr.eqs.x.ji <- vector("list", length = ngroups)
   OUT$sv.eqs.x <- vector("list", length = ngroups)
   OUT$sv.eqs.y <- vector("list", length = ngroups)

   # Determine elements
   g.index = unique(parlist$group) # because there is only one element
   for (g in g.index:g.index) {

        ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##     Names of latent rr variables (is always computed)
        ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        ## 1. lv.names for rr-variables actors
        rr.lv.names.ij <- unique( parlist$lhs[ parlist$group == g &
                                               parlist$op == "=~" &
                                               grepl("@AP",parlist$lhs) &
                                               grepl("@AP",parlist$rhs) ] )
        ## store rr-lvs for actors
        if("rr.lv.ij" %in% type) {  OUT$rr.lv.ij[[g]] <- rr.lv.names.ij  }

        ## 2. lv.names for rr-variables partners
        rr.lv.names.ji <- unique( parlist$lhs[ parlist$group == g &
                                               parlist$op == "=~" &
                                               grepl("@PA",parlist$lhs) &
                                               grepl("@PA",parlist$rhs) ] )
        ## store lvs actors
        if("rr.lv.ji" %in% type) {  OUT$rr.lv.ji[[g]] <- rr.lv.names.ji  }

        ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##     Names of indicators of latent rr variables
        ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        ## 3. rr.ind.actor --> indicators of lvs actors
        if(!(length(type) == 1L && type %in% c("rr.lv.ij"))) {
            rr.ind.ij <- unique( parlist$rhs[ parlist$group == g  &
                                              parlist$op == "=~"  &
                                              grepl("@AP",parlist$lhs) &
                                              grepl("@AP",parlist$rhs) ] )
        }
        if("rr.ov.ind.ij" %in% type) {  OUT$rr.ov.ind.ij[[g]] <- rr.ind.ij }

        ## 4. v.ind.partner --> indicators of lvs partners
        if(!(length(type) == 1L && type %in% c("rr.lv.p"))) {
            rr.ind.ji <- unique( parlist$rhs[ parlist$group == g  &
                                              parlist$op == "=~"  &
                                              grepl("@PA",parlist$lhs) &
                                              grepl("@PA",parlist$rhs) ] )
        }
        if("rr.ov.ind.ji" %in% type) {  OUT$rr.ov.ind.ji[[g]] <- rr.ind.ji }

        ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##    Names of observed variables that are in covariance formulas
        ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        if(!(length(type) == 1L && type %in% c("rr.lv.ij"))) {
            rr.cov.ij <- unique( parlist$rhs[ parlist$group == g  &
                                              parlist$op == "~~"  &
                                              grepl("@AP",parlist$lhs) &
                                              grepl("@AP",parlist$rhs) ] )
        }
        if("rr.cov.ij" %in% type) {  OUT$rr.cov.ij[[g]] <- rr.cov.ij[!(rr.cov.ij %in% rr.lv.names.ij)]}

        if(!(length(type) == 1L && type %in% c("rr.lv.ji"))) {
            rr.cov.ji <- unique( parlist$rhs[ parlist$group == g  &
                                              parlist$op == "~~"  &
                                              grepl("@PA",parlist$lhs) &
                                              grepl("@PA",parlist$rhs) ] )
        }
        if("rr.cov.ji" %in% type) {  OUT$rr.cov.ji[[g]] <- rr.cov.ji[!(rr.cov.ji %in% rr.lv.names.ji)]}

        ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ##    Now the regressions (op = "~" in parlist are left
        ##    possibilitites:
        ##       1. latent rr variable is outcome and/or predictor
        ##       2. obserbed rr variable is outcome and/or predictor
        ##       3. true AP or PA variable is predictor
        ##       4. anything left?
        ##
        ## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        ## 4. we start with the lhs - regressions and save all latent or
        ##    observed rr - vars in eqs.y.ij (eqs.y.ji)
        ##    that appear in lhs formulas

        if(!(length(type) == 1L && type %in% c("rr.lv.ij"))) {
            rr.eqs.y.ij <- unique( parlist$lhs[ parlist$group == g &
                                                parlist$op == "~"  &
                                                grepl("@AP",parlist$lhs) ] )
        }
        # store in rr.eqs.y.a
        if("rr.eqs.y.ij" %in% type) { OUT$rr.eqs.y.ij[[g]] <- rr.eqs.y.ij }

        if(!(length(type) == 1L && type %in% c("rr.lv.ji"))) {
            rr.eqs.y.ji <- unique( parlist$lhs[ parlist$group == g &
                                                parlist$op == "~"  &
                                                grepl("@PA",parlist$lhs) ] )
        }
        # store rr.eqs.y.partner
        if("rr.eqs.y.ji" %in% type) { OUT$rr.eqs.y.ji[[g]] <- rr.eqs.y.ij }

        # now, we save that lhs - variables that are true covariates
        if(!(length(type) == 1L && type %in% c("rr.lv.ij","rv.lv.ji"))) {
            sv.eqs.y <- unique( parlist$lhs[ parlist$group == g &
                                             parlist$op == "~"  &
                                             grepl("@F",parlist$lhs) ] )
        }
        # store in sv.eqs.y
        if("sv.eqs.y" %in% type) { OUT$sv.eqs.y[[g]] <- sv.eqs.y }

        # now, we save the rr-variables that are latent and that are observed
        if("rr.lv.y.ij" %in% type) {
           OUT$rr.lv.y.ij[[g]] <-
               rr.eqs.y.ij[rr.eqs.y.ij %in% rr.lv.names.ij]
        }
        if("rr.ov.y.ij" %in% type) {
           OUT$rr.ov.y.ij[[g]] <-
               rr.eqs.y.ij[!(rr.eqs.y.ij %in% rr.lv.names.ij )]
        }
        if("rr.lv.y.ji" %in% type) {
           OUT$rr.lv.y.ji[[g]] <-
               rr.eqs.y.ji[rr.eqs.y.ji %in% rr.lv.names.ji]
        }
        if("rr.ov.y.ji" %in% type) {
           OUT$rr.ov.y.ji[[g]] <-
               rr.eqs.y.ji[!(rr.eqs.y.ji %in% rr.lv.names.ji )]
        }

        ## 5. we start with the rhs - regressions and save all latent or
        ##    observed rr - vars in eqs.x.actor (eqs.x.partner)
        ##    that appear in rhs formulas

        if(!(length(type) == 1L && type %in% c("rr.lv.ij"))) {
            rr.eqs.x.ij <- unique( parlist$rhs[ parlist$group == g &
                                                parlist$op == "~"  &
                                                grepl("@AP",parlist$rhs) ] )
        }
        # store in rr.eqs.x.ij
        if("rr.eqs.x.ij" %in% type) { OUT$rr.eqs.x.ij[[g]] <- rr.eqs.x.ij }

        if(!(length(type) == 1L && type %in% c("rr.lv.ji"))) {
            rr.eqs.x.ji <- unique( parlist$rhs[ parlist$group == g &
                                                parlist$op == "~"  &
                                                grepl("@PA",parlist$rhs) ] )
        }
        # store rr.eqs.x.ji
        if("rr.eqs.x.ji" %in% type) { OUT$rr.eqs.x.ji[[g]] <- rr.eqs.x.ji }

        # now, we save that rhs - variables that are true covariates
        if(!(length(type) == 1L && type %in% c("rr.lv.ij","rv.lv.ji"))) {
            sv.eqs.x <- unique( parlist$rhs[ parlist$group == g &
                                             parlist$op == "~"  &
                                             grepl("@F",parlist$rhs) ] )
        }
        # store in sv.eqs.x
        if("sv.eqs.x" %in% type) { OUT$sv.eqs.x[[g]] <- sv.eqs.x }

        # now, we save the rr-variables that are latent and that are observed
        if("rr.lv.x.ij" %in% type) {
           OUT$rr.lv.x.ij[[g]] <-
               rr.eqs.x.ij[rr.eqs.x.ij %in% rr.lv.names.ij]
        }
        if("rr.ov.x.ij" %in% type) {
           OUT$rr.ov.x.ij[[g]] <-
               rr.eqs.x.ij[!(rr.eqs.x.ij %in% rr.lv.names.ij )]
        }
        if("rr.lv.x.ji" %in% type) {
           OUT$rr.lv.x.ji[[g]] <-
               rr.eqs.x.ji[rr.eqs.x.ji %in% rr.lv.names.ji]
        }
        if("rr.ov.x.ji" %in% type) {
           OUT$rr.ov.x.ji[[g]] <-
               rr.eqs.x.ji[!(rr.eqs.x.ji %in% rr.lv.names.ji )]
        }

   }

   if(length(type) == 1L) {
       OUT <- unlist(OUT[[type]])
   } else {
       OUT <- OUT[type]
   }

   OUT

}
