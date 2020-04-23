## File Name: SRM_PARTABLE_PERSON.R
## File Version: 0.11


## the following two functions are very similar to
## lavaan's lavaanify aka lav_partable functions
## SRM_PARTABLE_PERSON: lavaanify for persons
## SRM_PARTABLE_DYAD: lavaanify for dyads

SRM_PARTABLE_PERSON  <- function(PARLIST = NULL,
                                 as.a.data.frame=TRUE,
                                 ngroups = 1L)

{

    ## for-loop for groups
    ## call SRM_PARTABLE_FLAT to add default elements
    LIST <- SRM_PARTABLE_FLAT_PERSON(
                              PARLIST,
                              # definitions for default parameters
                              # 1. covariance actor-partner effects of one latent rr
                              auto.cov.lv.ap = TRUE,
                              # 2. covariance a-p effects of one observed rr
                              auto.cov.ov.ap = TRUE,
                              # 3. covariance a-p-effects of across latent rrs
                              auto.cov.lv.block = FALSE,
                              # 4. meanstructure
                              auto.int.ov = TRUE,
                              auto.int.lv = FALSE,
                              # definitions for fixed values
                              auto.fix.loa.first.ind.a=TRUE,
                              auto.fix.loa.first.ind.p=TRUE,
                              ngroups = ngroups )

    ## now, we handling the modifiers fixed values and equality constraints
    if (as.a.data.frame) {
       LIST <- as.data.frame(LIST, stringsAsFactors = FALSE)
    }
    TABLE <- SRM_PARTABLE_ORDER(LIST,ngroups=ngroups) # sort the list
    TABLE <- SRM_PARTABLE_FIXEDVALUES(TABLE,name="Person",ngroups=ngroups)
    return(TABLE)

}

