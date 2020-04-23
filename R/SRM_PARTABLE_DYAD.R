## File Name: SRM_PARTABLE_DYAD.R
## File Version: 0.16


SRM_PARTABLE_DYAD  <- function(PARLIST = NULL,
                               as.a.data.frame=TRUE,
                               ngroups = 1L)

{

    ## here for-loop for groups?
    ## call SRM_PARTABLE_FLAT to add default elements
    LIST <- SRM_PARTABLE_FLAT_DYAD(
                              PARLIST,
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
                              auto.fix.loa.ind.ij.ji = TRUE,
                              auto.fix.int.first.ind.ij=FALSE,
                              auto.fix.int.first.ind.ji=FALSE,
                              ngroups = ngroups
                              )

    ## now, we handling the modifiers fixed values and equality constraints
    if (as.a.data.frame) {
       LIST <- as.data.frame(LIST, stringsAsFactors = FALSE)
    }
    TABLE <- SRM_PARTABLE_ORDER(LIST, ngroups = ngroups)
    TABLE <- SRM_PARTABLE_FIXEDVALUES(TABLE,name="Dyad",ngroups = ngroups)
    return(TABLE)
}
