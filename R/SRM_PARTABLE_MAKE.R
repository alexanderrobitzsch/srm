## File Name: SRM_PARTABLE_MAKE.R
## File Version: 0.256


SRM_PARTABLE_MAKE <- function(model.syntax = '',
            ngroups = 1L, as.a.data.frame = TRUE,
            add.attributes = TRUE, data_colnames=NULL, method="ml")
{

    # Prepare the model syntax for parsing
    model <- SRM_PARSER_PREPARE(model.syntax)

    # Split the model into person and dyad part
    models  <- SRM_PARSER_SPLIT(model)
    model.p <- models[[1]]
    model.d <- models[[2]]

    # Analyses for the person.data
    # Ziel: Eine Liste, in dem eine Formel aufgeloest ist in die einzelnen
    # Komponenten
    # z.B. f1@A =~ V1@A+V2@A+V3@A
    # -->
    # lhs:   f1  f1  f1
    # op:    ~=  ~=  ~=
    # rhs:   V1  V2  V3
    # ptype: A   A   A
    # group: 1   1   1
    # fixed: 0   0   0
    # start: 0   0   0

    pers.list <- SRM_PARSER_LIST(model.p, ngroups = ngroups, name="Person")
    dyad.list <- SRM_PARSER_LIST(model.d, ngroups = ngroups, name="Dyad")
    dyad.list <- SRM_PARSER_LIST_ADD_DYAD_FACTORS(dyad.list)

    # We use pers.list to construct a pers.list that
    # contains the user-specified parameters, the default-parameters and
    # the starting values

    pers.table <- SRM_PARTABLE_PERSON(pers.list,as.a.data.frame=as.a.data.frame,ngroups = ngroups)
    dyad.table <- SRM_PARTABLE_DYAD(dyad.list,as.a.data.frame=as.a.data.frame,ngroups = ngroups)

    # Now, we construct the final table, that contains the names of the matrices
    # and the position of the to-be estimated parameters

    pers.matrix <- SRM_PARTABLE_TO_MATRIX( pers.table, name = "Person",
                                           add.attributes = add.attributes,
                                           as.a.data.frame = as.a.data.frame,
                                           ngroups = ngroups)

    dyad.matrix <- SRM_PARTABLE_TO_MATRIX( dyad.table, name = "Dyad",
                                           add.attributes = add.attributes,
                                           as.a.data.frame = as.a.data.frame,
                                           ngroups = ngroups)

    # We combine the Person-Table and the Dyad-Table and add start values
    srm_partable <- rbind(pers.matrix,dyad.matrix)
    srm_partable <- SRM_STARTVALUES(srm_partable,method.startvalues = "default")

    # Ok, some final edits (add a id-column)
    srm_partable <- SRM_IDVARIABLE(TABLE=srm_partable, method=method)

    # include level
    l1 <- strsplit( srm_partable$mat, split="_" )
    srm_partable$level <- unlist(lapply( l1, FUN = function(ll){ ll[2] } ) )
    srm_partable$level[ srm_partable$mat == "BETA" ] <- "U"

    # include variable which indicates unique parameters
    srm_partable$unid <- srm_partable$index
    srm_partable$unid[ is.na(srm_partable$unid) ] <- 0
    srm_partable$unid[ duplicated(srm_partable$unid) ] <- 0
    npar <- max(srm_partable$index, na.rm=TRUE)
    ind <- match(seq_len(npar), srm_partable$unid)

    # add the attributes
    if(add.attributes) {
        attr(srm_partable, "mmNames")  <- c(attr(pers.matrix, "mmNames"),
                                            attr(dyad.matrix, "mmNames"))
        attr(srm_partable, "mmRows")   <- c(attr(pers.matrix, "mmRows"),
                                            attr(dyad.matrix, "mmRows"))
        attr(srm_partable, "mmCols")   <- c(attr(pers.matrix, "mmCols"),
                                            attr(dyad.matrix, "mmCols"))
        attr(srm_partable, "mmSymmetric") <- c(attr(pers.matrix, "mmSymmetric"),
                                               attr(dyad.matrix, "mmSymmetric"))
        attr(srm_partable, "npar") <- max(npar, na.rm=TRUE)
        attr(srm_partable, "parm_extract") <- ind
    }

    #- parameter names
    par_names <- paste0(srm_partable$lhs, srm_partable$op, srm_partable$rhs)
    srm_partable$par_names <- par_names
    srm_partable[ srm_partable$unid == 0, "par_names"] <- ""

    srm_par1 <- srm_partable[ srm_partable$unid > 0 , ]
    srm_par1 <- srm_par1[ order(srm_par1$unid), ]
    attr(srm_partable, "par_names") <- srm_par1$par_names

    #- variable names
    var_names <- unique(c(paste(srm_partable$lhs), paste(srm_partable$rhs)))
    subs <- c("@AP", "@PA", "@A","@P")
    for (ss in subs){
      rrvar_names <- gsub(ss, "", var_names)
    }
    rrvar_names <- intersect( data_colnames, unique(rrvar_names) )
    attr(srm_partable, "rrvar_names") <- rrvar_names

    personcov_names <- gsub("@E", "", var_names)
    personcov_names <- intersect( data_colnames, unique( personcov_names) )
    personcov_names <- SRM_DEFINE_NULL_VECTOR(vec=personcov_names)
    attr(srm_partable, "personcov_names") <- personcov_names

    dyadcov_names <- gsub("@F", "", var_names)
    dyadcov_names <- intersect( data_colnames, unique(dyadcov_names) )
    dyadcov_names <- SRM_DEFINE_NULL_VECTOR(vec=dyadcov_names)
    attr(srm_partable, "dyadcov_names") <- dyadcov_names

    rownames(srm_partable) <- NULL
    return(srm_partable)
}
