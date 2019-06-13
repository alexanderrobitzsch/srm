## File Name: SRM_SUMMARY_PACKAGE_VERSIONS.R
## File Version: 0.02

SRM_SUMMARY_PACKAGE_VERSIONS <- function(object)
{
    si <- Sys.info()
    si2 <- utils::sessionInfo()
    cat( paste0(si2$R.version$version.string, " ", si2$R.version$system,
                " | nodename=", si["nodename"], " | login=", si["login"]), "\n" )

    #* print informations about package version
    pack <- "srm"
    d1 <- utils::packageDescription(pkg=pack)
    cat( paste(d1$Package, " ", d1$Version, " (", d1$Date, ")", sep = ""), "\n\n")
}
