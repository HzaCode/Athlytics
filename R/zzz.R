# R/zzz.R

.onAttach <- function(libname, pkgname) {

  pkg_desc <- utils::packageDescription(pkgname)
  pkg_version <- pkg_desc$Version
  
  startup_msg <- paste0(
    "\nLoading Athlytics version ", pkg_version, ".\n",
    "Analyze your Strava data with ease!\n",
    "Remember to authenticate using rStrava::strava_oauth() first.\n",
    "For documentation, see: https://hzacode.github.io/Athlytics/"
  )


  packageStartupMessage(startup_msg)
}

