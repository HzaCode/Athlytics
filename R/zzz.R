# R/zzz.R

.onAttach <- function(libname, pkgname) {

  pkg_desc <- utils::packageDescription(pkgname)
  pkg_version <- pkg_desc$Version
  
  startup_msg <- paste0(
    "\nLoading Athlytics version ", pkg_version, ".\n",
    "Analyze your Strava data locally with ease!\n",
    "Use load_local_activities() to get started.\n",
    "For documentation, see: https://hzacode.github.io/Athlytics/"
  )


  packageStartupMessage(startup_msg)
}

