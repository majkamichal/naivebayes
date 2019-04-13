.onAttach <- function(libname, pkgname) {

    rule <- paste0(rep("=", getOption("width")), collapse = "")
    v <- packageVersion("naivebayes")
    packageStartupMessage(rule)
    packageStartupMessage(paste0(" ::: Naive Bayes Classifier through the naivebayes package v.", v))
    packageStartupMessage(rule)
}


.onDetach <- function(libname,pkgname) {

    rule <- paste0(rep("=", getOption("width")), collapse = "")
    packageStartupMessage(rule)
    packageStartupMessage(" ::: Thank you for using the naivebayes package!")
    packageStartupMessage(rule)
}
