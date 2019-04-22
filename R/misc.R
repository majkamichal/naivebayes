.onAttach <- function(libname, pkgname) {

    v <- getNamespaceVersion("naivebayes")
    packageStartupMessage(paste0("naivebayes ", v, " loaded"))

    # rule <- paste0(rep("=", getOption("width")), collapse = "")
    # packageStartupMessage(rule)
    # packageStartupMessage(paste0(" ::: Naive Bayes Classifier through the naivebayes package v.", v))
    # packageStartupMessage(rule)
}


.onDetach <- function(libpath) {

    rule <- paste0(rep("=", getOption("width")), collapse = "")
    packageStartupMessage(rule)
    packageStartupMessage(" ::: Thank you for using the naivebayes package!")
    packageStartupMessage(rule)
}

models <- function() {
  c("Naive Bayes"                = "naive_bayes",
    "Bernoulli Naive Bayes"      = "bernoulli_naive_bayes",
    "Gaussian Naive Bayes"       = "gaussian_naive_bayes",
    "Poisson Naive Bayes"        = "poisson_naive_bayes",
    "Multinomial Naive Bayes"    = "multinomial_naive_bayes",
    "Non parametric Naive Bayes" = "nonparametric_naive_bayes")
}
