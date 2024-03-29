.onAttach <- function(libname, pkgname) {

    v <- getNamespaceVersion("naivebayes")
    packageStartupMessage(paste0("naivebayes ", v, " loaded"))
    packageStartupMessage("For more information please visit: ")
    packageStartupMessage("https://majkamichal.github.io/naivebayes/")
}


.onDetach <- function(libpath) {

    rule <- paste0(rep("=", getOption("width")), collapse = "")
    packageStartupMessage(rule)
    packageStartupMessage("Thank you for using the naivebayes package!")
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

.matrix_classes <- c("dgTMatrix", "dgCMatrix", "dsCMatrix", "dtCMatrix",
                     "dgeMatrix", "dsyMatrix", "dspMatrix", "dtrMatrix",
                     "dtpMatrix", "dpoMatrix", "dppMatrix", "dsRMatrix")


`%class%` <- function(lhs, rhs) {
  c1 <- class(lhs)[1]
  c2 <- class(rhs)[1]
  if (c1 %in% models()) {
    stats::predict(lhs, rhs, type = "class")
  } else if (c2 %in% models()) {
    stats::predict(rhs, lhs, type = "class")
  } else {
    stop("%class%: either left or right argument has to belong to the *_naive_bayes object family." , call. = FALSE)
  }
}

`%prob%` <- function(lhs, rhs) {
  c1 <- class(lhs)[1]
  c2 <- class(rhs)[1]
  if (c1 %in% models()) {
    stats::predict(lhs, rhs, type = "prob")
  } else if (c2 %in% models()) {
    stats::predict(rhs, lhs, type = "prob")
  } else {
    stop("%class%: either left or right argument has to belong to the *_naive_bayes object family." , call. = FALSE)
  }
}
