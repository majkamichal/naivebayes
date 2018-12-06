naive_bayes.formula <- function(formula, data, prior = NULL, laplace = 0,
                                usekernel = FALSE, subset,
                                na.action = stats::na.pass, ...) {

    mf <- match.call(expand.dots = FALSE)
    mf[c("prior", "laplace", "usekernel", "...")] <- NULL
    mf$na.action <- na.action
    mf[[1]] <- as.symbol("model.frame")
    data <- eval.parent(mf)
    y <- stats::model.response(data)
    data <- data[-1]

    if (!is.factor(y) && !is.character(y) && !is.logical(y))
        stop("y has to be either a factor or character or logical vector")

    res <- naive_bayes.default(data, y, prior, laplace, usekernel, ...)
    res$call <- match.call()
    res
}
