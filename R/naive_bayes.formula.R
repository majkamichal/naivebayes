naive_bayes.formula <- function(formula, data, prior = NULL, laplace = 0,
                                cp_method = c("gaussian", "kde", "bernoulli"),  subset,
                                na.action = stats::na.pass, ...) {

    cp_method <- match.arg(cp_method)
    mf <- match.call(expand.dots = FALSE)
    mf[c("prior", "laplace", "cp_method", "...")] <- NULL
    mf$na.action <- na.action
    mf[[1]] <- as.symbol("model.frame")
    data <- eval.parent(mf)
    y_name <- names(data)[1]
    y <- stats::model.response(data)
    data <- data[-1]

    if (!is.factor(y) && !is.character(y) && !is.logical(y))
        stop("y has to be either a factor or character or logical vector")

    res <- naive_bayes.default(data, y, prior, laplace, cp_method, ...)
    res$call <- match.call()
    res
}
