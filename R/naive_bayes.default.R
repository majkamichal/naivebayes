naive_bayes.default <- function (x, y, prior = NULL, laplace = 0,
                                 usekernel = FALSE, usepoisson = FALSE, ...)  {
    data <- as.data.frame(x)
    if (!is.factor(y) & !is.character(y) & !is.logical(y))
        stop("naive_bayes(): y has to be either a factor or character or logical vector")

    if (anyNA(y))
        warning("naive_bayes(): y contains NAs. They are excluded from the estimation process.", call. = FALSE)

    if (!is.factor(y))
        y <- factor(y)
    levels <- levels(y)
    nlev <- length(levels)
    vars <- names(data)

    if (nlev < 2)
        warning("naive_bayes(): y has less than two classes. ", call. = FALSE)

    if (is.null(prior)) {
        prior <- prop.table(table(y, dnn = ""))
    } else {
        if (length(prior) != nlev)
            stop(paste0("naive_bayes(): Vector with prior probabilities should have ",
                        nlev, " entries"))
        prior <- stats::setNames(prior / sum(prior), levels)
    }
    tables <- sapply(names(data), function(x, ...) {
        var <- data[[x]]
        if (is.numeric(var)) {
            if (is.integer(var) & usepoisson) {
                if (any(var < 0, na.rm = TRUE))
                    warning(paste0("naive_bayes(): The feature ", x, " is modelled with Poisson ",
                                   "distribution in \"naive_bayes\" and it contains negative counts"), call. = FALSE)
                tab <- rbind(tapply(var, y, function(x) (sum(x, na.rm = TRUE) + laplace) / ifelse(anyNA(x), length(x[!is.na(x)]), length(x))))
                attr(tab, "cond_dist") <- "Poisson"
                rownames(tab) <- "lambda"
                tab <- as.table(tab)
                if (any(tab == 0))
                    warning(paste0("naive_bayes(): Feature ", x, " - zero lambda estimates are present. Consider Laplace smoothing."), call. = FALSE)
                tab
            } else {
                if (usekernel) {
                    tab <- tapply(var, y, function(x, ...) stats::density(x, na.rm = TRUE, ...), ...)
                    attr(tab, "cond_dist") <- "KDE"
                    tab
                }
                else {
                    tab <- rbind(tapply(var, y, mean, na.rm = TRUE),
                                 tapply(var, y, stats::sd, na.rm = TRUE))
                    rownames(tab) <- c("mean", "sd")
                    names(dimnames(tab)) <- c(x, "")
                    attr(tab, "cond_dist") <- "Gaussian"
                    as.table(tab)
                }
            }
        }
        else {
            tab <- table(y, var, dnn = c("", x))
            if (any(tab == 0) & laplace == FALSE)
                warning(paste0("naive_bayes(): Feature ", x, " - zero probabilities are present. Consider Laplace smoothing."), call. = FALSE)
            tab <- t((tab + laplace) / (rowSums(tab) + laplace * ncol(tab)))
            attr(tab, "cond_dist") <- ifelse(nrow(tab) == 2, "Bernoulli", "Categorical")
            tab
        }
    }, simplify = FALSE, ...)
    attr(tables, "cond_dist") <- sapply(tables, attr, "cond_dist")
    class(tables) <- "naive_bayes_tables"
    structure(list(data = list(x = data, y = y), levels = levels,
                   laplace = laplace, tables = tables, prior = prior, usekernel = usekernel,
                   usepoisson = usepoisson, call = match.call()), class = "naive_bayes")
}
