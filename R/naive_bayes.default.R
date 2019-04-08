naive_bayes.default <- function (x, y, prior = NULL, laplace = 0,
                                 usekernel = FALSE, usepoisson = TRUE, ...)  {
    data <- as.data.frame(x)
    if (!is.factor(y))
        y <- factor(y)
    levels <- levels(y)
    vars <- names(data)
    if (!is.factor(y) & !is.character(y) & !is.logical(y))
        stop("y has to be either a factor or character or logical vector")

    if (is.null(prior)) {
        prior <- prop.table(table(y, dnn = ""))
    }
    else {
        if (length(prior) != length(levels))
            stop(paste0("Vector with prior probabilities should have ",
                        length(levels), " entries"))
        prior <- stats::setNames(prior / sum(prior), levels)
    }
    tables <- sapply(names(data), function(x, ...) {
        var <- data[[x]]
        if (is.numeric(var)) {
            if (is.integer(var) & usepoisson) {
                if (any(var < 0))
                    warning(paste0("The feature ", x, " is modelled with Poisson ",
                                   "distribution and contains negative counts"),  call. = FALSE)
                tab <- rbind(tapply(var, y, function(x) (sum(x, na.rm = TRUE) + laplace) / length(x)))
                rownames(tab) <- "lambda"
                tab
            } else {
                if (usekernel) {
                    tapply(var, y, function(x, ...) stats::density(x, na.rm = TRUE, ...), ...)
                }
                else {
                    tab <- rbind(tapply(var, y, mean, na.rm = TRUE),
                                 tapply(var, y, stats::sd, na.rm = TRUE))
                    rownames(tab) <- c("mean", "sd")
                    names(dimnames(tab)) <- c(x, "")
                    as.table(tab)
                }
            }
        }
        else {
            tab <- table(y, var, dnn = c("", x))
            t((tab + laplace) / (rowSums(tab) + laplace * ncol(tab)))
        }
    }, simplify = FALSE, ...)
    class(tables) <- "naive_bayes_tables"
    structure(list(data = list(x = data, y = y), levels = levels,
                   laplace = laplace, tables = tables, prior = prior, usekernel = usekernel,
                   usepoisson = usepoisson, call = match.call()), class = "naive_bayes")
}
