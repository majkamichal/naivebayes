naive_bayes.default <- function(x, y, prior = NULL, laplace = 0,
                                cp_method = c("gaussian", "kde", "bernoulli"), ...) {
    cp_method <- match.arg(cp_method)
    data <- as.data.frame(x)
    if (!is.factor(y))
        y <- factor(y)
    levels <- levels(y)
    vars <- names(data)

    if (!is.factor(y) && !is.character(y) && !is.logical(y))
        stop("y has to be either a factor or character or logical vector")

    if (is.factor(y) && nlevels(y) != length(levels)) {
        warning("Number of unique values in the class variable is not equal to number of levels")
        y <- as.character(y)
    }

    if ((cp_method == 'bernoulli') && any(apply(x, 2, function(z) !all(z %in% c(0, 1))))) {
        stop("x must only take on values in 0, 1 for bernoulli posteriors.")
    }

    if (is.null(prior)) {
        prior <- prop.table(table(y, dnn = ""))
    } else {
        if (length(prior) != length(levels))
            stop(paste0("Vector with prior probabilities should have ",
                        length(levels), " entries"))
        prior <- stats::setNames(prior / sum(prior), levels)
    }

    tables <- sapply(names(data), function(x) {
        var <- data[[x]]
        if (is.numeric(var)) {
            if (cp_method == 'kde') {
                tapply(var, y, function(x, ...) stats::density(x, na.rm = TRUE, ...))
            } else if (cp_method == 'gaussian') {
                tab <- rbind(tapply(var, y, mean, na.rm = TRUE),
                             tapply(var, y, stats::sd, na.rm = TRUE))
                rownames(tab) <- c("mean", "sd")
                names(dimnames(tab)) <- c(x, "")
                as.table(tab)
            } else if (cp_method == 'bernoulli') {
                tab <- tapply(var, y, mean, na.rm = TRUE, simplify = F)
                # rownames(tab) = 'mean'
                names(dimnames(tab)) <- c(x)
                as.table(tab)
            }
        } else {
            tab <- table(y, var, dnn = c("", x))
            t((tab + laplace) / (rowSums(tab) + laplace * nrow(tab)))
        }
    }, simplify = FALSE)

    structure(list(data = list(x = data, y = y), levels = levels,
                   laplace = laplace, tables = tables, prior = prior,
                   cp_method = cp_method, call = match.call()),
              class = "naive_bayes")
}
