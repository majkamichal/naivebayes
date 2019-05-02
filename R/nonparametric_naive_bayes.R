nonparametric_naive_bayes <- function (x, y, prior = NULL, ...)  {

    if (!is.factor(y) & !is.character(y) & !is.logical(y))
        stop("nonparametric_naive_bayes(): y has to be either a factor or character or logical vector", call. = FALSE)
    if (!is.factor(y))
        y <- factor(y)
    levels <- levels(y)
    nlev <- nlevels(y)
    vars <- colnames(x)
    if (nlev < 2)
        warning("nonparametric_naive_bayes(): y has less than two classes. ", call. = FALSE)
    if (is.null(vars)) {
        xname <- deparse(substitute(x))
        stop(paste0("nonparametric_naive_bayes(): Column names in the matrix x are required.\n",
                    "       Consider paste0(\"V\", 1:ncol(", xname, ")) as column names \n",
                    "       in both train and test datasets."), call. = FALSE)
    }
    if (class(x)[1] != "matrix") {
        stop("nonparametric_naive_bayes(): x has to be a numeric matrix. ", call. = FALSE)
        x <- as.matrix(x)
        if (mode(x) != "numeric")
            stop("nonparametric_naive_bayes(): x has to contain numeric columns.", call. = FALSE)
    }
    NAy <- anyNA(y)
    NAx <- anyNA(x)
    if (NAy) {
        na_y_bool <- is.na(y)
        len_na <- sum(na_y_bool)
        warning(paste0("nonparametric_naive_bayes(): y contains ", len_na, " missing",
                       ifelse(len_na == 1, " value", " values"), ". ",
                       ifelse(len_na == 1, "It is", "They are"),
                       " not included (together with the corresponding instances in x) ",
                       "into the estimation process."), call. = FALSE)
        y <- y[!na_y_bool]
        x <- x[!na_y_bool, ]
    }
    if (NAx) {
        len_nax <- sum(is.na(x))
        warning(paste0("nonparametric_naive_bayes(): x contains ", len_nax, " missing",
                       ifelse(len_nax == 1, " value", " values"), ". ",
                       ifelse(len_nax == 1, "It is", "They are"),
                       " not included into the estimation process."), call. = FALSE)
    }
    y_counts <- stats::setNames(tabulate(y), levels)
    y_min <- y_counts < 2
    if (any(y_min))
        stop(paste0("nonparametric_naive_bayes(): y variable has to contain at least two observations per class for estimation process.",
                    " Class ", paste0(levels[y_min], collapse =  ", "),
                    " has less than 2 observations."), call. = FALSE)
    if (is.null(prior)) {
        prior <- prop.table(y_counts)
    } else {
        if (length(prior) != nlev)
            stop(paste0("nonparametric_naive_bayes(): Vector with prior probabilities should have ",
                        nlev, " entries"))
        prior <- stats::setNames(prior / sum(prior), levels)
    }

    dens <- apply(x, 2, function(col, ...) {
        tab <- tapply(col, y, function(xclass, ...) stats::density(xclass, na.rm = TRUE, ...), ...)
        attr(tab, "cond_dist") <- "KDE"
        tab

    }, ...)
    attr(dens, "cond_dist") <- stats::setNames(rep("KDE", length(vars)), vars)
    class(dens) <- "naive_bayes_tables"
    structure(list(data = list(x = x, y = y), levels = levels,
                   dens = dens, prior = prior,
                   call = match.call()), class = "nonparametric_naive_bayes")
}

predict.nonparametric_naive_bayes <- function (object, newdata = NULL, type = c("class", "prob"), threshold = 0.001, eps = 0, ...) {

    if (is.null(newdata))
        newdata <- object$data$x
    if (!is.matrix(newdata))
        stop("predict.nonparametric_naive_bayes(): \"newdata\" has to be a numeric matrix with at least one row and two columns.", call. = FALSE)
    if (mode(newdata) != "numeric")
        stop("predict.nonparametric_naive_bayes(): \"newdata\" has to be a numeric matrix.", call. = FALSE)

    if (threshold < 0)
        stop("predict.nonparametric_naive_bayes(): threshold has to be non-negative.", call. = FALSE)
    if (eps < 0)
        stop("predict.nonparametric_naive_bayes(): eps has to be non-negative.", call. = FALSE)

    type <- match.arg(type)
    lev <- object$levels
    n_lev <- length(lev)
    n_obs <- dim(newdata)[1L]
    prior <- object$prior
    col_names <- colnames(newdata)
    tables <- object$dens
    features <- colnames(newdata)[colnames(newdata) %in% names(tables)]
    n_tables <- length(tables)
    n_features <- length(features)
    n_features_newdata <- ncol(newdata)

    if (n_features == 0) {
        if (type == "class") {
            warning(paste0("predict.nonparametric_naive_bayes(): ",
                           "No feature in the newdata corresponds to ",
                           "probability tables in the object. ",
                           "Classification is done based on the prior probabilities"), call. = FALSE)
            return(factor(rep(lev[which.max(prior)], n_obs),
                          levels = lev))
        } else {
            warning(paste0("predict.nonparametric_naive_bayes(): ",
                           "No feature in the newdata corresponds to ",
                           "probability tables in the object. ",
                           "Posterior probabilities are equal to prior probabilities."), call. = FALSE)
            return(matrix(prior, ncol = n_lev, nrow = n_obs,
                          byrow = TRUE, dimnames = list(NULL, lev)))
        }
    }
    if (n_features < n_tables) {
        warning(paste0("predict.nonparametric_naive_bayes(): Only ", n_features, " feature(s) out of ", n_tables,
                       " defined in the naive_bayes object \"", substitute(object),
                       "\" are used for prediction\n"), call. = FALSE)
    }
    if (n_features_newdata > n_features) {
        warning(paste0("predict.nonparametric_naive_bayes(): ",
                       "More features in the newdata are provided ",
                       "as there are probability tables in the object. ",
                       "Calculation is performed based on features to be found in the tables."), call. = FALSE)
        newdata <- newdata[ ,features, drop = FALSE]
    }

    NAx <- anyNA(newdata)
    if (NAx) {
        len_na <- sum(is.na(newdata))
        if (len_na > 0)
            warning(paste0("predict.nonparametric_naive_bayes(): ", len_na, " missing",
                           ifelse(len_na == 1, " value", " values"), " discovered in the newdata. ",
                           ifelse(len_na == 1, "It is", "They are"),
                           " not included into the calculation."), call. = FALSE)
        na <- apply(newdata, 2, anyNA)
    }

    eps <- ifelse(eps == 0, log(.Machine$double.xmin), log(eps))
    threshold <- log(threshold)

    post <- matrix(log(prior), nrow = n_obs, ncol = n_lev, byrow = TRUE)
    colnames(post) <- lev

    for (var in features) {
        V <- newdata[ ,var]
        tab <- tables[[var]]
        logp <- sapply(lev, function(z) {
            dens <- tab[[z]]
            log(stats::approx(dens$x, dens$y, xout = V, rule = 2, ties = "ordered")$y)
        })
        if (NAx) { if (na[var]) { logp[is.na(logp)] <- 0 }}
        logp[logp <= eps] <- threshold
        post <- post + logp
    }

    if (type == "class") {
        if (n_obs == 1) {
            return(factor(lev[which.max(post)], levels = lev))
        } else {
            return(factor(lev[max.col(post, "first")], levels = lev))
        }
    }
    else {
        if (n_obs == 1) {
            post <- t(as.matrix(apply(post, 2, function(x) { 1 / sum(exp(post - x)) })))
            colnames(post) <- lev
            return(post)
        }
        else {
            return(apply(post, 2, function(x) { 1 / rowSums(exp(post - x)) }))
        }
    }
}


print.nonparametric_naive_bayes <- function (x, ...) {

    model <- "Nonparametric Naive Bayes"
    n_char <- getOption("width")
    str_left_right <- paste0(rep("=", floor((n_char - nchar(model)) / 2)),
                             collapse = "")
    str_full <- paste0(str_left_right, " ", model, " ",
                       ifelse(n_char %% 2 != 0, "=", ""), str_left_right)
    len <- nchar(str_full)
    l <- paste0(rep("-", len), collapse = "")
    cat("\n")
    cat(str_full, "\n", "\n", "Call:", "\n")
    print(x$call)
    cat("\n")
    cat(l, "\n", "\n")
    cat(" A priori probabilities:", "\n")
    print(x$prior)
    cat("\n")
    cat(l, "\n", "\n")
    cat(" Conditional densities:", "\n")
    tabs <- get_tables(x)
    n <- length(tabs)
    indices <- seq_len(min(1,n))
    tabs <- tabs[indices]
    print(tabs)
    if (n > 1) {
        cat("\n\n")
        cat("# ... and", n - 1, ifelse(n - 1 == 1, "more table\n\n", "more tables\n\n"))
        cat(l)
    }
    cat("\n\n")
}

plot.nonparametric_naive_bayes <- function(x, which = NULL, ask = FALSE, legend = TRUE,
                                           legend.box = FALSE, arg.num = list(),
                                           prob = c("marginal", "conditional"), ...) {

    x$tables <- get_tables(x)
    class(x) <- "naive_bayes"
    x$data$x <- as.data.frame(x$data$x)
    x$usekernel <- TRUE
    x$usepoisson <- FALSE
    plot.naive_bayes(x, which = which, ask = ask, legend = legend, legend.box = legend.box,
                     arg.num = arg.num, prob = prob, ...)
}

summary.nonparametric_naive_bayes <- function(object, ...) {
    model <- "Nonparametric Naive Bayes"
    n_char <- getOption("width")
    str_left_right <- paste0(rep("=", floor((n_char - nchar(model)) / 2)),
                             collapse = "")
    str_full <- paste0(str_left_right, " ", model, " ",
                       ifelse(n_char %% 2 != 0, "=", ""), str_left_right)
    len <- nchar(str_full)
    l <- paste0(rep("-", len), collapse = "")
    cat("\n")
    cat(str_full, "\n", "\n")
    cat("- Call:", deparse(object$call), "\n")
    cat("- Classes:", nlevels(object$data$y), "\n")
    cat("- Samples:", length(object$data$y), "\n")
    cat("- Features:", length(object$dens), "\n")
    cat("- Prior probabilities: \n")
    cat("    -", paste0(names(object$prior), ": ", round(object$prior, 4), collapse = "\n    - "))
    cat("\n\n")
    cat(l, "\n")
}
