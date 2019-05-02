poisson_naive_bayes <- function (x, y, prior = NULL, laplace = 0, ...)  {

    if (!is.factor(y) & !is.character(y) & !is.logical(y))
        stop("poisson_naive_bayes(): y has to be either a factor or character or logical vector", call. = FALSE)
    if (!is.factor(y))
        y <- factor(y)
    levels <- levels(y)
    nlev <- nlevels(y)
    vars <- colnames(x)
    if (nlev < 2)
        warning("poisson_naive_bayes(): y has less than two classes. ", call. = FALSE)
    if (is.null(vars)) {
        xname <- deparse(substitute(x))
        stop(paste0("poisson_naive_bayes(): Column names in the matrix x are required.\n",
                    "       Consider paste0(\"V\", 1:ncol(", xname, ")) as column names \n",
                    "       in both train and test datasets."), call. = FALSE)
    }
    if (class(x)[1] != "matrix") {
        stop("poisson_naive_bayes(): x has to be a numeric 0-1 matrix. ", call. = FALSE)
        x <- as.matrix(x)
        if (mode(x) != "numeric")
            stop("poisson_naive_bayes(): x has to contain numeric columns with 0-1 values. ",
                 "Please consider coercing features to numeric 0-1 or using the general \"naive_bayes\"",
                 "function, which models \"character\", \"factor\" or \"logical\" variables with two levels with Bernoulli.", call. = FALSE)
    }
    NAy <- anyNA(y)
    NAx <- anyNA(x)
    if (NAy) {
        na_y_bool <- is.na(y)
        len_na <- sum(na_y_bool)
        warning(paste0("poisson_naive_bayes(): y contains ", len_na, " missing",
                       ifelse(len_na == 1, " value", " values"), ". ",
                       ifelse(len_na == 1, "It is", "They are"),
                       " not included (together with the corresponding instances in x) into the estimation process."), call. = FALSE)
        y <- y[!na_y_bool]
        x <- x[!na_y_bool, ]
    }
    if (NAx) {
        na_x_bool <- is.na(x)
        len_nax <- sum(na_x_bool)
        warning(paste0("poisson_naive_bayes(): x contains ", len_nax, " missing",
                       ifelse(len_nax == 1, " value", " values"), ". ",
                       ifelse(len_nax == 1, "It is", "They are"),
                       " not included into the estimation process."), call. = FALSE)
    }
    y_counts <- tabulate(y)
    if (any(y_counts == 0))
        stop("poisson_naive_bayes(): y variable has to contain at least one observation per class for estimation process.", call. = FALSE)
    y_counts <- stats::setNames(y_counts, levels)
    if (is.null(prior)) {
        prior <- prop.table(y_counts)
    } else {
        if (length(prior) != nlev)
            stop(paste0("poisson_naive_bayes(): Vector with prior probabilities should have ",
                        nlev, " entries"))
        prior <- stats::setNames(prior / sum(prior), levels)
    }
    if (!NAx) {
        lambda <- (rowsum(x, y, na.rm = TRUE) + laplace) / y_counts
    } else {
        n <- rowsum((!na_x_bool) * 1, y, na.rm = TRUE)
        if (any(n < 1)) {
            warning(paste0("poisson_naive_bayes(): x has to contain at least one ",
                           "non-missing observation per class for estimation process.",
                           " Infinite lambdas are present."), call. = FALSE)
        }
        lambda <- (rowsum(x, y, na.rm = TRUE) + laplace) / n
    }
    if (any(lambda == 0)) {
        ind_zero <- which(lambda == 0, arr.ind = TRUE)
        nzero <- nrow(ind_zero)
        if (!NAx)
            lambda[ind_zero] <-  1 / rowsum((!is.na(x)) * 1, y, na.rm = TRUE)[ind_zero]
        else
            lambda[ind_zero] <- 1 / n[ind_zero]
        warning(paste0("poisson_naive_bayes(): Zero estimates are present.",
        " Adding one pseudo-count to each empty feature/class cell.",
        " This applies to ", nzero, ifelse(nzero == 1, " empty cell.", " empty cells."),
        " Consider Laplace smoothing."), call. = FALSE)
    }
    structure(list(data = list(x = x, y = y), levels = levels,
                   laplace = laplace, params = t(lambda), prior = prior,
                   call = match.call()), class = "poisson_naive_bayes")
}

predict.poisson_naive_bayes <- function (object, newdata = NULL, type = c("class", "prob"), threshold = 0.001, eps = 0, ...) {

    if (is.null(newdata))
        newdata <- object$data$x
    if (!is.matrix(newdata))
        stop("predict.poisson_naive_bayes(): \"newdata\" has to be a numeric matrix with at least one row and two columns.", call. = FALSE)
    if (mode(newdata) != "numeric")
        stop("predict.poisson_naive_bayes(): \"newdata\" has to be a numeric matrix.", call. = FALSE)

    if (threshold < 0)
        stop("predict.poisson_naive_bayes(): threshold has to be non-negative.", call. = FALSE)
    if (eps < 0)
        stop("predict.poisson_naive_bayes(): eps has to be non-negative.", call. = FALSE)

    type <- match.arg(type)
    lev <- object$levels
    n_lev <- length(lev)
    newdata <- t(newdata)
    n_obs <- dim(newdata)[2L]
    prior <- object$prior
    lambda <- object$params
    row_names <- rownames(newdata)
    features <- row_names[row_names %in% rownames(lambda)]
    n_tables <- nrow(lambda)
    lambda <- lambda[features, , drop = FALSE]
    n_features <- length(features)
    n_features_newdata <- nrow(newdata)

    if (n_features == 0) {
        if (type == "class") {
            warning(paste0("predict.poisson_naive_bayes(): ",
                           "No feature in the newdata corresponds to ",
                           "probability tables in the object. ",
                           "Classification is done based on the prior probabilities"),
                    call. = FALSE)
            return(factor(rep(lev[which.max(prior)], n_obs),
                          levels = lev))
        } else {
            warning(paste0("predict.poisson_naive_bayes(): ",
                           "No feature in the newdata corresponds to ",
                           "probability tables in the object. ",
                           "Posterior probabilities are equal to prior probabilities."),
                    call. = FALSE)
            return(matrix(prior, ncol = n_lev, nrow = n_obs,
                          byrow = TRUE, dimnames = list(NULL, lev)))
        }
    }
    if (n_features < n_tables) {
        warning(paste0("predict.poisson_naive_bayes(): Only ", n_features,
                       " feature(s) out of ", n_tables,
                       " defined in the naive_bayes object \"", substitute(object),
                       "\" are used for prediction\n"),
                call. = FALSE)
    }
    if (n_features_newdata > n_features) {
        warning(paste0("predict.poisson_naive_bayes(): ",
                       "More features in the newdata are provided ",
                       "as there are probability tables in the object. ",
                       "Calculation is performed based on features to be found in the tables."),
                call. = FALSE)
        newdata <- newdata[ ,features, drop = FALSE]
    }

    NAx <- anyNA(newdata)
    if (NAx) {
        ind_na <- which(is.na(newdata))
        len_na <- length(ind_na)
        if (len_na > 0)
            warning(paste0("predict.poisson_naive_bayes(): ", len_na, " missing",
                           ifelse(len_na == 1, " value", " values"), " discovered in the newdata. ",
                           ifelse(len_na == 1, "It is", "They are"),
                           " not included into the calculation."), call. = FALSE)
    }
    eps <- ifelse(eps == 0, log(.Machine$double.xmin), log(eps))
    threshold <- log(threshold)

    post <- matrix(nrow = n_obs, ncol = n_lev)
    for (ith_class in seq_along(lev)) {
        ith_class_lambda <- lambda[ ,ith_class]
        ith_post <- -ith_class_lambda + newdata * log(ith_class_lambda) - lgamma(newdata + 1)
        if (NAx) ith_post[ind_na] <- 0
        ith_post[ith_post <= eps] <- threshold
        post[ ,ith_class] <- colSums(ith_post) + log(prior[ith_class])
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
            colnames(post) <- lev
            return(apply(post, 2, function(x) { 1 / rowSums(exp(post - x)) }))
        }
    }
}

print.poisson_naive_bayes <- function (x, ...) {

    model <- "Poisson Naive Bayes"
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
    cat( "Laplace smoothing:", x$laplace)
    cat("\n")
    cat("\n")
    cat(l, "\n", "\n")
    cat(" A priori probabilities:", "\n")
    print(x$prior)
    cat("\n")
    cat(l, "\n", "\n")
    cat(" Tables:", "\n")
    tabs <- get_tables(x)
    n <- length(tabs)
    indices <- seq_len(min(5,n))
    tabs <- tabs[indices]
    print(tabs)
    if (n > 5) {
        cat("\n\n")
        cat("# ... and", n - 5, ifelse(n - 5 == 1, "more table\n\n", "more tables\n\n"))
        cat(l)
    }
    cat("\n\n")
}

plot.poisson_naive_bayes <- function(x, which = NULL, ask = FALSE, legend = TRUE,
                                     legend.box = FALSE, arg.num = list(),
                                     prob = c("marginal", "conditional"), ...) {

    x$tables <- get_tables(x)
    class(x) <- "naive_bayes"
    x$data$x <- as.data.frame(x$data$x)
    x$usekernel <- FALSE
    x$usepoisson <- TRUE
    plot.naive_bayes(x, which = which, ask = ask, legend = legend, legend.box = legend.box,
                     arg.num = arg.num, prob = prob, ...)
}

coef.poisson_naive_bayes  <- function(object, ...) {
    as.data.frame(object$params)
}

summary.poisson_naive_bayes <- function(object, ...) {
    model <- "Poisson Naive Bayes"
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
    cat("- Laplace:", object$laplace, "\n")
    cat("- Classes:", nlevels(object$data$y), "\n")
    cat("- Samples:", length(object$data$y), "\n")
    cat("- Features:", nrow(object$params), "\n")
    cat("- Prior probabilities: \n")
    cat("    -", paste0(names(object$prior), ": ", round(object$prior, 4),
                        collapse = "\n    - "))
    cat("\n\n")
    cat(l, "\n")
}
