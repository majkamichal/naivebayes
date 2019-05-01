multinomial_naive_bayes <- function (x, y, prior = NULL, laplace = 0.5, ...)  {

    if (!is.factor(y) & !is.character(y) & !is.logical(y))
        stop("multinomial_naive_bayes(): y has to be either a factor or character or logical vector", call. = FALSE)
    if (!is.factor(y))
        y <- factor(y)
    levels <- levels(y)
    nlev <- nlevels(y)
    vars <- colnames(x)
    if (nlev < 2)
        warning("multinomial_naive_bayes(): y has less than two classes. ", call. = FALSE)
    if (is.null(vars)) {
        xname <- deparse(substitute(x))
        stop(paste0("multinomial_naive_bayes(): Column names in the matrix x are required.\n",
                    "       Consider paste0(\"V\", 1:ncol(", xname, ")) as column names \n",
                    "       in both train and test datasets."), call. = FALSE)
    }
    if (class(x)[1] != "matrix") {
        stop("multinomial_naive_bayes(): x has to be a numeric matrix.", call. = FALSE)
        x <- as.matrix(x)
        if (mode(x) != "numeric")
            stop("multinomial_naive_bayes(): x has to contain numeric columns.", call. = FALSE)
    }
    NAy <- anyNA(y)
    NAx <- anyNA(x)
    if (NAy) {
        na_y_bool <- is.na(y)
        len_na <- sum(na_y_bool)
        warning(paste0("multinomial_naive_bayes(): y contains ", len_na, " missing",
                       ifelse(len_na == 1, " value", " values"), ". ",
                       ifelse(len_na == 1, "It is", "They are"),
                       " not included (together with the corresponding instances in x) ",
                       "into the estimation process."), call. = FALSE)
        y <- y[!na_y_bool]
        x <- x[!na_y_bool, ]
    }
    if (NAx) {
        na_x_bool <- is.na(x)
        len_nax <- sum(na_x_bool)
        warning(paste0("multinomial_naive_bayes(): x contains ", len_nax, " missing",
                       ifelse(len_nax == 1, " value", " values"), ". ",
                       "They are not included into the estimation process."),
                call. = FALSE)
    }
    y_counts <- tabulate(y)
    if (any(y_counts == 0))
        stop(paste0("multinomial_naive_bayes(): y variable has to contain",
                    " at least one observation per class for estimation process."),
                    call. = FALSE)
    y_counts <- stats::setNames(y_counts, levels)
    if (is.null(prior)) {
        prior <- prop.table(y_counts)
    } else {
        if (length(prior) != nlev)
            stop(paste0("multinomial_naive_bayes(): Vector with prior probabilities should have ",
                        nlev, " entries"))
        prior <- stats::setNames(prior / sum(prior), levels)
    }

    params <- rowsum(x, y, na.rm = TRUE) + laplace
    params <- params / rowSums(params)

    if (any(params == 0)) {
        ind_zero <- which(params == 0, arr.ind = TRUE)
        nempty <- length(ind_zero[ ,2])
        warning(paste0("multinomial_naive_bayes(): There ", ifelse(nempty == 1, "is ", "are "),
                       nempty, " empty ", ifelse(nempty == 1, "cell ", "cells "),
                       "leading to zero estimates. Consider Laplace smoothing."), call. = FALSE)
    }
    structure(list(data = list(x = x, y = y), levels = levels,
                   laplace = laplace, params = t(params), prior = prior,
                   call = match.call()), class = "multinomial_naive_bayes")
}

predict.multinomial_naive_bayes <- function (object, newdata = NULL, type = c("class", "prob"), ...) {

    if (is.null(newdata))
        newdata <- object$data$x
    if (!is.matrix(newdata))
        stop("predict.multinomial_naive_bayes(): \"newdata\" has to be a numeric matrix with at least one row and two columns.", call. = FALSE)
    if (mode(newdata) != "numeric")
        stop("predict.multinomial_naive_bayes(): \"newdata\" has to be a numeric matrix.", call. = FALSE)

    type <- match.arg(type)
    lev <- object$levels
    n_lev <- length(lev)
    n_obs <- dim(newdata)[1L]
    prior <- object$prior
    params <- t(object$params)
    col_names <- colnames(newdata)
    features <- col_names[col_names %in% colnames(params)]
    n_tables <- ncol(params)
    params <- params[ ,features, drop = FALSE]
    n_features <- length(features)
    n_features_newdata <- ncol(newdata)

    if (n_features == 0) {
        if (type == "class") {
            warning(paste0("predict.multinomial_naive_bayes(): ",
                           "No feature in the newdata corresponds to ",
                           "probability tables in the object. ",
                           "Classification is done based on the prior probabilities"), call. = FALSE)
            return(factor(rep(lev[which.max(prior)], n_obs),
                          levels = lev))
        } else {
            warning(paste0("predict.multinomial_naive_bayes(): ",
                           "No feature in the newdata corresponds to ",
                           "probability tables in the object. ",
                           "Posterior probabilities are equal to prior probabilities."), call. = FALSE)
            return(matrix(prior, ncol = n_lev, nrow = n_obs,
                          byrow = TRUE, dimnames = list(NULL, lev)))
        }
    }
    if (n_features < n_tables) {
        warning(paste0("predict.multinomial_naive_bayes(): Only ", n_features, " feature(s) out of ", n_tables,
                       " defined in the naive_bayes object \"", substitute(object),
                       "\" are used for prediction\n"), call. = FALSE)
    }
    if (n_features_newdata > n_features) {
        warning(paste0("predict.multinomial_naive_bayes(): ",
                       "More features in the newdata are provided ",
                       "as there are parameter estimates in the object. ",
                       "Calculation is performed based on features to be found in the object."), call. = FALSE)
        newdata <- newdata[ ,features, drop = FALSE]
    }
    NAx <- anyNA(newdata)
    if (NAx) {
        ind_na <- which(is.na(newdata))
        len_na <- length(ind_na)
        if (len_na > 0)
            warning(paste0("predict.multinomial_naive_bayes(): ", len_na, " missing",
                           ifelse(len_na == 1, " value", " values"),
                           " discovered in the newdata. ",
                           ifelse(len_na == 1, "It is", "They are"),
                           " not included into the calculation."),
                    call. = FALSE)
        newdata[ind_na] <- 0
    }
    # if (object$laplace == 0)
    #     params[params <= 0] <- 0.001
    post <- tcrossprod(newdata, log(params))

    for (ith_class in seq_along(lev)) {
        post[ ,ith_class] <- post[ ,ith_class] + log(prior[ith_class])
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

print.multinomial_naive_bayes <- function (x, ...) {

    model <- "Multinomial Naive Bayes"
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
    params <- x$params
    names(dimnames(params)) <- c("Features", "Classes")
    n <- nrow(params)
    print(params[seq_len(min(10,n)), ])
    cat("\n")
    cat(l, "\n")

    if (n > 10) {
        cat("\n")
        cat("# ... and", n - 10, ifelse(n - 10 == 1, "more feature\n\n", "more features\n\n"))
        cat(l)
    }
    cat("\n\n")
}

plot.multinomial_naive_bayes <- function(x, ...) {

    stop("plot(): Plot method is not available for \"multinomial_naive_bayes\" objects.", call. = FALSE)
}

coef.multinomial_naive_bayes  <- function(object, ...) {
    as.data.frame(object$params)
}

summary.multinomial_naive_bayes <- function(object, ...) {
    model <- "Multinomial Naive Bayes"
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
    cat("    -", paste0(names(object$prior), ": ", round(object$prior, 4), collapse = "\n    - "))
    cat("\n\n")
    cat(l, "\n")
}
