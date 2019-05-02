bernoulli_naive_bayes <- function (x, y, prior = NULL, laplace = 0, ...)  {

    if (!is.factor(y) & !is.character(y) & !is.logical(y))
        stop("bernoulli_naive_bayes(): y has to be either a factor or character or logical vector", call. = FALSE)

    if (!is.factor(y))
        y <- factor(y)
    levels <- levels(y)
    nlev <- nlevels(y)
    vars <- colnames(x)

    if (nlev < 2)
        warning("bernoulli_naive_bayes(): y has less than two classes. ", call. = FALSE)

    if (is.null(vars)) {
        xname <- deparse(substitute(x))
        stop(paste0("bernoulli_naive_bayes(): Column names in the matrix x are required.\n",
                    "       Consider paste0(\"V\", 1:ncol(", xname, ")) as column names \n",
                    "       in both train and test datasets."), call. = FALSE)
    }
    if (class(x)[1] != "matrix") {
        stop("bernoulli_naive_bayes(): x has to be a numeric 0-1 matrix. ", call. = FALSE)
        x <- as.matrix(x)
        if (mode(x) != "numeric")
            stop("bernoulli_naive_bayes(): x has to contain numeric columns with 0-1 values. ",
                 "Please consider coercing features to numeric 0-1 or using the general \"naive_bayes\"",
                 "function, which models \"character\", \"factor\" or \"logical\" variables with two levels with Bernoulli.", call. = FALSE)
    }
    NAy <- anyNA(y)
    NAx <- anyNA(x)
    if (NAy) {
        na_y_bool <- is.na(y)
        len_na <- sum(na_y_bool)
        warning(paste0("bernoulli_naive_bayes(): y contains ", len_na, " missing",
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
        warning(paste0("bernoulli_naive_bayes(): x contains ", len_nax, " missing",
                       ifelse(len_nax == 1, " value", " values"), ". ",
                       ifelse(len_nax == 1, "It is", "They are"),
                       " not included into the estimation process."), call. = FALSE)
    }
    y_counts <- stats::setNames(tabulate(y), levels)
    y_min <- y_counts < 1
    if (any(y_min))
        stop(paste0("bernoulli_naive_bayes(): y variable has to contain at least ",
                    "one observation per class for estimation process.",
                    " Class ", paste0(levels[y_min], collapse =  ", "),
                    " has less than 1 observation."), call. = FALSE)
    if (is.null(prior)) {
        prior <- prop.table(y_counts)
    } else {
        if (length(prior) != nlev)
            stop(paste0("bernoulli_naive_bayes(): Vector with prior probabilities should have ",
                        nlev, " entries"))
        prior <- stats::setNames(prior / sum(prior), levels)
    }
    if (!NAx) {
        prob1 <- t((rowsum(x, y, na.rm = TRUE) + laplace) /  (y_counts + laplace * 2))
    } else {
        n <- rowsum((!na_x_bool) * 1, y)
        if (any(n < 2)) {
            warning(paste0("bernoulli_naive_bayes(): x has to contain at least one ",
                           "non-missing observation per class for estimation process."), call. = FALSE)
        }
        prob1 <- t((rowsum(x, y, na.rm = TRUE) + laplace) /  (n + laplace * 2))
    }
    if (any(prob1 == 0)) {
        nempty <- length(which(prob1 == 0, arr.ind = TRUE)[ ,1])
        warning(paste0("multinomial_naive_bayes(): There ", ifelse(nempty == 1, "is ", "are "),
                       nempty, " empty ", ifelse(nempty == 1, "cell ", "cells "),
                       "leading to zero estimates. Consider Laplace smoothing."), call. = FALSE)

    }
    structure(list(data = list(x = x, y = y), levels = levels,
                   laplace = laplace, prob1 = prob1, prior = prior,
                   call = match.call()), class = "bernoulli_naive_bayes")
}

predict.bernoulli_naive_bayes <- function(object, newdata = NULL, type = c("class", "prob"), ...) {

    if (is.null(newdata))
        newdata <- object$data$x
    if (!is.matrix(newdata))
        stop("predict.bernoulli_naive_bayes(): \"newdata\" has to be a matrix with at least one row and two columns.", call. = FALSE)
    if (mode(newdata) != "numeric")
        stop("predict.bernoulli_naive_bayes(): \"newdata\" has to be a matrix with numeric 0-1 values.", call. = FALSE)

    type <- match.arg(type)
    lev <- object$levels
    n_lev <- length(lev)
    n_obs <- dim(newdata)[1L]
    prior <- object$prior
    prob1 <- t(object$prob1)
    features <- colnames(newdata)[colnames(newdata) %in% colnames(prob1)]
    n_tables <- ncol(prob1)
    prob1 <- prob1[ ,features, drop = FALSE]
    n_features <- length(features)

    if (n_features == 0) {
        if (type == "class") {
            warning(paste0("predict.bernoulli_naive_bayes(): ",
                           "No feature in the newdata corresponds to ",
                           "probability tables in the object. ",
                           "Classification is done based on the prior probabilities"),
                    call. = FALSE)
            return(factor(rep(lev[which.max(prior)], n_obs),
                          levels = lev))
        } else {
            warning(paste0("predict.bernoulli_naive_bayes(): ",
                           "No feature in the newdata corresponds to ",
                           "probability tables in the object. ",
                           "Posterior probabilities are equal to prior probabilities."),
                    call. = FALSE)
            return(matrix(prior, ncol = n_lev, nrow = n_obs,
                          byrow = TRUE, dimnames = list(NULL, lev)))
        }
    }
    if (n_features < n_tables) {
        warning(paste0("predict.bernoulli_naive_bayes(): Only ", n_features, " feature(s) out of ", n_tables,
                       " defined in the naive_bayes object \"", substitute(object),
                       "\" are used for prediction\n"), call. = FALSE)
    }
    if (ncol(newdata) > n_features) {
        warning(paste0("predict.bernoulli_naive_bayes(): ",
                       "More features in the newdata are provided ",
                       "as there are probability tables in the object. ",
                       "Calculation is performed based on features to be found in the tables."),
                call. = FALSE)
        newdata <- newdata[ ,features, drop = FALSE]
    }
    if (object$laplace == 0) {
        threshold <- 0.001
        eps <- 0
        prob1[prob1 <= eps] <- threshold
        prob1[prob1 >= (1 - eps)] <- 1 - threshold
    }
    lprob1 <- log(prob1)
    lprob0 <- log(1 - prob1)
    NAs <- anyNA(newdata)
    if (NAs) {
        ind_na <- which(is.na(newdata), arr.ind = TRUE)
        len_na <- nrow(ind_na)
        warning(paste0("predict.bernoulli_naive_bayes(): ", len_na, " missing",
                       ifelse(len_na == 1, " value", " values"), " discovered in the newdata. ",
                       ifelse(len_na == 1, "It is", "They are"),
                       " not included into the calculation."), call. = FALSE)
        ind_obs <- ind_na[ ,1]
        ind_var <- ind_na[ ,2]
        newdata[ind_na] <- 1
        neutral <- do.call(rbind, tapply(ind_var, ind_obs, function(x) rowSums(lprob1[ ,x, drop = FALSE])))
        ind_obs <- sort(unique(ind_obs))
    }
    post <- tcrossprod(newdata, lprob1) + tcrossprod(1 - newdata, lprob0)
    if (NAs) {
        post[ind_obs, ] <- post[ind_obs, ] - neutral
    }
    for (ith_class in seq_along(prior))
        post[ ,ith_class] <- post[ ,ith_class] + log(prior[ith_class])

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
print.bernoulli_naive_bayes <- function (x, ...) {

    model <- "Bernoulli Naive Bayes"
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

plot.bernoulli_naive_bayes <- function(x, which = NULL, ask = FALSE,
                                       arg.cat = list(),
                                       prob = c("marginal", "conditional"), ...) {
    prob <- match.arg(prob)
    model <- "bernoulli_naive_bayes"
    if (!class(x) %in% model)
        stop(paste0("plot.bernoulli_naive_bayes(): x must be of class ",
                    model), call. = FALSE)

    tables <- get_bernoulli_tables(x$prob1)
    vars <- names(tables)
    prior <- x$prior
    if (is.null(x$data))
        stop("plot.bernoulli_naive_bayes(): The \"bernoulli_naive_bayes\" object does not contain data.", call. = FALSE)

    if (is.character(which) && !all(which %in% vars))
        stop("plot.bernoulli_naive_bayes(): At least one variable is not available.", call. = FALSE)

    if (length(which) > length(vars))
        stop("plot.bernoulli_naive_bayes(): Too many variables selected", call. = FALSE)

    if (!is.null(which) && !is.character(which) && !is.numeric(which))
        stop("plot.bernoulli_naive_bayes(): \"which\" has to be either character or numeric vector.", call. = FALSE)

    if (length(list(...)) > 0)
        warning("plot.bernoulli_naive_bayes(): Please specify additional parameters with 'arg.cat'", call. = FALSE)

    if (is.null(which))
        which <- seq_along(vars)

    if (is.numeric(which))
        v <- vars[which]

    if (is.character(which))
        v <- vars[vars %in% which]

    opar <- graphics::par()$ask
    graphics::par(ask = ask)
    on.exit(graphics::par(ask = opar))

    for (i in v) {
        i_tab <- tables[[i]]
        lev <- x$levels
        if (!("main" %in% names(arg.cat))) arg.cat$main <- ""
        if (!("color" %in% names(arg.cat))) arg.cat$color <- c("red", "yellow")
        arg.cat$ylab <- i
        if (prob == "marginal") {
            for (ith_class in 1:length(prior))
                i_tab[ ,ith_class] <- i_tab[ ,ith_class] * prior[ith_class]
        }
        params <- c(list(x = quote(t(i_tab))), c(arg.cat))
        do.call("mosaicplot", params)
    }
    invisible()
}

coef.bernoulli_naive_bayes  <- function(object, ...) {
    prob1 <- object$prob1
    levels <- object$levels
    nlev <- length(levels)
    m <- cbind(1 - prob1, prob1)
    ind <- rep(seq_len(nlev), each = 2)
    m <- m[ ,ifelse(seq_along(ind) %% 2 != 0, ind, ind + nlev)]
    colnames(m) <- (paste0(rep(levels, each = 2), ":", c("0", "1")))
    as.data.frame(m)
}

summary.bernoulli_naive_bayes <- function(object, ...) {
    model <- "Bernoulli Naive Bayes"
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
    cat("- Features:", nrow(object$prob1), "\n")
    cat("- Prior probabilities: \n")
    cat("    -", paste0(names(object$prior), ": ", round(object$prior, 4), collapse = "\n    - "))
    cat("\n\n")
    cat(l, "\n")
}
