bernoulli_naive_bayes <- function (x, y, prior = NULL, laplace = 0, ...)  {

    if (!is.factor(y) & !is.character(y) & !is.logical(y))
        stop("bernoulli_naive_bayes(): y must be either a factor or character or logical vector", call. = FALSE)
    if (!is.factor(y))
        y <- factor(y)
    levels <- levels(y)
    nlev <- nlevels(y)
    vars <- colnames(x)
    class_x <- class(x)[1]
    use_Matrix <- class_x %in% .matrix_classes
    if (!is.matrix(x) & !use_Matrix) {
        warning("bernoulli_naive_bayes(): x was coerced to matrix.", call. = FALSE)
        x <- as.matrix(x)
        if (mode(x) != "numeric")
            stop("bernoulli_naive_bayes(): x must be a matrix/dgCMatrix with with numeric {0,1} columns.", call. = FALSE)
    }
    if (use_Matrix) {
        if (!"Matrix" %in% rownames(utils::installed.packages()))
            stop("bernoulli_naive_bayes(): please install \"Matrix\" package.")
        if (class_x != "dgCMatrix")
            stop("bernoulli_naive_bayes(): dgCMatrix class from the Matrix package is only supported.", call. = FALSE)
    }
    if (nlev < 2)
        stop("bernoulli_naive_bayes(): y must contain at least two classes. ", call. = FALSE)
    if (is.null(vars))
        stop("bernoulli_naive_bayes(): x must have unique column names.\n", call. = FALSE)
    NAy <- anyNA(y)
    NAx <- anyNA(x)
    if (NAy) {
        na_y_bool <- is.na(y)
        len_na <- sum(na_y_bool)
        warning(paste0("bernoulli_naive_bayes(): y contains ", len_na, " missing",
                       ifelse(len_na == 1, " value", " values"), ". ",
                       ifelse(len_na == 1, "It is", "They are"),
                       " not included (also the corresponding rows in x) ",
                       "into the estimation process."), call. = FALSE)
        y <- y[!na_y_bool]
        x <- x[!na_y_bool, ]
    }
    if (NAx) {
        na_x <- is.na(x) * 1
        len_nax <- sum(na_x)
        warning(paste0("bernoulli_naive_bayes(): x contains ", len_nax, " missing",
                       ifelse(len_nax == 1, " value", " values"), ". ",
                       ifelse(len_nax == 1, "It is", "They are"),
                       " not included into the estimation process."), call. = FALSE)
    }
    y_counts <- stats::setNames(tabulate(y), levels)
    y_min <- y_counts < 1
    if (any(y_min))
        stop(paste0("bernoulli_naive_bayes(): y variable must contain at least ",
                    "one observation per class for estimation process.",
                    " Class ", paste0(levels[y_min], collapse =  ", "),
                    " has less than 1 observation."), call. = FALSE)
    if (is.null(prior)) {
        prior <- prop.table(y_counts)
    } else {
        if (length(prior) != nlev)
            stop(paste0("bernoulli_naive_bayes(): vector with prior probabilities must have ",
                        nlev, " entries"))
        prior <- stats::setNames(prior / sum(prior), levels)
    }
    if (!NAx) {
        prob1 <- if (use_Matrix) {
            params <- lapply(levels, function(lev) {
                Matrix::colSums(x[y == lev, , drop = FALSE], na.rm = TRUE) + laplace })
            params <- do.call("rbind", params)
            t(params / (y_counts + laplace * 2))
        } else {
            t((rowsum.default(x, y, na.rm = TRUE) + laplace) /  (y_counts + laplace * 2))
        }
    } else {
        n <- if (use_Matrix) {
            na_per_feature <- lapply(levels, function(lev) {
                Matrix::colSums(na_x[y == lev, , drop = FALSE], na.rm = TRUE) })
            n_feature_obs <- y_counts - do.call("rbind", na_per_feature)
            rownames(n_feature_obs) <- levels
            n_feature_obs
        } else {
            y_counts - rowsum.default(na_x, y)
        }
        if (any(n == 0)) {
            warning(paste0("bernoulli_naive_bayes(): x should contain at least one ",
                           "non-missing observation per class for parameter estimation purposes."), call. = FALSE)
        }
        prob1 <- if (use_Matrix) {
            params <- lapply(levels, function(lev) {
                Matrix::colSums(x[y == lev, , drop = FALSE], na.rm = TRUE) + laplace })
            params <- do.call("rbind", params)
            t(params / (n + laplace * 2))
        } else {
            t((rowsum.default(x, y, na.rm = TRUE) + laplace) /  (n + laplace * 2))
        }
    }
    if (any(prob1 == 0)) {
        nempty <- length(which(prob1 == 0, arr.ind = TRUE)[ ,1])
        warning(paste0("bernoulli_naive_bayes(): there ", ifelse(nempty == 1, "is ", "are "),
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
    class_x <- class(newdata)[1]
    use_Matrix <- class_x == "dgCMatrix"
    if (!is.matrix(newdata) & !use_Matrix)
        stop("predict.bernoulli_naive_bayes(): newdata must be numeric matrix or dgCMatrix (Matrix package) with at least one row and two named columns.", call. = FALSE)
    if (is.matrix(newdata) & mode(newdata) != "numeric")
        stop("predict.bernoulli_naive_bayes(): newdata must be a numeric matrix.", call. = FALSE)
    if (use_Matrix & !"Matrix" %in% rownames(utils::installed.packages()))
        stop("predict.bernoulli_naive_bayes(): please install Matrix package", call. = FALSE)

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
    n_features_newdata <- ncol(newdata)

    if (n_features == 0) {
        warning(paste0("predict.bernoulli_naive_bayes(): no feature in newdata corresponds to ",
                       "features defined in the object. Classification is based on prior probabilities."), call. = FALSE)
        if (type == "class") {
            return(factor(rep(lev[which.max(prior)], n_obs), levels = lev))
        } else {
            return(matrix(prior, ncol = n_lev, nrow = n_obs, byrow = TRUE, dimnames = list(NULL, lev)))
        }
    }
    if (n_features < n_tables) {
        warning(paste0("predict.bernoulli_naive_bayes(): only ", n_features, " feature(s) in newdata could be matched ",
                       "with ", n_tables, " feature(s) defined in the object."), call. = FALSE)
    }
    if (n_features_newdata > n_features) {
        warning(paste0("predict.bernoulli_naive_bayes(): newdata contains feature(s) that could not be matched ",
                       "with (", n_features, ") feature(s) defined in the object. Only matching features are used for calculation."), call. = FALSE)
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
        ind_na <- if (use_Matrix) Matrix::which(is.na(newdata), arr.ind = TRUE) else which(is.na(newdata), arr.ind = TRUE)
        len_na <- nrow(ind_na)
        warning(paste0("predict.bernoulli_naive_bayes(): ", len_na, " missing", ifelse(len_na == 1, " value", " values"),
                       " discovered in the newdata. ", ifelse(len_na == 1, "It is", "They are"), " not included in calculation."), call. = FALSE)
        ind_obs <- ind_na[ ,1]
        ind_var <- ind_na[ ,2]
        newdata[ind_na] <- 1
        neutral <- do.call(rbind, tapply(ind_var, ind_obs, function(x) rowSums(lprob1[ ,x, drop = FALSE])))
        ind_obs <- sort(unique(ind_obs))
    }

    if (use_Matrix) {
        post <- Matrix::tcrossprod(newdata, lprob1) + matrix(rowSums(lprob0), n_obs, n_lev, TRUE) - Matrix::tcrossprod(newdata, lprob0)

    } else {
        post <- tcrossprod(newdata, lprob1) + matrix(rowSums(lprob0), n_obs, n_lev, TRUE) - tcrossprod(newdata, lprob0)
    }
    if (NAs) {
        post[ind_obs, ] <- post[ind_obs, ] - neutral
    }

    for (ith_class in seq_along(prior)) {
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
            return(apply(post, 2, function(x) { 1 / if (use_Matrix) Matrix::rowSums(exp(post - x)) else rowSums(exp(post - x)) }))
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
        stop("plot.bernoulli_naive_bayes(): object does not contain data.", call. = FALSE)

    if (is.character(which) && !all(which %in% vars))
        stop("plot.bernoulli_naive_bayes(): at least one variable is not available.", call. = FALSE)

    if (length(which) > length(vars))
        stop("plot.bernoulli_naive_bayes(): too many variables selected", call. = FALSE)

    if (!is.null(which) && !is.character(which) && !is.numeric(which))
        stop("plot.bernoulli_naive_bayes(): which must be either character or numeric vector.", call. = FALSE)

    if (length(list(...)) > 0)
        warning("plot.bernoulli_naive_bayes(): please specify additional parameters using arg.cat parameter", call. = FALSE)

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
