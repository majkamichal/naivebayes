gaussian_naive_bayes <- function (x, y, prior = NULL, ...)  {

    if (!is.factor(y) & !is.character(y) & !is.logical(y))
        stop("gaussian_naive_bayes(): y must be either a factor or character or logical vector", call. = FALSE)
    if (!is.factor(y))
        y <- factor(y)
    levels <- levels(y)
    nlev <- nlevels(y)
    vars <- colnames(x)
    class_x <- class(x)[1]
    use_Matrix <- class_x %in% .matrix_classes
    if (!is.matrix(x) & !use_Matrix) {
        warning("gaussian_naive_bayes(): x was coerced to matrix.", call. = FALSE)
        x <- as.matrix(x)
        if (mode(x) != "numeric")
            stop("gaussian_naive_bayes(): x must be a matrix/dgCMatrix with with numeric columns.", call. = FALSE)
    }
    if (use_Matrix) {
        if (!"Matrix" %in% rownames(utils::installed.packages()))
            stop("gaussian_naive_bayes(): please install \"Matrix\" package.")
        if (class_x != "dgCMatrix")
            stop("gaussian_naive_bayes(): dgCMatrix class from the Matrix package is only supported.", call. = FALSE)
    }
    if (nlev < 2)
        stop("gaussian_naive_bayes(): y must contain at least two classes. ", call. = FALSE)
    if (is.null(vars))
        stop("gaussian_naive_bayes(): x must have unique column names.\n", call. = FALSE)
    NAy <- anyNA(y)
    NAx <- anyNA(x)
    if (NAy) {
        na_y_bool <- is.na(y)
        len_na <- sum(na_y_bool)
        warning(paste0("gaussian_naive_bayes(): y contains ", len_na, " missing",
                       ifelse(len_na == 1, " value", " values"), ". ",
                       ifelse(len_na == 1, "It is", "They are"),
                       " not included (together with the corresponding instances in x) ",
                       " into the estimation process."),
                call. = FALSE)
        y <- y[!na_y_bool]
        x <- x[!na_y_bool, ]
    }
    if (NAx) {
        na_x <- is.na(x) * 1
        len_nax <- sum(na_x)
        warning(paste0("gaussian_naive_bayes(): x contains ", len_nax, " missing",
                       ifelse(len_nax == 1, " value", " values"), ". ",
                       ifelse(len_nax == 1, "It is", "They are"),
                       " not included (also the corresponding rows in x) ",
                       "into the estimation process."), call. = FALSE)
    }
    y_counts <- stats::setNames(tabulate(y), levels)
    y_min <- y_counts < 2
    if (any(y_min))
        stop(paste0("gaussian_naive_bayes(): y variable has to contain at least ",
                    "two observations per class for estimation process.",
                    " Class ", paste0(levels[y_min], collapse =  ", "),
                    " has less than 2 observations."), call. = FALSE)
    if (is.null(prior)) {
        prior <- prop.table(y_counts)
    } else {
        if (length(prior) != nlev)
            stop(paste0("gaussian_naive_bayes(): Vector with prior probabilities should have ",
                        nlev, " entries"))
        prior <- stats::setNames(prior / sum(prior), levels)
    }
    if (!NAx) {
        if (use_Matrix) {
            params <- do.call("rbind", lapply(levels, function(lev) {
                lev_subset <- x[y == lev, , drop = FALSE]
                mu <- Matrix::colMeans(lev_subset, na.rm = TRUE)
                sd <- sqrt((Matrix::colSums(lev_subset^2, na.rm = TRUE) - mu^2 * y_counts[lev]) / (y_counts[lev] - 1))
                rbind(mu, sd) }))
            mu <- params[rownames(params) == "mu", ] ; rownames(mu) <- levels
            sd <- params[rownames(params) == "sd", ] ; rownames(sd) <- levels
        } else {
            mu <- rowsum(x, y, na.rm = TRUE) / y_counts
            sd <- sqrt((rowsum(x^2, y, na.rm = TRUE) - mu^2 * y_counts) / (y_counts - 1))
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
        if (any(n < 2))
            warning("gaussian_naive_bayes(): infinite variances (NaN) are present, ",
                    "in each case due to less than two observations after removing missing values.", call. = FALSE)
        if (use_Matrix) {
            params <- do.call("rbind", lapply(levels, function(lev) {
                lev_subset <- x[y == lev, , drop = FALSE]
                mu <- Matrix::colMeans(lev_subset, na.rm = TRUE)
                nlev <- n[rownames(n) == lev]
                sd <- sqrt((Matrix::colSums(lev_subset^2, na.rm = TRUE) - mu^2 * nlev) / (nlev - 1))
                rbind(mu, sd) }))
            mu <- params[rownames(params) == "mu", ] ; rownames(mu) <- levels
            sd <- params[rownames(params) == "sd", ] ; rownames(sd) <- levels
        } else {
            mu <- rowsum(x, y, na.rm = TRUE) / n
            sd <- sqrt((rowsum(x^2, y, na.rm = TRUE) - mu^2 * n) / (n - 1))
        }
    }
    structure(list(data = list(x = x, y = y), levels = levels,
                   params = list(mu = mu, sd = sd), prior = prior,
                   call = match.call()), class = "gaussian_naive_bayes")
}

predict.gaussian_naive_bayes <- function (object, newdata = NULL, type = c("class", "prob"), threshold = 0.001, eps = 0, ...) {

    if (is.null(newdata))
        newdata <- object$data$x
    class_x <- class(newdata)[1]
    use_Matrix <- class_x == "dgCMatrix"
    if (!is.matrix(newdata) & !use_Matrix)
        stop("predict.gaussian_naive_bayes(): newdata must be numeric matrix or dgCMatrix (Matrix package) with at least one row and two named columns.", call. = FALSE)
    if (is.matrix(newdata) & mode(newdata) != "numeric")
        stop("predict.gaussian_naive_bayes(): newdata must be a numeric matrix.", call. = FALSE)
    if (use_Matrix & !"Matrix" %in% rownames(utils::installed.packages()))
        stop("predict.gaussian_naive_bayes(): please install Matrix package", call. = FALSE)

    if (threshold < 0)
        stop("predict.gaussian_naive_bayes(): threshold must be non-negative.", call. = FALSE)
    if (eps < 0)
        stop("predict.gaussian_naive_bayes(): eps must be non-negative.", call. = FALSE)

    type <- match.arg(type)
    lev <- object$levels
    n_lev <- length(lev)
    newdata <- if (use_Matrix) Matrix::t(newdata) else t.default(newdata)
    n_obs <- dim(newdata)[2L]
    prior <- object$prior
    mu <- object$params$mu
    sd <- object$params$sd
    row_names <- rownames(newdata)
    features <- row_names[row_names %in% colnames(mu)]
    n_tables <- ncol(mu)
    mu <- mu[ ,features, drop = FALSE]
    sd <- sd[ ,features, drop = FALSE]
    n_features <- length(features)
    n_features_newdata <- nrow(newdata)

    if (n_features == 0) {
        warning(paste0("predict.gaussian_naive_bayes(): no feature in newdata corresponds to ",
                       "features defined in the object. Classification is based on prior probabilities."), call. = FALSE)
        if (type == "class") {
            return(factor(rep(lev[which.max(prior)], n_obs), levels = lev))
        } else {
            return(matrix(prior, ncol = n_lev, nrow = n_obs, byrow = TRUE, dimnames = list(NULL, lev)))
        }
    }
    if (n_features < n_tables) {
        warning(paste0("predict.gaussian_naive_bayes(): only ", n_features, " feature(s) in newdata could be matched ",
                       "with ", n_tables, " feature(s) defined in the object."), call. = FALSE)
    }
    if (n_features_newdata > n_features) {
        warning(paste0("predict.gaussian_naive_bayes(): newdata contains feature(s) that could not be matched ",
                       "with (", n_features, ") feature(s) defined in the object. Only matching features are used for calculation."), call. = FALSE)
        newdata <- newdata[ ,features, drop = FALSE]
    }
    NAx <- anyNA(newdata)
    if (NAx) {
        ind_na <- if (use_Matrix) Matrix::which(is.na(newdata)) else which(is.na(newdata))
        len_na <- length(ind_na)
        warning("predict.gaussian_naive_bayes(): ", len_na, " missing",
                ifelse(len_na == 1, " value", " values"), " discovered in the newdata. ",
                ifelse(len_na == 1, "It is", "They are"), " not included in calculation.", call. = FALSE)
    }
    sd[sd <= eps] <- threshold
    eps <- ifelse(eps == 0, log(.Machine$double.xmin), log(eps))
    threshold <- log(threshold)

    post <- matrix(nrow = n_obs, ncol = n_lev)
    for (ith_class in seq_along(lev)) {
        ith_class_sd <- sd[ith_class, ]
        ith_post <- -0.5 * log(2 * pi * ith_class_sd^2) - 0.5 * ((newdata - mu[ith_class, ]) / ith_class_sd)^2
        if (NAx) ith_post[ind_na] <- 0
        ith_post[ith_post <= eps] <- threshold
        post[ ,ith_class] <- if (use_Matrix) Matrix::colSums(ith_post) + log(prior[ith_class]) else colSums(ith_post) + log(prior[ith_class])
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
            return(apply(post, 2, function(x) { 1 / if (use_Matrix) Matrix::rowSums(exp(post - x)) else rowSums(exp(post - x)) }))
        }
    }
}

print.gaussian_naive_bayes <- function (x, ...) {

    model <- "Gaussian Naive Bayes"
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

plot.gaussian_naive_bayes <- function(x, which = NULL, ask = FALSE, legend = TRUE,
                                      legend.box = FALSE, arg.num = list(),
                                      prob = c("marginal", "conditional"), ...) {

    x$tables <- get_tables(x)
    class(x) <- "naive_bayes"
    x$data$x <- as.data.frame(x$data$x)
    x$usekernel <- FALSE
    x$usepoisson <- FALSE
    plot.naive_bayes(x, which = which, ask = ask, legend = legend, legend.box = legend.box,
                     arg.num = arg.num, prob = prob, ...)
}

coef.gaussian_naive_bayes  <- function(object, ...) {
    params <- object$params
    levels <- object$levels
    nlev <- length(levels)
    m <- do.call("rbind", params)
    m <- t(m)
    ind <- rep(seq_len(nlev), each = 2)
    m <- m[ ,ifelse(seq_along(ind) %% 2 != 0, ind, ind + nlev)]
    colnames(m) <- (paste0(rep(levels, each = 2), ":", c("mu", "sd")))
    as.data.frame(m)
}

summary.gaussian_naive_bayes <- function(object, ...) {
    model <- "Gaussian Naive Bayes"
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
    cat("- Samples:", length(object$data$y), "\n")
    cat("- Features:", ncol(object$params$mu), "\n")
    cat("- Prior probabilities: \n")
    cat("    -", paste0(names(object$prior), ": ", round(object$prior, 4),
                        collapse = "\n    - "))
    cat("\n\n")
    cat(l, "\n")
}
