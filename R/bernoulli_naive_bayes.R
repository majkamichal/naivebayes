bernoulli_naive_bayes <- function (x, y, prior = NULL, laplace = 0, ...)  {

    if (!is.factor(y) & !is.character(y) & !is.logical(y))
        stop("bernoulli_naive_bayes(): y has to be either a factor or character or logical vector", call. = FALSE)

    if (!is.factor(y))
        y <- factor(y)
    levels <- levels(y)
    nlev <- nlevels(y)
    vars <- colnames(x)


    if (is.null(vars)) {
        xname <- deparse(substitute(x))
        stop(paste0("bernoulli_naive_bayes(): Column names in the matrix x are required.\n",
                    "       Consider paste0(\"V\", 1:ncol(", xname, ")) as column names \n",
                    "       in both train and test datasets."), call. = FALSE)
    }
    if (class(x) != "matrix") {
        stop("bernoulli_naive_bayes(): x has to be a numeric 0-1 matrix. ", call. = FALSE)
        x <- as.matrix(x)
        if (mode(x) != "numeric")
            stop("bernoulli_naive_bayes(): x has to contain numeric columns with 0-1 values. ",
                 "Please consider coercing features to numeric 0-1 or using the general \"naive_bayes\"",
                 "function, which models \"character\", \"factor\" or \"logical\" variables with two levels with Bernoulli.", call. = FALSE)
    }
    if (anyNA(y))
        stop("bernoulli_naive_bayes(): y contains NAs. They are excluded from the estimation process.", call. = FALSE)

    y_counts <- stats::setNames(tabulate(y), levels)

    if (is.null(prior)) {
        prior <- prop.table(y_counts)
    } else {
        if (length(prior) != length(levels))
            stop(paste0("bernoulli_naive_bayes(): Vector with prior probabilities should have ",
                        length(levels), " entries"))
        prior <- stats::setNames(prior / sum(prior), levels)
    }

    prob1 <- t((rowsum(x, y, na.rm = TRUE) + laplace) /  (y_counts + laplace * 2))

    structure(list(data = list(x = x, y = y), levels = levels,
                   laplace = laplace, prob1 = prob1, prior = prior,
                   call = match.call()), class = "bernoulli_naive_bayes")
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
    cat(" A priori probabilities:", "\n")
    print(x$prior)
    cat("\n")
    cat(l, "\n", "\n")
    cat(" Tables:", "\n")
    tabs <- get_bernoulli_tables(x$prob1)
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
                                       arg.cat = list(), ...) {

    model <- "bernoulli_naive_bayes"
    if (!class(x) %in% model)
        stop(paste0("plot.bernoulli_naive_bayes(): x must be of class ",
                    model), call. = FALSE)

    tables <- get_bernoulli_tables(x$prob1)
    vars <- names(tables)

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
        arg.cat$xlab <- i
        params <- c(list(x = quote(t(i_tab))), c(arg.cat))
        do.call("mosaicplot", params)
    }
    invisible()
}


get_bernoulli_tables <- function(prob1) {
    if (!is.matrix(prob1))
        stop("prob1 has to be a matrix and prob1 element of the bernoulli_naive_bayes object")
    nrows <- nrow(prob1)
    tables <- lapply(seq_len(nrows), function(i) {
        ith_row <- prob1[i, ]
        ith_tab <- as.table(rbind(1 - ith_row, ith_row))
        rownames(ith_tab) <- c("0", "1")
        ith_tab
    })
    names(tables) <- rownames(prob1)
    attr(tables, "cond_dist") <- rep("Bernoulli", nrows)
    class(tables) <- "naive_bayes_tables"
    tables
}


predict.bernoulli_naive_bayes <- function(object, newdata = NULL, type = c("class", "prob"), ...) {

    if (is.null(newdata))
        newdata <- object$data$x
    if (!is.matrix(newdata))
        stop("predict.bernoulli_naive_bayes(): \"newdata\" has to be a matrix with at least one row and two columns.", call. = FALSE)
    if (mode(newdata) != "numeric")
        stop("predict.bernoulli_naive_bayes(): \"newdata\" has to be a matrix with numeric 0-1 values.", call. = FALSE)

    # na <- sapply(newdata, anyNA)
    type <- match.arg(type)
    lev <- object$levels
    n_lev <- length(lev)
    n_obs <- dim(newdata)[1L]
    prior <- object$prior
    prob1 <- t(object$prob1)
    features <- colnames(newdata)[colnames(newdata) %in% colnames(prob1)]
    n_tables <- ncol(prob1)
    prob1 <- prob1[ ,features]
    n_features <- length(features)


    if (n_features == 0) {
        if (type == "class") {
            warning(paste0("predict.bernoulli_naive_bayes(): ",
                           "No feature in the newdata correspond to ",
                           "probability tables in the object. ",
                           "Classification is done based on the prior probabilities"), call. = FALSE)
            return(factor(rep(lev[which.max(prior)], n_obs),
                          levels = lev))
        } else {
            warning(paste0("predict.bernoulli_naive_bayes(): ",
                           "No feature in the newdata correspond to ",
                           "probability tables in the object. ",
                           "Posterior probabilities are equal to prior probabilities."), call. = FALSE)
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
                       "Calculation is performed based on features to be found in the tables."), call. = FALSE)
        newdata <- newdata[ ,features]
    }

    post <- tcrossprod(newdata, log(prob1)) + tcrossprod(1 - newdata, log(1 - prob1))
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


bernoulli_tables_to_df  <- function(object, format = "wide") {

    if (class(object) != "bernoulli_naive_bayes")
        stop("bernoulli_tables_to_df() expects object of class \"bernoulli_naive_bayes\".", call. = FALSE)

    if (length(format) != 1 | !format %in% c("wide", "long"))
        stop("bernoulli_tables_to_df(): format has to be either \"wide\" or \"long\".", call. = FALSE)

    if (format == "wide") {
        prob1 <- object$prob1
        levels <- object$levels
        nlev <- length(levels)
        m <- cbind(1 - prob1, prob1)
        ind <- rep(seq_len(nlev), each = 2)
        m <- m[ ,ifelse(seq_along(ind) %% 2 != 0, ind, ind + nlev)]
        colnames(m) <- (paste0(rep(levels, each = 2), ":", c("0", "1")))
        as.data.frame(m)
    } else {
        # TODO
        "I am working on the long format :) "
    }


}
