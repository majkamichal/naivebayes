tables <- function (object, which = NULL) {

    if (!class(object) %in% models()) {
        stop(paste0("tables(): Tables are available only for ",
                    paste0(models(), collapse = ", "),
                    " objects."), call. = FALSE)
    }
    if (class(object) == "bernoulli_naive_bayes") {
        tabs <- get_bernoulli_tables(object$prob1)
    } else if  (class(object) == "gaussian_naive_bayes") {
        tabs <- get_gaussian_tables(object$params)

        # Need a general function get_tables: input class, say, "naive_bayes" and
        # then it applies specialized get_*_tables and returns the desired tables.

    } else {
        tabs <- object$tables
    }
    vars <- names(tabs)
    cond_dist <- get_cond_dist(object)

    if (is.null(cond_dist))
        cond_dist <- recognize_cond_dist(tabs)

    if (is.character(which) && !all(which %in% vars))
        stop("tables(): At least one variable is not available")

    if (length(which) > length(vars))
        stop("tables(): Too many variables selected")

    if (!is.null(which) && !is.character(which) && !is.numeric(which))
        stop("tables(): \"which\" has to be either character or numeric vector")

    if (is.null(which))
        which <- seq_along(vars)

    if (is.numeric(which))
        v <- vars[which]

    if (is.character(which))
        v <- vars[vars %in% which]

    res <- tabs[v]
    attr(res, "cond_dist") <- cond_dist[v]
    res
}


get_bernoulli_tables <- function(prob1) {
    if (!is.matrix(prob1))
        stop("prob1 has to be a matrix and prob1 element of the bernoulli_naive_bayes object")
    n_tables <- nrow(prob1)
    tables <- lapply(seq_len(n_tables), function(i) {
        ith_row <- prob1[i, ]
        ith_tab <- as.table(rbind(1 - ith_row, ith_row))
        rownames(ith_tab) <- c("0", "1")
        ith_tab
    })
    names(tables) <- rownames(prob1)
    attr(tables, "cond_dist") <- rep("Bernoulli", n_tables)
    class(tables) <- "naive_bayes_tables"
    tables
}

get_gaussian_tables <- function(params) {
    if (!is.list(params))
        stop("get_gaussian_tables(): params has to be a list with parameter estimates.", call. = FALSE)
    mu <- params$mu
    sd <- params$sd
    n_tables <- ncol(mu)
    tables <- lapply(seq_len(n_tables), function(i) {
        ith_mu <- mu[ ,i]
        ith_sd <- sd[ ,i]
        ith_tab <- as.table(rbind(ith_mu, ith_sd))
        rownames(ith_tab) <- c("mu", "sd")
        ith_tab
    })
    names(tables) <- colnames(mu)
    attr(tables, "cond_dist") <- rep("Gaussian", n_tables)
    class(tables) <- "naive_bayes_tables"
    tables
}
