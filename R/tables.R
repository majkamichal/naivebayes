tables <- function (object, which = NULL) {

    obj_class <- class(object)[1]
    if (!obj_class %in% models()) {
        stop("tables(): tables are available only for ", paste0(models(), collapse = ", "), " objects.", call. = FALSE)
    }
    tabs <- get_tables(object)
    cond_dist <- get_cond_dist(object)
    vars <- if (obj_class == "multinomial_naive_bayes"){
        rownames(tabs)
    } else { names(tabs) }

    if (is.null(cond_dist))
        cond_dist <- recognize_cond_dist(tabs)

    if (is.numeric(which)) {
        len_x <- length(vars)
        if (any(which > len_x))
            stop(paste0("tables(): There ", ifelse(len_x == 1, "is", "are"), " only ", len_x,
                        ifelse(len_x == 1, " table.", " \"naive_bayes\" tables.")), call. = FALSE)
    }

    if (is.character(which) && !all(which %in% vars))
        stop("tables(): at least one variable is not available")

    if (length(which) > length(vars))
        stop("tables(): too many variables selected")

    if (!is.null(which) && !is.character(which) && !is.numeric(which))
        stop("tables(): which must be either character or numeric vector")

    if (is.null(which))
        which <- seq_along(vars)

    if (is.numeric(which))
        v <- vars[which]

    if (is.character(which))
        v <- vars[vars %in% which]

    if (obj_class == "multinomial_naive_bayes") {
        res <- tabs[v, ,drop = FALSE]
        return(res)
    }
    res <- tabs[v]
    attr(res, "cond_dist") <- cond_dist[v]
    res
}

get_tables <- function(object) {
    model <- class(object)
    if (!model %in% models()) {
        stop("tables(): tables are available only for ", paste0(models(), collapse = ", "), " objects.", call. = FALSE)
    }
    switch(model,
           "naive_bayes"               = object$tables,
           "bernoulli_naive_bayes"     = get_bernoulli_tables(object$prob1),
           "gaussian_naive_bayes"      = get_gaussian_tables(object$params),
           "poisson_naive_bayes"       = get_poisson_tables(object$params),
           "multinomial_naive_bayes"   = get_multinomial_tables(object$params),
           "nonparametric_naive_bayes" = object$dens,
    )
}

get_bernoulli_tables <- function(prob1) {
    if (!is.matrix(prob1))
        stop("prob1 must be a matrix and an element of the bernoulli_naive_bayes object")
    n_tables <- nrow(prob1)
    vars <- rownames(prob1)
    tables <- lapply(seq_len(n_tables), function(i) {
        ith_row <- prob1[i, ]
        ith_tab <- as.table(rbind(1 - ith_row, ith_row))
        rownames(ith_tab) <- c("0", "1")
        ith_tab
    })
    names(tables) <- vars
    class(tables) <- "naive_bayes_tables"
    attr(tables, "cond_dist") <- stats::setNames(rep("Bernoulli", n_tables), vars)
    tables
}

get_gaussian_tables <- function(params) {
    if (!is.list(params))
        stop("get_gaussian_tables(): params must be a list with parameter estimates.", call. = FALSE)

    mu <- params$mu
    sd <- params$sd
    vars <- colnames(mu)
    n_tables <- ncol(mu)
    tables <- lapply(seq_len(n_tables), function(i) {
        ith_mu <- mu[ ,i]
        ith_sd <- sd[ ,i]
        ith_tab <- as.table(rbind(ith_mu, ith_sd))
        rownames(ith_tab) <- c("mu", "sd")
        ith_tab
    })
    names(tables) <- vars
    class(tables) <- "naive_bayes_tables"
    attr(tables, "cond_dist") <- stats::setNames(rep("Gaussian", n_tables), vars)
    tables
}

get_poisson_tables <- function(params) {
    if (!is.matrix(params))
        stop("get_poisson_tables(): params must be a matrix with parameter estimates.", call. = FALSE)

    vars <- rownames(params)
    n_tables <- length(vars)
    tables <- lapply(seq_len(n_tables), function(i) {
        ith_lambda <- params[i, ]
        ith_tab <- as.table(rbind(ith_lambda))
        rownames(ith_tab) <- c("lambda")
        ith_tab
    })
    names(tables) <- vars
    class(tables) <- "naive_bayes_tables"
    attr(tables, "cond_dist") <- stats::setNames(rep("Poisson", n_tables), vars)
    tables
}

get_multinomial_tables <- function(params) {
    if (!is.matrix(params))
        stop("get_multinomial_tables(): params must be a matrix with parameter estimates.", call. = FALSE)
    vars <- rownames(params)
    n_tables <- length(vars)
    attr(params, "cond_dist") <- stats::setNames(rep("Multinomial", n_tables), vars)
    params
}
