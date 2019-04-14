tables <- function (object, which = NULL) {

    if (!class(object) %in% models()) {
        stop(paste0("tables(): Tables are available only for ",
                    paste0(models(), collapse = ", "),
                    " objects."), call. = FALSE)
    }
    if (class(object) == "bernoulli_naive_bayes") {
        tabs <- get_bernoulli_tables(object$prob1)

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
