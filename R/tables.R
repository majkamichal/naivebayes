tables <- function (object, which = NULL) {

    vars <- names(object$tables)
    tabs <- object$tables
    cond_dist <- get_cond_dist(object)
    if (is.null(cond_dist))
        cond_dist <- recognize_cond_dist(tabs)

    if (is.character(which) && !all(which %in% vars))
        stop("At least one variable is not available")

    if (length(which) > length(vars))
        stop("too many variables selected")

    if (!is.null(which) && !is.character(which) && !is.numeric(which))
        stop("\"which\" has to be either character or numeric vector")

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
