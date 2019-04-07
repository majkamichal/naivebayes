tables <- function (object, which = NULL)
{
    vars <- names(object$tables)

    n_char <- getOption("width")
    str_left_right <- paste0(rep("=", floor((n_char - 11)/2)),
                             collapse = "")
    str_full <- paste0(str_left_right, " Naive Bayes ",
                       ifelse(n_char%%2 != 0, "=", ""), str_left_right)
    len <- nchar(str_full)
    l <- paste0(rep("-", len), collapse = "")
    cat("\n")
    cat(str_full, "\n", "\n")

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

    object$tables[v]
}

