
`[.naive_bayes_tables` <- function(x, i, ...) {
    class(x)  <- "list"
    res <- x[i]
    class(res) <- "naive_bayes_tables"
    res
}

print.naive_bayes_tables <- function(object, ...) {

    n_char <- getOption("width")
    str_left_right <- paste0(rep("=", floor((n_char - 11)/2)),
                             collapse = "")
    str_full <- paste0(str_left_right, " Naive Bayes ",
                       ifelse(n_char%%2 != 0, "=", ""), str_left_right)
    len <- nchar(str_full)
    l <- paste0(rep("-", len), collapse = "")
    cat("\n")
    cat(str_full, "\n")

    n <- length(object)
    for (i in 1:n) {
        ith_tab <- object[[i]]
        ith_name <- names(object)[i]
        if (class(ith_tab) == "array") {
            for (ith_factor in names(ith_tab)) {
                if (i != 1) {
                    cat(l)
                    cat("\n")
                }
                cat("\n")
                cat(paste0(ith_name, "::", ith_factor, " (KDE)", "\n"))
                print(ith_tab[[ith_factor]])
            }
        } else {
            if (i != 1) {
                cat(l)
                cat("\n")
            }
            cat("\n")
            if (any(rownames(ith_tab) == "lambda") & nrow(ith_tab) == 1) {
                dist <- "(Poisson)"
            }
            if (nrow(ith_tab) == 2 & all(!rownames(ith_tab) %in% c("mean", "sd"))) {
                dist <- "(Bernoulli)"
            }
            if (nrow(ith_tab) > 2) {
                dist <- "(Multinomial)"
            }
            if (nrow(ith_tab) == 2 & all(rownames(ith_tab) %in% c("mean", "sd"))) {
                dist <- "(Gaussian)"
            }
            cat(paste0("Feature: ", ith_name, " ", dist, "\n"))
            print(ith_tab)
            cat("\n")

        }
    }
    cat(l)
}

tables <- function (object, which = NULL) {

    vars <- names(object$tables)
    tables <- object$tables

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

    tables[v]
}

