print.naive_bayes_tables <- function(x, ...) {

    symbol = ":::"
    n_char <- getOption("width")
    str_left_right <- paste0(rep("=", floor((n_char - 11)/2)),
                             collapse = "")
    str_full <- paste0(str_left_right, " Naive Bayes ",
                       ifelse(n_char%%2 != 0, "=", ""), str_left_right)
    len <- nchar(str_full)
    l <- paste0(rep("-", len), collapse = "")
    cat("\n")
    n <- length(x)
    for (i in 1:n) {
        ith_tab <- x[[i]]
        ith_name <- names(x)[i]
        if (class(ith_tab) == "array") {
            for (ith_factor in names(ith_tab)) {
                cat("\n")
                cat(l)
                cat("\n")
                cat(paste0(" ", symbol, " ", ith_name, "::", ith_factor, " (KDE)", "\n"))
                cat(l)
                cat("\n")
                print(ith_tab[[ith_factor]])
            }
        } else {
            cat("\n")
            cat(l)
            cat("\n")
            if (any(rownames(ith_tab) == "lambda") & nrow(ith_tab) == 1) {
                dist <- "(Poisson)"
            }
            if (nrow(ith_tab) == 2 & all(!rownames(ith_tab) %in% c("mean", "sd"))) {
                dist <- "(Bernoulli)"
            }
            if (nrow(ith_tab) > 2) {
                dist <- "(Categorical)"
            }
            if (nrow(ith_tab) == 2 & all(rownames(ith_tab) %in% c("mean", "sd"))) {
                dist <- "(Gaussian)"
            }
            cat(paste0(" ", symbol, " ", ith_name, " ", dist, "\n"))
            cat(l)
            cat("\n")
            if (dist == "(Poisson)") cat("\n")
            print(ith_tab)
            if (dist == "(Poisson)") cat("\n")

        }
    }
    cat(l)
}

`[.naive_bayes_tables` <- function(x, i) {

    if (missing(i)) {
        return(x)
    }

    len_i <- length(i)
    len_x <- length(x)
    nam_x <- names(x)
    class(x)  <- "list"

    if (any(is.na(i))) {
        stop(paste0("NAs are not allowed for indexing of \"naive_bayes\" tables"), call. = FALSE)
    }

    if ((len_x < len_i) | is.numeric(i) & any(i > len_x))
        stop(paste0("There ", ifelse(len_x == 1, "is", "are"), " only ", len_x,
                    ifelse(len_x == 1, " \"naive_bayes\" table", " \"naive_bayes\" tables")), call. = FALSE)

    if (!is.numeric(i) & !is.character(i) & !is.factor(i) & !is.logical(i))
        stop("Indexing vector can only be \"character\", \"factor\", \"numeric\" or \"logical\"")

    if (is.numeric(i)) {
        if (any(i < 0) | any(i %% 1 != 0))
            stop("Indexing vector should contain only positive integers", call. = FALSE)
    }
    if (is.character(i) & any(!i %in% nam_x))
        stop("Indexing vector does not contain correct name(s) of feature(s)", call. = FALSE)

    res <- x[i]
    class(res) <- "naive_bayes_tables"
    res
}

