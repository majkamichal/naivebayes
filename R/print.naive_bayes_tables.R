print.naive_bayes_tables <- function(x, ...) {

    symbol = ":::"
    n_char <- getOption("width")
    str_left_right <- paste0(rep("=", floor((n_char - 11) / 2)),
                             collapse = "")
    str_full <- paste0(str_left_right, " Naive Bayes ",
                       ifelse(n_char %% 2 != 0, "=", ""), str_left_right)
    len <- nchar(str_full)
    l <- paste0(rep("-", len), collapse = "")
    n <- length(x)
    cond_dists <- get_cond_dist(x)
    if (is.null(cond_dists)) {
        cond_dists <- recognize_cond_dist(x)
    }
    for (i in 1:n) {
        ith_tab <- x[[i]]
        ith_name <- names(x)[i]
        ith_dist <- cond_dists[i]
        if (ith_dist == "KDE") {
            for (ith_factor in names(ith_tab)) {
                cat("\n")
                cat(l, "\n")
                cat(paste0(" ", symbol, " ", ith_name, "::", ith_factor,
                           " (", ith_dist, ")", "\n"))
                cat(l, "\n")
                print(ith_tab[[ith_factor]])
            }
        } else {
            cat("\n")
            cat(l, "\n")
            cat(paste0(" ", symbol, " ", ith_name, " (", ith_dist, ") ", "\n"))
            cat(l, "\n")
            if (ith_dist == "Poisson") cat("\n")
            print(ith_tab)
        }
    }
    cat("\n")
    cat(l)
}

`[.naive_bayes_tables` <- function(x, i) {

    if (missing(i)) {
        return(x)
    }

    len_i <- length(i)
    len_x <- length(x)
    nam_x <- names(x)
    cond_dist <- attr(x, "cond_dist")
    class(x)  <- "list"

    if (any(is.na(i))) {
        stop(paste0("`[`: NAs are not allowed for indexing of \"naive_bayes\" tables."), call. = FALSE)
    }

    if (!is.numeric(i) & !is.character(i) & !is.factor(i) & !is.logical(i))
        stop("`[`: Indexing vector can only be \"character\", \"factor\", \"numeric\" or \"logical\".")

    if (is.numeric(i)) {
        if (any(i < 0) | any(i %% 1 != 0))
            stop("`[`: Indexing vector should contain only positive integers.", call. = FALSE)
        if (any(i > len_x))
            stop(paste0("`[`: There ", ifelse(len_x == 1, "is", "are"), " only ", len_x,
                        ifelse(len_x == 1, " table.", " \"naive_bayes\" tables.")), call. = FALSE)
    }
    if (is.logical(i)) {
        if (length(i) > len_x)
            stop(paste0("`[`: There ", ifelse(len_x == 1, "is", "are"), " only ", len_x,
                        ifelse(len_x == 1, " table.", " \"naive_bayes\" tables.")), call. = FALSE)
        if (all(i == FALSE)) {
            return(list())
        }
    }
    if ((is.character(i) | is.factor(i)) & any(!i %in% nam_x))
        stop("`[`: Undefined columns selected - indexing vector does not contain correct name(s) of feature(s).", call. = FALSE)

    res <- x[i]
    class(res) <- "naive_bayes_tables"
    attr(res, "cond_dist") <- cond_dist
    res
}

get_cond_dist <- function(object) {
    if (class(object) == "naive_bayes") {
        cond_dist <- attr(object$tables, "cond_dist")
    } else if (class(object) == "naive_bayes_tables") {
        cond_dist <- attr(object, "cond_dist")
    } else if (class(object) == "bernoulli_naive_bayes") {
        vars <- rownames(object$prob1)
        cond_dist <- stats::setNames(rep("Bernoulli", length(vars)), vars)
    } else  if (class(object) == "gaussian_naive_bayes") {
        vars <- colnames(object$params$mu)
        cond_dist <- stats::setNames(rep("Gaussian", length(vars)), vars)
    } else  if (class(object) == "poisson_naive_bayes") {
        vars <- rownames(object$params)
        cond_dist <- stats::setNames(rep("Poisson", length(vars)), vars)
    } else  if (class(object) == "multinomial_naive_bayes") {
        vars <- rownames(object$params)
        cond_dist <- stats::setNames(rep("Multinomial", length(vars)), vars)
    } else  if (class(object) == "nonparametric_naive_bayes") {
        cond_dist <- attr(object$dens, "cond_dist")
    } else {
        stop(paste0("get_cond_dist() expects ", paste0(models(), collapse = ", "),
                    ", naive_bayes_tables objects."), call. = FALSE)
    }
    cond_dist
}


recognize_cond_dist <- function(tab) {

    sapply(tab, function(ith_tab) {
        if (class(ith_tab) == "array") {
            cond_dist <- "KDE"
        } else if (class(ith_tab) == "table") {
            rnames <- rownames(ith_tab)
            norm_par <- c("mean", "sd")
            if (any(rownames(ith_tab) == "lambda") & nrow(ith_tab) == 1)
                cond_dist <- "Poisson"
            if (nrow(ith_tab) == 2 & all(!rnames %in% norm_par))
                cond_dist <- "Bernoulli"
            if (nrow(ith_tab) == 2 & all(rnames %in% norm_par))
                cond_dist <- "Gaussian"
            if (nrow(ith_tab) > 2)
                cond_dist <- "Categorical"
        } else {
            cond_dist <- ""
        }
        cond_dist
    })
}
