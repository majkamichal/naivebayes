predict.naive_bayes <- function (object, newdata = NULL, type = c("class", "prob"),
                                 threshold = 0.001, eps = 0, ...) {
    if (is.null(newdata)) {
        newdata <- object$data$x
    } else {
        if (is.matrix(newdata) | is.data.frame(newdata)) {
            newdata <- as.data.frame(newdata)
        }
        else {
            stop("predict.naive_bayes(): \"newdata\" in \"predict\" function has to be either a matrix or a data.frame\n", call. = FALSE)
        }
    }

    if (threshold < 0)
        stop("predict.naive_bayes(): threshold has to be non-negative.", call. = FALSE)
    if (eps < 0)
        stop("predict.naive_bayes(): eps has to be non-negative.", call. = FALSE)

    na <- sapply(newdata, anyNA)
    type <- match.arg(type)
    lev <- object$levels
    n_lev <- length(lev)
    n_obs <- dim(newdata)[1L]
    usekernel <- object$usekernel
    usepoisson <- ifelse(is.null(object$usepoisson), FALSE, object$usepoisson)
    prior <- as.double(object$prior)
    tables <- object$tables
    features <- names(newdata)[names(newdata) %in% names(tables)]
    n_features <- length(features)
    n_tables <- length(tables)
    if (n_features < n_tables) {
        warning(paste0("predict.naive_bayes(): Only ", n_features, " feature(s) out of ", n_tables,
                       " defined in the naive_bayes object \"", substitute(object),
                       "\" are used for prediction\n"), call. = FALSE)
    }
    ind_factor <- sapply(newdata, class) == "factor" & names(newdata) %in% names(tables)
    if (any(ind_factor)) {
        ind_missing_levels <- which((sapply(newdata[ind_factor],
                                            nlevels) != sapply(tables[ind_factor], nrow)) == TRUE)
        nm <- length(ind_missing_levels)
        if (nm > 0) {
            stop(paste0("predict.naive_bayes(): ", ifelse(nm == 1, "Feature ", "Features "),
                        paste0(names(ind_missing_levels), collapse = " "),
                        ifelse(nm == 1, " is of class \"factor\" ",
                               " are of class \"factor\" "), "but compared to the corresponding probability ",
                        ifelse(nm == 1, "table", "tables"), " from the object \"",
                        substitute(object), "\" ", ifelse(nm == 1, "it misses",
                                                          "they miss"), " some levels.", " Please consider filling missing levels or coercing to \"character\"", "\n"), call. = FALSE)
        }
    }
    log_sum <- matrix(log(prior), ncol = n_lev, nrow = n_obs, byrow = TRUE)
    colnames(log_sum) <- lev
    for (var in features) {
        V <- newdata[[var]]
        tab <- tables[[var]]
        if (is.numeric(V)) {
            if (is.integer(V) & usepoisson) {
                p <- sapply(lev, function(lambda) {
                    stats::dpois(V, lambda = tab[ ,lambda])
                })
                p[p <= eps] <- threshold
                if (na[var])
                    p[is.na(p)] <- 1
                log_sum <- log_sum + log(p)
            } else {
                if (usekernel) {
                    p <- sapply(lev, function(z) {
                        dens <- tab[[z]]
                        stats::approx(dens$x, dens$y, xout = V, rule = 2, ties = "ordered")$y
                    })
                    p[p <= eps] <- threshold
                    if (na[var])
                        p[is.na(p)] <- 1
                    log_sum <- log_sum + log(p)
                }
                else {
                    dimnames(tab) <- NULL
                    s <- tab[2, ]
                    s[s <= eps] <- threshold
                    p <- sapply(seq_along(lev), function(z) {
                        stats::dnorm(V, tab[1, z], s[z])
                    })
                    p[p <= 0] <- threshold
                    if (na[var])
                        p[is.na(p)] <- 1
                    log_sum <- log_sum + log(p)
                }
            }
        }
        else {
            if (class(V) == "logical")
                V <- as.character(V)
            if (object$laplace == 0)
                tab[tab <= eps] <- threshold
            tab <- log(tab)
            if (na[var]) {
                na_ind <- which(is.na(V))
                V[na_ind] <- attributes(tab)$dimnames[[1]][1]
                logp <- tab[V, ]
                if (n_obs == 1)
                    logp <- 0
                else logp[na_ind, ] <- 0
                log_sum <- log_sum + logp
            }
            else {
                log_sum <- log_sum + tab[V, ]
            }
        }
    }
    if (type == "class") {
        if (n_obs == 1) {
            return(factor(lev[which.max(log_sum)], levels = lev))
        }
        if (n_features == 0) {
            return(factor(rep(lev[which.max(prior)], n_obs),
                          levels = lev))
        }
        else {
            return(factor(lev[max.col(log_sum, "first")], levels = lev))
        }
    }
    else {
        if (n_obs == 1) {
            post <- sapply(log_sum, function(x) { 1 / sum(exp(log_sum - x)) })
            mat <- t(as.matrix(post))
            colnames(mat) <- lev
            return(mat)
        }
        if (n_features == 0) {
            return(matrix(prior, ncol = n_lev, nrow = n_obs,
                          byrow = TRUE, dimnames = list(NULL, lev)))
        }
        else {
            return(apply(log_sum, 2, function(x) { 1 / rowSums(exp(log_sum - x)) }))
        }
    }
}
