predict.naive_bayes <- function(object, newdata = NULL, type = c("class", "prob"),
                                threshold = 0.001, ...) {
    
    if (is.null(newdata)) newdata <- object$data$x
    else newdata <- as.data.frame(newdata)
    na <- sapply(newdata, anyNA)
    type <- match.arg(type)
    lev <- object$levels
    n_lev <- length(lev)
    n_obs <- dim(newdata)[1L]
    usekernel <- object$usekernel
    prior <- as.double(object$prior)
    tables <- object$tables
    features <- names(newdata)[names(newdata) %in% names(tables)]
    log_sum <- 0
    
    for (var in features) {
        V <- newdata[[var]]
        if (is.numeric(V)) {
            tab <- tables[[var]]
            if (usekernel) {
                p <- sapply(lev, function(z) {
                    dens <- tab[[z]]
                    stats::approx(dens$x, dens$y, xout = V, rule = 2)$y
                })
                p[p == 0] <- threshold
                if (na[var]) p[is.na(p)] <- 1
                log_sum <- log_sum + log(p)
            } else {
                dimnames(tab) <- NULL
                s <- tab[2, ]
                s[s == 0] <- threshold
                p <- sapply(seq_along(lev), function(z) {
                    stats::dnorm(V, tab[1, z], s[z])
                })
                p[p == 0] <- threshold
                if (na[var]) p[is.na(p)] <- 1
                log_sum <- log_sum + log(p)
            }
        } else {
            tab <- tables[[var]]
            if (class(V) == "logical") V <- as.character(V)
            if (na[var]) {
                na_ind <- which(is.na(V))
                V[na_ind] <- attributes(tab)$dimnames[[1]][1]
                p <- tab[V, ]
                if (n_obs == 1) p <- 1
                else p[na_ind, ] <- 1
            } else {
                p <- tab[V, ]
            }
            log_sum <- log_sum + log(p)
        }
    }
    if (type == "class") {
        if (n_obs == 1) {
            post <- log_sum + log(prior)
            return(factor(lev[which.max(post)], levels = lev))
        } else {
            post <- t(t(log_sum) + log(prior))
            return(factor(lev[max.col(post, "first")], levels = lev))
        }
    } else {
        if (n_obs == 1) {
            lik <- exp(log_sum + log(prior))
            post <- sapply(lik, function(prob) {
                prob / sum(lik)
            })
            mat <- t(as.matrix(post))
            colnames(mat) <- lev
            return(mat)
        } else {
            lik <- exp(t(t(log_sum) + log(prior)))
            dimnames(lik) <- NULL
            rs <- rowSums(lik)
            colnames(lik) = lev
            return(apply(lik, 2, function(prob) {
                prob / rs
            }))
        }
    }
}