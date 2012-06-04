multi.scale <-
function(numeric.var, grouping.var, order.by = "original", 
    digits = 2) {
    G <- as.character(substitute(grouping.var))
    G <- G[length(G)]
    N <- as.character(substitute(numeric.var))
    N <- N[length(N)]
    X <- data.frame(ID = seq_along(numeric.var), group = grouping.var, 
        num = numeric.var)
    X <- X[order(X$group), ]
    X$scale.by.group <- unlist(aggregate(num ~ group, X, scale)$num)
    X$outlier.by.group <- 
        ifelse(abs(X$scale.by.group) >= 3, "3 sd", 
        ifelse(abs(X$scale.by.group) >= 2 & abs(X$scale.by.group) < 3, 
            "2 sd", 
        ifelse(abs(X$scale.by.group) >= 1.5 & abs(X$scale.by.group) < 2, 
            "1.5 sd", "")
        )
    )
    X$scale.by.all <- scale(X$num)
    X$outlier.for.all <- 
        ifelse(abs(X$scale.by.all) >= 3, "3 sd", 
        ifelse(abs(X$scale.by.all) >= 2 & abs(X$scale.by.all) < 3, "2 sd", 
        ifelse(abs(X$scale.by.all) >= 1.5 & abs(X$scale.by.all) < 2, 
            "1.5 sd", "")
        )
    )
    msd <- function(x, digits) {
        MSD <- c(mean = mean(x), sd = sd(x), n = length(x), total = sum(x))
        round(MSD, digits)
    }
    mean.sd.n <- aggregate(num ~ group, X, function(x) msd(x, 
        digits = digits))
    mean.sd.n$group <- as.character(mean.sd.n$group)
    ALL <- c("ALL", msd(numeric.var, digits = digits))
    mean.sd.n <- rbind(as.matrix(mean.sd.n), ALL)
    rownames(mean.sd.n) <- 1:nrow(mean.sd.n)
    mean.sd.n <- as.data.frame(mean.sd.n)
    X <- switch(order.by, 
        original = X[order(X$ID), ], 
        group = X
    )
    X$ID <- NULL
    colnames(mean.sd.n) <- c(G, "mean", "sd", "n.turns", "total")
    names(X) <- c(G, N, names(X)[-c(1:2)])
    list(SCALED_OBSERVATIONS = X, DESCRIPTIVES_BY_GROUP = mean.sd.n)
}
