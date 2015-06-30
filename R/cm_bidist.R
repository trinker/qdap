#helper function used in cm_distance(not for export) 
cm_bidist <-
function(code_x, code_y, grouping.var = NULL) {
    x <- cm_describe(code_x, grouping.var)
    x <- x[as.numeric(as.character(x[, "code_x"])) > 0, ]
    y <- cm_describe(code_y, grouping.var)
    y <- y[as.numeric(as.character(y[, "code_y"])) > 0, ]
    Dnc <- sapply(1:nrow(x), function(i) {
            yind <- 1:nrow(y)
            if (sum(y[, "start"] >= x[i, "start"] & y[, "start"] <= x[i, "end"]) > 0 |
                sum(y[, "end"] >= x[i, "start"] & y[, "end"] <= x[i, "start"]) > 0 |
                sum(sapply(yind, function(j) {
                        y[j, "start"] < x[i, "start"] & y[j, "end"] > x[i, "end"]
                    }
                )) > 0) {
                return(0)
            }
            sdif <- c(y[, "start"], y[, "end"]) - x[i, "start"]
            edif <- c(y[, "start"], y[, "end"]) - x[i, "end"]
            min(abs(c(sdif, edif)))
        }
    )
    Dc <- sapply(1:nrow(x), function(i) {
        FUN <- function(xstart, xend, ystart){
             max(0, min(ystart[ystart > xstart] - xend))
        }
        suppressWarnings(FUN(x[i, "start"], x[i, "end"], y[, "start"]))
        }
    )
    Dc[is.infinite(Dc)] <- NA
    v <- list(associated_distance = Dnc, mean.sd.n_assoc_dist = c(mean(stats::na.omit(Dnc)), 
        stats::sd(stats::na.omit(Dnc)), length(stats::na.omit(Dnc))), causal_distance = Dc, 
        mean.sd.n_causal_dist = c(mean(stats::na.omit(Dc)), stats::sd(stats::na.omit(Dc)), length(stats::na.omit(Dc))))
    v2 <- invisible(lapply(v, function(x){
        x[is.nan(x)] <- NA
        return(x)
    })) 
    return(v2)
}
