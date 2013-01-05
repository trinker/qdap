#currently used as a helper function in: cm_distance (not currently exported)
cm_se2vect <- function(start, end, n.words = NULL, safety = TRUE) {
    dat <- data.frame(start =start, end = end)
    if (is.null(n.words)){
        n.words <- max(dat$end)
    }
    if (safety) {
        y <- dat[, "start"]
        if(sum(diff(y) < 0) > 0){
            stop("each element of the start column is not growing")
        }
    }
    x <- rep(0, n.words)
    x[unlist(lapply(1:nrow(dat), function(i) dat$start[i]:(dat$end[i])))] <- 1
    x
}
