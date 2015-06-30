#helper function for readability statisitics (not exported)
partition <-
function(x, k = 100){
    n <- length(x)
    if (sum(stats::na.omit(x)) < k) {
        ans <- rep(NA, n)
        return(factor(ans))
    }
    breaks <- c()
    repeat{
        spot <- suppressWarnings(min(which(cumsum(x) >= k)))
        if (!is.finite(spot)) {
            break 
        }
        x <- x[-c(1:spot)]
        breaks <- c(breaks, spot)
    }
    ans <- rep(NA, n)
    groups <- paste("sel_", rep(1:(length(breaks)), breaks), sep = "")
    ans[1:length(groups)] <- groups
    return(factor(ans))
}
