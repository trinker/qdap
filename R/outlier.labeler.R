outlier.labeler <- function(x, standardize=TRUE){
    if (standardize) x <- scale(x)
    y <- ifelse(abs(x) >= 3, "3sd", 
        ifelse(abs(x) >= 2 & abs(x) < 3, "2sd", 
        ifelse(abs(x) >= 1.5 & abs(x) < 2, 
        "1.5sd", "-")))
    return(y)
}