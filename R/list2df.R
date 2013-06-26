## Internal function to convert a list of named vectors into a data.frame
list2df <- function(list.object, col1 = "X1", col2 = "X2") {
    dat <- data.frame(x = unlist(list.object, ,FALSE), 
        y = rep(names(list.object), sapply(list.object, length)), 
        stringsAsFactors = FALSE)
    colnames(dat) <- c(col1, col2)
    dat
}