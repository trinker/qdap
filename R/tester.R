#helper function for termco_d and termco (not exported)
tester <-
function(string, code, type="logical"){
    C <- as.character(substitute(code))
    test <- function(S, x) any(S==x)
    x <- sapply(string, function(x)test(x, C))
    if (type=="logical") {
        x 
    } else {
        if (type=="numeric") {
            which(x)
        } else {
            stop("type must be logical or numeric")
        }
    }
}
