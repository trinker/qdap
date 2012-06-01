qda.capitalizer <-
function(qda.obj, caps.list=NULL){
    CAPPER <- function(Y, caps.list) {
        Y[, 1] <- sapply(Y[, 1], function(x) capitalizer(x, caps.list), 
            USE.NAMES = FALSE)
        Y
    }
    CAPPER2 <- function(Y, caps.list) {
        Y <- sapply(Y, function(x) capitalizer(x, caps.list), USE.NAMES = FALSE)
        Y
    }
    if (is.null(comment(X))) {
            z <- lapply(qda.obj, function(x) CAPPER2(x, caps.list=caps.list))
            return(z, warning("not a qda object"))
    } else{
        if (comment(X)%in%c("fwl", "fswl", "rfswl")) {
            z <- lapply(qda.obj, function(x) CAPPER(x, caps.list=caps.list))
            return(z)
        } else {
            if (comment(X)%in%c("cwl", "swl")) {
                z <- lapply(qda.obj, function(x) CAPPER2(x, caps.list=caps.list))
                return(z)
            }
        }
    }
}
