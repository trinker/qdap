#internal function to make environments for data sets
qenvir <- 
function(dat, mode.out = "character") {
    hash <- function(x, mode.out) {
        evnt <- new.env(hash = TRUE, size = nrow(x), 
            parent = emptyenv())
        FUN <- paste0("as.", mode.out)
        FUN <- match.fun(FUN)
        apply(x, 1, function(col) {
            assign(col[1], FUN(col[2]), envir = evnt)
        })
        return(evnt)
    }  
    hash(dat, mode.out = mode.out)                                                                   
}
#env.syn <- qenvir(SYNONYM)
#exists("color", envir=env.syn)