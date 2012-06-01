term.count <-
function(str, mat){
    tester <- function(x, y){
        p <- suppressWarnings(unlist(gregexpr(x, y, fixed = FALSE)))
        j <- suppressWarnings(if(is.na(str) | length(p) == 1 & p<1) { 
                0 
            } else {
                length(p)
            }
        )
        return(j)
    }
    y <- sapply(mat, function(x) tester(x, str))
    return(y)
}
