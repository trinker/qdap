#helper function for termco_d and termco (not exported)
term.count <-
function(str, mat){
    tester <- function(x, y){
        p <- suppressWarnings(unlist(gregexpr(x, y, fixed = TRUE)))
        j <- suppressWarnings(if(length(p) == 1 && (is.na(p) |  p < 1)) { 
                0 
            } else {
                length(p)
            }
        )
        return(j)
    }
    spacer <- function(string){
        sapply(string, function(x) paste0(" ", x, " "), USE.NAMES = FALSE)
    }
    str <- spacer(str)
    y <- sapply(mat, function(x) tester(x, str))
    return(y)
}
