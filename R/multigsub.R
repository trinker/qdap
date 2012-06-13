multigsub <-
function(pattern, replacement, text.var, spacer = TRUE, ...){
    if (spacer) {
        spc <- function(y) {
            paste0(" ", y, " ")
        }
        replacement <- spc(replacement)
    }
    key <- data.frame(pat=pattern, rep=replacement, 
        stringsAsFactors = FALSE)
    msubs <-function(K, x, ...){
        sapply(seq_len(nrow(K)), function(i){
                x <<- gsub(K[i, 1], K[i, 2], x, ...)
            }
        )
       return(gsub(" +", " ", x))
    }
    x <- Trim(msubs(K=key, x=text.var, ...))
    return(x)
}

mgsub <- multigsub