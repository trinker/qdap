multigsub <-
function(pattern, replacement, x, ...){
    key <- data.frame(pat=pattern, rep=replacement, 
        stringsAsFactors = FALSE)
    msubs <-function(K, x, ...){
        sapply(seq_len(nrow(K)), function(i){
                x <<- gsub(K[i, 1], K[i, 2], x, ...)
            }
        )
        return(x)
    }
    x <- msubs(K=key, x=x, ...)
    return(x)
}

mgsub <- multigsub
