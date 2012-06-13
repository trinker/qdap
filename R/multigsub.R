multigsub <-
function(pattern, replacement, text.var, ...){
    key <- data.frame(pat=pattern, rep=replacement, 
        stringsAsFactors = FALSE)
    msubs <-function(K, x, ...){
        sapply(seq_len(nrow(K)), function(i){
                x <<- gsub(K[i, 1], K[i, 2], x, ...)
            }
        )
        return(x)
    }
    x <- msubs(K=key, x=text.var, ...)
    return(x)
}

mgsub <- multigsub
