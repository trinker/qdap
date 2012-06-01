NAer <-
function(x, replace=0){
    if (is.vector(x)){
          x[is.na(x)] <- replace
          return(x )
    } else {
          y <- apply(x, 1, function(x) {x[is.na(x)] <- replace; return(x)})
          y <- data.frame(t(y))
          return(y)
    }
}
