spaste <- 
function(terms, left = FALSE){
  pas <- function(x) paste0(" ", x, " ")
  if (left) {
    pas <- function(x) paste0(" ", x)
  }
  if (is.list(terms)) {
    z <- lapply(terms, pas)
  } else {
    z <- pas(terms)
  }
  return(z)
}
