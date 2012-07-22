spaste <- 
function(terms, leading = TRUE, trailing = TRUE){
  if (leading) {
    s1 <- " "
  } else {
    s1 <- ""
  }
  if (trailing) {
    s2 <- " "
  } else {
    s2 <- ""
  }
  pas <- function(x) paste0(s1, x, s2)
  if (is.list(terms)) {
    z <- lapply(terms, pas)
  } else {
    z <- pas(terms)
  }
  return(z)
}