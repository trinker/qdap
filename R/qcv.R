qcv <- 
  function(..., terms = NULL, space.wrap = FALSE, trailing = FALSE, 
    leading = FALSE){
  if (!is.null(terms)) {
    x <- strsplit(terms, split=",")
  } else {
    x <- substitute(...())
  }
  z <- Trim(unlist(lapply(x, function(y) as.character(y))))
  if (space.wrap){
    z <- spaste(z)
  }      
  z <- spaste(z, trailing = trailing, leading = leading) 
  return(z)
}