qcv <- 
function(..., space.wrap = FALSE, sep = ",", fixed = TRUE){
  x <- substitute(...())
  z <- Trim(unlist(lapply(x, function(y) as.character(y)))  )
  if (space.wrap){
    z <- spaste(z)
  }      
  return(z)
}