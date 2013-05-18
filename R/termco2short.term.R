#helper function for termco_d and termco (not exported)
termco2short.term <- 
function(termco.object, trim.terms = TRUE){
  CLASS <- class(termco.object)
  short.name <- function(df){
    mn <- gsub("(.*)\\)([^\\)]*)", "\\1\\2", colnames(df))
    colnames(df) <- Trim(gsub("term(", "", mn, fixed=TRUE))
    return(df)
  }
  o <- lapply(termco.object, function(x){
      if (is.data.frame(x)){
        short.name(x)
      } else {
        x
      }
    }
  )
  class(o) <- CLASS
  return(o)
}
