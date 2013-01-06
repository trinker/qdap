#helper function for termco_d and termco_a (not exported)
term.find <-
function(str, mat, logic = FALSE){
  tester <- function(x, y, logic){
    p <- suppressWarnings(unlist(grepl(x, y, fixed = TRUE)))
    if (!logic) {
        p <- which(p)
    }
    return(p)
  }
  spacer <- function(string){
    sapply(string, function(x) paste0(" ", x, " "), USE.NAMES = FALSE)
  }
  str <- spacer(strip(str, lower.case = FALSE))
  if (logic) {
      findit <- function(x, logic = TRUE) {
        sapply(x, function(z) tester(z, str, logic))
      }
  } else {
      findit <- function(x, logic = FALSE) {
        sort(unique(c(unlist(sapply(x, 
          function(z) tester(z, str, logic))))))
      }
  }
  if (is.list(mat)) {
    a <- lapply(mat, findit, logic = logic)
  } else {
    a <- findit(mat, logic = logic)
  }
  return(a)
}