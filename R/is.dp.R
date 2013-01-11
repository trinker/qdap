#is.dp (double punctuation) internal function for checking/warning 
#extra punctuation
is.dp <- function(text.var) {
  punct <- c(".", "?", "!", "|")
  any(sapply(strsplit(text.var, NULL), function(x) {
    sum(x %in% punct) > 1
  }
  ))
}