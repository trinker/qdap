incomplete.replace <-
function(text.var, scan.mode = FALSE) {
  pat <- "\\?*\\?[.]+|[.?!]*\\? [.][.?!]+|[.?!]*\\. [.?!]+|
        [.?!]+\\. [.?!]*|[.?!]+\\.[.?!]*|[.?!]*\\.[.?!]+"
    if (scan.mode) {
      wid <- options()$width
      options(width = 10000)
      sel <- grepl(pat, scrubber(text.var))
      x <- data.frame(row.num = which(sel), 
                      text = as.character(text.var[sel]))
      print(left.just(x, 2))
      options(width = wid)
    } else {
      x <- gsub(pat, "|", scrubber(text.var))
      return(x)
    }
}
incomp <- incomplete.replace