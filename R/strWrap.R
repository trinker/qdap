strWrap <-
function(text = "clipboard", width = 70) {
    x <- if (text == "clipboard") {
         paste(readClipboard(), collapse=" ")
    } else {
        text
    }
    x <- gsub("\\s+", " ", gsub("\n|\t", " ", x))
    x <- strwrap(x, width = width)
    writeClipboard(x, format = 1)
    writeLines(strwrap(x, width = width))
}
