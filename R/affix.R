affix <-
function(char.string, n.char, affix="suffix") {
    Trim <- function(x) gsub("^\\s+|\\s+$", "", x)
    ender <- function(x, s) {
        nc <- nchar(x)
        substring(x, (nc - s + 1), nc)
    }
    beginer <- function(x, s) substring(x, 1, s)
    if (affix == "suffix") {
        sapply(Trim(as.character(char.string)), function(x) ender(x, 
            n.char))
    } else {
        if (affix == "prefix") {
            sapply(Trim(as.character(char.string)), function(x) beginer(x, 
                n.char))
        } else {
            cat("\nWARNING:\nIncorrect affix argument!\n\n")
        }
    }
}
