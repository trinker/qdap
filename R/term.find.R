#helper function for word.associate, termco_d and termco(not exported)
term.find <-
function(str, mat, logic = FALSE, unlist = FALSE, apostrophe.remove = FALSE, 
    char.keep = "~~", digit.remove = FALSE, ...){
    tester <- function(x, y, logic){
        p <- suppressWarnings(unlist(grepl(x, y, fixed = TRUE)))
        if (!logic) {
            p <- which(p)
        }
        return(p)
    }
    spacer <- function(string){
        sapply(string, function(x) paste0(" ", x, " "), 
            USE.NAMES = FALSE)
    }
    str <- spacer(strip(str, lower.case = FALSE, char.keep = char.keep, 
        apostrophe.remove = apostrophe.remove, digit.remove = digit.remove, ...))
    findit1 <- function(x, logic = TRUE) {
        sapply(x, function(z) tester(z, str, logic))
    }
    findit2 <- function(x, logic = FALSE) {
        sort(unique(c(unlist(sapply(x, function(z) {
            tester(z, str, logic)})))))
    }
    if (logic) {
        findit <- findit1
    } else {
        findit <- findit1
    }
    if (is.list(mat)) {
        a <- lapply(mat, findit, logic = logic)
    } else {
        a <- findit(mat, logic = logic)
    }
    if (unlist) {
        a <- unlist(a)
    }
    a
}
