#' Grab Begin/End of String to Character
#' 
#' \code{beg2char} - Grab from beginning of string to a character(s).
#' 
#' @param text.var, A character string
#' @param char The character from which to grab until/from.
#' @param noc Number of times the character appears before the grab.
#' @param include logical.  If \code{TRUE} includes the character in the grab.
#' @return returns a vector of text with char on/forward removed.
#' @author Josh O'Brien, Justin Haynes and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @references https://stackoverflow.com/q/15909626/1000343
#' @rdname beg2char
#' @export
#' @examples
#' \dontrun{
#' x <- c("a_b_c_d", "1_2_3_4", "<_?_._:")
#' beg2char(x, "_")
#' beg2char(x, "_", 2)
#' beg2char(x, "_", 3)
#' beg2char(x, "_", 4)
#' beg2char(x, "_", 3, include=TRUE)
#' 
#' char2end(x, "_")
#' char2end(x, "_", 2)
#' char2end(x, "_", 3)
#' char2end(x, "_", 4)
#' char2end(x, "_", 3, include=TRUE)
#' 
#' x2 <- gsub("_", " ", x)
#' char2end(x2, " ", 2)
#' beg2char(x2, " ", 2)
#' 
#' x3 <- gsub("_", "\\^", x)
#' char2end(x3, "^", 2)
#' beg2char(x3, "^", 2)
#' }
beg2char <- 
function(text.var, char = " ", noc = 1, include = FALSE) {

    ## escape special characters
    specchar <- c(".", "|", "(", ")", "[", "{", "^", "$", "*", "+", "?", "\\")
    if(char %in% specchar) {
        char <- paste0("\\", char)
    }
    if (!include) {
        matchToNth <- function(char, n) {
            others <- paste0("[^", char, "]*") ## matches "[^_]*" if char is "_"
            mainPat <- paste0(c(rep(c(others, char), n-1), others), collapse="")
            paste0("(^", mainPat, ")", "(.*$)")
        }
        gsub(matchToNth(char, noc), "\\1", text.var)
    } else {
        inc <- ifelse(include, char, "?")
    
        ins <- paste(rep(paste0(char, ".+"), noc - 1), collapse="")
        rep <- paste0("^(.+", ins, inc, ").*$")
        if (noc == 1) {
            rep <- paste0("(", char, ")", ".*$")
        }
        gsub(rep, "\\1", text.var)
    }
}


#' Grab Begin/End of Sting to Character
#' 
#' \code{char2end} - Grab from character(s) to end of string.
#' 
#' @rdname beg2char
#' @export
char2end <- function(text.var, char = " ", noc = 1, include = FALSE) {
    inc <- ifelse(include, char, "")
    specchar <- c(".", "|", "(", ")", "[", "{", "^", "$", "*", "+", "?", "\\")
    if(char %in% specchar) {
        char <- paste0("\\", char)
    }
    ins <- paste(rep(paste0(char, ".*"), noc - 1), collapse="")
    rep <- paste0("^.*", ins, paste0("?", char), "(.*)$")
    paste0(inc, gsub(rep, "\\1", text.var))
}
