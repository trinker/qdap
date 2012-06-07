#' Transcript Apply of Extraction the Information Inside Brackets
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param txt %% ~~Describe \code{txt} here~~
#' @param br %% ~~Describe \code{br} here~~
#' @param with %% ~~Describe \code{with} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (txt, br = c("(", "[", "{", "all"), with = FALSE) 
#' {
#'     br <- match.arg(br)
#'     left <- if ("all" == br) {
#'         "\(|\{|\["
#'     }
#'     else {
#'         sprintf("\%s", br)
#'     }
#'     map <- c(`\(` = "\)", `\[` = "\]", `\{` = "\}", `\(|\{|\[` = "\)|\}|\]")
#'     fmt <- if (with == TRUE) {
#'         "(%s).*?(%s)"
#'     }
#'     else {
#'         "(?<=%s).*?(?=%s)"
#'     }
#'     re <- sprintf(fmt, left, map[left])
#'     if (length(txt) == 1) {
#'         unlist(regmatches(txt, gregexpr(re, txt, perl = TRUE)))
#'     }
#'     else {
#'         regmatches(txt, gregexpr(re, txt, perl = TRUE))
#'     }
#'   }
#' 
bracketXtract <-
function(txt, br = c("(", "[", "<", "{", "all"), with=FALSE){
    br <- match.arg(br)
    left <- if ("all" == br) {
        "\\(|\\{|<|\\["
    } else {
        sprintf("\\%s", br)
    }
    map <- c(`\\(`="\\)", `\\[`="\\]", `\\{`="\\}",
             `\\<`="\\>", `\\(|\\{|<|\\[`="\\)|\\}|\\>|\\]")
    fmt <- if (with==TRUE) {
        "(%s).*?(%s)"
    } else {
        "(?<=%s).*?(?=%s)"
    }
    re <- sprintf(fmt, left, map[left])
    if(length(txt)==1){
        unlist(regmatches(txt, gregexpr(re, txt, perl=TRUE)))
    }else{  
        regmatches(txt, gregexpr(re, txt, perl=TRUE)) 
    }
}

