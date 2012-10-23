#' Extract Bracketted Text
#' 
#' Transcript Apply Extraction of Bracket Encased Text
#' 
#' @param text.var The text variable
#' @param bracket The type of bracketted text to extract.  This is one of 
#' the strings "curly", "square", "round", "angle" and "all".  
#' These strings correspond to: {, [, (, < or all four types.
#' @param with logical.  If TRUE returns the brackets and the bracketted text.
#' @return Returns a list of vectors of bracketed text.
#' @author  Martin Morgan and Tyler Rinker <tyler.rinker@gmail.com>.
#' @seealso \code[qdap]{\link{bracketX}}
#' @references \url{http://stackoverflow.com/questions/8621066/remove-text-inside-brackets-parens-and-or-braces}
#' @keywords bracket extract
#' @examples
#' examp2 <- examp2 <- structure(list(person = structure(c(1L, 2L, 1L, 3L), 
#'     .Label = c("bob", "greg", "sue"), class = "factor"), text = 
#'     c("I love chicken [unintelligible]!", 
#'     "Me too! (laughter) It's so good.[interupting]", 
#'     "Yep it's awesome {reading}.", "Agreed. {is so much fun}")), .Names = 
#'     c("person", "text"), row.names = c(NA, -4L), class = "data.frame")      
#' 
#' examp2                                                              
#' bracketX(examp2$text, 'square')  
#' bracketX(examp2$text, 'curly')  
#' bracketX(examp2$text)  
#'                                               
#' examp2                                              
#' bracketXtract(examp2$text, 'square')  
#' bracketXtract(examp2$text, 'curly')  
#' bracketXtract(examp2$text)  
#' 
#' paste2(bracketXtract(examp2$text, 'curly'), " ")
bracketXtract <-
function(text.var, bracket = "all", with = FALSE){   
    br <- bracket
    br <- ifelse(br=="round", "(", 
        ifelse(br=="square", "[", 
        ifelse(br=="curly", "{",
        ifelse(br=="html", "<",
        ifelse(br=="angle", "<", br)))))
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
    if(length(text.var)==1){
        unlist(regmatches(text.var, gregexpr(re, text.var, perl=TRUE)))
    }else{  
        regmatches(text.var, gregexpr(re, text.var, perl=TRUE)) 
    }
}

