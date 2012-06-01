bracketXtract <-
function(txt, br = c("(", "[", "{", "all"), with=FALSE){
    br <- match.arg(br)
    left <- if ("all" == br) {
        "\\(|\\{|\\["
    } else {
        sprintf("\\%s", br)
    }
    map <- c(`\\(`="\\)", `\\[`="\\]", `\\{`="\\}",
        `\\(|\\{|\\[`="\\)|\\}|\\]")
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
