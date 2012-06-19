stemmer <-
function(text.var, rm.bracket = TRUE, capitalize = TRUE, 
    warn = TRUE, ...){
    require(tm)
    txt <- as.character(text.var)
    if (rm.bracket){
        txt <- bracketX(txt)
    }
    nc <- nchar(txt)
    end.mark1 <- substring(txt, nc, nc)
    end.mark2 <- substring(txt, nc - 1, nc)
    sent.type <- ifelse(end.mark2 %in% c("*.", "*?", "*!", "*|"), end.mark2,
        ifelse(end.mark1 %in% c(".", "?", "!", "|"), end.mark1,    
        ifelse(is.na(end.mark1), NA, "")))
    bl <- sent.type == ""
    if (any(na.omit(bl)) & warn) {
        warning(paste(
        "The following row(s) do have standard qdap punctuation endmarks:\n", 
        " rows:", which(bl)))
    }
    LIST <- stopwords(txt, stopwords = NULL, strip = TRUE)
    LIST <- lapply(LIST, function(x) ifelse(is.na(x),
            NA, tm::stemDocument(x)))
    if (capitalize){
        LIST <- lapply(LIST, function(x) capitalizer(x, ...))
    }
    if (length(text.var) == 1) {
        if (!is.na(text.var)) {
            LIST <- paste2(LIST, sep=" ")
        }
    }
    txt2 <- paste2(list(lapply(LIST, paste2, sep=" "), sent.type), sep="")
    if (capitalize){
        capit <- function (string) {
            if (is.na(string)) {
                NA
            } else {
                nc <- nchar(string)
                paste0(toupper(substring(string, 1, 1)), substr(string, 2, nc))
            }
        }
        txt2 <- unlist(lapply(Trim(txt2), capit))
    }
    return(txt2)
}

stem.words <- 
function(...) {
  stemmer(c(...),capitalize = FALSE, warn = FALSE)
}
