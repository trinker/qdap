#' Find Duplicated Words in a Text String
#' 
#' Find duplicated word/word chunks in a string.  Intended for internal use.
#' @param string A character string.
#' @param threshhold An interger of the minimal number of repeats.
#' @return Returns a vector of all duplicated words/chunks.
#' @examples
#' \dontrun{
#' duplicates(DATA$state)
#' duplicates(DATA$state[1])
#' }
duplicates <- #used in trans.venn
function(string, threshhold=1){
    x<-sort(unlist(strsplit(string, " ")))
    if (threshhold > 1) {
        names(table(x))[table(x) >= threshhold]
    } else {
        unique(x[duplicated(x)])
    }
}