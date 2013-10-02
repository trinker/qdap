#' Find Duplicated Words in a Text String
#' 
#' Find duplicated word/word chunks in a string.  Intended for internal use.
#' @param string A character string.
#' @param threshold An integer of the minimal number of repeats.
#' @return Returns a vector of all duplicated words/chunks.
#' @export
#' @examples
#' \dontrun{
#' duplicates(DATA$state)
#' duplicates(DATA$state[1])
#' }
duplicates <- #used in trans_venn
function(string, threshold=1){
    x<-sort(unlist(strsplit(string, " ")))
    if (threshold > 1) {
        names(table(x))[table(x) >= threshold]
    } else {
        unique(x[duplicated(x)])
    }
}
