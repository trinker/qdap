#' Break and Stretch if Multiple Persons per Cell 
#' 
#' Look for cells with multiple people and create separate rows for each person.
#' 
#' @param dataframe A dataframe that contains the person variable.
#' @param person.var The person variable to be stretched.
#' @param sep The separator(s) to search for and break on.  Default is: 
#' c("and", "&", ",")
#' @param track.reps logical.  If \code{TRUE} leaves the row names of person 
#' variable cells that were repeated and stretched.
#' @return Returns an expanded dataframe with person variable stretched and 
#' accompanying rows repeated.
#' @export
#' @examples
#' \dontrun{
#' DATA$person <- as.character(DATA$person)
#' DATA$person[c(1, 4, 6)] <- c("greg, sally, & sam", 
#'     "greg, sally", "sam and sally")
#' 
#' speakerSplit(DATA)
#' speakerSplit(DATA, track.reps=TRUE)
#' 
#' DATA$person[c(1, 4, 6)] <- c("greg_sally_sam", 
#'     "greg.sally", "sam; sally")
#' 
#' speakerSplit(DATA, sep = c(".", "_", ";"))
#' 
#' DATA <- qdap::DATA  #reset DATA
#' }
speakerSplit <- function(dataframe, person.var = 1, 
    sep=c("and", "&", ","), track.reps = FALSE) {
    if (!is.numeric(person.var)) {
        person.var <- which(colnames(dataframe) %in% person.var)
    } 
    locs <- which(rowSums(termco(dataframe[, person.var], 
        seq_along(dataframe[, person.var]), sep, 
        char.keep =sep)[[1]][, -c(1:2)]) > 0)
    ps <- ifelse(gsub("[[:punct:]]", "", sep) == "", "\\", "")
    newp <- lapply(dataframe[locs, person.var], function(x) {
        unblanker(Trim(unlist(strsplit(x, paste(paste0(ps, sep), 
        collapse="|")))))
    })
    lens <- unlist(lapply(newp, length))
    expa <- rep(1, nrow(dataframe))
    expa[locs] <- lens
    DF <- dataframe[rep(seq_len(dim(dataframe)[1]), expa), ]
    unlist(lapply(seq_along(locs), function(i){
        (locs[i] + cumsum(lens)[i]):(cumsum(lens)[i])
    }))
    w <- c(0, cumsum(lens - 1))
    a <- w[-length(w)] + locs
    b <- a + (lens - 1)
    locs2 <- unlist(lapply(seq_along(a), function(i) a[i]:b[i]))
    DF[locs2, person.var] <- unlist(newp)
    if (!track.reps) {
        rownames(DF) <- NULL
    } 
    DF
}
