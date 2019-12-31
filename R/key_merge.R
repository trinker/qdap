#' Merge Demographic Information with Person/Text Transcript
#' 
#' Wrapper function (\code{\link[base]{merge}}) for merging demographic 
#' information with a person/text transcript.
#' 
#' @param transcript.df The text/person transcript dataframe
#' @param key.df The demographic dataframe.
#' @param common.column The column(s) shared by \code{transcript.df} and 
#' \code{key.df}.  If \code{NULL} function defaults to use any columns with the 
#' same name.
#' @param defualt.arrange logical.  If \code{TRUE} will arrange the columns with 
#' text to the far right.
#' @return Outputs a merged transcript dataframe with demographic information.
#' @seealso \code{\link[base]{merge}}
#' @export
#' @examples
#' \dontrun{
#' #First view transcript dataframe and demographics dataframe.
#' ltruncdf(list(raj, raj.demographics), 10, 50)
#' merged.raj <- key_merge(raj, raj.demographics)
#' htruncdf(merged.raj, 10, 40)
#' }
key_merge <-
function(transcript.df, key.df, common.column = NULL, 
    defualt.arrange = TRUE) {
    transcript.df$IDCOL <- 1:nrow(transcript.df)
    if (!is.null(common.column)) {
        if (length(common.column) == 1) {
            CC <- c(common.column,  common.column) 
        } else {
            if (length(common.column) == 2) {
                CC <- c(common.column[1],  common.column[2])    
            } else {
                stop("common.column must be off length 1 or 2")
            }         
        }
    } else {
        cc <- colnames(transcript.df)
        CC <- cc[cc %in% colnames(key.df)]
    }
    DF <- merge(transcript.df, key.df, by = CC, 
        incomparables = NA)
    if (defualt.arrange) {
        DF <- DF[, c(1, 3:ncol(DF), 2)]
    }
    DF <- DF[order(DF$IDCOL), ]
    DF$IDCOL <- NULL
    rownames(DF) <- NULL
    return(DF)
}
