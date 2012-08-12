#' Merge Demogrphic Information with Person/Text Transcript
#' 
#' Wrapper function for merging demogrphic information with person/text transcript
#' 
#' @param transcript.df The text/person transcript dataframe
#' @param key.df The demographic dataframe.
#' @param common.column The column(s) shared by transcript.ef and key.df.  If NULL function defaults to use any columns with the same name.
#' @param defualt.arrange logical.  If TRUE will arrange the columns with text to the far right.
#' @return Outputs a merged transcript dataframe with demographic information.
#' @keywords merge, demographic
#' @seealso 
#' \code{\link[base]{merge}},
#' @examples
#'merged.raj <- key_merge(raj, raj.demographics)
#'htruncdf(merged.raj, 10, 40)
#' 
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