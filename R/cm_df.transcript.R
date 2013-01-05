#' Transcript With Word Number
#' 
#' Out put a transcript with word  number/index above for easy input back into 
#' qdap after coding.
#' 
#' @param text.var text.var The text variable
#' @param grouping.var The grouping variables.  Default NULL generates one word 
#' list for all text.  Also takes a single grouping variable or a list of 1 or 
#' more grouping variables.
#' @param file A connection, or a character string naming the file to print to 
#' (e.g. .doc, .txt).
#' @param indent Number of spaces to indent.
#' @param width Width to output the file (defaults to 70; this is generally a 
#' good width and indent for a .docx file).
#' @return Returns a transcript by grouping variable with word number above each 
#' word.  This makes use with cm_df2long transfer/usage easier because the 
#' researcher has coded on a transcript with the numeric word index already.
#' @note It is recommended that the researcher actually codes on the out put 
#' from this file.  If a file already exists cm_df.transcript will append to 
#' that file.
#' @author DWin, Gavin Simpson and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @seealso See Also as \code{\link[qdap]{cm_df2long}}
#' See Also as \code{\link[qdap]{cm_df.temp}}
#' @keywords transcript
#' @export
#' @examples
#' \dontrun{
#' with(mraja1spl, cm_df.transcript(dialogue, list(person)))
#' with(mraja1spl, cm_df.transcript(dialogue, list(sex, fam.aff, died)))
#' with(mraja1spl, cm_df.transcript(dialogue, list(person), file="foo.doc"))
#' # delete("foo.doc")   #delete the file just created
#' }
cm_df.transcript <-
function(text.var, grouping.var, file = NULL, indent = 4, width = 70){
    if (is.list(grouping.var)) {
        grouping.var <- paste2(grouping.var)
    }
    L2 <- sentCombine(text.var, grouping.var)
    DF <- data.frame(group = names(L2), text=unlist(L2))
    DF2 <- cm_df.temp(DF, "text")
    y <- rle(as.character(DF2$group))
    lens <- y$lengths
    group <- y$values
    x <- cumsum(lens)
    L3 <- split(DF2, as.factor(rep(seq_along(x), lens)))
    invisible(lapply(seq_along(L3), function(i){
        numbtext(L3[[i]][, "text"], width = width, 
        lengths = L3[[i]][, "word.num"], txt.file=file, 
        name=group[i], indent=indent)
    }))
}
